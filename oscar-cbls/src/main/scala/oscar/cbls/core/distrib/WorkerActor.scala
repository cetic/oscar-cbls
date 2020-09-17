package oscar.cbls.core.distrib

import java.util.concurrent.Executors

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import org.slf4j.{Logger, LoggerFactory}
import oscar.cbls.core.computation.{Solution, Store}
import oscar.cbls.core.search.{MoveFound, Neighborhood, NoMoveFound}

import scala.collection.SortedMap
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

case class SearchRequest(neighborhoodID:RemoteNeighborhoodIdentification,
                         acc:(Long,Long) => Boolean,
                         obj:IndependentOBj,
                         startSolution:IndependentSolution){
  override def toString: String = s"SearchRequest($neighborhoodID,$acc,$obj)"
}


case class SearchTask(request:SearchRequest,
                      searchId:Long,
                      workGiver:ActorRef[MessageToWorkGiver]){
  override def toString: String = s"SearchTask($request,$searchId,${workGiver.path})"
}

sealed trait MessageToWorker
final case class StartSearch(search:SearchTask, startID:Long, replyTo:ActorRef[MessagesToSupervisor]) extends MessageToWorker
final case class AbortSearch(searchId:Long) extends MessageToWorker
final case class WrappedSearchEnded(result: SearchEnded) extends MessageToWorker
final case class ShutDownWorker() extends MessageToWorker
final case class Ping(replyTo:ActorRef[Unit]) extends MessageToWorker


object WorkerActor {
  def startWorkerAndActorSystem(neighborhoods:SortedMap[Int,RemoteNeighborhood],
                                m:Store,
                                master:ActorRef[MessagesToSupervisor],
                                workerName:String = "worker",
                                verbose:Boolean):ActorSystem[MessageToWorker] = {

    val  startLogger:Logger = LoggerFactory.getLogger("WorkerObject")
    startLogger.info("Starting actor system and worker:" + workerName)
    ActorSystem(createWorkerBehavior(neighborhoods,m,master,verbose), workerName)
  }

  def spawnWorker(neighborhoods:SortedMap[Int,RemoteNeighborhood],
                  m:Store,
                  master:ActorRef[MessagesToSupervisor],
                  context:ActorContext[_],
                  workerName:String = "worker",
                  verbose:Boolean) :ActorRef[MessageToWorker] = {
    context.spawn(createWorkerBehavior(neighborhoods,m,master,verbose),workerName)
  }

  def createWorkerBehavior(neighborhoods:SortedMap[Int,RemoteNeighborhood],
                           m:Store,
                           master:ActorRef[MessagesToSupervisor],
                           verbose:Boolean=false):Behavior[MessageToWorker] = {
    new WorkerActor(neighborhoods, m, master, verbose).initBehavior()
  }

  val nbCores:Int = Runtime.getRuntime.availableProcessors()
}

class WorkerActor(neighborhoods:SortedMap[Int,RemoteNeighborhood],
                  m:Store,
                  master:ActorRef[MessagesToSupervisor],
                  verbose:Boolean) {

  //This is the single thread that is ready to perform all computation.
  // There is at most one computation per worker at any point in time, so the threadPool is 1.
  private val executorForComputation = Executors.newFixedThreadPool(1)
  private val executionContextForComputation: scala.concurrent.ExecutionContext = ExecutionContext.fromExecutor(executorForComputation)

  //this is a shared variable. it is not good, but that's the only way to send the abort signal to the Future that contains the computation.
  //Scala requires the final and volatile flags
  @volatile
  private final var shouldAbortComputation:Boolean = false

  @volatile
  private final var nbExploredNeighborhoods:Int = 0

  @volatile
  private final var nbAbortedNeighborhoods:Int = 0

  private def doSearch(searchRequest:SearchRequest):IndependentSearchResult = {
    searchRequest.startSolution.makeLocal(m).restoreDecisionVariables()
    shouldAbortComputation = false
    val neighborhood = neighborhoods(searchRequest.neighborhoodID.neighborhoodID)
    neighborhood.explore(searchRequest.neighborhoodID.parameters, searchRequest.obj.convertToOBj(m), searchRequest.acc, shouldAbort = () => shouldAbortComputation) match{
      case NoMoveFound => IndependentNoMoveFound()
      case MoveFound(m) => IndependentMoveFound(m.getIndependentMove(this.m))
    }
  }

  abstract class WorkerState
  case class IAmBusy(search:SearchTask) extends WorkerState
  case class Aborting(search:SearchTask) extends WorkerState
  case class Idle() extends WorkerState
  case class ShuttingDown() extends WorkerState

  def initBehavior():Behavior[MessageToWorker] = {
    Behaviors.setup { context =>
      master ! NewWorkerEnrolled(context.self)
      if(verbose) context.log.info(s"ready for work nbNeighborhoods:${neighborhoods.size}")
      next(Idle())
    }
  }

  private def next(state:WorkerState): Behavior[MessageToWorker] = {
    Behaviors.receive { (context, command) =>
      command match {
        case Ping(replyTo) =>
          replyTo ! Unit
          Behaviors.same

        case StartSearch(newSearch,startID,replyTo) =>
          state match {
            case ShuttingDown() =>
              Behaviors.same
            case IAmBusy(search) =>
              //we do not tell anything to the workGiver; the supervisor will have to find another slave
              if(verbose) context.log.info(s"got command for start search:${newSearch.searchId} but already busy")

              replyTo ! SearchNotStarted(newSearch,context.self)
              Behaviors.same

            case Aborting(search) =>
              if(verbose) context.log.info(s"got command for start search:${newSearch.searchId} but already busy aborting a search")

              replyTo ! SearchNotStarted(newSearch,context.self)
              Behaviors.same

            case Idle() =>
              if(verbose) context.log.info(s"starting search:${newSearch.searchId}")

              this.nbExploredNeighborhoods += 1

              val futureResult = Future {
                //TODO: the abort does not work; Future is probably not the way to go here.
                //TODO: how about startSol, obj and acceptance?
                SearchCompleted(newSearch.searchId,doSearch(newSearch.request))
              }(executionContextForComputation)

              context.pipeToSelf(futureResult) {
                // map the Future value to a message, handled by this actor
                case Success(s) => WrappedSearchEnded(s)
                case Failure(e) =>
                  WrappedSearchEnded(SearchCrashed(newSearch.searchId, newSearch.request.neighborhoodID, e, context.self))
              }
              replyTo ! SearchStarted(newSearch, startID, context.self)
              next(IAmBusy(newSearch))
          }

        case AbortSearch(searchId:Long) =>
          state match {
            case ShuttingDown() =>
              Behaviors.same
            case IAmBusy(search) =>
              if (searchId == search.searchId) {
                if(verbose) context.log.info(s"got abort command for search:$searchId; aborting")
                shouldAbortComputation = true //shared variable
                nbAbortedNeighborhoods += 1
                next(Aborting(search))
              }else{
                if(verbose) context.log.info(s"got abort command for search:$searchId but busy on search:${search.searchId}; ignoring")

                //otherwise, ignore.
                Behaviors.same
              }

            case Aborting(search) =>
              //this is an error; ignore
              if(verbose) context.log.info(s"got abort command for search:$searchId; and was already aborting from ${search.searchId}; ignoring")

              Behaviors.same

            case Idle() =>
              //this is possibly an error
              if(verbose) context.log.info(s"got abort command for search:$searchId; and was idle; ignoring")

              Behaviors.same
          }

        case WrappedSearchEnded(result) =>
          // send result to work giver

          state match {
            case IAmBusy(search) =>
              require(search.searchId == result.searchID)
              if(verbose) context.log.info(s"finished search:${search.searchId}, sending result $result to ${search.workGiver.path}")
              search.workGiver ! result

              result match{
                case _:SearchCrashed =>
                  master!Crash(context.self)
                case _ => ;
              }

              master ! ReadyForWork(context.self,Some(search.searchId),Some(search.request.neighborhoodID.neighborhoodID))
              next(Idle())

            case Aborting(search) =>
              //ok, we've done it for nothing.
              if(verbose) context.log.info(s"aborted search:${search.searchId}")
              master ! ReadyForWork(context.self,Some(search.searchId),Some(search.request.neighborhoodID.neighborhoodID))
              next(Idle())

            case Idle() =>
              //this is an error
              if(verbose) context.log.info(s"finished search ${result.searchID} and was actually idle??")
              Behaviors.same

            case ShuttingDown() =>
              if(verbose) context.log.info(s"shutdown; explored $nbExploredNeighborhoods neighborhoods; aborted $nbAbortedNeighborhoods")
              executorForComputation.shutdown()
              Behaviors.stopped
          }

        case ShutDownWorker() =>
          state match {
            case IAmBusy(search) =>
              shouldAbortComputation = true //shared variable
              //we kill the other thread
              if(verbose) context.log.info(s"aborting prior to shutdown")
              next(ShuttingDown())
            case Aborting(search) =>
              if(verbose) context.log.info(s"got shutdown command")
              next(ShuttingDown())
            case Idle() =>
              if(verbose) context.log.info(s"shutdown; explored $nbExploredNeighborhoods neighborhoods; aborted $nbAbortedNeighborhoods")
              executorForComputation.shutdown()
              Behaviors.stopped
            case ShuttingDown() =>
              if(verbose) context.log.info(s"got shutdown command")
              Behaviors.same
          }
      }
    }
  }
}
