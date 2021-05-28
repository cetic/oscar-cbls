package oscar.cbls.core.distrib

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import org.slf4j.{Logger, LoggerFactory}
import oscar.cbls.core.computation.Store

import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.SortedMap
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

sealed abstract class MessageToWorker
final case class StartSearch(search: SearchTask, startID: Long, replyTo: ActorRef[MessagesToSupervisor]) extends MessageToWorker
final case class AbortSearch(searchId: Long, keepAliveIfOjBelow:Option[Long] = None) extends MessageToWorker
final case class WrappedSearchEnded(result: SearchEnded) extends MessageToWorker
final case class ShutDownWorker() extends MessageToWorker
final case class Ping(replyTo: ActorRef[ExternalWorkerState]) extends MessageToWorker

sealed abstract class ExternalWorkerState
case class WorkerBusy(searchId: Long, durationMs:Long) extends ExternalWorkerState
case class WorkerAborting(searchID: Long) extends ExternalWorkerState
case class WorkerIdle() extends ExternalWorkerState
case class WorkerShuttingDown() extends ExternalWorkerState

object WorkerActor {

  def startWorkerAndActorSystem(neighborhoods: SortedMap[Int, RemoteNeighborhood],
                                m: Store,
                                master: ActorRef[MessagesToSupervisor],
                                workerName: String = "worker",
                                verbose: Boolean): ActorSystem[MessageToWorker] = {

    val startLogger: Logger = LoggerFactory.getLogger("WorkerObject")
    startLogger.info("Starting actor system and worker:" + workerName)
    ActorSystem(createWorkerBehavior(neighborhoods, m, master, verbose), workerName)
  }

  def createWorkerBehavior(neighborhoods: SortedMap[Int, RemoteNeighborhood],
                           m: Store,
                           master: ActorRef[MessagesToSupervisor],
                           verbose: Boolean = false): Behavior[MessageToWorker] = {
    new WorkerActor(neighborhoods, m, master, verbose).initBehavior()
  }

  def spawnWorker(neighborhoods: SortedMap[Int, RemoteNeighborhood],
                  m: Store,
                  master: ActorRef[MessagesToSupervisor],
                  context: ActorContext[_],
                  workerName: String = "worker",
                  verbose: Boolean): ActorRef[MessageToWorker] = {
    context.spawn(createWorkerBehavior(neighborhoods, m, master, verbose), workerName)
  }
}

class WorkerActor(neighborhoods: SortedMap[Int, RemoteNeighborhood],
                  m: Store,
                  master: ActorRef[MessagesToSupervisor],
                  verbose: Boolean) {

  sealed abstract class WorkerState
  case class IAmBusy(search: SearchTask, started:Long) extends WorkerState
  case class Aborting(search: SearchTask) extends WorkerState
  case class Idle() extends WorkerState
  case class ShuttingDown() extends WorkerState

  //This is the single thread that is ready to perform all computation.
  // There is at most one computation per worker at any point in time, so the threadPool is 1.
  private val executorForComputation = Executors.newFixedThreadPool(1,
    new java.util.concurrent.ThreadFactory {
      final private val threadNumber = new AtomicInteger(1)
      override def newThread(r: Runnable): Thread = {
        val t = new Thread(null, r, "workerThread" + threadNumber.getAndIncrement, 0)
        t.setDaemon(false)
        t.setPriority(Thread.MAX_PRIORITY)
        t
      }
    }
  )
  private val executionContextForComputation: scala.concurrent.ExecutionContext = ExecutionContext.fromExecutor(executorForComputation)

  //this is a shared variable. it is not good, but that's the only way to send the abort signal to the Future that contains the computation.
  //Scala requires the final and volatile flags
  @volatile
  private final var shouldAbortComputation: Boolean = false

  @volatile
  private final var nbExploredNeighborhoods: Int = 0

  @volatile
  private final var nbAbortedNeighborhoods: Int = 0

  @volatile
  private final var currentNeighborhood: RemoteNeighborhood = null

  @volatile
  final var currentModelNr:Option[Int] = None


  def initBehavior(): Behavior[MessageToWorker] = {
    Behaviors.setup { context =>
      master ! NewWorkerEnrolled(context.self)
      if (verbose) context.log.info(s"ready for work nbNeighborhoods:${neighborhoods.size}")
      next(Idle())
    }
  }

  private def doSearch(searchRequest: SearchRequest,searchId:Long): (IndependentSearchResult,Int) = {
    searchRequest.startSolutionOpt match{
      case None => require(currentModelNr.isDefined)
      case Some(startSolution) =>
        if(this.currentModelNr.isEmpty || startSolution.solutionId != this.currentModelNr.get) {
          startSolution.makeLocal(m).restoreDecisionVariables(withoutCheckpoints = true)
          currentModelNr = Some(startSolution.solutionId)
        }
    }

    shouldAbortComputation = false

    val neighborhood = neighborhoods(searchRequest.neighborhoodID.neighborhoodID)
    currentNeighborhood = neighborhood

    val startTime = System.currentTimeMillis()
    val result = if(searchRequest.doAllMoves){
      neighborhood.doAllMoves(
        searchRequest.neighborhoodID.parameters,
        searchRequest.obj.convertToObjective(m),
        searchRequest.acc,
        shouldAbort = () => shouldAbortComputation,
        searchId = searchId,
        sendProgressTo = searchRequest.sendProgressTo)
    }else{
      neighborhood.getMove(
        searchRequest.neighborhoodID.parameters,
        searchRequest.obj.convertToObjective(m),
        searchRequest.acc,
        shouldAbort = () => shouldAbortComputation,
        sendFullSolution = searchRequest.sendFullSolution,
        searchId = searchId,
        sendProgressTo = searchRequest.sendProgressTo)
    }
    (result,(System.currentTimeMillis() - startTime).toInt)
  }

  private def next(state: WorkerState): Behavior[MessageToWorker] = {
    Behaviors.receive { (context, command) =>
      command match {
        case Ping(replyTo) =>

          replyTo ! (state match{
            case IAmBusy(search,started) => WorkerBusy(search.searchId, System.currentTimeMillis() - started)
            case Aborting(search) => WorkerAborting(search.searchId)
            case Idle() => WorkerIdle()
            case ShuttingDown() => WorkerShuttingDown()
          })

          Behaviors.same

        case StartSearch(newSearch, startID, replyTo) =>
          state match {
            case ShuttingDown() =>
              Behaviors.same
            case IAmBusy(search,startedTime) =>
              //we do not tell anything to the workGiver; the supervisor will have to find another slave
              if (verbose) context.log.info(s"got command for start search:${newSearch.searchId} but already busy")

              replyTo ! SearchNotStarted(newSearch.searchId, startID, context.self)
              Behaviors.same

            case Aborting(search) =>
              //TODO: il faudrait pouvoir stocker cette recherche en local ici pour déjà avoir la recherche suivante en cas d'abort
              if (verbose) context.log.info(s"got command for start search:${newSearch.searchId} but already busy aborting a search")

              replyTo ! SearchNotStarted(newSearch.searchId, startID, context.self)
              Behaviors.same

            case Idle() =>
              if (verbose) context.log.info(s"starting search:${newSearch.searchId}")

              this.nbExploredNeighborhoods += 1

              val futureResult = Future {
                val (result,durationMS) = doSearch(newSearch.request,newSearch.searchId)
                SearchCompleted(newSearch.searchId, result, durationMS)
              }(executionContextForComputation)

              context.pipeToSelf(futureResult) {
                // map the Future value to a message, handled by this actor
                case Success(s) => WrappedSearchEnded(s)
                case Failure(e) =>
                  WrappedSearchEnded(SearchCrashed(newSearch.searchId, newSearch.request.neighborhoodID, e, context.self))
              }
              replyTo ! SearchStarted(newSearch.searchId, startID, context.self)
              next(IAmBusy(newSearch,System.currentTimeMillis()))
          }

        case AbortSearch(searchId, keepAliveIfOjBelow:Option[Long]) =>
          state match {
            case ShuttingDown() =>
              Behaviors.same
            case IAmBusy(search,startTimeMs) =>
              if (searchId == search.searchId) {

                val mustAbort:Boolean = if(search.request.doAllMoves && keepAliveIfOjBelow.isDefined){
                  currentNeighborhood != null && currentNeighborhood.bestObjSoFar >= keepAliveIfOjBelow.get
                }else true

                if(mustAbort) {
                  if(verbose) context.log.info(s"got abort command for search:$searchId; aborting")
                  shouldAbortComputation = true //shared variable
                  nbAbortedNeighborhoods += 1
                  search.sendResultTo ! SearchAborted(searchId)
                  next(Aborting(search))
                }else{
                  if(verbose) context.log.info(s"ignoring conditional abort command, for search:$searchId bestOBj:${currentNeighborhood.bestObjSoFar} < threshold:${keepAliveIfOjBelow.get}")
                  Behaviors.same
                }
              } else {
                if (verbose) context.log.info(s"got abort command for search:$searchId but busy on search:${search.searchId}; ignoring")

                //otherwise, ignore.
                Behaviors.same
              }

            case Aborting(search) =>
              //this is an error; ignore
              if (verbose) context.log.info(s"got abort command for search:$searchId; and was already aborting from ${search.searchId}; ignoring")

              Behaviors.same

            case Idle() =>
              //this is possibly an error
              if (verbose) context.log.info(s"got abort command for search:$searchId; and was idle; ignoring")

              Behaviors.same
          }

        case WrappedSearchEnded(result) =>
          // send result to work giver

          state match {
            case IAmBusy(search,startTimeMs) =>
              require(search.searchId == result.searchID)
              if (verbose) context.log.info(s"finished search:${search.searchId}, sending result $result to ${search.sendResultTo.path}")

              search.sendResultTo ! result

              result match {
                case _: SearchCrashed =>
                  master ! Crash(context.self)
                case _ => ;
              }
              val moveFound = result match{
                case c:SearchCompleted =>
                  c.searchResult match{
                    case _:IndependentMoveFound => true
                    case _ => false
                  }
                case _ => false
              }
              master ! ReadyForWork(
                context.self,
                Some(search.searchId),
                Some((search.request.neighborhoodID.neighborhoodID, moveFound)),
                currentModelNr)

              next(Idle())

            case Aborting(search) =>
              //ok, we've done it for nothing.
              if (verbose) context.log.info(s"aborted search:${search.searchId}")
              master ! ReadyForWork(context.self, Some(search.searchId), Some((search.request.neighborhoodID.neighborhoodID,false)),currentModelNr)
              next(Idle())

            case Idle() =>
              //this is an error
              if (verbose) context.log.info(s"finished search ${result.searchID} and was actually idle??")
              Behaviors.same

            case ShuttingDown() =>
              if (verbose) context.log.info(s"shutdown; explored $nbExploredNeighborhoods neighborhoods; aborted $nbAbortedNeighborhoods")
              executorForComputation.shutdown()
              Behaviors.stopped
          }

        case ShutDownWorker() =>
          state match {
            case IAmBusy(search,startTimeMs) =>
              shouldAbortComputation = true //shared variable
              //we kill the other thread
              if (verbose) context.log.info(s"aborting prior to shutdown")
              next(ShuttingDown())
            case Aborting(search) =>
              if (verbose) context.log.info(s"got shutdown command")
              next(ShuttingDown())
            case Idle() =>
              if (verbose) context.log.info(s"shutdown; explored $nbExploredNeighborhoods neighborhoods; aborted $nbAbortedNeighborhoods")
              executorForComputation.shutdown()
              Behaviors.stopped
            case ShuttingDown() =>
              if (verbose) context.log.info(s"got shutdown command")
              Behaviors.same
          }
      }
    }
  }
}
