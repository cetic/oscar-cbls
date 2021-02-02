package oscar.cbls.lib.search.combinators.distributed

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import oscar.cbls.core.computation.Store
import oscar.cbls.core.distrib.{AbortSearch, Crash, IndependentMoveFound, IndependentSearchResult, IndependentSolution, MessageToWorkGiver, MessageToWorker, MessagesToSupervisor, NewWorkerEnrolled, Ping, ReadyForWork, RemoteNeighborhood, RemoteNeighborhoodIdentification, SearchAborted, SearchCompleted, SearchCrashed, SearchEnded, SearchNotStarted, SearchRequest, SearchStarted, SearchTask, ShutDownWorker, StartSearch, ThreadFactory, WorkGiver, WrappedSearchEnded}
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.{Move, MoveFound, Neighborhood, NoMoveFound, SearchResult}
import oscar.cbls.lib.search.combinators.{Atomic, DistributedCombinator}

import java.util.concurrent.Executors
import scala.collection.SortedMap
import scala.collection.immutable.SortedMap
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Random, Success}

//VLSN
//Restart
class
DistributedRestart(search:Neighborhood,
                   randomize:Neighborhood,
                   maxConsecutiveRestartWithoutImprovement:Long,
                   maxWorkers:Int,
                   obj:Objective, atMost:Duration.Inf)
  extends DistributedCombinator(
    Array(
      (_:List[Long]) => Atomic(randomize maxMoves 1 exhaust search,_=>false,aggregateIntoSingleMove = true),
      (_:List[Long]) => Atomic(search,_=>false,aggregateIntoSingleMove = true))){
  //1 is first search
  //0 is randomize

  val restartAndSearch = remoteNeighborhoods(0).getRemoteIdentification(Nil)
  val search = remoteNeighborhoods(1).getRemoteIdentification(Nil)

  abstract class RestartState(ongoingSearches:SortedMap[],
                              errorMessage:Option[String])

  override def getMove(obj: Objective, initialObj:Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {

    val independentObj = obj.getIndependentObj

    val model = obj.model


    def searchRequest(startSol:IndependentSolution, neighborhood:RemoteNeighborhoodIdentification) =
      SearchRequest(
        neighborhood,
        acceptanceCriteria,
        independentObj,
        startSol)

    def searchRequestSearch(startSol: IndependentSolution) =
      searchRequest(startSol,search)

    def searchRequestRestartAndSearchSearch(startSol: IndependentSolution) =
      searchRequest(startSol,restartAndSearch)

    def initBehavior(supervisorRef: ActorRef[MessagesToSupervisor]): Behavior[MessageToWorkGiver] = {
      Behaviors.setup { context =>
        val supervisorRef: ActorRef[MessagesToSupervisor] = supervisor.supervisorActor

        val initSol = IndependentSolution(model.solution())

        val ongoingSearches =
          (supervisor ! delegateWithActor(context.self, searchRequestSearch(initSol))
            :: (0 until maxWorkers).toList.map(
            _ => delegateWithActor(context.self, searchRequestRestartAndSearchSearch(initSol))))

      }
    }

    private def next(currentState: RestartState, supervisorRef: ActorRef[MessagesToSupervisor]): Behavior[MessageToWorker] = {
      Behaviors.receive { (context, command) =>
        command match {
          case Ping(replyTo) =>
            replyTo ! Unit
            Behaviors.same
        }
      }
    }

    supervisor.startDedicatedActor(dedicatedActor:(ActorRef[MessagesToSupervisor] => Behavior[MessageToWorkGiver]) = initBehavior)
  }

  bestGlobalMoveSoFarAndNumberOfRestarts = None

  val independentObj = obj.getIndependentObj
  val startSol = IndependentSolution(obj.model.solution())

  val searchRequests = remoteNeighborhoods.map(l =>
    SearchRequest(
      l.getRemoteIdentification(Nil),
      acceptanceCriteria,
      independentObj,
      startSol))

  val searchRequests2 = if (randomOrder) Random.shuffle(searchRequests.toList).toArray else searchRequests
  val move = delegateSearchesStopAtFirst(searchRequests2)


  val searchResult:Future[SearchResult] = ???
  Await.result(searchResult, atMost)
}
}






class WorkerActor(neighborhoods: SortedMap[Int, RemoteNeighborhood],
                  m: Store,
                  master: ActorRef[MessagesToSupervisor],
                  verbose: Boolean) {


  def initBehavior(): Behavior[MessageToWorker] = {
    Behaviors.setup { context =>
      master ! NewWorkerEnrolled(context.self)
      if (verbose) context.log.info(s"ready for work nbNeighborhoods:${neighborhoods.size}")
      next(Idle())
    }
  }

  private def next(state: WorkerState): Behavior[MessageToWorker] = {
    Behaviors.receive { (context, command) =>
      command match {
        case Ping(replyTo) =>
          replyTo ! Unit
          Behaviors.same

        case StartSearch(newSearch, startID, replyTo) =>
          state match {
            case ShuttingDown() =>
              Behaviors.same
            case IAmBusy(search) =>
              //we do not tell anything to the workGiver; the supervisor will have to find another slave
              if (verbose) context.log.info(s"got command for start search:${newSearch.searchId} but already busy")

              replyTo ! SearchNotStarted(newSearch, context.self)
              Behaviors.same

            case Aborting(search) =>
              if (verbose) context.log.info(s"got command for start search:${newSearch.searchId} but already busy aborting a search")

              replyTo ! SearchNotStarted(newSearch, context.self)
              Behaviors.same

            case Idle() =>
              if (verbose) context.log.info(s"starting search:${newSearch.searchId}")

              this.nbExploredNeighborhoods += 1

              val futureResult = Future {
                //TODO: the abort does not work; Future is probably not the way to go here.
                //TODO: how about startSol, obj and acceptance?
                SearchCompleted(newSearch.searchId, doSearch(newSearch.request))
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

        case AbortSearch(searchId) =>
          state match {
            case ShuttingDown() =>
              Behaviors.same
            case IAmBusy(search) =>
              if (searchId == search.searchId) {
                if (verbose) context.log.info(s"got abort command for search:$searchId; aborting")
                shouldAbortComputation = true //shared variable
                nbAbortedNeighborhoods += 1
                next(Aborting(search))
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
            case IAmBusy(search) =>
              require(search.searchId == result.searchID)
              if (verbose) context.log.info(s"finished search:${search.searchId}, sending result $result to ${search.workGiver.path}")
              search.workGiver ! result

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
                Some((search.request.neighborhoodID.neighborhoodID, moveFound)))

              next(Idle())

            case Aborting(search) =>
              //ok, we've done it for nothing.
              if (verbose) context.log.info(s"aborted search:${search.searchId}")
              master ! ReadyForWork(context.self, Some(search.searchId), Some((search.request.neighborhoodID.neighborhoodID,false)))
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
            case IAmBusy(search) =>
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

  abstract class WorkerState

  case class IAmBusy(search: SearchTask) extends WorkerState

  case class Aborting(search: SearchTask) extends WorkerState

  case class Idle() extends WorkerState

  case class ShuttingDown() extends WorkerState
}
