package oscar.cbls.lib.search.combinators.distributed

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.util.Timeout
import oscar.cbls.core.computation.Store
import oscar.cbls.core.distrib._
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.{Neighborhood, NoMoveFound, SearchResult}
import oscar.cbls.lib.search.combinators.Atomic

import scala.collection.SortedMap
import scala.collection.immutable.SortedMap
import scala.concurrent.duration.{Duration, DurationInt}
import scala.concurrent.{Await, Future, Promise}
import scala.util.{Failure, Random, Success}
/*
//VLSN
//Restart
class DistributedRestart(search:Neighborhood,
                         randomize:Neighborhood,
                         maxConsecutiveRestartWithoutImprovement:Long,
                         maxWorkers:Int,
                         obj:Objective,
                         atMost:Duration = Duration.Inf)
  extends DistributedCombinator(
    Array(
      (_:List[Long]) => Atomic(randomize maxMoves 1 exhaust search,_=>false,aggregateIntoSingleMove = true),
      (_:List[Long]) => Atomic(search,_=>false,aggregateIntoSingleMove = true))) {
  //1 is first search
  //0 is randomize

  val restartAndSearch = remoteNeighborhoods(0).getRemoteIdentification(Nil)
  val search = remoteNeighborhoods(1).getRemoteIdentification(Nil)

  override def getMove(obj: Objective, initialObj: Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {

    val independentObj = obj.getIndependentObj
    val model = obj.model

    def searchRequest(startSol: IndependentSolution, neighborhood: RemoteNeighborhoodIdentification) =
      SearchRequest(
        neighborhood,
        acceptanceCriteria,
        independentObj,
        startSol)

    def searchRequestSearch(startSol: IndependentSolution) =
      searchRequest(startSol, search)

    def searchRequestRestartAndSearchSearch(startSol: IndependentSolution) =
      searchRequest(startSol, restartAndSearch)

    case class MessageToRestart
    case class OngoingSearch(initialObj: Long, searchID: Long)

    val supervisorActor = supervisor.supervisorActor

    val resultPromise = Promise[SearchEnded]()
    val futureResult: Future[SearchEnded] = resultPromise.future

    def initBehavior(supervisorRef: ActorRef[MessagesToSupervisor]): Behavior[MessageToWorkGiver] = {
      Behaviors.setup { context =>

        val initSol = IndependentSolution(model.solution())

        implicit val responseTimeout: Timeout = 3.seconds
        context.ask[MessagesToSupervisor, Long](supervisorActor,
          res => DelegateSearchSendSolutionTo(searchRequestSearch(initSol), context.self, res)) {
          case Success(searchID: Long) => OngoingSearch(initialObj, searchID)
          case Failure(_) => SearchNotStarted(search, worker)
        }

        for (i <- 0 until maxWorkers - 1) {
          context.ask[MessagesToSupervisor, Long](supervisorActor,
            res => DelegateSearchSendSolutionTo(searchRequestRestartAndSearchSearch(initSol), context.self,res)) {
            case Success(searchID: Long) => OngoingSearch(initialObj, searchID)
            case Failure(_) => SearchNotStarted(search, worker)
          }
        }
        next()
      }
    }

    private def next(ongoingSearches: SortedMap[Long, Long] = SortedMap.empty): Behavior[MessageToWorker] = {
      Behaviors.receive { (context, command) =>
        command match {
          case Ping(replyTo) =>
            replyTo ! Unit
            Behaviors.same
        }
      }
    }

    supervisor.startDedicatedActor(initBehavior)

    Await.result(futureResult,Duration.Inf) match {
      case  SearchCompleted(searchID, searchResult) => searchResult.getLocalResult(model)
      case SearchAborted(searchID) => NoMoveFound
      case SearchCrashed(searchID, neighborhood, exception: Throwable, worker) =>
        val e = new Exception(s"Crash happened at worker:${worker}: \n${exception.getMessage}\nwhen performing neighborhood:${neighborhood}")
        e.setStackTrace(
          //This trims the stack trace to hide the intermediary calls to threads, futures and the like.
          (exception.getStackTrace.toList.reverse.dropWhile(!_.getClassName.contains("oscar.cbls")).reverse
            //c.exception.getStackTrace.toList
            ::: e.getStackTrace.toList).toArray)
        supervisor.shutdown()
        throw e
    }
  }
}


*/