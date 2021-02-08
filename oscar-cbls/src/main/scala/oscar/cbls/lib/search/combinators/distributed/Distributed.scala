package oscar.cbls.lib.search.combinators.distributed

import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.util.Timeout
import oscar.cbls.core.distrib
import oscar.cbls.core.distrib._
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.{MoveFound, Neighborhood, NoMoveFound, SearchResult}

import scala.concurrent.duration.{Duration, DurationInt}
import scala.concurrent.{Await, Future, Promise}
import scala.util.Random

abstract class DistributedCombinator(neighborhoods:Array[List[Long] => Neighborhood]) extends Neighborhood {

  var remoteNeighborhoods:Array[RemoteNeighborhood] = null
  var supervisor:Supervisor = null

  def delegateSearch(searchRequest:SearchRequest, sendResultTo:ActorRef[SearchEnded]):Future[Long] =
    supervisor.delegateSearch(searchRequest, sendResultTo)

  override def labelAndExtractRemoteNeighborhoods(supervisor: Supervisor,
                                                  currentID: Int,
                                                  acc: List[RemoteNeighborhood]): (Int, List[RemoteNeighborhood]) = {
    this.supervisor = supervisor
    val (newID,newAcc,neighborhoods2) = labelAndExtractRemoteNeighborhoodsOutOf(currentID, acc, neighborhoods)
    remoteNeighborhoods = neighborhoods2
    (newID,newAcc)
  }

  private def labelAndExtractRemoteNeighborhoodsOutOf(currentID:Int,
                                                      acc:List[RemoteNeighborhood],
                                                      neighborhoods:Array[List[Long] => Neighborhood]):
  (Int,List[RemoteNeighborhood],Array[RemoteNeighborhood]) = {
    var currentIDNow: Int = currentID
    var accNow: List[RemoteNeighborhood] = acc
    val toReturnArray = neighborhoods.map(n => {
      val r = new RemoteNeighborhood(currentIDNow, n)
      currentIDNow += 1
      accNow = r :: accNow
      r
    })
    (currentIDNow, accNow,toReturnArray)
  }

  override def collectProfilingStatistics: List[Array[String]] = {
    super.collectProfilingStatistics
  }
}

class Remote(neighborhoods:Neighborhood)
  extends DistributedCombinator(Array(_ => neighborhoods)) {

  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {

    val independentObj = obj.getIndependentObj
    val startSol = IndependentSolution(obj.model.solution())

    val searchRequest = SearchRequest(
      remoteNeighborhoods(0).getRemoteIdentification(Nil),
      acceptanceCriteria,
      independentObj,
      startSol)

    import akka.actor.typed.scaladsl.AskPattern._
    implicit val timeout: Timeout = 3.seconds
    implicit val system: ActorSystem[_] = supervisor.system
    val futureResult = supervisor.supervisorActor.ask[SearchEnded](ref => DelegateSearch(searchRequest, null, ref))

    Await.result(futureResult,Duration.Inf) match{
      case SearchCompleted(_, searchResult) =>
        searchResult.getLocalResult(obj.model)
      case  c:SearchCrashed =>
        supervisor.throwRemoteExceptionAndShutDown(c)
        null
    }
  }
}

class DistributedBest(neighborhoods:Array[Neighborhood])
  extends DistributedCombinator(neighborhoods.map(x => (y:List[Long]) => x)) {

  override def getMove(obj: Objective, initialObj:Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {

    val independentObj = obj.getIndependentObj
    val startSol = IndependentSolution(obj.model.solution())

    import akka.actor.typed.scaladsl.AskPattern._
    implicit val timeout: Timeout = 3.seconds
    implicit val system: ActorSystem[_] = supervisor.system

    val futureResults =  remoteNeighborhoods.map(l => {
      val request = SearchRequest(
        l.getRemoteIdentification(Nil),
        acceptanceCriteria,
        independentObj,
        startSol)
      supervisor.supervisorActor.ask[SearchEnded](ref => DelegateSearch(request, null, ref))
    }).toList

    val independentMoveFound:Iterable[IndependentMoveFound] = futureResults.flatMap(futureResult =>
      Await.result(futureResult,Duration.Inf) match{
        case SearchCompleted(_, searchResult) =>
          searchResult match{
            case _:IndependentNoMoveFound => None
            case m:IndependentMoveFound => Some(m)
          }
        case c:SearchCrashed =>
          supervisor.throwRemoteExceptionAndShutDown(c)
          null
      }
    )

    if (independentMoveFound.isEmpty) NoMoveFound
    else independentMoveFound.minBy(_.objAfter).getLocalResult(obj.model)
  }
}


class DistributedFirst(neighborhoods:Array[Neighborhood])
  extends DistributedCombinator(neighborhoods.map(x => (y:List[Long]) => x)) {

  override def getMove(obj: Objective, initialObj: Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {

    val independentObj = obj.getIndependentObj
    val startSol = IndependentSolution(obj.model.solution())

    val resultPromise = Promise[SearchEnded]()
    val futureResult: Future[SearchEnded] = resultPromise.future

    supervisor.spawnNewActor(Behaviors.setup { context:ActorContext[Any] => {
      for (remoteNeighborhood <- remoteNeighborhoods) {
        val request = SearchRequest(
          remoteNeighborhood.getRemoteIdentification(Nil),
          acceptanceCriteria,
          independentObj,
          startSol)
        implicit val timeout: Timeout = 3.seconds
        implicit val system: ActorSystem[_] = supervisor.system
        supervisor.supervisorActor ! DelegateSearch(request, context.self, context.self)
      }
      next()
    }},"DistributedFirst")

    def next(runningSearchIDs:List[Long] = List.empty, nbKnownSearches:Int = 0, bestSolutionSoFar:Option[IndependentMoveFound] = None): Behavior[Any] = {
      Behaviors.receive { (context, command) =>
        command match {
          case newSearchID: Long =>
            val newSearchIDs = newSearchID :: runningSearchIDs
            if (bestSolutionSoFar.isDefined) {
              for (searchID <- newSearchIDs) {
                supervisor.supervisorActor ! CancelSearchToSupervisor(searchID)
              }
              if (nbKnownSearches + 1 == neighborhoods.length) {
                resultPromise.success(SearchCompleted(0, bestSolutionSoFar.get))
                Behaviors.stopped
              } else {
                next(newSearchIDs, nbKnownSearches + 1, bestSolutionSoFar)
              }
            } else {
              next(newSearchIDs, nbKnownSearches + 1, bestSolutionSoFar)
            }

          case SearchCompleted(searchID,_:IndependentNoMoveFound) =>
            val newSearchIDs = searchIDs.filterNot()
          case s:SearchCompleted(searchID,m:IndependentMoveFound) =>
        resultPromise.success(s)
          case c:SearchCrashed =>

          case _ =>


        }
      }
    }

  }

}
