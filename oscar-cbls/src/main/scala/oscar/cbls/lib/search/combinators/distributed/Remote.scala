package oscar.cbls.lib.search.combinators.distributed

import akka.actor.typed.ActorSystem
import oscar.cbls.core.distrib.{DelegateSearch, IndependentSolution, SearchCompleted, SearchCrashed, SearchEnded, SearchRequest}
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.{DistributedCombinator, Neighborhood, NoMoveFound, SearchResult}
import akka.util.Timeout

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, DurationInt}


class Remote(neighborhoods:Neighborhood)
  extends DistributedCombinator(Array(_ => neighborhoods)) {

  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {

    val independentObj = obj.getIndependentObj
    val startSol = Some(IndependentSolution(obj.model.solution()))

    val searchRequest = SearchRequest(
      remoteNeighborhoods(0).getRemoteIdentification(Nil),
      acceptanceCriteria,
      independentObj,
      startSol)

    import akka.actor.typed.scaladsl.AskPattern._
    //TODO look for an adequate timeout or stopping mechanism
    implicit val timeout: Timeout = 1.hour
    implicit val system: ActorSystem[_] = supervisor.system

    val futureResult = supervisor.supervisorActor.ask[SearchEnded](ref => DelegateSearch(searchRequest, ref))
    Await.result(futureResult,Duration.Inf) match {
      case SearchCompleted(_, searchResult, durationMS) =>
        searchResult.getLocalResult(obj.model)
      case c:SearchCrashed =>
        supervisor.throwRemoteExceptionAndShutDown(c)
        null
      case _ =>
        // Search Aborted, should not happen
        NoMoveFound
    }
  }
}
