package oscar.cbls.lib.search.combinators.distributed

import akka.actor.typed.ActorSystem
import oscar.cbls.core.distrib.{DelegateSearch, IndependentSearchResult, IndependentSolution, SearchCompleted, SearchCrashed, SearchEnded, SingleMoveSearch}
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.{DistributedCombinator, Neighborhood, NoMoveFound, SearchResult}
import akka.util.Timeout

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, DurationInt}

class Remote(neighborhoods:Neighborhood)
  extends DistributedCombinator(Array(neighborhoods)) {

  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {

    val independentObj = obj.getIndependentObj
    val startSol = IndependentSolution(obj.model.solution())

    import akka.actor.typed.scaladsl.AskPattern._
    //TODO look for an adequate timeout or stopping mechanism
    implicit val timeout: Timeout = 1.hour
    implicit val system: ActorSystem[_] = supervisor.system

    val futureResult = supervisor.supervisorActor.ask[SearchEnded](ref =>
      DelegateSearch(
        SingleMoveSearch(
          remoteTaskId = remoteNeighborhoodIdentifications(0),
          acc = acceptanceCriteria,
          obj = independentObj,
          startSolutionOpt = Some(startSol),
          sendResultTo = ref
        )))

    Await.result(futureResult,Duration.Inf) match {
      case SearchCompleted(_, searchResult: IndependentSearchResult, _) =>
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
