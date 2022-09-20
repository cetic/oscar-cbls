package oscar.cbls.lib.search.combinators.distributed

import akka.actor.typed.ActorSystem
import akka.util.Timeout
import oscar.cbls.core.distrib.{DelegateSearch, IndependentMoveFound, IndependentNoMoveFound, IndependentSearchResult, IndependentSolution, SearchCompleted, SearchCrashed, SearchEnded, SingleMoveSearch, StartSomeSearch}
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.{DistributedCombinator, Neighborhood, NoMoveFound, SearchResult}

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, DurationInt}

class DistributedBest(neighborhoods:Array[Neighborhood],useHotRestart:Boolean = true)
  extends DistributedCombinator(neighborhoods) {

  override def getMove(obj: Objective, initialObj:Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {

    val independentObj = obj.getIndependentObj
    val startSol = Some(IndependentSolution(obj.model.solution()))

    import akka.actor.typed.scaladsl.AskPattern._
    //TODO look for an adequate timeout or stopping mechanism
    implicit val timeout: Timeout = 1.hour
    implicit val system: ActorSystem[_] = supervisor.system

    val futureResults =  remoteNeighborhoodIdentifications.map(r => {

      supervisor.supervisorActor.ask[SearchEnded](ref =>
        DelegateSearch(SingleMoveSearch(
          remoteTaskId = r,
          acc =  acceptanceCriteria,
          obj = independentObj,
          startSolutionOpt = startSol,
          sendResultTo = ref
        ), waitForMoreSearch = useHotRestart))
    }).toList

    if(useHotRestart) {
      //now that all searches are sent, tell the supervisor to start searches, so it can use hotRestart
      supervisor.supervisorActor ! StartSomeSearch
    }

    val independentMoveFound:Iterable[IndependentMoveFound] = futureResults.flatMap(futureResult =>
      Await.result(futureResult,Duration.Inf) match {
        case SearchCompleted(_, searchResult: IndependentSearchResult, _) =>
          searchResult match {
            case IndependentNoMoveFound => None
            case m:IndependentMoveFound => Some(m)
          }
        case c:SearchCrashed =>
          supervisor.throwRemoteExceptionAndShutDown(c)
          None
        case _ =>
          // Search aborted
          None
      }
    )

    if (independentMoveFound.isEmpty) NoMoveFound
    else independentMoveFound.minBy(_.objAfter).getLocalResult(obj.model)
  }
}
