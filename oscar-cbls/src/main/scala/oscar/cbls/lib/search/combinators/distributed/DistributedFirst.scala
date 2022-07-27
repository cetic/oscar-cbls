package oscar.cbls.lib.search.combinators.distributed

import akka.actor.typed.{ActorSystem, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.util.Timeout
import oscar.cbls.core.distrib.{CancelSearchToSupervisor, DelegateSearch, GetNewUniqueID, IndependentMoveFound, IndependentNoMoveFound, IndependentSearchResult, IndependentSolution, RemoteTaskIdentification, SearchAborted, SearchCompleted, SearchCrashed, SearchEnded, SingleMoveSearch}
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.{DistributedCombinator, Neighborhood, NoMoveFound, SearchResult}

import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration.{Duration, DurationInt}
import scala.util.{Failure, Random, Success}

class DistributedFirst(neighborhoods:Array[Neighborhood],useHotRestart:Boolean = false)
  extends DistributedCombinator(neighborhoods) {

  override def getMove(obj: Objective, initialObj: Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {

    val independentObj = obj.getIndependentObj
    val startSol = Some(IndependentSolution(obj.model.solution()))

    val resultPromise = Promise[WrappedData]()
    val futureResult: Future[WrappedData] = resultPromise.future

    abstract class WrappedData
    case class WrappedSearchEnded(searchEnded:SearchEnded) extends WrappedData
    case class WrappedGotUniqueID(uniqueID:Long,remote:RemoteTaskIdentification) extends WrappedData
    case class WrappedError(msg:Option[String] = None, crash:Option[SearchCrashed] = None) extends WrappedData

    implicit val system: ActorSystem[_] = supervisor.system
    //TODO look for the adequate timeout supervisor
    implicit val timeout: Timeout = 1.hour

    supervisor.spawnNewActor(Behaviors.setup { context:ActorContext[WrappedData] => {
      //starting up all searches
      for (r <- Random.shuffle(remoteNeighborhoodIdentifications.toList)){
        context.ask[GetNewUniqueID,Long](supervisor.supervisorActor,ref => GetNewUniqueID(ref)) {
          case Success(uniqueID:Long) => WrappedGotUniqueID(uniqueID:Long,r)
          case Failure(ex) => WrappedError(msg=Some(s"Supervisor actor timeout : ${ex.getMessage}"))
        }
      }
      next(
        runningSearchIDs = List.empty,
        nbFinishedSearches = 0)

    }},"DistributedFirst")

    def next(runningSearchIDs:List[Long],
             nbFinishedSearches:Int): Behavior[WrappedData] = {
      Behaviors.receive { (context, command) =>
        command match {
          case w@WrappedSearchEnded(searchEnded: SearchEnded) =>
            searchEnded match {
              case SearchCompleted(_, searchResult, _) =>
                searchResult match {
                  case _: IndependentMoveFound =>

                    for (r <- runningSearchIDs) {
                      supervisor.supervisorActor ! CancelSearchToSupervisor(r)
                    }
                    resultPromise.success(w)
                    Behaviors.stopped

                  case IndependentNoMoveFound =>

                    val newNbFinishedSearches = nbFinishedSearches + 1
                    if (newNbFinishedSearches == neighborhoods.length) {
                      resultPromise.success(w) //it is a NoMoveFound
                      Behaviors.stopped
                    } else {
                      next(
                        runningSearchIDs = runningSearchIDs,
                        nbFinishedSearches = newNbFinishedSearches)
                    }
                }

              case SearchAborted(_) =>
                //ignore it.
                next(runningSearchIDs = runningSearchIDs,
                  nbFinishedSearches = nbFinishedSearches + 1)

              case _: SearchCrashed =>
                for (r <- runningSearchIDs) {
                  supervisor.supervisorActor ! CancelSearchToSupervisor(r)
                }
                resultPromise.success(w)
                Behaviors.stopped
            }

          case WrappedGotUniqueID(uniqueId: Long, remoteNeighborhoodIdentification) =>

            context.ask[DelegateSearch, SearchEnded](
              supervisor.supervisorActor,
              ref => DelegateSearch(
                SingleMoveSearch(
                  uniqueSearchId = uniqueId,
                  remoteTaskId = remoteNeighborhoodIdentification,
                  acc = acceptanceCriteria,
                  obj = independentObj,
                  startSolutionOpt = startSol,
                  sendResultTo = ref
                ),
                waitForMoreSearch = runningSearchIDs.size >= neighborhoods.length-1
              )
            ) {
              case Success(searchEnded) => WrappedSearchEnded(searchEnded)
              case Failure(_) => WrappedError(msg = Some(s"Supervisor actor timeout on $command"))
            }
            next(runningSearchIDs = uniqueId :: runningSearchIDs, nbFinishedSearches = nbFinishedSearches)

          case w: WrappedError =>

            for (r <- runningSearchIDs) {
              supervisor.supervisorActor ! CancelSearchToSupervisor(r)
            }
            resultPromise.success(w)
            Behaviors.stopped
        }
      }
    }

    //await seems to block the actor system??
    Await.result(futureResult, Duration.Inf) match{
      case WrappedSearchEnded(searchEnded) =>
        searchEnded match {
          case SearchCompleted(_, searchResult: IndependentSearchResult, _) => searchResult.getLocalResult(obj.model)
          case _ => NoMoveFound
        }
      case WrappedError(msg:Option[String],crash:Option[SearchCrashed])=>
        if(msg.isDefined){
          supervisor.shutdown()
          throw new Error(s"${msg.get}")
        }
        if(crash.isDefined){
          supervisor.throwRemoteExceptionAndShutDown(crash.get)
        }
        throw new Error("Error in DistributedFirst")
      case e =>
        throw new Error(s"Unknown error in DistributedFirst : $e")
    }
  }
}
