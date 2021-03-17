package oscar.cbls.lib.search.combinators.distributed

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorSystem, Behavior}
import akka.util.Timeout
import oscar.cbls.core.distrib._
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.{Neighborhood, NoMoveFound, SearchResult}

import scala.concurrent.duration.{Duration, DurationInt}
import scala.concurrent.{Await, Future, Promise}
import scala.util.{Failure, Success}

abstract class DistributedCombinator(neighborhoods:Array[List[Long] => Neighborhood]) extends Neighborhood {

  var remoteNeighborhoods:Array[RemoteNeighborhood] = null
  var supervisor:Supervisor = null

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
    //TODO look for an adequate timeout or stopping mechanism
    implicit val timeout: Timeout = 1.hour
    implicit val system: ActorSystem[_] = supervisor.system

    val futureResult = supervisor.supervisorActor.ask[SearchEnded](ref => DelegateSearch(searchRequest, ref))
    Await.result(futureResult,Duration.Inf) match {
      case SearchCompleted(_, searchResult) =>
        searchResult.getLocalResult(obj.model)
      case c:SearchCrashed =>
        supervisor.throwRemoteExceptionAndShutDown(c)
        null
      case _ =>
        // Search Aborted
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
    //TODO look for an adequate timeout or stopping mechanism
    implicit val timeout: Timeout = 1.hour
    implicit val system: ActorSystem[_] = supervisor.system

    val futureResults =  remoteNeighborhoods.indices.map(i => {

      val request = SearchRequest(
        remoteNeighborhoods(i).getRemoteIdentification(Nil),
        acceptanceCriteria,
        independentObj,
        startSol)

      supervisor.supervisorActor.ask[SearchEnded](ref => DelegateSearch(request, ref))
    }).toList

    val independentMoveFound:Iterable[IndependentMoveFound] = futureResults.flatMap(futureResult =>
      Await.result(futureResult,Duration.Inf) match {
        case SearchCompleted(_, searchResult) =>
          searchResult match{
            case _:IndependentNoMoveFound => None
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

class DistributedFirst(neighborhoods:Array[Neighborhood])
  extends DistributedCombinator(neighborhoods.map(x => (y:List[Long]) => x)) {

  override def getMove(obj: Objective, initialObj: Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {

    val independentObj = obj.getIndependentObj
    val startSol = IndependentSolution(obj.model.solution())

    val resultPromise = Promise[WrappedData]()
    val futureResult: Future[WrappedData] = resultPromise.future

    abstract class WrappedData
    case class WrappedSearchEnded(searchEnded:SearchEnded) extends WrappedData
    case class WrappedGotUniqueID(uniqueID:Long,neighborhoodIndice:Int) extends WrappedData
    case class WrappedError(msg:Option[String] = None, crash:Option[SearchCrashed] = None) extends WrappedData

    implicit val system: ActorSystem[_] = supervisor.system
    //TODO look for the adequate timeout supervisor
    implicit val timeout: Timeout = 1.hour

    supervisor.spawnNewActor(Behaviors.setup { context:ActorContext[WrappedData] => {
      //starting up all searches
      for (i <- remoteNeighborhoods.indices){
        context.ask[GetNewUniqueID,Long](supervisor.supervisorActor,ref => GetNewUniqueID(ref)) {
          case Success(uniqueID:Long) => WrappedGotUniqueID(uniqueID:Long,i)
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
              case SearchCompleted(searchID: Long, searchResult: IndependentSearchResult) =>
                searchResult match {
                  case _: IndependentMoveFound =>

                    for (r <- runningSearchIDs) {
                      supervisor.supervisorActor ! CancelSearchToSupervisor(r)
                    }

                    resultPromise.success(w)
                    Behaviors.stopped

                  case _: IndependentNoMoveFound =>

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

              case c: SearchCrashed =>
                for (r <- runningSearchIDs) {
                  supervisor.supervisorActor ! CancelSearchToSupervisor(r)
                }
                resultPromise.success(w)
                Behaviors.stopped
            }

          case WrappedGotUniqueID(uniqueID: Long, neighborhoodIndice: Int) =>

            val request = SearchRequest(
              remoteNeighborhoods(neighborhoodIndice).getRemoteIdentification(Nil),
              acceptanceCriteria,
              independentObj,
              startSol)

            context.ask[DelegateSearch, SearchEnded](supervisor.supervisorActor, ref => DelegateSearch(request, ref, uniqueID)) {
              case Success(searchEnded) => WrappedSearchEnded(searchEnded)
              case Failure(_) => WrappedError(msg = Some(s"Supervisor actor timeout on $command"))
            }

            next(runningSearchIDs = uniqueID :: runningSearchIDs,
              nbFinishedSearches = nbFinishedSearches)

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
      case WrappedSearchEnded(searchEnded:SearchEnded) =>
        searchEnded match {
          case SearchCompleted(searchID, searchResult) => searchResult.getLocalResult(obj.model)
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
