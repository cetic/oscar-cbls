/*package oscar.cbls.lib.search.combinators.distributed

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorSystem, Behavior}
import akka.util.Timeout
import oscar.cbls.core.distrib._
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.{DistributedCombinator, Neighborhood, NoMoveFound, SearchResult}

import scala.annotation.tailrec
import scala.concurrent.duration.{Duration, DurationInt}
import scala.concurrent.{Await, Future, Promise}
import scala.util.{Failure, Success}

class DistributedBestSlopeFirst(neighborhoods:Array[Neighborhood],
                                nbWorkers: Int,
                                refresh: Option[Int] = None,
                                tabuLength:Int = 4)
  extends DistributedCombinator(neighborhoods.map(x => (y:List[Long]) => x)) {
  require(nbWorkers >= 1, "at least one worker is needed")

  val durations: Array[Long] = neighborhoods.map(_ => 0L)
  val gains: Array[Long] = neighborhoods.map(_ => 0L)
  val tabuUntilIt: Array[Int] = neighborhoods.map(_ => 0)

  def getSortedNeighborhoods(currentIt:Int):List[Int] = {
    val tabuSlopeId = neighborhoods.indices.toList.map(i => {
      val slope = if(durations(i) == 0) Int.MinValue else - (gains(i).toDouble / durations(i).toDouble)
      val tabu = tabuUntilIt(i)
      (if(tabu > currentIt) 1 else 0, slope, i)
    })
    tabuSlopeId.sorted.map(_._3)
  }

  //If we want a refresh, it will happen at this iteration number
  var nextRefreshIt: Int = refresh match{
    case None => Int.MaxValue
    case Some(x) => x
  }

  var currentIt = 0

  override def getMove(obj: Objective, initialObj: Long, acceptanceCriteria: (Long, Long) => Boolean):SearchResult = {
    currentIt += 1

    if(nextRefreshIt == currentIt) {
      nextRefreshIt = nextRefreshIt + refresh.getOrElse(0)
      for (i <- neighborhoods.indices) {
        durations(i) = 0
        gains(i) = 0
        tabuUntilIt(i) = currentIt
      }
    }

    val independentObj = obj.getIndependentObj
    val startSol = Some(IndependentSolution(obj.model.solution()))

    val resultPromise = Promise[WrappedData]()
    val futureResult : Future[WrappedData] = resultPromise.future

    abstract class WrappedData
    case class WrappedSearchEnded(searchEnded:SearchEnded, neighborhoodIndice: Int, uniqueId: Long) extends WrappedData
    case class WrappedGotUniqueID(uniqueID: Long, neighborhoodIndice: Int) extends WrappedData
    case class WrappedError(msg: Option[String]=None, crash:Option[SearchCrashed] = None) extends WrappedData

    implicit val system: ActorSystem[_] = supervisor.system
    //TODO look for the adequate timeout supervisor
    implicit val timeout: Timeout = 1.hour

    supervisor.spawnNewActor(Behaviors.setup { context:ActorContext[WrappedData] => {
      init(getSortedNeighborhoods(currentIt), nbWorkers, nbStartingAndRunningSearches = 0, context)
    }}, "DistributedBestSlopeFirst")


    @tailrec
    def init(neighborhoodList: List[Int], nbSearchesToStart: Int, nbStartingAndRunningSearches:Int, context: ActorContext[WrappedData]): Behavior[WrappedData] = {
      neighborhoodList match {
        case h :: t if nbSearchesToStart > 0 =>
          initiateNeighborhoodExploration(h, context)
          init(t, nbSearchesToStart - 1, nbStartingAndRunningSearches+1, context)
        case _ =>
          next(nextSearchesToStart = neighborhoodList,
            runningSearchIDs = List.empty,
            nbFinishedSearches= 0,
            nbStartingAndRunningSearches = nbStartingAndRunningSearches,
            currentIt)
      }
    }

    def next(nextSearchesToStart : List[Int],
             runningSearchIDs:List[Long],
             nbFinishedSearches: Int,
             nbStartingAndRunningSearches:Int,
             currentIt:Int): Behavior[WrappedData] = {
      Behaviors.receive{ (context, command) =>
        command match {
          case w@WrappedSearchEnded(searchEnded:SearchEnded, neighborhoodIndice: Int, uniqueId: Long) =>
            searchEnded match {
              case SearchCompleted(searchID: Long, searchResult: IndependentSearchResult, durationMS) =>
                searchResult match {
                  case moveFound: IndependentMoveFound =>

                    //stop at the first found move
                    durations(neighborhoodIndice) = durations(neighborhoodIndice) + durationMS
                    gains(neighborhoodIndice) += moveFound.objAfter - initialObj

                    for (r <- runningSearchIDs) {
                      supervisor.supervisorActor ! CancelSearchToSupervisor(r)
                    }

                    //make better
                    resultPromise.success(w)
                    Behaviors.stopped
                  case _: IndependentNoMoveFound =>

                    durations(neighborhoodIndice) = durations(neighborhoodIndice) + durationMS
                    tabuUntilIt(neighborhoodIndice) = currentIt + tabuLength

                    val nextRunningSearchID = runningSearchIDs.filter(_ != uniqueId)

                    nextSearchesToStart match{
                      case h::t =>
                        initiateNeighborhoodExploration(h, context)
                        next(nextSearchesToStart = t, nextRunningSearchID, nbStartingAndRunningSearches, nbFinishedSearches+1 ,currentIt)
                      case Nil if nbStartingAndRunningSearches == 1 => //-1 because we just finished one, actually
                        resultPromise.success(w) //it is a NoMoveFound
                        Behaviors.stopped
                      case Nil =>
                        next(nextSearchesToStart = Nil, nextRunningSearchID, nbStartingAndRunningSearches-1, nbFinishedSearches+1 ,currentIt)
                    }
                }

              case c: SearchCrashed =>
                for (r <- runningSearchIDs) {
                  supervisor.supervisorActor ! CancelSearchToSupervisor(r)
                }
                resultPromise.success(w)
                Behaviors.stopped
            }

          case WrappedGotUniqueID(uniqueID: Long, neighborhoodIndice: Int) =>
            //start search with val request
            val request = SearchRequest(remoteNeighborhoods(neighborhoodIndice).getRemoteIdentification(Nil),
              acceptanceCriteria,
              independentObj,
              startSol,
              doAllMoves = true)
            //context.ask thing

            context.ask[DelegateSearch, SearchEnded](supervisor.supervisorActor, ref => DelegateSearch(request, ref, uniqueID)) {
              case Success(searchEnded) => WrappedSearchEnded(searchEnded, neighborhoodIndice, uniqueID)
              case Failure(_) => WrappedError(msg = Some(s"Error in WrappedGotUniqueID, uniqueID=$uniqueID"))
            }

            next(nextSearchesToStart, uniqueID :: runningSearchIDs, nbStartingAndRunningSearches, nbFinishedSearches ,currentIt)

          case w: WrappedError =>
            for(r <- runningSearchIDs) {
              supervisor.supervisorActor ! CancelSearchToSupervisor(r)
            }
            resultPromise.success(w)
            Behaviors.stopped
        }
      }
    }



    def initiateNeighborhoodExploration(neighborhoodIndice:Int, context: ActorContext[WrappedData]): Unit = {
      context.ask[GetNewUniqueID,Long](supervisor.supervisorActor,ref => GetNewUniqueID(ref)) {
        case Success(uniqueID:Long) => WrappedGotUniqueID(uniqueID,neighborhoodIndice)
        case Failure(ex) => WrappedError(msg=Some(s"Supervisor actor timeout : ${ex.getMessage}"))
      }
    }

    Await.result(futureResult, Duration.Inf) match {
      case WrappedSearchEnded(searchEnded:SearchEnded, _, _) =>
        searchEnded match {
          case SearchCompleted(searchID, searchResult, durationMs) =>searchResult.getLocalResult(obj.model)
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
        throw new Error("Error in DistributedBestSlopeFirst")
      case e => throw new Error(s"Uknown error in DistributedBestSlopeFirst : $e")
    }
  }
}

*/