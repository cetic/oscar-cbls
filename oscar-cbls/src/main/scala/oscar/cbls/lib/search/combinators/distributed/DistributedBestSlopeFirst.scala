package oscar.cbls.lib.search.combinators.distributed

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

/**
 * This distributed combinator is useful if there are more neighborhoods than workers,
 * to prioritize which neighborhood to explore first, by exploring the ones that have the best slope in priority
 *
 * Since it is a distributed combinator, it is only worth using if the neighborhoods are time consuming to explore
 *
 * The neighborhood systematically uses all available workers.
 *
 * @param neighborhoods the neighborhoods to explore
 * @param refresh to force the exploration of all neighborhoods every "refresh" iterations.
 *                This is when some neighborhoods perform very poorly at the start of the search
 *                and become more interesting during the search,
 *                or when some have guards conditions that are eventually met
 * @param tabuLength when a neighborhood does not find a move, we set it t oa very low priority
 *                   for a number of iteration equal to "the number of available workers" + tabulength
 */
class DistributedBestSlopeFirst(neighborhoods:Array[Neighborhood],
                                refresh: Int = 10,
                                tabuLength:Int = 4)
  extends DistributedCombinator(neighborhoods.map(x => (y:List[Long]) => x)) {

  val durationsMs: Array[Long] = neighborhoods.map(_ => 0L)
  val deltaObjs: Array[Long] = neighborhoods.map(_ => 0L)
  val tabuUntilIt: Array[Int] = neighborhoods.map(_ => 0)

  def getSortedNeighborhoods(currentIt:Int):List[Int] = {
    val tabuSlopeId = neighborhoods.indices.toList.map(i => {
      val slope = if(durationsMs(i) == 0) Int.MinValue else (deltaObjs(i).toDouble / durationsMs(i).toDouble)
      val tabu = tabuUntilIt(i)
      ((tabu - currentIt) max 0, slope, i)
    })

    tabuSlopeId.sorted.map(_._3)
  }

  //If we want a refresh, it will happen at this iteration number
  var nextRefreshIt: Int = refresh

  var currentIt = 0

  override def getMove(obj: Objective, initialObj: Long, acceptanceCriteria: (Long, Long) => Boolean):SearchResult = {
    currentIt += 1

    val nbWorkers = supervisor.waitForAtLestOneWorker()
    require(nbWorkers >= 1, "at least one worker is needed")

    if(currentIt >= nextRefreshIt) {
      nextRefreshIt = nextRefreshIt + refresh
      for (i <- neighborhoods.indices) {
        durationsMs(i) = 0
        deltaObjs(i) = 0
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
            nbFinishedSearches  = 0,
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
                    //TODO: wait for the best move among the first responders?
                    durationsMs(neighborhoodIndice) = durationsMs(neighborhoodIndice) + durationMS
                    deltaObjs(neighborhoodIndice) += moveFound.objAfter - initialObj

                    for (r <- runningSearchIDs) {
                      supervisor.supervisorActor ! CancelSearchToSupervisor(r)
                    }

                    resultPromise.success(w)
                    Behaviors.stopped
                  case _: IndependentNoMoveFound =>
                    durationsMs(neighborhoodIndice) = durationsMs(neighborhoodIndice) + durationMS
                    tabuUntilIt(neighborhoodIndice) = currentIt + tabuLength + 1 + nbWorkers

                    val nextRunningSearchID = runningSearchIDs.filter(_ != uniqueId)

                    nextSearchesToStart match{
                      case h::t =>
                        initiateNeighborhoodExploration(h, context)

                        next(nextSearchesToStart = t,
                          runningSearchIDs = nextRunningSearchID,
                          nbFinishedSearches = nbFinishedSearches+1,
                          nbStartingAndRunningSearches = nbStartingAndRunningSearches,
                          currentIt = currentIt)

                      case Nil if nbStartingAndRunningSearches == 1 => // because we just finished one, actually
                        resultPromise.success(w) //it is a NoMoveFound
                        Behaviors.stopped
                      case Nil =>

                        next(nextSearchesToStart = Nil,
                          runningSearchIDs = nextRunningSearchID,
                          nbFinishedSearches = nbFinishedSearches+1,
                          nbStartingAndRunningSearches = nbStartingAndRunningSearches - 1,
                          currentIt = currentIt)

                    }
                }

              case _:SearchCrashed =>
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

            context.ask[DelegateSearch, SearchEnded](supervisor.supervisorActor, ref => DelegateSearch(request, ref, uniqueID)) {
              case Success(searchEnded) => WrappedSearchEnded(searchEnded, neighborhoodIndice, uniqueID)
              case Failure(_) => WrappedError(msg = Some(s"Error in WrappedGotUniqueID, uniqueID=$uniqueID"))
            }

            next(nextSearchesToStart = nextSearchesToStart,
              runningSearchIDs = uniqueID :: runningSearchIDs,
              nbStartingAndRunningSearches = nbStartingAndRunningSearches,
              nbFinishedSearches = nbFinishedSearches,
              currentIt = currentIt)

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

