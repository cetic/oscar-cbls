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
 * @param tabuLength when a neighborhood does not find a move, we set it to a very low priority
 *                   for a number of iteration equal to "the number of available workers" + tabulength
 */
class DistributedBestSlopeFirst(neighborhoods:Array[Neighborhood],
                                refresh: Int = 100,
                                tabuLength:Int = 4)
  extends DistributedCombinator(neighborhoods) {

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

    //TODO:  we might use fewer workers,
    // so that they would be available faster for the next round
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
    case class WrappedSearchEnded(searchEnded:SearchEnded[IndependentSearchResult], neighborhoodIndice: Int, priorityOfSearch:Int, uniqueId: Long) extends WrappedData
    case class WrappedGotUniqueID(uniqueID: Long, neighborhoodIndice: Int,priorityOfSearch:Int) extends WrappedData
    case class WrappedError(msg: Option[String]=None, crash:Option[SearchCrashed] = None) extends WrappedData

    implicit val system: ActorSystem[_] = supervisor.system
    //TODO look for the adequate timeout supervisor
    implicit val timeout: Timeout = 1.hour

    supervisor.spawnNewActor(Behaviors.setup { context:ActorContext[WrappedData] => {
      init(getSortedNeighborhoods(currentIt), priorityOfNextSearch = 0, nbWorkers, nbStartingAndRunningSearches = 0, context)
    }}, "DistributedBestSlopeFirst")

    @tailrec
    def init(neighborhoodList: List[Int], priorityOfNextSearch:Int, nbSearchesToStart: Int, nbStartingAndRunningSearches:Int, context: ActorContext[WrappedData]): Behavior[WrappedData] = {
      neighborhoodList match {
        case h :: t if nbSearchesToStart > 0 =>
          initiateNeighborhoodExploration(h, priorityOfNextSearch, context)
          init(t, priorityOfNextSearch + 1, nbSearchesToStart - 1, nbStartingAndRunningSearches+1, context)
        case _ =>
          next(nextSearchesToStart = neighborhoodList,
            priorityOfNextSearch = priorityOfNextSearch,
            runningSearchIDs = List.empty,
            nbFinishedSearches  = 0,
            nbStartingAndRunningSearches = nbStartingAndRunningSearches,
            nbStartedSearches = 0,
            currentIt = currentIt,
            responses = Array.fill(neighborhoods.length)(null))
      }
    }

    def next(nextSearchesToStart : List[Int],
             priorityOfNextSearch:Int,
             runningSearchIDs:List[Long],
             nbFinishedSearches: Int,
             nbStartingAndRunningSearches:Int,
             nbStartedSearches:Int,
             currentIt:Int,
             responses:Array[IndependentSearchResult]): Behavior[WrappedData] = {
      Behaviors.receive{ (context, command) =>
        command match {
          case w@WrappedSearchEnded(searchEnded:SearchEnded[IndependentSearchResult], neighborhoodIndice: Int, priorityOfSearch:Int, uniqueId: Long) =>
            searchEnded match {
              case w:SearchCrashed =>
                for (r <- runningSearchIDs) {
                  supervisor.supervisorActor ! CancelSearchToSupervisor(r)
                }
                resultPromise.success(WrappedError(msg = Some("DistributedBestSlopeFirst triggered distant error"),crash = Some(w)))
                Behaviors.stopped

              case SearchCompleted(searchID: Long, searchResult: IndependentSearchResult, durationMS) =>

                responses(priorityOfSearch) = searchResult

                durationsMs(neighborhoodIndice) = durationsMs(neighborhoodIndice) + durationMS

                searchResult match{
                  case moveFound: IndependentMoveFound =>
                    deltaObjs(neighborhoodIndice) += moveFound.objAfter - initialObj
                  case _: IndependentNoMoveFound => ;
                    tabuUntilIt(neighborhoodIndice) = currentIt + tabuLength + 1 + nbWorkers
                }
                val nextRunningSearchID = runningSearchIDs.filter(_ != uniqueId)

                //iterate on the array
                //we stop the search successful if there is a MoveFound preceded only by NoMoveFound
                //We stop the search NoMoveFound if there are only NoMoveFound
                //we carry on if there is a null preceded only by NoMoveFound

                //we start new searches if there are more searches to start

                def stopSearch(searchResult:IndependentSearchResult): Behavior[WrappedData] ={
                  for (r <- runningSearchIDs) {
                    supervisor.supervisorActor ! CancelSearchToSupervisor(r)
                  }
                  resultPromise.success(
                    WrappedSearchEnded(searchEnded = SearchCompleted(uniqueSearchID = 0, searchResult, 0),
                      neighborhoodIndice = 0, priorityOfSearch = 0, uniqueId = 0))
                  Behaviors.stopped
                }

                @tailrec
                def checkStopNoMoveFoundSoFar(i:Int): Behavior[WrappedData] ={
                  if(i == responses.length){
                    //only noMoveFound, so return NoMoveFound
                    stopSearch(searchResult = responses(0))
                  }else{
                    responses(i) match{
                      case null =>
                        //waiting for more responses, so carry on, and start one more search if possible

                        nextSearchesToStart match {
                          case h :: t =>
                            initiateNeighborhoodExploration(h, priorityOfSearch = priorityOfNextSearch, context)

                            next(nextSearchesToStart = t,
                              priorityOfNextSearch + 1,
                              runningSearchIDs = nextRunningSearchID,
                              nbFinishedSearches = nbFinishedSearches + 1,
                              nbStartingAndRunningSearches = nbStartingAndRunningSearches,
                              nbStartedSearches = nbStartedSearches,
                              currentIt = currentIt,
                              responses = responses)
                          case Nil =>
                            next(nextSearchesToStart = Nil,
                              priorityOfNextSearch,
                              runningSearchIDs = nextRunningSearchID,
                              nbFinishedSearches = nbFinishedSearches + 1,
                              nbStartingAndRunningSearches = nbStartingAndRunningSearches,
                              nbStartedSearches = nbStartedSearches,
                              currentIt = currentIt,
                              responses = responses)
                        }

                      case moveFound: IndependentMoveFound =>
                        //we can return this one

                        //check that there is no better solution, actually in the array
                        var best = moveFound
                        for(j <- responses.indices){
                          responses(j) match{
                            case m:IndependentMoveFound if m.objAfter < best.objAfter =>
                              best = m
                            //if(i != j) println(s"worth it! i:$i j:$j objI:${moveFound.objAfter} objJ:${m.objAfter}")
                            case _ => ;
                          }
                        }

                        stopSearch(searchResult = best)
                      case _: IndependentNoMoveFound =>
                        //so far, no Move found, goto next position in the array
                        checkStopNoMoveFoundSoFar(i+1)
                    }
                  }
                }

                checkStopNoMoveFoundSoFar(0)
            }

          case WrappedGotUniqueID(uniqueID: Long, neighborhoodIndice: Int,priorityOfSearch:Int) =>
            //start search with val request

            context.ask[DelegateSearch, SearchEnded[IndependentSearchResult]](supervisor.supervisorActor, ref => DelegateSearch(SingleMoveSearch(
              uniqueSearchId = uniqueID,
              remoteTaskId = remoteNeighborhoodIdentifications(neighborhoodIndice),
              acc = acceptanceCriteria,
              obj = independentObj,
              sendFullSolution = false,
              startSolutionOpt = startSol,
              sendResultTo = ref), waitForMoreSearch = nbStartedSearches < nbWorkers-1)) {
              case Success(searchEnded) => WrappedSearchEnded(searchEnded, neighborhoodIndice, priorityOfSearch, uniqueID)
              case Failure(_) => WrappedError(msg = Some(s"Error in WrappedGotUniqueID, uniqueID=$uniqueID"))
            }

            next(nextSearchesToStart = nextSearchesToStart,
              priorityOfNextSearch =  priorityOfNextSearch:Int,
              runningSearchIDs = uniqueID :: runningSearchIDs,
              nbFinishedSearches = nbFinishedSearches,
              nbStartingAndRunningSearches = nbStartingAndRunningSearches,
              nbStartedSearches = nbStartedSearches + 1,
              currentIt = currentIt,
              responses = responses)

          case w: WrappedError =>
            for(r <- runningSearchIDs) {
              supervisor.supervisorActor ! CancelSearchToSupervisor(r)
            }
            resultPromise.success(w)
            Behaviors.stopped
        }
      }
    }

    def initiateNeighborhoodExploration(neighborhoodIndice:Int, priorityOfSearch:Int, context: ActorContext[WrappedData]): Unit = {
      context.ask[GetNewUniqueID,Long](supervisor.supervisorActor,ref => GetNewUniqueID(ref)) {
        case Success(uniqueID:Long) => WrappedGotUniqueID(uniqueID,neighborhoodIndice,priorityOfSearch)
        case Failure(ex) => WrappedError(msg=Some(s"Supervisor actor timeout : ${ex.getMessage}"))
      }
    }

    Await.result(futureResult, Duration.Inf) match {
      case w:WrappedSearchEnded =>
        w.searchEnded match {
          case c:SearchCompleted[IndependentSearchResult] => c.searchResult.getLocalResult(obj.model)
          case _ => NoMoveFound
        }
      case WrappedError(msg:Option[String],crash:Option[SearchCrashed]) =>
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

