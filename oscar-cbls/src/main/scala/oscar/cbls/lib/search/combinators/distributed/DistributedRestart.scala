package oscar.cbls.lib.search.combinators.distributed


import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.util.Timeout
import oscar.cbls.core.distrib._
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.{Neighborhood, NoMoveFound, SearchResult}
import oscar.cbls.visual.SingleFrameWindow

import scala.collection.immutable.SortedMap
import scala.concurrent.duration.{Duration, DurationInt}
import scala.concurrent.{Await, Future, Promise}
import scala.util.{Failure, Success}

/**
 * Performs a distributed restart search.
 * every time a new best obj is discovered, some ongoing searches are interrupted,
 * and new searches are started from the new best solution, with a randomization first.
 *
 * The restarts stop when a number of searches where performed starting from the best found solution
 * without any improvement over it.
 *
 * @param baseSearch the base neighborhood that is repeatedly searched to perform the descent
 * @param baseRandomize the randomization procedure in use. It is used for each new search performed,
 *                      except for the start, where one search is performed without randomization.
 * @param nbConsecutiveRestartWithoutImprovement the restarts stop when this number of restart was performed
 *                                               and finished without any improvement on the best known solution.
 * @param nbOngoingSearchesToCancelWhenNewBest whenever an improving solution is found,
 *                                             the strategy attempts to cancel this number of ongoing searches
 * @param factorOnObjForThresholdToContinueDespiteBeingCanceled whenever a search is cancelled, it can still carry on
 *                                                              if its current obj value is <= the best known obj * this value
 * @param maxWorkers the maximal number of searches that are allowed to run at the same time
 * @param performInitialNonRandomizeDescent set to false if there should not be an initial descent without randomization
 * @param gracefulStop if an improving solution is found past the stop criterion, start it all over again, otherwise ignore it
 * @param visu true for a visualization of the objective function and the performed searches
 * @param visuTitle the title of the visualization
 */
class DistributedRestart(baseSearch:Neighborhood,
                         baseRandomize:Neighborhood,
                         nbConsecutiveRestartWithoutImprovement:Int,
                         nbOngoingSearchesToCancelWhenNewBest:Int,
                         factorOnObjForThresholdToContinueDespiteBeingCanceled:Double = 1,
                         maxWorkers:Int,  //TODO: obtain this from the supervisor, and regularly update about it
                         performInitialNonRandomizeDescent:Boolean = true,
                         gracefulStop:Boolean = true,
                         visu:Boolean = false,
                         visuTitle:String = "distributedRestartObj")
  extends DistributedCombinator(
    Array(
      (_:List[Long]) => baseRandomize maxMoves 1 exhaust baseSearch,
      (_:List[Long]) => baseSearch)) {
  //0 is randomize and search
  //1 is first search

  //TODO: add a mechanism to perform complete search on the worker side, so that the worker can send progress info here, and a display can be provided with all the ongoing searches
  //TODO: add a mechanism to remove the 1 hour timeout on searches; this is ugly stuff

  override def getMove(obj: Objective, initialObj: Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {


    val independentObj = obj.getIndependentObj
    val model = obj.model
    val startSol = IndependentSolution(obj.model.solution())

    val resultPromise = Promise[WrappedData]()
    val futureResult: Future[WrappedData] = resultPromise.future

    abstract class WrappedData
    case class WrappedSearchEnded(searchEnded:SearchEnded) extends WrappedData
    case class WrappedGotUniqueID(uniqueID:Long,neighborhoodIndice:Int) extends WrappedData
    case class WrappedError(msg:Option[String] = None, crash:Option[SearchCrashed] = None) extends WrappedData
    case class WrappedFinalAnswer(move:Option[LoadIndependentSolutionMove]) extends WrappedData
    case class WrappedDisplay(display:ActorRef[SearchProgress]) extends WrappedData

    implicit val system: ActorSystem[_] = supervisor.system
    implicit val timeout: Timeout = 3.seconds


    supervisor.spawnNewActor(Behaviors.setup { context:ActorContext[WrappedData] => {
      if (visu) {
        val displayBehavior = Behaviors.setup { context: ActorContext[SearchProgress] => initDisplayActor(context) }
        context.ask[SpawnNewActor[SearchProgress], ActorRef[SearchProgress]](supervisor.supervisorActor, ref => SpawnNewActor(displayBehavior, "displayActor", ref)) {
          case Success(actorRef: ActorRef[SearchProgress]) => WrappedDisplay(actorRef)
          case Failure(_) => WrappedError(msg = Some("supervisor actor timeout1"))
        }
        waitVisu()
      } else {
        startSearch(None,context)
      }
    }},"DistributedRestart")

    def waitVisu() : Behavior[WrappedData] = {
      Behaviors.receive { (context, command) =>
        command match {
          case WrappedDisplay(displayActor) =>
            startSearch(Some(displayActor), context)
        }
      }
    }

    def startSearch(visu:Option[ActorRef[SearchProgress]],context:ActorContext[WrappedData]): Behavior[WrappedData] = {
      //starting up all searches
      context.log.info(s"starting restart search, init obj: $initialObj")

      context.ask[GetNewUniqueID,Long](supervisor.supervisorActor,ref => GetNewUniqueID(ref)) {
        case Success(uniqueID:Long) => WrappedGotUniqueID(uniqueID:Long,if(performInitialNonRandomizeDescent) 1 else 0)
        case Failure(_) => WrappedError(msg=Some("supervisor actor timeout1"))
      }

      for(i <- 1 until maxWorkers) {
        context.ask[GetNewUniqueID, Long](supervisor.supervisorActor, ref => GetNewUniqueID(ref)) {
          case Success(uniqueID: Long) => WrappedGotUniqueID(uniqueID: Long, 0)
          case Failure(_) => WrappedError(msg = Some("supervisor actor timeout2"))
        }
      }

      next(
        runningSearchIDsAndIsItFromBestSoFar = SortedMap.empty,
        bestObjSoFar = initialObj,
        bestMoveSoFar = None,
        nbCompletedSearchesOnBestSoFar = 0,
        display = visu)

    }

    def next(runningSearchIDsAndIsItFromBestSoFar:SortedMap[Long,Boolean],
             bestObjSoFar:Long,
             bestMoveSoFar:Option[LoadIndependentSolutionMove],
             nbCompletedSearchesOnBestSoFar:Int,
             display:Option[ActorRef[SearchProgress]]): Behavior[WrappedData] = {
      Behaviors.receive { (context, command) =>
        command match {
          case WrappedDisplay(displayActor) =>
            next(runningSearchIDsAndIsItFromBestSoFar,
              bestObjSoFar,
              bestMoveSoFar,
              nbCompletedSearchesOnBestSoFar,
              display = Some(displayActor))
          case w@WrappedSearchEnded(searchEnded: SearchEnded) =>
            searchEnded match {
              case SearchCompleted(searchID: Long, searchResult: IndependentSearchResult) =>

                searchResult match {
                  case moveFound: IndependentMoveFound =>
                    //does it improve over best SoFar?
                    if (moveFound.objAfter < bestObjSoFar) {
                      //We did improve over best so far

                      val wasCancelled = !runningSearchIDsAndIsItFromBestSoFar.isDefinedAt(searchID)
                      context.log.info(s"new solution: improved over best so far: ${moveFound.objAfter}" + (if (wasCancelled) " was conditionally cancelled" else ""))

                      var newRunning = runningSearchIDsAndIsItFromBestSoFar.flatMap({case (x:Long,y:Boolean) => if(x == searchID) None else Some((x,false))})

                      context.ask[GetNewUniqueID, Long](supervisor.supervisorActor, ref => GetNewUniqueID(ref)) {
                        case Success(uniqueID: Long) => WrappedGotUniqueID(uniqueID: Long, 0)
                        case Failure(_) => WrappedError(msg = Some("supervisor actor timeout3"))
                      }

                      var i = 0
                      while(i < nbOngoingSearchesToCancelWhenNewBest && newRunning.nonEmpty){
                        i += 1
                        val ongoingSearches = newRunning.keys.toArray
                        val selectedSearchNrToKill = scala.util.Random.self.nextInt(ongoingSearches.length)
                        val selectedSearchToKill = ongoingSearches(selectedSearchNrToKill)

                        newRunning = newRunning.-(selectedSearchToKill)
                        supervisor.supervisorActor ! CancelSearchToSupervisor(selectedSearchToKill,Some((moveFound.objAfter * factorOnObjForThresholdToContinueDespiteBeingCanceled).toLong))
                      }

                      if(i > 0) {
                        context.log.info(s"cancelled $i ongoing search to start new searches instead from bestSoFar")
                      }

                      next(newRunning,
                        bestObjSoFar = moveFound.objAfter,
                        bestMoveSoFar = Some(moveFound.move.asInstanceOf[LoadIndependentSolutionMove]),
                        nbCompletedSearchesOnBestSoFar = 0,
                        display)

                    }else{
                      //We did NOT improve over best so far

                      val wasRunningOnBestSoFar = runningSearchIDsAndIsItFromBestSoFar.getOrElse(searchID,false)
                      if(wasRunningOnBestSoFar){
                        //We were running on BestSoFar, so stop criterion progressed
                        if(nbCompletedSearchesOnBestSoFar +1 >= nbConsecutiveRestartWithoutImprovement){
                          //we finished :-)
                          context.log.info(s"new solution: not improved over best so far, was working on bestSoFar, finished, canceling ${runningSearchIDsAndIsItFromBestSoFar.size -1} ongoing searches finalOBj:$bestObjSoFar")

                          for(searchID <- runningSearchIDsAndIsItFromBestSoFar.keys){
                            //TODO: this one MUST be conditional, and we should WAIT for all searches to be concluded before exiting
                            supervisor.supervisorActor ! CancelSearchToSupervisor(searchID,Some((bestObjSoFar * factorOnObjForThresholdToContinueDespiteBeingCanceled).toLong))
                          }

                          val nbRunningSearchesToStop = runningSearchIDsAndIsItFromBestSoFar.size -1

                          nextCompleting(nbRunningSearches = nbRunningSearchesToStop,
                            bestObjSoFar,
                            bestMoveSoFar,
                            display,
                            context)
                        }else{
                          //progress on stop criterion, but not finished yet
                          context.log.info(s"new solution: not improved over best so far, was working on bestSoFar, not yet finished (${nbCompletedSearchesOnBestSoFar +1}/$nbConsecutiveRestartWithoutImprovement)")

                          context.ask[GetNewUniqueID, Long](supervisor.supervisorActor, ref => GetNewUniqueID(ref)) {
                            case Success(uniqueID: Long) => WrappedGotUniqueID(uniqueID: Long, 0)
                            case Failure(_) => WrappedError(msg = Some("supervisor actor timeout4"))
                          }

                          next(runningSearchIDsAndIsItFromBestSoFar.-(searchID),
                            bestObjSoFar = bestObjSoFar,
                            bestMoveSoFar = bestMoveSoFar,
                            nbCompletedSearchesOnBestSoFar+1,
                            display)
                        }
                      }else{
                        //We were NOT running on BestSoFar, so stop criterion NOT progressed
                        context.log.info(s"new solution: not improved over best so far, was not working on bestSoFar")

                        context.ask[GetNewUniqueID, Long](supervisor.supervisorActor, ref => GetNewUniqueID(ref)) {
                          case Success(uniqueID: Long) => WrappedGotUniqueID(uniqueID: Long, 0)
                          case Failure(_) => WrappedError(msg = Some("supervisor actor timeout5"))
                        }

                        next(runningSearchIDsAndIsItFromBestSoFar.-(searchID),
                          bestObjSoFar = bestObjSoFar,
                          bestMoveSoFar = bestMoveSoFar,
                          nbCompletedSearchesOnBestSoFar,
                          display)
                      }
                    }

                  case _: IndependentNoMoveFound =>
                    //this is more unlikely, but nevertheless we must support it
                    //We did NOT improve over best so far

                    val wasRunningOnBestSoFar = runningSearchIDsAndIsItFromBestSoFar.getOrElse(searchID,false)
                    if(wasRunningOnBestSoFar){
                      //We were running on BestSoFar, so stop criterion progressed
                      if(nbCompletedSearchesOnBestSoFar +1 >= nbConsecutiveRestartWithoutImprovement){
                        //we finished :-)

                        context.log.info(s"no move found, was working on bestSoFar, finished, canceling ${runningSearchIDsAndIsItFromBestSoFar.size -1} ongoing searches finalOBj:$bestObjSoFar")

                        for(searchID <- runningSearchIDsAndIsItFromBestSoFar.keys){
                          //TODO: this one MUST be conditional, and we should WAIT for all searches to be concluded before exiting
                          supervisor.supervisorActor ! CancelSearchToSupervisor(searchID,Some((bestObjSoFar * factorOnObjForThresholdToContinueDespiteBeingCanceled).toLong))
                        }

                        val nbRunningSearchesToStop = runningSearchIDsAndIsItFromBestSoFar.size - (if(runningSearchIDsAndIsItFromBestSoFar.isDefinedAt(searchID)) 1 else 0)

                        nextCompleting(nbRunningSearches = nbRunningSearchesToStop,
                          bestObjSoFar,
                          bestMoveSoFar,
                          display,
                          context)
                      }else{
                        //progress on stop criterion, but not finished yet

                        context.log.info(s"no move found, was working on bestSoFar, not yet finished  (${nbCompletedSearchesOnBestSoFar +1}/$nbConsecutiveRestartWithoutImprovement)")

                        context.ask[GetNewUniqueID, Long](supervisor.supervisorActor, ref => GetNewUniqueID(ref)) {
                          case Success(uniqueID: Long) => WrappedGotUniqueID(uniqueID: Long, 0)
                          case Failure(_) => WrappedError(msg = Some("supervisor actor timeout6"))
                        }

                        next(runningSearchIDsAndIsItFromBestSoFar.-(searchID),
                          bestObjSoFar = bestObjSoFar,
                          bestMoveSoFar = bestMoveSoFar,
                          nbCompletedSearchesOnBestSoFar+1,
                          display)
                      }
                    }else{
                      //We were NOT running on BestSoFar, so stop criterion NOT progressed

                      context.log.info(s"no move found, was not working on bestSoFar")

                      context.ask[GetNewUniqueID, Long](supervisor.supervisorActor, ref => GetNewUniqueID(ref)) {
                        case Success(uniqueID: Long) => WrappedGotUniqueID(uniqueID: Long, 0)
                        case Failure(_) => WrappedError(msg = Some("supervisor actor timeout7"))
                      }

                      next(runningSearchIDsAndIsItFromBestSoFar.-(searchID),
                        bestObjSoFar = bestObjSoFar,
                        bestMoveSoFar = bestMoveSoFar,
                        nbCompletedSearchesOnBestSoFar,
                        display)
                    }
                }
              case SearchAborted(_) =>
                //ignore it.
                context.log.info(s"got abort confirmation; starting new search")

                context.ask[GetNewUniqueID, Long](supervisor.supervisorActor, ref => GetNewUniqueID(ref)) {
                  case Success(uniqueID: Long) => WrappedGotUniqueID(uniqueID: Long, 0)
                  case Failure(_) => WrappedError(msg = Some("supervisor actor timeout11"))
                }

                next(runningSearchIDsAndIsItFromBestSoFar = runningSearchIDsAndIsItFromBestSoFar,
                  bestObjSoFar = bestObjSoFar,
                  bestMoveSoFar = bestMoveSoFar,
                  nbCompletedSearchesOnBestSoFar = nbCompletedSearchesOnBestSoFar,
                  display)

              case c: SearchCrashed =>
                for (r <- runningSearchIDsAndIsItFromBestSoFar.keys) {
                  supervisor.supervisorActor ! CancelSearchToSupervisor(r)
                }
                resultPromise.success(WrappedError(crash = Some(c)))
                Behaviors.stopped
            }

          case WrappedGotUniqueID(uniqueID: Long, neighborhoodIndice: Int) =>

            //start a search
            val request = SearchRequest(
              remoteNeighborhoods(neighborhoodIndice).getRemoteIdentification(Nil),
              acceptanceCriteria,
              independentObj,
              startSolution = bestMoveSoFar match{
                case Some(load) => load.s
                case None => startSol
              },
              doAllMoves = true,
              sendProgressTo = display,
              sendFullSolution = true)

            implicit val timeout: Timeout = 1.hour //TODO: put a proper value here;

            context.ask[DelegateSearch, SearchEnded](supervisor.supervisorActor, ref => DelegateSearch(request, ref, uniqueID)) {
              case Success(searchEnded) => WrappedSearchEnded(searchEnded)
              case Failure(_) => WrappedError(msg = Some("supervisor actor timeout8"))
            }

            next(runningSearchIDsAndIsItFromBestSoFar = runningSearchIDsAndIsItFromBestSoFar + (uniqueID -> true),
              bestObjSoFar = bestObjSoFar,
              bestMoveSoFar = bestMoveSoFar,
              nbCompletedSearchesOnBestSoFar = nbCompletedSearchesOnBestSoFar,
              display)

          case w: WrappedError =>

            for (r <- runningSearchIDsAndIsItFromBestSoFar.keys) {
              supervisor.supervisorActor ! CancelSearchToSupervisor(r)
            }
            resultPromise.success(w)
            Behaviors.stopped
        }
      }
    }


    def nextCompleting(nbRunningSearches:Int,
                       bestObjSoFar:Long,
                       bestMoveSoFar:Option[LoadIndependentSolutionMove],
                       display:Option[ActorRef[SearchProgress]],
                       context:ActorContext[WrappedData]): Behavior[WrappedData] = {

      if(!gracefulStop){
        context.log.info(s"non-graceful stop.")
        resultPromise.success(WrappedFinalAnswer(move=bestMoveSoFar))
        Behaviors.stopped
      } else if (nbRunningSearches == 0) {
        context.log.info(s"graceful stop: all search completed.")
        resultPromise.success(WrappedFinalAnswer(move=bestMoveSoFar))
        Behaviors.stopped
      } else {
        context.log.info(s"graceful stop: waiting for $nbRunningSearches to complete")
        Behaviors.receive { (context, command) =>
          command match {
            case WrappedDisplay(displayActor) =>
              nextCompleting(nbRunningSearches,
                bestObjSoFar,
                bestMoveSoFar,
                Some(displayActor),
                context)

            case w@WrappedSearchEnded(searchEnded: SearchEnded) =>
              searchEnded match {
                case SearchCompleted(searchID: Long, searchResult: IndependentSearchResult) =>
                  searchResult match {
                    case moveFound: IndependentMoveFound if moveFound.objAfter < bestObjSoFar =>
                      //We did improve over best so far, so we have to restart anyway

                      context.log.info(s"new solution: improved over best so far: ${moveFound.objAfter} although search criterion was reached, so restart search")

                      for (i <- (0 until (maxWorkers - nbRunningSearches + 1))) {
                        context.ask[GetNewUniqueID, Long](supervisor.supervisorActor, ref => GetNewUniqueID(ref)) {
                          case Success(uniqueID: Long) => WrappedGotUniqueID(uniqueID: Long, 0)
                          case Failure(_) => WrappedError(msg = Some("supervisor actor timeout3"))
                        }
                      }

                      next(SortedMap.empty,
                        bestObjSoFar = moveFound.objAfter,
                        bestMoveSoFar = Some(moveFound.move.asInstanceOf[LoadIndependentSolutionMove]),
                        nbCompletedSearchesOnBestSoFar = 0,
                        display)

                    case _ =>
                      nextCompleting(nbRunningSearches - 1,
                        bestObjSoFar,
                        bestMoveSoFar,
                        display,
                        context)
                  }

                case SearchAborted(_) =>
                  nextCompleting(nbRunningSearches - 1,
                    bestObjSoFar,
                    bestMoveSoFar,
                    display,
                    context)

                case c:SearchCrashed =>

                  resultPromise.success(WrappedError(crash = Some(c)))
                  Behaviors.stopped
              }
            case _:WrappedGotUniqueID =>
              nextCompleting(nbRunningSearches - 1,
                bestObjSoFar,
                bestMoveSoFar,
                display,
                context)

            case w: WrappedError =>

              resultPromise.success(w)
              Behaviors.stopped
          }
        }
      }
    }

    def initDisplayActor(context: ActorContext[SearchProgress]):Behavior[SearchProgress] = {
      val display = new DistributedObjDisplay()
      val window = SingleFrameWindow.show(display, visuTitle)

      nextDisplayActor(display,window)
    }

    def nextDisplayActor(display: DistributedObjDisplay, window:SingleFrameWindow): Behavior[SearchProgress] = {
      Behaviors.receive { (context, command) =>
        display.addValue(command.searchId, command.obj, command.timeMs, command.aborted)
        Behaviors.same
      }
    }

    //await seems to block the actor system??
    Await.result(futureResult, Duration.Inf) match{
      case WrappedSearchEnded(searchEnded:SearchEnded) =>
        searchEnded match{
          case SearchCompleted(searchID, searchResult: IndependentSearchResult) => searchResult.getLocalResult(obj.model)
        }
      case WrappedFinalAnswer(move:Option[LoadIndependentSolutionMove]) =>
        move match{
          case None => NoMoveFound
          case Some(load) => load.makeLocal(model)
        }
      case WrappedError(msg:Option[String],crash:Option[SearchCrashed])=>
        if(msg.isDefined){
          supervisor.shutdown()
          throw new Error(msg.get)
        }
        if(crash.isDefined){
          supervisor.throwRemoteExceptionAndShutDown(crash.get)
        }
        throw new Error("error in DistributedRestart")
      case x =>
        throw new Error("unknown error in DistributedFirst")
        null
    }
  }
}
