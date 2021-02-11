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

class DistributedRestart(baseSearch:Neighborhood,
                         baseRandomize:Neighborhood,
                         nbConsecutiveRestartWithoutImprovement:Int,
                         nbOngoingSearchesToCancelWhenNewBest:Int,
                         maxWorkers:Int,
                         performInitialNonRandomizeDescent:Boolean = true)
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
      //starting up all searches
      context.log.info(s"starting restart search, init obj: $initialObj")

      val displayBehavior = Behaviors.setup { context:ActorContext[SearchProgress] => initDisplayActor(context)}
      context.ask[SpawnNewActor[SearchProgress],ActorRef[SearchProgress]](supervisor.supervisorActor,ref => SpawnNewActor(displayBehavior,"displayActor", ref)) {
        case Success(actorRef: ActorRef[SearchProgress]) => WrappedDisplay(actorRef)
        case Failure(_) => WrappedError(msg = Some("supervisor actor timeout1"))
      }

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
        display = null)

    }},"DistributedRestart")

    def next(runningSearchIDsAndIsItFromBestSoFar:SortedMap[Long,Boolean],
             bestObjSoFar:Long,
             bestMoveSoFar:Option[LoadIndependentSolutionMove],
             nbCompletedSearchesOnBestSoFar:Int,
             display:ActorRef[SearchProgress]): Behavior[WrappedData] = {
      Behaviors.receive { (context, command) =>
        command match {
          case WrappedDisplay(displayActor) =>
            next(runningSearchIDsAndIsItFromBestSoFar,
              bestObjSoFar,
              bestMoveSoFar,
              nbCompletedSearchesOnBestSoFar,
              display = displayActor)
          case w@WrappedSearchEnded(searchEnded: SearchEnded) =>
            searchEnded match {
              case SearchCompleted(searchID: Long, searchResult: IndependentSearchResult) =>
                val wasAtuallyCancelled = !runningSearchIDsAndIsItFromBestSoFar.isDefinedAt(searchID)

                searchResult match {
                  case moveFound: IndependentMoveFound =>
                    //does it improve over best SoFar?
                    if (moveFound.objAfter < bestObjSoFar) {
                      //We did improve over best so far
                      context.log.info(s"new solution: improved over best so far: ${moveFound.objAfter}")

                      var newRunning = runningSearchIDsAndIsItFromBestSoFar.flatMap({case (x:Long,y:Boolean) => if(x == searchID) None else Some((x,false))})

                      if(!wasAtuallyCancelled) {
                        context.ask[GetNewUniqueID, Long](supervisor.supervisorActor, ref => GetNewUniqueID(ref)) {
                          case Success(uniqueID: Long) => WrappedGotUniqueID(uniqueID: Long, 0)
                          case Failure(_) => WrappedError(msg = Some("supervisor actor timeout3"))
                        }
                      }

                      var i = 0
                      while(i < nbOngoingSearchesToCancelWhenNewBest && newRunning.nonEmpty){
                        i += 1
                        val ongoingSearches = newRunning.keys.toArray
                        val selectedSearchNrToKill = scala.util.Random.self.nextInt(ongoingSearches.size)
                        val selectedSearchToKill = ongoingSearches(selectedSearchNrToKill)

                        context.ask[GetNewUniqueID, Long](supervisor.supervisorActor, ref => GetNewUniqueID(ref)) {
                          case Success(uniqueID: Long) => WrappedGotUniqueID(uniqueID: Long, 0)
                          case Failure(_) => WrappedError(msg = Some("supervisor actor timeout11"))
                        }

                        newRunning = newRunning.-(selectedSearchToKill)
                        supervisor.supervisorActor ! CancelSearchToSupervisor(selectedSearchToKill)
                      }

                      if(i >0) {
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
                            supervisor.supervisorActor ! CancelSearchToSupervisor(searchID)
                          }

                          resultPromise.success(WrappedFinalAnswer(move=bestMoveSoFar))
                          Behaviors.stopped
                        }else{
                          //progress on stop criterion, but not finished yet
                          context.log.info(s"new solution: not improved over best so far, was working on bestSoFar, not yet finished (${nbCompletedSearchesOnBestSoFar +1}/$nbConsecutiveRestartWithoutImprovement)")

                          if(!wasAtuallyCancelled) {
                            context.ask[GetNewUniqueID, Long](supervisor.supervisorActor, ref => GetNewUniqueID(ref)) {
                              case Success(uniqueID: Long) => WrappedGotUniqueID(uniqueID: Long, 0)
                              case Failure(_) => WrappedError(msg = Some("supervisor actor timeout4"))
                            }
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

                        if(!wasAtuallyCancelled) {
                          context.ask[GetNewUniqueID, Long](supervisor.supervisorActor, ref => GetNewUniqueID(ref)) {
                            case Success(uniqueID: Long) => WrappedGotUniqueID(uniqueID: Long, 0)
                            case Failure(_) => WrappedError(msg = Some("supervisor actor timeout5"))
                          }
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
                          supervisor.supervisorActor ! CancelSearchToSupervisor(searchID)
                        }

                        resultPromise.success(WrappedFinalAnswer(move=bestMoveSoFar))
                        Behaviors.stopped
                      }else{
                        //progress on stop criterion, but not finished yet

                        context.log.info(s"no move found, was working on bestSoFar, not yet finished  (${nbCompletedSearchesOnBestSoFar +1}/$nbConsecutiveRestartWithoutImprovement)")

                        if(!wasAtuallyCancelled) {
                          context.ask[GetNewUniqueID, Long](supervisor.supervisorActor, ref => GetNewUniqueID(ref)) {
                            case Success(uniqueID: Long) => WrappedGotUniqueID(uniqueID: Long, 0)
                            case Failure(_) => WrappedError(msg = Some("supervisor actor timeout6"))
                          }
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

                      if(!wasAtuallyCancelled) {
                        context.ask[GetNewUniqueID, Long](supervisor.supervisorActor, ref => GetNewUniqueID(ref)) {
                          case Success(uniqueID: Long) => WrappedGotUniqueID(uniqueID: Long, 0)
                          case Failure(_) => WrappedError(msg = Some("supervisor actor timeout7"))
                        }
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
                context.log.info(s"got abort confirmation")
                //TODO: why does it not show??

                next(runningSearchIDsAndIsItFromBestSoFar = runningSearchIDsAndIsItFromBestSoFar,
                  bestObjSoFar = bestObjSoFar,
                  bestMoveSoFar = bestMoveSoFar,
                  nbCompletedSearchesOnBestSoFar = nbCompletedSearchesOnBestSoFar,
                  display)

              case c: SearchCrashed =>
                for (r <- runningSearchIDsAndIsItFromBestSoFar.keys) {
                  supervisor.supervisorActor ! CancelSearchToSupervisor(r)
                }
                resultPromise.success(w)
                Behaviors.stopped
            }

          case WrappedGotUniqueID(uniqueID: Long, neighborhoodIndice: Int) =>


            /*
                        //start a search
                        val request = SearchRequest(
                          remoteNeighborhoods(neighborhoodIndice).getRemoteIdentification(Nil),
                          acceptanceCriteria,
                          independentObj,
                          startSolution = bestMoveSoFar match{
                            case Some(load) => load.s
                            case None => startSol
                          },
                          sendFullSolution = true)
            */

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
              sendProgressTo = Some(display),
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


    def initDisplayActor(context: ActorContext[SearchProgress]):Behavior[SearchProgress] = {
      val display = new DistributedObjDisplay("distributedRestartObj")
      val window = SingleFrameWindow.show(display, "distributedRestartObj")

      nextDisplayActor(display)
    }

    def nextDisplayActor(display: DistributedObjDisplay): Behavior[SearchProgress] = {
      Behaviors.receive { (context, command) =>
        display.addValue(command.searchId, command.obj)
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
