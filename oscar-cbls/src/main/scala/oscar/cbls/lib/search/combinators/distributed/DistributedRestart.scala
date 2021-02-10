package oscar.cbls.lib.search.combinators.distributed

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorSystem, Behavior}
import akka.util.Timeout
import oscar.cbls.core.distrib._
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.{Neighborhood, NoMoveFound, SearchResult}
import oscar.cbls.lib.search.combinators.Atomic

import scala.collection.immutable.SortedMap
import scala.concurrent.duration.{Duration, DurationInt}
import scala.concurrent.{Await, Future, Promise}
import scala.util.{Failure, Success}

class DistributedRestart(baseSearch:Neighborhood,
                         baseRandomize:Neighborhood,
                         nbConsecutiveRestartWithoutImprovement:Long,
                         maxWorkers:Int)
  extends DistributedCombinator(
    Array(
      (_:List[Long]) => Atomic(baseRandomize maxMoves 1 exhaust baseSearch,_=>false,aggregateIntoSingleMove = true),
      (_:List[Long]) => Atomic(baseSearch,_=>false,aggregateIntoSingleMove = true))) {
  //0 is randomize and search
  //1 is first search

  override def getMove(obj: Objective, initialObj: Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {

    val restartAndSearch = remoteNeighborhoods(0).getRemoteIdentification(Nil)
    val search = remoteNeighborhoods(1).getRemoteIdentification(Nil)

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

    implicit val system: ActorSystem[_] = supervisor.system
    implicit val timeout: Timeout = 3.seconds

    supervisor.spawnNewActor(Behaviors.setup { context:ActorContext[WrappedData] => {
      //starting up all searches

      context.ask[GetNewUniqueID,Long](supervisor.supervisorActor,ref => GetNewUniqueID(ref)) {
        case Success(uniqueID:Long) => WrappedGotUniqueID(uniqueID:Long,1)
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
        nbCompletedSearchesOnBestSoFar = 0)

    }},"DistributedRestart")

    def next(runningSearchIDsAndIsItFromBestSoFar:SortedMap[Long,Boolean],
             bestObjSoFar:Long,
             bestMoveSoFar:Option[LoadIndependentSolutionMove],
             nbCompletedSearchesOnBestSoFar:Int): Behavior[WrappedData] = {
      Behaviors.receive { (context, command) =>
        command match {
          case w@WrappedSearchEnded(searchEnded: SearchEnded) =>
            searchEnded match {
              case SearchCompleted(searchID: Long, searchResult: IndependentSearchResult) =>
                searchResult match {
                  case moveFound: IndependentMoveFound =>
                    //does it improve over best SoFar?
                    if (moveFound.objAfter < bestObjSoFar) {
                      //We did improve over best so far
                      context.log.info(s"new solution: improved over best so far")

                      val newRunning = runningSearchIDsAndIsItFromBestSoFar.flatMap({case (x:Long,y:Boolean) => if(x == searchID) None else Some((x,false))})

                      context.ask[GetNewUniqueID, Long](supervisor.supervisorActor, ref => GetNewUniqueID(ref)) {
                        case Success(uniqueID: Long) => WrappedGotUniqueID(uniqueID: Long, 0)
                        case Failure(_) => WrappedError(msg = Some("supervisor actor timeout3"))
                      }

                      next(newRunning,
                        bestObjSoFar = moveFound.objAfter,
                        bestMoveSoFar = Some(moveFound.move.asInstanceOf[LoadIndependentSolutionMove]),
                        nbCompletedSearchesOnBestSoFar = 0)
                    }else{
                      //We did NOT improve over best so far

                      val wasRunningOnBestSoFar = runningSearchIDsAndIsItFromBestSoFar(searchID)
                      if(wasRunningOnBestSoFar){
                        //We were running on BestSoFar, so stop criterion progressed
                        if(nbCompletedSearchesOnBestSoFar +1 >= nbConsecutiveRestartWithoutImprovement){
                          //we finished :-)
                          context.log.info(s"new solution: not improved over best so far, was working on bestSoFar, finished")

                          for(searchID <- runningSearchIDsAndIsItFromBestSoFar.keys){
                            supervisor.supervisorActor ! CancelSearchToSupervisor(searchID)
                          }

                          resultPromise.success(WrappedFinalAnswer(move=bestMoveSoFar))
                          Behaviors.stopped
                        }else{
                          //progress on stop criterion, but not finished yet
                          context.log.info(s"new solution: not improved over best so far, was working on bestSoFar, not yet finished")

                          context.ask[GetNewUniqueID, Long](supervisor.supervisorActor, ref => GetNewUniqueID(ref)) {
                            case Success(uniqueID: Long) => WrappedGotUniqueID(uniqueID: Long, 0)
                            case Failure(_) => WrappedError(msg = Some("supervisor actor timeout4"))
                          }

                          next(runningSearchIDsAndIsItFromBestSoFar.-(searchID),
                            bestObjSoFar = bestObjSoFar,
                            bestMoveSoFar = bestMoveSoFar,
                            nbCompletedSearchesOnBestSoFar+1)
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
                          nbCompletedSearchesOnBestSoFar)
                      }
                    }

                  case _: IndependentNoMoveFound =>
                    //this is more unlikely, but nevertheless we must support it
                    //We did NOT improve over best so far
                    context.log.info(s"noMoveFound")

                    val wasRunningOnBestSoFar = runningSearchIDsAndIsItFromBestSoFar(searchID)
                    if(wasRunningOnBestSoFar){
                      //We were running on BestSoFar, so stop criterion progressed
                      if(nbCompletedSearchesOnBestSoFar +1 >= nbConsecutiveRestartWithoutImprovement){
                        //we finished :-)

                        for(searchID <- runningSearchIDsAndIsItFromBestSoFar.keys){
                          supervisor.supervisorActor ! CancelSearchToSupervisor(searchID)
                        }

                        resultPromise.success(WrappedFinalAnswer(move=bestMoveSoFar))
                        Behaviors.stopped
                      }else{
                        //progress on stop criterion, but not finished yet

                        context.ask[GetNewUniqueID, Long](supervisor.supervisorActor, ref => GetNewUniqueID(ref)) {
                          case Success(uniqueID: Long) => WrappedGotUniqueID(uniqueID: Long, 0)
                          case Failure(_) => WrappedError(msg = Some("supervisor actor timeout6"))
                        }

                        next(runningSearchIDsAndIsItFromBestSoFar.-(searchID),
                          bestObjSoFar = bestObjSoFar,
                          bestMoveSoFar = bestMoveSoFar,
                          nbCompletedSearchesOnBestSoFar+1)
                      }
                    }else{
                      //We were NOT running on BestSoFar, so stop criterion NOT progressed
                      context.ask[GetNewUniqueID, Long](supervisor.supervisorActor, ref => GetNewUniqueID(ref)) {
                        case Success(uniqueID: Long) => WrappedGotUniqueID(uniqueID: Long, 0)
                        case Failure(_) => WrappedError(msg = Some("supervisor actor timeout7"))
                      }

                      next(runningSearchIDsAndIsItFromBestSoFar.-(searchID),
                        bestObjSoFar = bestObjSoFar,
                        bestMoveSoFar = bestMoveSoFar,
                        nbCompletedSearchesOnBestSoFar)
                    }
                }
              case SearchAborted(_) =>
                //ignore it.
                next(runningSearchIDsAndIsItFromBestSoFar = runningSearchIDsAndIsItFromBestSoFar,
                  bestObjSoFar = bestObjSoFar,
                  bestMoveSoFar = bestMoveSoFar,
                  nbCompletedSearchesOnBestSoFar = nbCompletedSearchesOnBestSoFar)

              case c: SearchCrashed =>
                for (r <- runningSearchIDsAndIsItFromBestSoFar.keys) {
                  supervisor.supervisorActor ! CancelSearchToSupervisor(r)
                }
                resultPromise.success(w)
                Behaviors.stopped
            }

          case WrappedGotUniqueID(uniqueID: Long, neighborhoodIndice: Int) =>

            context.log.info(s"startNewSearch")

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

            implicit val timeout: Timeout = 1.hour //TODO: put a proper value here!!!

            context.ask[DelegateSearch, SearchEnded](supervisor.supervisorActor, ref => DelegateSearch(request, ref, uniqueID)) {
              case Success(searchEnded) => WrappedSearchEnded(searchEnded)
              case Failure(_) => WrappedError(msg = Some("supervisor actor timeout8"))
            }

            next(runningSearchIDsAndIsItFromBestSoFar = runningSearchIDsAndIsItFromBestSoFar + (uniqueID -> true),
              bestObjSoFar = bestObjSoFar,
              bestMoveSoFar = bestMoveSoFar,
              nbCompletedSearchesOnBestSoFar = nbCompletedSearchesOnBestSoFar)

          case w: WrappedError =>

            for (r <- runningSearchIDsAndIsItFromBestSoFar.keys) {
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
