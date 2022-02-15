package oscar.cbls.lib.search.neighborhoods.vlsn

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorSystem, Behavior}
import akka.util.Timeout
import com.sun.javafx.css.Combinator
import oscar.cbls.core.computation.Store
import oscar.cbls.core.distrib._
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.{DistributedCombinator, LoadSolutionMove, MoveFound, Neighborhood, NeighborhoodCombinator, NoMoveFound, SearchResult}

import scala.annotation.tailrec
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.concurrent.duration.{Duration, DurationInt}
import scala.concurrent.{Await, Future, Promise}
import scala.util.{Failure, Success}


class DistributedVLSN(v:Int,
                      initVehicleToRoutedNodesToMove:() => Map[Int,SortedSet[Int]],
                      initUnroutedNodesToInsert:() => SortedSet[Int],
                      nodeToRelevantVehicles:() => Map[Int,Iterable[Int]],

                      targetVehicleNodeToInsertNeighborhood:Int => Int => Neighborhood,
                      targetVehicleNodeToMoveNeighborhood:Int => Int => Neighborhood,
                      nodeToRemoveNeighborhood:Int => Neighborhood,
                      removeAndReInsert:Int => () => Unit,

                      vehicleToObjectives:Array[Objective],
                      unroutedPenalty:Objective,
                      globalObjective:Objective,


                      enrichment:Option[EnrichmentParameters] = None
                     )
  extends NeighborhoodCombinator() {

  require(v == vehicleToObjectives.length)
  var remoteExplorerIdentification:RemoteExplorerIdentification = _
  var supervisor:Supervisor = _

  val store:Store = globalObjective.model

  override def labelAndExtractRemoteTasks(supervisor: supervisor,
                                          currentID: Int,
                                          nbDistributedCombinators: Int,
                                          acc: List[RemoteTask[_]]): (Int, Int, List[RemoteTask[_]]) = {
    val (a,b,c,d) = new RemoteTasks(vehicleToObjectives,
      globalObjective,
      unroutedPenalty,
      targetVehicleNodeToInsertNeighborhood,
      targetVehicleNodeToMoveNeighborhood,
      removeAndReInsert).labelAndExtractRemoteTasks(supervisor,
      currentID,
      nbDistributedCombinators,
      acc)

    remoteExplorerIdentification = d
    this.supervisor = supervisor
    (a,b,c)
  }

  override def getMove(obj: Objective, initialObj: Long, acceptanceCriteria: (Long, Long) => Boolean):SearchResult = {
    val nbWorkers = supervisor.waitForAtLestOneWorker()
    require(nbWorkers >= 1, "at least one worker is needed")


    //for the final result; we use the Message to VLSN for ease
    //but this is a bit idiotic since it does not carry the same data
    val resultPromise = Promise[MessageToVLSN]()
    val futureResult : Future[MessageToVLSN] = resultPromise.future

    implicit val system: ActorSystem[_] = supervisor.system
    //TODO look for the adequate timeout supervisor
    implicit val timeout: Timeout = 1.hour

    supervisor.spawnNewActor(Behaviors.setup { context:ActorContext[MessageToVLSN] => {
      init(nbWorkers, context)
    }}, "DistributedVLSN")

    @tailrec
    def init(nbWorkers:Int, context: ActorContext[MessageToVLSN]): Behavior[MessageToVLSN] = {

    }

    def startingVLSNEnrichment(cachedExplorations: Option[CachedExplorations],
                               hotRestart:Option[HotRestartInfo],
                               vehicleToRoutedNodes:Map[Int,Iterable[Int]],
                               unroutedNodesToInsert:Iterable[Int],
                               context: ActorContext[MessageToVLSN]): Behavior[MessageToVLSN] = {

      val moveExplorer:DistributedMoveExplorer = new DistributedMoveExplorer(
        v,
        vehicleToRoutedNodes,
        unroutedNodesToInsert,
        nodeToRelevantVehicles(),

        nodeToRemoveNeighborhood:Int => Neighborhood,

        vehicleToObjectives:Array[Objective],
        unroutedPenalty:Objective,
        globalObjective:Objective,

        cachedExplorations,
        hotRestart,
        enrichment.getOrElse(
          EnrichmentParameters(injectAllCacheBeforeEnriching = false,
            minNbEdgesToExplorePerLevel = Int.MaxValue,
            minNbAddedEdgesPerLevel =  Int.MaxValue,
            nbEdgesPerBundle =  Int.MaxValue,
            nbEdgesPerPriorityBundle = Int.MaxValue)
        ),
        remoteExplorerIdentification:RemoteExplorerIdentification,
        store,
        supervisor
      )

      val nbStartedBatches =
        moveExplorer.startEnrichmentBatchesForTheLevel(dirtyNodes = None,
          dirtyVehicles = None,
          context,
          verbose)

      enrichingVLSNGraph(moveExplorer,
        nbStartedBatches,
        dirtyNodes = SortedSet.empty,
        liveNodes = Array.fill(moveExplorer.nbNodesInVLSNGraph)(true),
        dirtyVehicles = SortedSet.empty,
        cycles = List.empty,
        vehicleToRoutedNodes,
        unroutedNodesToInsert,
      )
    }

    def enrichingVLSNGraph(explorer:DistributedMoveExplorer,
                           nbRunningExplorations:Int,
                           dirtyNodes:SortedSet[Int],
                           liveNodes:Array[Boolean],
                           dirtyVehicles:SortedSet[Int],
                           cycles:List[List[Edge]],
                           vehicleToRoutedNodes:Map[Int,Iterable[Int]],
                           unroutedNodesToInsert:Iterable[Int],
                          ): Behavior[MessageToVLSN] = {
      Behaviors.receive { (context, command) =>
        command match {
          case i: InjectionReady =>
            i.injectMoves()

            enrichingVLSNGraph(explorer, nbRunningExplorations - 1)

            if (nbRunningExplorations == 0) {
              //perform cycle detection, in the agent

              //store the cycles
              enrichToNextLevel(explorer: DistributedMoveExplorer,
                nbRunningExplorations: Int,
                dirtyNodes: SortedSet[Int],
                liveNodes: Array[Boolean],
                dirtyVehicles: SortedSet[Int],
                cycles: List[List[Edge]],
                vehicleToRoutedNodes: Map[Int, Iterable[Int]],
                unroutedNodesToInsert: Iterable[Int])
            }

          case e: WrappedError =>
            resultPromise.success(e)
            Behaviors.stopped
        }
      }
    }

    def enrichToNextLevel(explorer:DistributedMoveExplorer,
                          nbRunningExplorations:Int,
                          dirtyNodes:SortedSet[Int],
                          liveNodes:Array[Boolean],
                          dirtyVehicles:SortedSet[Int],
                          cycles:List[List[Edge]],
                          vehicleToRoutedNodes:Map[Int,Iterable[Int]],
                          unroutedNodesToInsert:Iterable[Int]
                         ): Behavior[MessageToVLSN] = {
      //enriching the graph again
      if (explorer.allMovesExplored) {
        if (cycles.nonEmpty) {
          //moves were found
          //commit al lthe stored moves on the model
          //re-optimization?!
          //we have to restart the VLSN from the cache

          startingVLSNEnrichment(cachedExplorations: Option[CachedExplorations],
            hotRestart:Option[HotRestartInfo],
            vehicleToRoutedNodes:Map[Int,Iterable[Int]],
            unroutedNodesToInsert:Iterable[Int],
            context: ActorContext[MessageToVLSN])
        } else {
          //finished

        }
      }else{
        //enrich the VLSN further
        startingVLSNEnrichment(???,context)
      }
    }



    Await.result(futureResult, Duration.Inf) match {
      case f:FinalResult =>
        f.solution match {
          case None => NoMoveFound
          case Some((solution,objAfter)) => MoveFound(LoadSolutionMove(solution,objAfter))
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
      case e => throw new Error(s"Uknown error in DistributedVLSN : $e")
    }
  }
}

