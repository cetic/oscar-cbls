package examples.oscar.cbls.distributed

import oscar.cbls._
import oscar.cbls.algo.generator.RandomGraphGenerator
import oscar.cbls.algo.graph._
import oscar.cbls.algo.search.KSmallest
import oscar.cbls.business.routing._
import oscar.cbls.business.routing.invariants.RouteLengthOnConditionalGraph
import oscar.cbls.core.computation.{CBLSIntConst, CBLSIntVar, IntValue, Store}
import oscar.cbls.core.distributed.Supervisor
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.{First, JumpNeighborhood, Neighborhood}
import oscar.cbls.lib.search.combinators.distributed.DistributedFirst
import oscar.cbls.visual.SingleFrameWindow
import oscar.cbls.visual.graph.TspBridgeVisu

import scala.collection.immutable.SortedSet
import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
import scala.concurrent.duration.DurationInt
import scala.language.implicitConversions
import scala.util.Random

object TspBridgeDistributed extends App {

  val n = 50
  val v = 5
  val nbNodes = 500
  val nbConditionalEdges = 350
  val nbNonConditionalEdges = 1600
  val nbTransitNodes = nbNodes

  println(Long.MaxValue)

  println("generating random graph")
  val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(
    nbNodes=nbNodes,
    nbConditionalEdges = nbConditionalEdges,
    nbNonConditionalEdges = nbNonConditionalEdges,
    nbTransitNodes = nbTransitNodes,
    seed = Some(1))
  println("end generating random graph")

  println("start Dijkstra")
  val underApproximatingDistanceInGraphAllBridgesOpen:Array[Array[Long]] = DijkstraDistanceMatrix.buildDistanceMatrix(graph, _ => true)
  println("end Dijkstra")

  def createSearchProcedure(withVisu:Boolean) : (Store,Neighborhood,Objective,() => Unit) = {

    val m = Store()

    //initially all bridges open
    val bridgeConditionArray = Array.tabulate(nbConditionalEdges)(c => CBLSIntVar(m, 1, 0 to 1, s"bridge_${c}_open"))

    val openBridges = filter(bridgeConditionArray).setName("openBridges")

    val costPerBridge = 20

    val bridgeCost: IntValue = cardinality(openBridges) * costPerBridge
    val myVRP = new VRP(m, n, v)

    val routeLengthInvar = RouteLengthOnConditionalGraph(
      myVRP.routes,
      n = n,
      v = v,
      openConditions = openBridges,
      nodeInRoutingToNodeInGraph = identity, //we keep it simple for this random example
      graph = graph,
      underApproximatingDistance = (a, b) => underApproximatingDistanceInGraphAllBridgesOpen(a)(b),
      distanceIfNotConnected = Int.MaxValue / 10,
      freeReturn = true)

    val neededConditions = routeLengthInvar.neededConditions

    val routeLength: IntValue = sum(routeLengthInvar.distancePerVehicle)

    val penaltyForUnrouted = 1000L

    val obj: Objective = routeLength + bridgeCost - length(myVRP.routes) * penaltyForUnrouted + CBLSIntConst(n * penaltyForUnrouted)

    m.close()
    println("finished model")

    // //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // visu

    val visu: TspBridgeVisu =
      if (withVisu) {
        val vv = new TspBridgeVisu(graph, v = v, n, (a, b) => underApproximatingDistanceInGraphAllBridgesOpen(a)(b))
        SingleFrameWindow.show(vv, s"TspBridge(tspN:$n tspV:$v graphN:$nbNodes graphE:${nbNonConditionalEdges + nbConditionalEdges} graphNCE:$nbNonConditionalEdges graphCE:$nbConditionalEdges)")
        vv
      } else null

    // //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // search

    val routedPostFilter = (node: Int) => (neighbor: Int) => myVRP.isRouted(neighbor)

    //this is an array, that, for each node in the routing problem,
    // keeps the sorted closest other point in the routing problem
    val closestRoutingPoint: Array[Iterable[Int]] = Array.tabulate(n)((nodeInGraph: Int) =>
      KSmallest.lazySort(
        Array.tabulate(n)(i => i),
        (otherNode: Int) => underApproximatingDistanceInGraphAllBridgesOpen(nodeInGraph)(otherNode)
      ))

    // Takes an unrouted node and insert it at the best position within the 10 closest nodes (inserting it after this node)
    def routeUnroutedPoint(k: Int) = insertPointUnroutedFirst(myVRP.unrouted,
      () => myVRP.kFirst(k, (x: Int) => closestRoutingPoint(x), routedPostFilter),
      myVRP,
      neighborhoodName = "InsertUF",
      hotRestart = false,
      selectNodeBehavior = First(), // Select the first unrouted node in myVRP.unrouted
      selectInsertionPointBehavior = First()) // Inserting after the first node in myVRP.kFirst(10,...)

    // Moves a routed node to a better place (best neighbor within the 10 closest nodes)
    def onePtMove(k: Int) = onePointMove(
      myVRP.routed,
      () => myVRP.kFirst(20, (x: Int) => closestRoutingPoint(x), routedPostFilter),
      myVRP)


    def myThreeOpt(k: Int) =
      threeOpt(potentialInsertionPoints = myVRP.routed,
        relevantNeighbors = () => myVRP.kFirst(k, (x: Int) => closestRoutingPoint(x), routedPostFilter),
        vrp = myVRP)

    def switchBridge = assignNeighborhood(bridgeConditionArray, "switchBridge")

    def swapBridge = swapsNeighborhood(bridgeConditionArray, "swapBridge")
    def swapBridgeMod(modulo:Int,shift:Int) = {
      val range = bridgeConditionArray.indices.filter(_%modulo == shift)
      swapsNeighborhood(bridgeConditionArray, searchZone1 =  () => range, name=s"swapBridge($modulo,$shift)")
    }

    def closeAllUselessBridges = new JumpNeighborhood("closeUselessBridges") {
      override def doIt(): Unit = {
        val neededCond = neededConditions.value
        for (c <- 0 until nbConditionalEdges if !(neededCond contains c)) {
          bridgeConditionArray(c) := 0
        }
      }
    }

    def closeUsedBridge = assignNeighborhood(bridgeConditionArray, name = "closeUsedBridge", searchZone = neededConditions)

    val search = (new DistributedFirst(Array(
      routeUnroutedPoint(50),
      onePtMove(20),
      myThreeOpt(20)))
      onExhaust {
      println(s"finished inserts; neededBridges:$neededConditions")
    }
      exhaust (closeAllUselessBridges maxMoves 1)
      exhaust (
      new DistributedFirst(
        Array(
          onePtMove(40),
          closeUsedBridge,
          myThreeOpt(40),
          onePtMove(20) andThen switchBridge name "switchAndMove",
          switchBridge,
          swapBridgeMod(5,0),
          swapBridgeMod(5,1),
          swapBridgeMod(5,2),
          swapBridgeMod(5,3),
          swapBridgeMod(5,4)
        ),
      )
        .onExhaustRestartAfterJump({
          //open up all bridges
          val partOfBridges = Random.shuffle((0 until nbConditionalEdges).toList).take(nbConditionalEdges/2)
          for (bridge <- partOfBridges) {
            bridgeConditionArray(bridge) := 1
          }},
          maxRestartWithoutImprovement = 2,
          minRestarts = 3,
          obj = obj,
          name = "OpenAllBridges"))
      )

    var nextDisplay:Long = 0
    val fullSearch =
      (if(withVisu) {
        search afterMove {
          if(System.currentTimeMillis() > nextDisplay) {
            visu.redraw(SortedSet.empty[Int] ++ openBridges.value.toList, myVRP.routes.value)
            nextDisplay = System.currentTimeMillis() + 500 //0.5 second
          }
        } showObjectiveFunction obj
      }else search)

    def finalPrint():Unit = {
      if(withVisu){
        visu.redraw(SortedSet.empty[Int] ++ openBridges.value.toList, myVRP.routes.value)
      }
      println(myVRP)
      println(openBridges)
      println(s"neededBridges:${routeLengthInvar.neededConditions.value}")
      println("routeLength:" + routeLength.value)
    }

    (m,fullSearch,obj,() => finalPrint())
  }

  //main search; distributed combinators delegate to worker
  val (store,search,obj,finalPrint) = createSearchProcedure(true)
  val supervisor = Supervisor.startSupervisorAndActorSystem(search,verbose=false,tic=1.seconds)

  val nbWorker = 6
  for (_ <- (0 until nbWorker).par) {
    //creating each worker, with its own model and search procedure (we do in in parallel)
    val (store2, search2, _, _) = createSearchProcedure(false)
    supervisor.createLocalWorker(store2, search2)
  }

  search.verbose = 2

  search.doAllMoves(obj = obj)

  supervisor.shutdown()

  finalPrint()
}

