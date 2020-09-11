package oscar.examples.cbls.tspBridge

import oscar.cbls._
import oscar.cbls.algo.graph._
import oscar.cbls.algo.search.KSmallest
import oscar.cbls.business.routing._
import oscar.cbls.business.routing.invariants.RouteLengthOnConditionalGraph
import oscar.cbls.core.computation.{CBLSIntConst, CBLSIntVar, IntValue, Store}
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.{First, JumpNeighborhood}
import oscar.cbls.test.graph.RandomGraphGenerator
import oscar.cbls.visual.SingleFrameWindow

import scala.collection.immutable.SortedSet

object TspBridgeFreeReturn extends App {

  val n = 50
  val v = 5
  val nbNodes = 500
  val nbConditionalEdges = 350
  val nbNonConditionalEdges = 1600
  val nbTransitNodes = nbNodes

  println("generating random graph")
  val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(
    nbNodes=nbNodes,
    nbConditionalEdges = nbConditionalEdges,
    nbNonConditionalEdges = nbNonConditionalEdges,
    nbTransitNodes = nbTransitNodes,
    mapSide = 1000,
    seed = Some(1))
  println("end generating random graph")

  println("start Dijkstra")
  val underApproximatingDistanceInGraphAllBridgesOpen:Array[Array[Long]] = DijkstraDistanceMatrix.buildDistanceMatrix(graph, _ => true)
  println("end Dijkstra")

  val m = Store()//checker = Some(new ErrorChecker()))

  //initially all bridges open
  val bridgeConditionArray = Array.tabulate(nbConditionalEdges)(c => CBLSIntVar(m, 1, 0 to 1, "bridge_${c}_open"))

  val openBridges = filter(bridgeConditionArray).setName("openBridges")

  val costPerBridge = 40

  val bridgeCost:IntValue = cardinality(openBridges) * costPerBridge
  val myVRP = new VRP(m,n,v)

  val routeLengthInvar = RouteLengthOnConditionalGraph(
    myVRP.routes,
    n = n,
    v = v,
    openConditions = openBridges,
    nodeInRoutingToNodeInGraph = identity, //we keep it simple for this random example
    graph = graph,
    underApproximatingDistance = (a,b) => underApproximatingDistanceInGraphAllBridgesOpen(a)(b),
    distanceIfNotConnected = Int.MaxValue/10,
    freeReturn = true)

  val neededConditions = routeLengthInvar.neededConditions

  val routeLength:IntValue = sum(routeLengthInvar.distancePerVehicle)

  val penaltyForUnrouted  = 1000L

  val obj:Objective = routeLength + bridgeCost - length(myVRP.routes) * penaltyForUnrouted + CBLSIntConst(n * penaltyForUnrouted)

  m.close()
  println("finished model")

  // //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // visu

  val visu = new TspBridgeVisu(graph, v = v, n,(a,b) => underApproximatingDistanceInGraphAllBridgesOpen(a)(b),freeReturn = true)
  SingleFrameWindow.show(visu,s"TspBridge(tspN:$n tspV:$v graphN:$nbNodes graphE:${nbNonConditionalEdges + nbConditionalEdges} graphNCE:$nbNonConditionalEdges graphCE:$nbConditionalEdges)")
  // //////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  val routedPostFilter = (node:Int) => (neighbor:Int) => myVRP.isRouted(neighbor)

  //this is an array, that, for each node in the routing problem,
  // keeps the sorted closest other point in the routing problem
  val closestRoutingPoint:Array[Iterable[Int]] = Array.tabulate(n)((nodeInGraph:Int) =>
    KSmallest.lazySort(
      Array.tabulate(n)(i => i),
      (otherNode:Int) => underApproximatingDistanceInGraphAllBridgesOpen(nodeInGraph)(otherNode.toInt)
    ))

  // Takes an unrouted node and insert it at the best position within the 10 closest nodes (inserting it after this node)
  def routeUnroutedPoint(k:Int) = profile(insertPointUnroutedFirst(myVRP.unrouted,
    ()=>myVRP.kFirst(k,(x:Int) =>closestRoutingPoint(x),routedPostFilter),
    myVRP,
    neighborhoodName = "InsertUF",
    hotRestart = false,
    selectNodeBehavior = First(), // Select the first unrouted node in myVRP.unrouted
    selectInsertionPointBehavior = First())) // Inserting after the first node in myVRP.kFirst(10,...)

  // Moves a routed node to a better place (best neighbor within the 10 closest nodes)
  def onePtMove(k:Int) = onePointMove(
    myVRP.routed,
    ()=>myVRP.kFirst(20,(x:Int) =>closestRoutingPoint(x),routedPostFilter),
    myVRP)


  def myThreeOpt(k:Int) = profile(
    threeOpt(potentialInsertionPoints = myVRP.routed,
      relevantNeighbors =()=>myVRP.kFirst(k,(x:Int) =>closestRoutingPoint(x),routedPostFilter),
      vrp = myVRP))

  def switchBridge = assignNeighborhood(bridgeConditionArray,"switchBridge")

  def swapBridge = swapsNeighborhood(bridgeConditionArray,"swapBridge")

  def closeAllUselessBridges = new JumpNeighborhood("closeUselessBridges"){
    override def doIt(): Unit = {
      val neededCond = neededConditions.value
      for(c <- 0 until nbConditionalEdges if !(neededCond contains c)){
        bridgeConditionArray(c) := 0
      }
    }
  }

  def closeUsedBridge = profile(assignNeighborhood(bridgeConditionArray,name = "closeUsedBridge",searchZone = neededConditions))

  val search = (bestSlopeFirst(List(
    routeUnroutedPoint(50),
    myThreeOpt(20),
    profile(onePtMove(20))),refresh = 20)
    onExhaust {println(s"finished inserts; neededBridges:$neededConditions")}
    exhaust (profile(closeAllUselessBridges) maxMoves 1)
    exhaust (
    bestSlopeFirst(
      List(
        profile(onePtMove(40)),
        myThreeOpt(40),
        profile(swapBridge),
        closeUsedBridge,
        profile(onePtMove(20) andThen switchBridge name "switchAndMove"),
        profile(switchBridge)),
      refresh = 10)
      onExhaustRestartAfterJump(
      for(bridge <- bridgeConditionArray.indices){
        bridgeConditionArray(bridge) := 1
      },
      maxRestartWithoutImprovement = 2,
      obj,
      randomizationName = "OpenAllBridges"))
    afterMove{
    //println(openBridges.value.mkString(";"))
    visu.redraw(SortedSet.empty[Int] ++ openBridges.value.toList.map(_.toInt), myVRP.routes.value)
  }) showObjectiveFunction obj

  search.verbose = 1

  search.doAllMoves(obj = obj)

  println(search.profilingStatistics)

  println(myVRP)
  println(openBridges)
  println(s"neededBridges:${routeLengthInvar.neededConditions}")
  visu.redraw(SortedSet.empty[Int] ++ openBridges.value.toList, myVRP.routes.value)

  println(s"Route Length :$routeLength")

  //TODO: we should actually substract the open & not used bridges from the objective function, and never try to switch them.
}

