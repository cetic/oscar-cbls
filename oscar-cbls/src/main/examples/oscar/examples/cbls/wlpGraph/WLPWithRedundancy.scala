package oscar.examples.cbls

import oscar.cbls._
import oscar.cbls.algo.graph.{ConditionalGraphWithIntegerNodeCoordinates, DijkstraDistanceMatrix}
import oscar.cbls.algo.search.KSmallest
import oscar.cbls.core.computation.ChangingIntValue
import oscar.cbls.lib.invariant.graph.KVoronoiZones
import oscar.cbls.lib.invariant.logic.Filter
import oscar.cbls.lib.invariant.set.Cardinality
import oscar.cbls.lib.search.neighborhoods._
import oscar.cbls.test.graph.RandomGraphGenerator
import oscar.cbls.util.StopWatch
import oscar.cbls.visual.graph.GraphViewer
import oscar.cbls.visual.{ColorGenerator, SingleFrameWindow}

import scala.collection.immutable.SortedMap
import scala.swing.Color

object WLPWithRedundancy extends App with StopWatch{
  //the number of warehouses
  val W:Int = 250


  //the number of delivery points
  val D:Int = 500

  // the number of per delivery points
  val k:Int = 3

  //nb conditional edges
  val nbConditionalEdges:Int =  (W + D) / 5

  //nb non conditional edges
  val nbNonConditionalEdges =  (W+D)*5
  val displayDelay = 200

  println("RedundantWarehouseAndBridgeLocation(W:" + W + " D:" + D + " B:" + nbConditionalEdges + ")")
  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  val costForOpeningWarehouse =  Array.fill[Long](W)(800)



  println("generate random graph")
  val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(
    nbNodes=(W+D),
    nbConditionalEdges=nbConditionalEdges,
    nbNonConditionalEdges=nbNonConditionalEdges,
    nbTransitNodes = W+D,
    mapSide = 800,
    seed = Some(2))


  val m = new Store()//checker = Some(new ErrorChecker))

  val deliveryToNode = Array.tabulate(D)(i => graph.nodes(i + W))
  val warehouseToNode =  Array.tabulate(W)(w => graph.nodes(w))

  val warehouseOpenArray = Array.tabulate(W)(i => new CBLSIntVar(m,0,0 to 1,"warehouse " + i + " open"))
  val openWarehouses : SetValue = Filter(warehouseOpenArray).setName("open warehouses")
  val closedWarehouses : SetValue = Filter(warehouseOpenArray,_ == 0).setName("closed warehouses")

  val conditionalEdgesOpenArray = Array.tabulate(nbConditionalEdges)(i => new CBLSIntVar(m,0,0 to 1,"conditional edge " + i + "open"))
  val openEdges = Filter(conditionalEdgesOpenArray).setName("conditional Edges Open")

  val centroid2Nodes = SortedMap[Long,Long]() ++ Array.tabulate(W)(i => i.toLong -> i.toLong).toMap

  val x = Cardinality(openEdges)

  var costOfBridgesPerBridge = 7

  println("init VZone")

  val kvor : KVoronoiZones= KVoronoiZones(graph,
    openEdges,
    openWarehouses,
    k,
    deliveryToNode.map(_.id.toLong),
    m,
    defaultDistanceForUnreachableNode = 1000
  )

  println("End Init VZone")

  val distanceToClosestCentroidMap = kvor.trackedNodeToDistanceAndCentroidMap

  val distanceToClosestCentroid = Array.tabulate(D)((i : Int) => distanceToClosestCentroidMap(deliveryToNode(i).id))

  val totalDistancePerNode = distanceToClosestCentroid.map(n => sum(n.map(i => i._2)))

  val totalDistance = sum(totalDistancePerNode)

  val nbWarehousesOpen : ChangingIntValue = Cardinality(openWarehouses)

  val obj = Objective(totalDistance + sum(costForOpeningWarehouse, openWarehouses) + (x*costOfBridgesPerBridge))

  m.close()

  val centroidColors = ColorGenerator.generateRandomColors(W)

  println("Visual Creation")
  val visual = new GraphViewer(graph:ConditionalGraphWithIntegerNodeCoordinates,
    centroidColor = SortedMap.empty[Int,Color] ++ warehouseToNode.toList.map(node => (node.id,centroidColors(node.id))),nbNodesPerNode = k)

  SingleFrameWindow.show(visual,title = "Redundant Warehouse and bridge location", 2125, 2125)

  visual.redrawMultipleNodes(
    openEdges.value,
    openWarehouses.value,
    distanceToClosestCentroidMap.mapValues(tab => tab.map(centroidAndDistance => centroidAndDistance._1.value)),
    k,
    extraCentroids = (0L until W).toArray,
    extraPath = List()
  )


  /**********************************************************************************************************************
    *
    *             Search Model
    *
    *
    *********************************************************************************************************************/

    val timeStartingModel = System.currentTimeMillis()
  val distanceMatrixAllEdgeOpen = DijkstraDistanceMatrix.buildDistanceMatrix(graph,_ => true)
  println("Time to compute matrix: " + (System.currentTimeMillis() - timeStartingModel))

  val warehouseToWarehouseDistance = Array.tabulate(W)(w1 => Array.tabulate(W)(w2 => w2.toLong).sortWith((w2_1 : Long,w2_2 : Long) => distanceMatrixAllEdgeOpen(w1)(w2_1) < distanceMatrixAllEdgeOpen(w1)(w2_2)))

  def kNearestOpenWarehouse(k : Int,w : Int) = KSmallest.kFirst(k,warehouseToWarehouseDistance(w),warehouseOpenArray(_).value == 1)
  def kNearestClosedWarehouse(k : Int,w : Int) = KSmallest.kFirst(k,warehouseToWarehouseDistance(w),warehouseOpenArray(_).value == 0)
  def kNearestWarehouse(k : Int,w : Int) = KSmallest.kFirst(k,warehouseToWarehouseDistance(w))

  def swapClosest(k : Int) =
    SwapsNeighborhood(warehouseOpenArray,
      name = "SwapWarehouse with " + k + " Closest",
      searchZone1 = () => openWarehouses.value,
      searchZone2 = () => (w : Long,_ : Long) => kNearestClosedWarehouse(k,w))

  def makeAssignClose(assign: AssignMove,k : Int) = {
    AssignNeighborhood(warehouseOpenArray,name = "assign Close",searchZone = () => kNearestClosedWarehouse(k,assign.id))

  }

  def open3Warehouses =
    profile(AssignNeighborhood(warehouseOpenArray,name = "Open 3 close Warehouses",searchZone = () => closedWarehouses.value) dynAndThen ((move : AssignMove) => makeAssignClose(move,10) andThen makeAssignClose(move,10)))

  val warehouseToEdgesDistance =
    Array.tabulate(W)(w1 => Array.tabulate(nbConditionalEdges)(c => c.toLong).sortWith((c1 : Long, c2 : Long) =>
      (distanceMatrixAllEdgeOpen(w1)(graph.conditionToConditionalEdges(c1).nodeA.id) min distanceMatrixAllEdgeOpen(w1)(graph.conditionToConditionalEdges(c1).nodeB.id)) < (distanceMatrixAllEdgeOpen(w1)(graph.conditionToConditionalEdges(c1).nodeA.id) min distanceMatrixAllEdgeOpen(w1)(graph.conditionToConditionalEdges(c1).nodeB.id))))

  def kNearestEdges(k : Int,w : Int) = KSmallest.kFirst(k,warehouseToEdgesDistance(w))

  def AssignCloseEdge(assign : AssignMove,k : Int) = {
    AssignNeighborhood(conditionalEdgesOpenArray,"assignEdgeClose",searchZone = () => kNearestEdges(k,assign.id))
  }

  val assignWarehouseAndEdge = profile(AssignNeighborhood(warehouseOpenArray,"SwitchWarehouseAndEdgeClose") dynAndThen(AssignCloseEdge(_,10)))

  val swapWarehouseThenAssignEdge =
    profile(swapClosest(5) dynAndThen((swap : SwapMove) => AssignNeighborhood(conditionalEdgesOpenArray,"SwitchCloseEdge",searchZone = () => KSmallest.kFirst(10,warehouseToEdgesDistance(swap.idI)))))

  var lastDisplay = this.getWatch
  println("Time to prepare: " + (System.currentTimeMillis() - timeStartingModel))
  val search =
    (bestSlopeFirst(
      List(
        profile(AssignNeighborhood(warehouseOpenArray,"Assign Warehouse")),
        profile(AssignNeighborhood(warehouseOpenArray,"OpenWarehouses",searchZone = () => openWarehouses.value)),
        profile(AssignNeighborhood(conditionalEdgesOpenArray,"Assign Edge")),
        assignWarehouseAndEdge,
        swapWarehouseThenAssignEdge,
        profile(swapClosest(20))
      )
    ) onExhaustRestartAfter(RandomizeNeighborhood(warehouseOpenArray, () => W/5,"Randomize1"), 4, obj)) afterMove (
      if(lastDisplay + displayDelay <= this.getWatch){ //} && obj.value < bestDisplayedObj) {

        visual.redrawMultipleNodes(
          openEdges.value,
          openWarehouses.value,
          distanceToClosestCentroidMap.mapValues(tab => tab.map(centroidAndDistance => centroidAndDistance._1.value)),
          k,
          extraPath = List(),
          extraCentroids = (0L until W).toArray)
        lastDisplay = this.getWatch
      })


  search.verbose = 2

  val start = System.currentTimeMillis()
  search.doAllMoves(obj = obj)




  println("fini en " + ((System.currentTimeMillis() - start)/60000) + "m")
  println(search.profilingStatistics)
  println(obj)

  visual.redrawMultipleNodes(
    openEdges.value,
    openWarehouses.value,
    distanceToClosestCentroidMap.mapValues(tab => tab.map(centroidAndDistance => {centroidAndDistance._1.value})),
    k,
    extraCentroids = (0L until W).toArray,
    extraPath = List()
  )

  println((0 until centroidColors.length).map(i => i + " : " + centroidColors(i).toString()).mkString("\n"))

}