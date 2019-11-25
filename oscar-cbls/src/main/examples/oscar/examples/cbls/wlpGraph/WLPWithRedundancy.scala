package oscar.examples.cbls

import java.io.PrintWriter

import oscar.cbls._
import oscar.cbls.algo.graph.ConditionalGraphWithIntegerNodeCoordinates
import oscar.cbls.core.computation.ChangingIntValue
import oscar.cbls.lib.invariant.graph.KVoronoiZones
import oscar.cbls.lib.invariant.logic.Filter
import oscar.cbls.lib.invariant.set.Cardinality
import oscar.cbls.lib.search.combinators._
import oscar.cbls.lib.search.neighborhoods._
import oscar.cbls.test.graph.RandomGraphGenerator
import oscar.cbls.util.StopWatch
import oscar.cbls.visual.{ColorGenerator, SingleFrameWindow}
import oscar.cbls.visual.graph.GraphViewer

import scala.collection.immutable.SortedMap
import scala.swing.Color

object WarehouseAndBridgeLocation extends App with StopWatch{
  //the number of warehouses
  val W:Int = 4

  //the number of delivery points
  val D:Int = 20

  // the number of per delivery points
  val k:Int = 2

  //nb conditional edges
  val nbConditionalEdges:Int =  (W + D) / 5

  //nb non conditional edges
  val nbNonConditionalEdges =  (W+D)*3

  val displayDelay = 1000

  println("WarehouseAndBridgeLocation(W:" + W + " D:" + D + " B:" + nbConditionalEdges + ")")
  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  val costForOpeningWarehouse =  Array.fill[Long](W)(800)



  println("generate random graph")
  val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(
    nbNodes=(W+D),
    nbConditionalEdges=nbConditionalEdges,
    nbNonConditionalEdges=nbNonConditionalEdges,
    nbTransitNodes = W+D,
    mapSide = 200)

//
 // println(graph.nodes.mkString("\n"))
//  println(graph.nodes(875).incidentEdges.mkString(";"))
//  println(graph.nodes(698).incidentEdges.mkString(";"))
//  println(graph.nodes(31).incidentEdges.mkString(";"))
//  println(graph.edges.mkString("\n"))

  val m = new Store(checker = Some(new ErrorChecker))

  val deliveryToNode = Array.tabulate(D)(i => graph.nodes(i + W))
  val warehouseToNode =  Array.tabulate(W)(w => graph.nodes(w))

  val warehouseOpenArray = Array.tabulate(W)(i => new CBLSIntVar(m,0,0 to 1,"warehouse " + i + " open"))
  val openWarehouses : SetValue = Filter(warehouseOpenArray).setName("open warehouses")

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

  val distanceToClothestCentroidMap = kvor.trackedNodeToDistanceAndCentroidMap

  val distanceToClothestCentroid = Array.tabulate(D)((i : Int) => distanceToClothestCentroidMap(deliveryToNode(i).id))

  //println(Array.tabulate(D)(i => deliveryToNode(i).id + " : " + distanceToClothestCentroid(i).map(c => c._2).mkString(";")).mkString("\n"))

  val totalDistancePerNode = distanceToClothestCentroid.map(n => sum(n.map(i => i._2)))

  val totalDistance = sum(totalDistancePerNode)

  val nbWarehousesOpen : ChangingIntValue = Cardinality(openWarehouses)

  val obj = Objective(totalDistance + sum(costForOpeningWarehouse, openWarehouses) + (x*costOfBridgesPerBridge))
  m.registerForPartialPropagation(nbWarehousesOpen)

  m.close()

  val centroidColors = ColorGenerator.generateRandomColors(W)

  println("Visual Creation")
  val visual = new GraphViewer(graph:ConditionalGraphWithIntegerNodeCoordinates,
    centroidColor = SortedMap.empty[Int,Color] ++ warehouseToNode.toList.map(node => (node.id,centroidColors(node.id))))

  SingleFrameWindow.show(visual,title = "Warehouse and bridge location", 1025, 1105)

  println(Array.tabulate(graph.nbNodes)(i => i + " : " + graph.coordinates(i)).mkString("\n"))


  visual.redraw(
    openEdges.value,
    openWarehouses.value,
    distanceToClothestCentroidMap.mapValues(t => t(0)._1.value),
    extraPath = List()
  )

  val centroidList = List(0,1,2,3)
  centroidList.foreach(i=> warehouseOpenArray(i) := 1)

//  val centroidList = List(0,1,2,3,4,5,6)
//  centroidList.foreach(i => warehouseOpenArray(i) := 1)
  println(Array.tabulate(D)(i => deliveryToNode(i).id + " : " + distanceToClothestCentroid(i).map(c => c._1.toString + " - " + c._2).mkString(";")).mkString("\n"))
//
//  warehouseOpenArray(7) := 1
//  println(kvor.nodeLabeling(7).centroidList.mkString(";"))
//  println("Total Warehouses Open : " + nbWarehousesOpen.value)
//  println(kvor.nodeLabeling(7).centroidList.mkString(";"))
//
//  warehouseOpenArray(5) := 0
//  println(Array.tabulate(D)(i => deliveryToNode(i).id + " : " + distanceToClothestCentroid(i).map(c => c._1.toString + " - " + c._2).mkString(";")).mkString("\n"))
//
//  println(kvor.nodeLabeling(7).centroidList.mkString(";"))

//
//
//  conditionalEdgesOpenArray(0) := 1
//////  warehouseOpenArray(4) := 1
//  println(Array.tabulate(D)(i => deliveryToNode(i).id + " : " + distanceToClothestCentroid(i).map(c => c._1.toString + " - " + c._2).mkString(";")).mkString("\n"))
////
// conditionalEdgesOpenArray(0) := 0
//  ////  warehouseOpenArray(4) := 1
//  println(Array.tabulate(D)(i => deliveryToNode(i).id + " : " + distanceToClothestCentroid(i).map(c => c._1.toString + " - " + c._2).mkString(";")).mkString("\n"))

//
//  warehouseOpenArray(26) := 1
//  warehouseOpenArray(25) := 0
//
//
//  println(Array.tabulate(D)(i => deliveryToNode(i).id + " : " + distanceToClothestCentroid(i).map(c => c._1.toString + " - " + c._2).mkString(";")).mkString("\n"))


/*  def redraw(openConditions:SortedSet[Long],
             centroids:SortedSet[Long],
             nodeToCentroid:SortedMap[Long,Long],
             hideClosedEdges:Boolean = false,
             hideRegularEdges:Boolean = false,
             hideOpenEdges:Boolean=false,
             emphasizeEdges:Iterable[Edge] = List.empty,
             extraPath:Iterable[RevisableDistance])*/



  //SingleFrameWindow.show(visual,title = "Warehouse and bridge location", 1025, 1105)
//
//  println("toto 22")
//


  val search =
    BestSlopeFirst(
      List(
        Profile(AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse") andThen AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse")),
        Profile(AssignNeighborhood(conditionalEdgesOpenArray,"Open Edge") andThen AssignNeighborhood(conditionalEdgesOpenArray,"Open Edge")),
        Profile(AssignNeighborhood(conditionalEdgesOpenArray,"Open Edge") andThen AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse")),
        Profile(AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse") andThen AssignNeighborhood(conditionalEdgesOpenArray,"Open Edge"))
      )
    )


        /*),
        Profile(AssignNeighborhood(conditionalEdgesOpenArray, "SwitchConditions")))) afterMove (
      if(lastDisplay + displayDelay <= this.getWatch){ //} && obj.value < bestDisplayedObj) {
        bestDisplayedObj = obj.value

        visual.redraw(
          openEdges.value,
          openWarehouses.value,
          trackedNodeToDistanceAndCentroid.mapValues({case (v1, v2) => v2.value}),
          hideClosedEdges = false,
          hideRegularEdges = true,
          hideOpenEdges = false,
          emphasizeEdges = vor.spanningTree(deliveryNodeList),
          List(distanceMinMax.getPath) ::: selectedDistances.map(_.getPath).toList
        )

        lastDisplay = this.getWatch
      }) showObjectiveFunction(obj)*/


  search.verbose = 2
//

  val start = System.currentTimeMillis()
  //search.doAllMoves(obj = obj)
  println("fini en " + ((System.currentTimeMillis() - start)/60000) + "m")

  visual.redraw(
    openEdges.value,
    openWarehouses.value,
    distanceToClothestCentroidMap.mapValues(t => t(0)._1.value),
    extraPath = List()
  )


  conditionalEdgesOpenArray(0) := 1
  conditionalEdgesOpenArray(1) := 1
  conditionalEdgesOpenArray(2) := 1
  conditionalEdgesOpenArray(3) := 1
  println(distanceToClothestCentroidMap.get(5).get(0)._1.value)

  conditionalEdgesOpenArray(0) := 0
  conditionalEdgesOpenArray(1) := 1
  conditionalEdgesOpenArray(2) := 1
  conditionalEdgesOpenArray(3) := 1

  val nodesTikz = Array.tabulate(graph.nbNodes)(i => "\\coordinate (node_" + i + ") at (" + graph.coordinates(i)._1.toFloat/25 + "," + graph.coordinates(i)._2.toFloat/25 + ");").mkString("\n")
  val edgeTikz = graph.edges.map(e => "\\draw[edge" + (if (e.conditionID == None) "" else {if (openEdges.value.contains(e.conditionID.get)) ",red" else ",dashed"}) + "] (node_" + e.nodeA.id + ") -- (node_" + e.nodeB.id + ");").mkString("\n");
  val centroidDraw = Array.tabulate(W)(i => if (openWarehouses.value.contains(i)) "\\node[centroid,draw=colorCentroid" + i + ",fill=colorCentroid" + i + "!50] at (node_" + i + ") {};" else "").mkString("\n")
  val nodeDrawfst = Array.tabulate(D)(i => "\\node[normalFst" + (if (distanceToClothestCentroidMap.get(i+W).get(0)._1.value != -1) ",draw=colorCentroid" + distanceToClothestCentroidMap.get(i+W).get(0)._1.value + ",fill=colorCentroid"+  distanceToClothestCentroidMap.get(i+W).get(0)._1.value + "!50" else "") + "] at (node_" + (i + W) + ") {};").mkString("\n")
  val nodeDrawsnd = Array.tabulate(D)(i => "\\node[normalSnd" + (if (distanceToClothestCentroidMap.get(i+W).get(1)._1.value != -1) ",draw=colorCentroid" + distanceToClothestCentroidMap.get(i+W).get(1)._1.value + ",fill=colorCentroid"+  distanceToClothestCentroidMap.get(i+W).get(1)._1.value + "!50" else "") + "] at (node_" + (i + W) + ") {};").mkString("\n")

  val tikzfile = new PrintWriter("graph4.tex")
  tikzfile.write(nodeDrawsnd + "\n\n" + edgeTikz + "\n\n" + centroidDraw + "\n\n" + nodeDrawfst)
  tikzfile.close()

  println(graph.edges.filter(p => p.conditionID != None).mkString("\n"))
}