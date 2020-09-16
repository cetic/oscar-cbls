package oscar.examples.cbls.routing.limPDPBenchmark

import java.io.{File, PrintWriter}

import oscar.cbls._
import oscar.cbls.business.routing._
import oscar.cbls.business.routing.invariants.global.{GlobalConstraintCore, RouteLength}
import oscar.cbls.business.routing.invariants.timeWindow.{TimeWindowConstraint, TransferFunction}
import oscar.cbls.business.routing.invariants.vehicleCapacity.GlobalVehicleCapacityConstraint
import oscar.cbls.core.objective.CascadingObjective
import oscar.cbls.core.search.{Best, Neighborhood, NoMoveNeighborhood}
import oscar.cbls.lib.search.neighborhoods.vlsn._

import scala.annotation.tailrec
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.io.Source

object PDPTW_VLSN_Li_Lim_Benchmark_Simple extends App {
  val multFactor: Long = 1000

  runMany()
  def runOne(): Unit = {
    println("usage: This fileName enrichment partition enrichmentSpec shiftInsert")
    val fileName = args(0)
    val enrichment: Int = args(1).toInt
    val partition: Int = args(2).toInt
    val enrichmentSpec: Int = args(3).toInt
    val shiftInsert: Int = args(4).toInt
    val injectAllCacheBeforeEnriching: Boolean = args(5).toBoolean

    println(runBenchmark(fileName: String, enrichment: Int, partition: Int, enrichmentSpec: Int, shiftInsert: Int, injectAllCacheBeforeEnriching = injectAllCacheBeforeEnriching))
  }

  def runMany(): Unit = {
    println("usage: This fileName")

    //  try pw.print(s) finally pw.close()
    val fileNames = args(0).split(";")

    val resultFileName: String = args(1)
    val pw = new PrintWriter(new File(resultFileName))

    pw.println("start")
    pw.flush()
    System.gc()

    try {
      for(fileName <- fileNames) {
        pw.println(runBenchmark(fileName: String, enrichment = 0, partition = 0, enrichmentSpec = 0, shiftInsert = 0, injectAllCacheBeforeEnriching = false))
        pw.flush()
        System.gc()
//        pw.println(runBenchmark(fileName: String, enrichment = 0, partition = 0, enrichmentSpec = 0, shiftInsert = 0, injectAllCacheBeforeEnriching = false))
//        pw.flush()
//        System.gc()
//        pw.println(runBenchmark(fileName: String, enrichment = 0, partition = 0, enrichmentSpec = 0, shiftInsert = 0, injectAllCacheBeforeEnriching = false))
//        pw.flush()
//        System.gc()
        for (shiftInsert <- Some(0)) {
          for (partition <- Some(9)) {
            for (enrichmentSpec <- List(4)) {
              for(injectAllCacheBeforeEnriching <- List(true)){
                pw.println(runBenchmark(
                  fileName: String,
                  enrichment = 1,
                  partition,
                  enrichmentSpec,
                  shiftInsert,
                  injectAllCacheBeforeEnriching))
                pw.flush()
                System.gc()
              }
            }
          }
        }
      }
  }finally{
    pw.close()
  }
}



case class PDP(fromNode: Int, toNode: Int, demand: Int) {
  def nodeList: List[Int] = List(fromNode, toNode)
  def precedence: (Int, Int) = (fromNode, toNode)
}

def readData(fileName: String): (Int, Array[Array[Long]], List[PDP], Int, Array[TransferFunction]) = {

  //multiplie tout par 1000, puis ceil
  case class Node(id: Int, x: Long, y: Long, demand: Int, earlyLine: Long, deadline: Long, duration: Long, pickUP: Int, delivery: Int) {
    def distance(that: Node): Long = math.sqrt(((this.x - that.x) * (this.x - that.x) + (this.y - that.y) * (this.y - that.y)).toDouble).ceil.toLong
  }

  val s = Source.fromFile(fileName)
  val lines = s.getLines()

  val Array(v, capacity, _) = lines.next().split("\\t\\s*").map(_.toInt)

  var allNodesList: List[Node] = Nil
  while (lines.hasNext) {
  val nodeInfo = lines.next().split("\\t\\s*").map(_.toInt)
  //Node(id:Int,      x:Int,        y:Int,      demand:Int,  earlyLine:Int,deadline:Int,duration:Int,pickUP:Int,delivery:Int)
  allNodesList = Node(
  nodeInfo(0),
  x=multFactor*nodeInfo(1),
  y=multFactor*nodeInfo(2),
  demand=nodeInfo(3),
  earlyLine=multFactor*nodeInfo(4),
  deadline=multFactor*nodeInfo(5),
  duration=multFactor*nodeInfo(6),
  pickUP=nodeInfo(7),
  delivery=nodeInfo(8)) :: allNodesList
}

  s.close()

  val allNodesArray: Array[Node] = Array.fill(allNodesList.head.id + 1)(null)
  while (allNodesList.nonEmpty) {
  val head = allNodesList.head
  allNodesList = allNodesList.tail
  allNodesArray(head.id) = head
}

  //println(allNodesArray.mkString("\n"))

  val oscarN: Int = allNodesArray.length - 1 + v

  def oscarNodeToLinNode(oscarNode: Int): Int = if (oscarNode < v) 0 else oscarNode - v + 1

  def linNodeToOscarNode(linNode: Int): Int = {
  require(linNode != 0)
  linNode - 1 + v
}

  val singleNodeTransferFunctions = Array.tabulate(oscarN)(node => {

  val nodeData = allNodesArray(oscarNodeToLinNode(node))
  if (node < v) TransferFunction.createFromEarliestAndLatestArrivalTime(node, nodeData.earlyLine, nodeData.deadline)
  else TransferFunction.createFromEarliestAndLatestArrivalTime(node, nodeData.earlyLine, nodeData.deadline, nodeData.duration)
})

  val distanceMatrix = Array.tabulate(oscarN)(node1 => Array.tabulate(oscarN)(node2 =>
  allNodesArray(oscarNodeToLinNode(node1)).distance(allNodesArray(oscarNodeToLinNode(node2)))
  ))

  //pickUpDeliveryChains
  val pdps = allNodesArray.toList.flatMap(nodeData => {
  if (nodeData.id == 0) None
  else if (nodeData.pickUP == 0 && nodeData.delivery != 0)
  Some(PDP(fromNode = linNodeToOscarNode(nodeData.id), toNode = linNodeToOscarNode(nodeData.delivery), demand = nodeData.demand))
  else None
})
  (v, distanceMatrix, pdps, capacity, singleNodeTransferFunctions)
}


  def runBenchmark(fileName: String, enrichment: Int, partition: Int, enrichmentSpec: Int, shiftInsert: Int, injectAllCacheBeforeEnriching:Boolean): String = {

  var toReturn = s"file:\t${fileName.split("""\\""").last}"

  toReturn = toReturn + s"\tenrichment?:\t$enrichment\tpartition:\t$partition\tenrichmentSpec:\t$enrichmentSpec\tshiftInsert:\t$shiftInsert\tinjectAllCcheBefore:\t$injectAllCacheBeforeEnriching"

  val m = new Store(noCycle = false)

  val (v, symmetricDistance, pdpList, capacity, transferFunctions) = readData(fileName)
  val n = symmetricDistance.length

  println(s"VLSN(PDPTW) v:$v n:$n pdp:${pdpList.length}")

  val penaltyForUnrouted = 10000*multFactor

  val nodeToContentDelta = Array.fill(n)(0L)
  for (pdp <- pdpList) {
  nodeToContentDelta(pdp.fromNode) = pdp.demand.toLong
  nodeToContentDelta(pdp.toNode) = -pdp.demand.toLong
}

  val vehiclesCapacity = Array.fill(v)(capacity.toLong)

  val myVRP = new VRP(m, n, v)
  val vehicles = 0 until v

  val pickUpPointToDeliveryPoint = Array.fill(n)(-1)
  for(pdp <- pdpList){
  pickUpPointToDeliveryPoint(pdp.fromNode) = pdp.toNode
}

  val allPickupPoints:SortedSet[Int] = SortedSet(pdpList.map(_.fromNode):_*)

  def isPickup(i: Int):Boolean = pickUpPointToDeliveryPoint(i) != -1

  val gc = GlobalConstraintCore(myVRP.routes, v)

  // Distance
  val vehiclesRouteLength = RouteLength(gc, n, v, symmetricDistance(_)(_))

  //Time window constraints
  val timeWindowViolations = TimeWindowConstraint(gc, n, v, transferFunctions, symmetricDistance)
  val timeWindowConstraints = new ConstraintSystem(m)
  for (vehicle <- 0 until v) {
  timeWindowConstraints.add(timeWindowViolations(vehicle) === 0)
}

  m.registerForPartialPropagation(timeWindowViolations.toIndexedSeq: _*)

  //Chains
  val precedenceRoute = myVRP.routes.createClone()

  //precedences & sameRoute
  val precedencesConstraints = new ConstraintSystem(m)
  //TODO: we need a faster precedence constraint!
  val precedenceViolation = precedence(precedenceRoute, pdpList.map(pdp => (pdp.fromNode,pdp.toNode)))
  precedencesConstraints.add(0 === precedenceViolation)

  val vehicleOfNodes1 = vehicleOfNodes(precedenceRoute, v)
  for (pdp <- pdpList) {
  precedencesConstraints.add(vehicleOfNodes1(pdp.fromNode) === vehicleOfNodes1(pdp.toNode))
}

  // Vehicle content
  val violationOfContentOfVehicle = GlobalVehicleCapacityConstraint(gc, n, v, vehiclesCapacity, nodeToContentDelta)

  val unroutedPickups = myVRP.unrouted inter allPickupPoints

  //Objective function
  val unroutedPenalty = penaltyForUnrouted * (n - length(myVRP.routes))

  val obj = CascadingObjective(
  timeWindowConstraints,
  precedencesConstraints,
  sum(violationOfContentOfVehicle),
  sum(vehiclesRouteLength) + unroutedPenalty
  )

  val objPerVehicle = Array.tabulate[Objective](v)(vehicle =>
  CascadingObjective(
  timeWindowConstraints,
  precedencesConstraints,
  violationOfContentOfVehicle(vehicle),
  vehiclesRouteLength(vehicle))
  )

  val unroutedPenaltyObj = Objective(unroutedPenalty)

  m.close()
  // //////////////////////////////////////////////////////////////////////////////////////////////////

  @tailrec
  def trimRouteFromNodeRemoveNode(routeOfVehicle:List[Int], node:Int):List[Int] = {
  routeOfVehicle match{
  case Nil => Nil
  case h::t =>
  if (h==node) t
  else trimRouteFromNodeRemoveNode(t,node)
}
}

  def insertNodeVLSN(node:Int,
  relevantPredecessors:Iterable[Int]) =
  insertPointUnroutedFirst(
  () => Some(node),
  () => _ => relevantPredecessors,
  selectInsertionPointBehavior = Best(),
  vrp=myVRP,
  positionIndependentMoves = true)

  def insertPDPVLSN(vehicle:Int):Int => Neighborhood = {
  val routeOfVehicle: List[Int] =
  myVRP.getRouteOfVehicle(vehicle)

  pickUp:Int => {
  val delivery = pickUpPointToDeliveryPoint(pickUp)

  dynAndThen(
  insertNodeVLSN(
  pickUp,
  routeOfVehicle),
  (insertMove: InsertPointMove) =>
  if (timeWindowConstraints.Violation.value != 0) {
  NoMoveNeighborhood
} else {
  val nodesInRouteAfterPickUp = pickUp :: trimRouteFromNodeRemoveNode(routeOfVehicle,insertMove.newPredecessor)

  insertNodeVLSN(
  delivery,
  nodesInRouteAfterPickUp)
}) name s"insertPDPVLSN($pickUp $delivery vehicle:$vehicle)"
}
}

  // //////////////////////////////////////////////////////////////////////////////////////////////////

  def moveNodeVLSN(node:Int,
  relevantPredecessors:Iterable[Int]) =
  onePointMove(
  () => Some(node),
  () => _ => relevantPredecessors,
  myVRP,
  selectDestinationBehavior = Best(),
  hotRestart = false,
  positionIndependentMoves = true
  )

  def movePDPToAnotherVehicleVLSN(vehicle:Int):Int=>Neighborhood = {

  val routeOfVehicle: List[Int] =
  myVRP.getRouteOfVehicle(vehicle)

  pickUp: Int => {
  val delivery = pickUpPointToDeliveryPoint(pickUp)

  dynAndThen(
  moveNodeVLSN(
  pickUp,
  routeOfVehicle),
  (move: OnePointMoveMove) =>
  if (timeWindowConstraints.Violation.value != 0) {
  NoMoveNeighborhood
} else {
  val nodesInRouteAfterPickUp = pickUp :: trimRouteFromNodeRemoveNode(routeOfVehicle,move.newPredecessor)
  moveNodeVLSN(delivery, nodesInRouteAfterPickUp)
}) name s"movePDPVLSN($pickUp $delivery vehicle:$vehicle)"
}
}

  // //////////////////////////////////////////////////////////////////////////////////////////////////

  def moveNodesVLSN(nodes:List[Int],
  relevantPredecessors:Iterable[Int], filter:Boolean = false) =
  onePointMove(
  () => nodes,
  () => x => if(filter) relevantPredecessors.filter(_!=x) else relevantPredecessors,
  myVRP,
  selectDestinationBehavior = Best(),
  hotRestart = false,
  positionIndependentMoves = true
  )

  def movePointWithinVehicle(vehicle:Int):Neighborhood = {
  val routeOfVehicle = myVRP.getRouteOfVehicle(vehicle)
  val movableNodes = routeOfVehicle.filter(_ >=v)

  onePointMove(
  () => movableNodes,
  () => movedNode => routeOfVehicle.filter(_!=movedNode),
  myVRP,
  selectDestinationBehavior = Best(),
  hotRestart = false,
  positionIndependentMoves = true)
}

  def movePDPWithinVehicle(vehicle:Int):Neighborhood = {

  val routeOfVehicle: List[Int] =
  myVRP.getRouteOfVehicle(vehicle)

  val movableNodes = routeOfVehicle.filter(_>=v)

  val pickUpNodes = movableNodes.filter(node => pickUpPointToDeliveryPoint(node) != -1)

  dynAndThen(
  moveNodesVLSN(pickUpNodes, routeOfVehicle),
  (moveNode:OnePointMoveMove) => {
  val nodesInRouteAfterPickUp = moveNode.movedPoint :: trimRouteFromNodeRemoveNode(routeOfVehicle, moveNode.movedPoint)
  val delivery = pickUpPointToDeliveryPoint(moveNode.movedPoint)
  NoMoveNeighborhood orElse moveNodeVLSN(delivery, nodesInRouteAfterPickUp)
}) name s"movePDPWithinVehicle:$vehicle)"
}


  def movePDP:Neighborhood = {
  val allRoutedNodes = myVRP.routed.value.toList
  dynAndThen(
  moveNodesVLSN(allRoutedNodes.filter(isPickup), allRoutedNodes, filter = true),
  (moveNode:OnePointMoveMove) => {

  val pickUp = moveNode.movedPoint
  val targetVehicle = myVRP.vehicleOfNode(pickUp).valueInt
  val routeOfVehicle: List[Int] = myVRP.getRouteOfVehicle(targetVehicle)
  val nodesInRouteAfterPickUp = pickUp :: trimRouteFromNodeRemoveNode(routeOfVehicle, pickUp)
  val delivery = pickUpPointToDeliveryPoint(moveNode.movedPoint)
  moveNodeVLSN(delivery, nodesInRouteAfterPickUp)
}) name s"movePDP"
}


  def removeNode(node: Int) = removePoint(
  () => List(node),
  myVRP,
  positionIndependentMoves = true,
  hotRestart = false)

  def removePDPVLSN(pickUp:Int):Neighborhood =
  removeNode(pickUp) andThen removeNode(pickUpPointToDeliveryPoint(pickUp))

  def removeAndReInsertVLSN(pickUp: Int): () => Unit = {

  val checkpointBeforeRemove = myVRP.routes.defineCurrentValueAsCheckpoint(true)

  val delivery = pickUpPointToDeliveryPoint(pickUp)

  myVRP.routes.remove(myVRP.routes.value.positionOfAnyOccurrence(pickUp).get)
  myVRP.routes.remove(myVRP.routes.value.positionOfAnyOccurrence(delivery).get)

  () => {
  myVRP.routes.rollbackToTopCheckpoint(checkpointBeforeRemove)
  myVRP.routes.releaseTopCheckpoint()
}
}

  //TODO: speedup this 3-opt; it eats most of the run time because Precedence is SSLLOOWWW
  def threeOptOnVehicle(vehicle: Int):Neighborhood = {
  val nodesOfTargetVehicle = myVRP.getRouteOfVehicle(vehicle)
  //insertions points are position where we perform the insert,
  // basically the segment will start in place of the insertion point and the insertion point will be moved upward
  val nodesOfTargetVehicleButVehicle = nodesOfTargetVehicle.filter(_ != vehicle)

  (threeOpt(() => nodesOfTargetVehicle,
  () => _ => nodesOfTargetVehicleButVehicle,
  myVRP, breakSymmetry = false)
  filter ((t: ThreeOptMove) =>
  if (t.flipSegment) t.segmentEndPosition - t.segmentStartPosition < 4
  else math.min(math.abs(t.insertionPoint - t.segmentStartPosition), math.abs(t.insertionPoint - t.segmentEndPosition)) < 6))
}

  def vlsn = {
  //VLSN neighborhood
  new VLSN(
  v,
  () => myVRP.getVehicleToRouteMap.view.mapValues(_.filter(pickUpPointToDeliveryPoint(_) != -1)).toMap,
  initUnroutedNodesToInsert = unroutedPickups,
  nodeToRelevantVehicles = () => SortedMap.empty[Int,Iterable[Int]] ++ allPickupPoints.toList.map(p => (p,vehicles)),

  targetVehicleNodeToInsertNeighborhood = insertPDPVLSN,
  targetVehicleNodeToMoveNeighborhood = movePDPToAnotherVehicleVLSN,
  removePDPVLSN,

  removeNodeAndReInsert = removeAndReInsertVLSN,

  reOptimizeVehicle = Some(vehicle => Some(threeOptOnVehicle(vehicle) exhaustBack movePDPWithinVehicle(vehicle))),

  objPerVehicle,
  unroutedPenaltyObj,
  obj,

  cycleFinderAlgoSelection = CycleFinderAlgoType.Mouthuy,
  //1
  enrichmentSchemeSpec = {
  val toReturnp = enrichment match {
  case 0 =>
  NoEnrichment(shiftInsert)
  case 1 =>

  CompositeEnrichmentSchemeSpec(
  partition match {
  case 0 => SameSizeRandomPartitionsSpec(nbPartitions = 2)//OK
  case 1 => SameSizeRandomPartitionsSpec(nbPartitions = 5) //non
  case 2 => SameSizeRandomPartitionsSpec(nbPartitions = 10) //non
  case 3 => SameSizeRandomPartitionsSpec(nbPartitions = 15) //non
  case 4 => SameSizeRandomPartitionsSpec(nbPartitions = 20) //OK
  case 5 => VehicleStructuredSameSizePartitionsSpreadUnroutedSpec(20) //OK
  case 6 => VehiclePartitionSpec() //non

  case 7 => SameSizeRandomPartitionsSpec(nbPartitions = 30) //ok
  case 8 => SameSizeRandomPartitionsSpec(nbPartitions = 40) //non
  case 9 => SpreadVehiclePartitionSpec(nbPartition = 20)

},
  enrichmentSpec match {
  case 0 => LinearRandomSchemeSpec(nbEnrichmentLevels = 5)
  case 1 => LinearRandomSchemeSpec(nbEnrichmentLevels = 10)
  case 2 => LinearRandomSchemeSpec(nbEnrichmentLevels = 15)
  case 3 => LinearRandomSchemeSpec(nbEnrichmentLevels = 20) //bon
  case 4 => DivideAndConquerSchemeSpec()  //bon
  case 5 => LinearRandomSchemeSpec(nbEnrichmentLevels = 40) //pas bon
},
  shiftInsert)
}
  println(toReturnp)
  toReturnp
},

  name = "VLSN",
  reoptimizeAtStartUp = true,
  injectAllCacheBeforeEnriching = injectAllCacheBeforeEnriching,
  //        debugNeighborhoodExploration = true
  )
}

  // ///////////////////////////////////////////////////////////////////////////////////////////////////

  val startTime = System.nanoTime()
  val search = vlsn maxMoves 1 //exhaust movePDP
  search.verbose = 2

  search.doAllMoves(obj = obj)

  val endTime = System.nanoTime()

  println(myVRP)
  println(s"obj:${obj.value}")

  toReturn += s"\tobj:\t${obj.value.toDouble/multFactor.toDouble} \tnbUnrouted:\t${myVRP.unroutedNodes.size}\tusedV:\t${myVRP.movingVehicles.size}\tdurationMS:\t${(endTime - startTime) / (1000 * 1000)}"
  println(toReturn)
  toReturn
}
}

