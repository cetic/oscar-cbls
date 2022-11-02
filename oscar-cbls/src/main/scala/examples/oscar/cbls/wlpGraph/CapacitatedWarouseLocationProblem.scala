package examples.oscar.cbls.wlpGraph

import oscar.cbls._
import oscar.cbls.algo.graph.{ConditionalGraphWithIntegerNodeCoordinates, DijkstraDistanceMatrix}
import oscar.cbls.algo.search.KSmallest
import oscar.cbls.core.computation.{CBLSIntVar, ChangingIntValue, IntValue, Invariant, ShortIntNotificationTarget, Store}
import oscar.cbls.core.constraint.ConstraintSystem
import oscar.cbls.core.objective.{CascadingObjective, Objective}
import oscar.cbls.core.propagation.Checker
import oscar.cbls.core.search.{ConstantMoveNeighborhood, EvaluableCodedMove}
import oscar.cbls.lib.invariant.graph.KVoronoiZones
import oscar.cbls.lib.invariant.logic.{Cluster, Filter, IntElement}
import oscar.cbls.lib.invariant.numeric.SumElements
import oscar.cbls.lib.invariant.set.Cardinality
import oscar.cbls.lib.search.neighborhoods.vlsn.{CycleFinderAlgoType, VLSN}
import oscar.cbls.lib.search.neighborhoods._
import oscar.cbls.algo.generator.RandomGraphGenerator
import oscar.cbls.util._
import oscar.cbls.visual.SingleFrameWindow
import oscar.cbls.visual.graph.GraphViewer
import oscar.cbls.visual.ColorGenerator

import scala.collection.immutable.{SortedMap, SortedSet, TreeMap}
import scala.swing.Color

class StoreToWarehouseDistance(warehouseTab : Array[CBLSIntVar],distanceTab : Array[CBLSIntVar],storeWarehouseDistance : Array[CBLSIntVar],defaultDistance : Int = 10000)
  extends Invariant
  with ShortIntNotificationTarget{
  storeWarehouseDistance.foreach(v => v.setDefiningInvariant(this))
  val nbElement = warehouseTab.length
  (0 until nbElement).foreach(i => registerStaticAndDynamicDependency(warehouseTab(i),i))
  (0 until nbElement).foreach(i => registerStaticAndDynamicDependency(distanceTab(i),nbElement + i))

  finishInitialization()

  case class ChangedTrace(warehouse : Long,newDistance : Long)

  private val allreadyNotified = Array.fill(storeWarehouseDistance.length)(false)
  private val newValues : Array[Option[Long]] = Array.fill(storeWarehouseDistance.length)(None)
  var changedValues : List[Int]= List()

  Array.tabulate(nbElement)(i => {
    if (warehouseTab(i).value != -1)
      storeWarehouseDistance(warehouseTab(i).valueInt) := distanceTab(i).value
  })

  override def checkInternals(c: Checker): Unit = {
    val warehouseTabValues = warehouseTab.map(v => v.newValue)
    println(warehouseTab.mkString(";"))
    println(distanceTab.mkString(";"))
    println(storeWarehouseDistance.map(v => v.name + ":=" + v.newValue).mkString("\n"))
    Array.tabulate(storeWarehouseDistance.length)(i => {
      require(
        if (warehouseTabValues.contains(i)) {
          storeWarehouseDistance(i).newValue == distanceTab(warehouseTabValues.indexOf(i)).newValue
        } else
          storeWarehouseDistance(i).newValue == defaultDistance)
    })
  }

  override def notifyIntChanged(v : ChangingIntValue,id : Int,oldValue : Int, newValue : Int): Unit = {
    if (id < nbElement) {
      if (oldValue != -1) {
        changedValues = if (allreadyNotified(oldValue)) changedValues else oldValue :: changedValues
        allreadyNotified(oldValue) = true
      }
      if (newValue != -1) {
        changedValues = if (allreadyNotified(newValue)) changedValues else newValue :: changedValues
        allreadyNotified(newValue) = true
        //println(newValues.mkString(";"))
        newValues(newValue) = Some(distanceTab(id).newValue)
      }
    } else {
      val warehouseIndex = warehouseTab(id - nbElement).newValueInt
      if (warehouseIndex != -1) {
        changedValues = if (allreadyNotified(warehouseIndex)) changedValues else warehouseIndex :: changedValues
        allreadyNotified(warehouseIndex) = true
        newValues(warehouseIndex) = Some(newValue)
      }
    }
    scheduleForPropagation()
  }

  def applyChanges(values : List[Int]) : Unit = {
    values match {
      case Nil =>
      case head::tail =>
        storeWarehouseDistance(head) := (newValues(head) match {
          case None => defaultDistance
          case Some(newValue) => newValue
        })
        newValues(head) = None
        allreadyNotified(head) = false
        applyChanges(tail)
    }
  }

  override def performPropagation(): Unit = {
    applyChanges(changedValues)
    changedValues = List()
  }

}

//class AssignAndAutoAssignNeighborhood extends

object CapacitatedWarouseLocationProblem extends App with StopWatch {

  //the number of warehouses
  val W:Int = 200

  //the number of delivery points
  val D:Int = 1000

  // the number of per delivery points
  val k:Int = 5

  //nb conditional edges
  val nbConditionalEdges:Int =  (W + D) / 5

  //nb non conditional edges
  val nbNonConditionalEdges =  (W+D)*5
  val displayDelay = 100

  val defaultCostForNoOpenWarehouse = 10000
  val defaultDistanceForWarehouse = 10000

  val costForOpeningWarehouse =  800



  println("generate random graph")
  val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(
    nbNodes = W+D,
    nbConditionalEdges = nbConditionalEdges,
    nbNonConditionalEdges = nbNonConditionalEdges,
    nbTransitNodes = W+D,
    mapSide = 800,
    seed = Some(1))

  val m = Store()//checker = Some(new ErrorChecker))

  val deliveryToNode = Array.tabulate(D)(i => graph.nodes(i + W))
  val warehouseToNode =  Array.tabulate(W)(w => graph.nodes(w))
  def nodeIdToDeliveryNumber(id : Int) = id - W

  val warehouseCapacity = Array.fill(W)(10)

  val warehouseOpenArray = Array.tabulate(W)(i => new CBLSIntVar(m,0,0 to 1,s"warehouse $i open"))
  val openWarehouses = Filter(warehouseOpenArray).setName("open warehouses")
  openWarehouses.setName("OpenW")
  val closedWarehouses = Filter(warehouseOpenArray,_ == 0)
  closedWarehouses.setName("ClosedW")
  val nbOpenWarehouses = Cardinality(openWarehouses)

  val conditionalEdgesOpenArray = Array.tabulate(nbConditionalEdges)(i => new CBLSIntVar(m,0,0 to 1,s"conditional edge $i open"))
  val openEdges = Filter(conditionalEdgesOpenArray).setName("conditional Edges Open")

  val deliveryToWarehouse = Array.tabulate(D)(i => new CBLSIntVar(m,W,0 until W + 1,s"Warehouse that serves node ${i + W}"))

  def deliveryToWarhouseMap = {
    var acc = SortedMap.empty[Int,CBLSIntVar]
    for (d <- 0 until D) {
      acc = acc + {(W + d) -> deliveryToWarehouse(d)}
    }
    acc
  }

  val deliveryServedByWarehouseCluster = Cluster.makeDense(deliveryToWarehouse)

  val deliveryServedByWarehouse = deliveryServedByWarehouseCluster.clusters

  val unServedDelivery = Filter(deliveryToWarehouse,_ == W)
  unServedDelivery.setName("Unserved")

  println(deliveryServedByWarehouse.mkString("\n"))

  val nbStorePerWarehouse = Array.tabulate(W)(w => Cardinality(deliveryServedByWarehouse(w)))

  val x = Cardinality(openEdges)

  var costOfBridgesPerBridge = 7

  println("Init VZone")

  val kvor : KVoronoiZones= KVoronoiZones(graph,
    openEdges,
    openWarehouses,
    k,
    deliveryToNode.map(_.id),
    m,
    defaultDistanceForUnreachableNode = defaultDistanceForWarehouse,
    W

  )

  println("End Init VZone")

  val distanceToClosestCentroidMap = kvor.trackedNodeToDistanceAndCentroidMap

  val distanceToClosestCentroid = Array.tabulate(D)((i : Int) => distanceToClosestCentroidMap(deliveryToNode(i).id))

  val store2WarehouseDistance = Array.tabulate(D)((i : Int) => Array.tabulate(W + 1)(j => CBLSIntVar(m,10000,0 until Int.MaxValue,s"Distance from D $i to W $j")))

  val storeDistanceToWarehouse : Array[IntValue] = Array.tabulate(D)(i => IntElement(deliveryToWarehouse(i),store2WarehouseDistance(i)))

  val totalDistancePerWarehouse = deliveryServedByWarehouse.map(c => SumElements(storeDistanceToWarehouse,c))

  val totalDistance = sum(storeDistanceToWarehouse)

  for (i <- 0 until D) {
    new StoreToWarehouseDistance(distanceToClosestCentroid(i).map(e => e._1),distanceToClosestCentroid(i).map(e => e._2),store2WarehouseDistance(i))
  }

  val c = ConstraintSystem(m)

  (0 until W).foreach(w => c.post(nbStorePerWarehouse(w) le warehouseCapacity(w)))

  val objPerWarehouse : Array[Objective] = Array.tabulate(W)((w : Int) => max2(nbStorePerWarehouse(w) - warehouseCapacity(w),0) * defaultDistanceForWarehouse + totalDistancePerWarehouse(w))

  val unservedPenalty = totalDistancePerWarehouse(W)

  val edgeCost = x * costOfBridgesPerBridge

  val warehouseCost = costForOpeningWarehouse * nbOpenWarehouses

  val constantObjective = Objective(unservedPenalty + edgeCost + warehouseCost)

  val obj = new CascadingObjective(c,totalDistance + edgeCost + warehouseCost)

  m.close()

  val centroidColors = ColorGenerator.generateRandomColors(W)

  val visual = new GraphViewer(graph:ConditionalGraphWithIntegerNodeCoordinates,
    centroidColor = SortedMap.empty[Int,Color] ++ warehouseToNode.toList.map(node => (node.id,centroidColors(node.id))))

  SingleFrameWindow.show(visual,title = "Warehouse and bridge location", 1025, 1105)

  visual.redraw(openEdges.value,
    openWarehouses.value,
    TreeMap(deliveryToWarhouseMap.view.mapValues(e => if (e.valueInt == W) -1 else e.valueInt).toIndexedSeq:_*),
    extraPath = List()
  )

  val distanceMatrixAllEdgesOpen = DijkstraDistanceMatrix.buildDistanceMatrix(graph,_ => true)

  def kClosestStores(getK : Int => Int) = Array.tabulate(W)(i => deliveryToNode.map(n => n.id).sortWith((s1,s2) => distanceMatrixAllEdgesOpen(i)(s1) < distanceMatrixAllEdgesOpen(i)(s2)).take(getK(i)))

  val halfTheClosest = kClosestStores(i => warehouseCapacity(i) /2)

  val kClosestStoresByStore = Array.tabulate(D)(d => (0 until D).sortWith((d1,d2) => distanceMatrixAllEdgesOpen(d + W)(d1 + W) < distanceMatrixAllEdgesOpen(d + W)(d2 + W)).slice(1,10))

  val storeToWarehouseDistance = Array.tabulate(D)(d => (0 until W).sortWith((w1,w2) => distanceMatrixAllEdgesOpen(deliveryToNode(d).id)(w1) < distanceMatrixAllEdgesOpen(deliveryToNode(d).id)(w2)))

  def kNearestOpenWarehouseToStore(k : Int, d : Int) = KSmallest.kFirst(k,storeToWarehouseDistance(d),warehouseOpenArray(_).value == 1)

  val warehouseToWarehouseDistance = Array.tabulate(W)(w => (0 until W).sortWith((w1,w2) => distanceMatrixAllEdgesOpen(w)(w1) < distanceMatrixAllEdgesOpen(w)(w2)))

  def kNearestOpenWarehouseToWarehouse(k : Int, w : Int) = KSmallest.kFirst(k,warehouseToWarehouseDistance(w),warehouseOpenArray(_).value == 1)

  println(Array.tabulate(D)(i => kClosestStoresByStore(i).map(d => distanceMatrixAllEdgesOpen(i + W)(d + W)).mkString(";")).mkString("\n"))

  val swapStore = SwapsNeighborhood(deliveryToWarehouse,"SwapStores",searchZone2 = () => (s1,_) => kClosestStoresByStore(s1))

  val swapWarehouses = SwapsNeighborhood(warehouseOpenArray,"SwapWarehouses",searchZone1 = () => closedWarehouses.value,searchZone2 = () => (w1,_) => kNearestOpenWarehouseToWarehouse(10,w1))

  def transferStores(m : SwapMove) = {
    ConstantMoveNeighborhood(transferStoreMove(m.idI,deliveryServedByWarehouse(m.idJ).value,m))
  }

  def transferStoreMove(wId : Int,storesIndex : Iterable[Int],m : SwapMove) = new EvaluableCodedMove(() => {
    val savedValues = storesIndex.map(i => (i,deliveryToWarehouse(i).value))
    storesIndex.foreach(i => deliveryToWarehouse(i) := wId)
    () => {
      savedValues.foreach(iAndV => deliveryToWarehouse(iAndV._1) := iAndV._2)
    }
  })

  def moveAssignStore(wId : Int) = new EvaluableCodedMove(() => {
    val closest = halfTheClosest
    val savedValues = closest(wId).map(s => deliveryToWarehouse(nodeIdToDeliveryNumber(s)).value)
    closest(wId).foreach(i => deliveryToWarehouse(nodeIdToDeliveryNumber(i)) := wId)
    () => {
      closest(wId).indices.foreach(i => deliveryToWarehouse(nodeIdToDeliveryNumber(closest(wId)(i))) := savedValues(i))
    }
  })

  def assignHalfTheClosestStores (m : AssignMove) = {
    ConstantMoveNeighborhood(moveAssignStore(m.id))
  }

  def assignForInsertVLSN(w: Int,d:Int,t : String) =
    AssignNeighborhood(
      deliveryToWarehouse,s"InsertDelivery${d}_${w}_$t",
      searchZone = () => Some(d - W),
      domain = (_,_) => Some(w),
      hotRestart = false)


  println(distanceToClosestCentroidMap(W).mkString("\n"))

  val vlsn = new VLSN(W:Int,
    initVehicleToRoutedNodesToMove = () => SortedMap.empty[Int,SortedSet[Int]] ++ (0 until W).map(i => (i, deliveryServedByWarehouse(i).value.map(i => i + W))), //() => SortedMap[Long,SortedSet[Long]],
    initUnroutedNodesToInsert= () => unServedDelivery.value.map(i => i + W),
    nodeToRelevantVehicles = () => distanceToClosestCentroidMap.view.mapValues(_.map(_._1.valueInt).toList.filter(_ != W)).toMap,//() => Map[Long,Iterable[Long]],

    // puisqu'on fait plusieurs inserts de nodes différents sur le même véhicule.
    targetVehicleNodeToInsertNeighborhood = w => d => assignForInsertVLSN(w,d,"insert"),//:Long => Long => Neighborhood,
    targetVehicleNodeToMoveNeighborhood = w => d => assignForInsertVLSN(w,d,"move"),//:Long => Long => Neighborhood,
    nodeToRemoveNeighborhood = d => assignForInsertVLSN(W,d,"remove"),//:Long => Neighborhood,

    removeNodeAndReInsert= d => {
      val oldWarehouse = deliveryToWarehouse(d - W).value
      deliveryToWarehouse(d - W) := W
      () => deliveryToWarehouse(d - W) := oldWarehouse
    },//:Long => () => Unit,

    reOptimizeVehicle= None,//:Option[Long => Option[Neighborhood]],

    vehicleToObjective= objPerWarehouse, //:Array[Objective],
    unroutedPenalty= constantObjective,//:Objective,
    globalObjective= obj,//:Objective,
    cycleFinderAlgoSelection= CycleFinderAlgoType.Mouthuy,//:CycleFinderAlgoType = CycleFinderAlgoType.Mouthuy,
    doAfterCycle = Some(() => if(lastDisplay + displayDelay <= this.getWatch){ //} && obj.value < bestDisplayedObj) {

      visual.redraw(openEdges.value,
        openWarehouses.value,
        TreeMap(deliveryToWarhouseMap.view.mapValues(e => if (e.value == W) -1 else e.valueInt).toIndexedSeq:_*),
        extraPath = List(),
        extraCentroids = (0 until W).toArray
      )
      lastDisplay = this.getWatch
    }),
    maxIt = 1,
  name ="VLSN")

  def closeWarehouseToNodeMap(w : Int,k : Int) = SortedMap.empty[Int,SortedSet[Int]]  ++ (0 until W).map(w => w -> SortedSet[Int]()) ++ kNearestOpenWarehouseToWarehouse(k,w).map(w1 => w1 -> deliveryServedByWarehouse(w1).value.map(_ + W))

  def closeOfTwoWarehousesToNodeMap(w1 : Int,w2 : Int,k : Int) = {
    SortedMap.empty[Int,SortedSet[Int]]  ++ (0 until W).map(w => w -> SortedSet[Int]()) ++ kNearestOpenWarehouseToWarehouse(k,w1).map(w => w -> deliveryServedByWarehouse(w).value.map(_ + W)) ++ kNearestOpenWarehouseToWarehouse(k,w2).map(w => w -> deliveryServedByWarehouse(w).value.map(_ + W))
  }

  def vlsnForAssignAndThen(m : AssignMove) = new VLSN(W:Int,
    initVehicleToRoutedNodesToMove = () => closeWarehouseToNodeMap(m.id,20),
    initUnroutedNodesToInsert= () => unServedDelivery.value.map(i => i + W),
    nodeToRelevantVehicles = () => distanceToClosestCentroidMap.view.mapValues(_.map(_._1.valueInt).toList.filter(_ != W)).toMap,//() => Map[Long,Iterable[Long]],

    // puisqu'on fait pleuiseurs inserts de nodes différents sur le même véhicule.
    targetVehicleNodeToInsertNeighborhood= w => d => assignForInsertVLSN(w,d,"insert"),//:Long => Long => Neighborhood,
    targetVehicleNodeToMoveNeighborhood= w => d => assignForInsertVLSN(w,d,"move"),//:Long => Long => Neighborhood,
    nodeToRemoveNeighborhood= d => assignForInsertVLSN(W,d,"remove"),//:Long => Neighborhood,

    removeNodeAndReInsert= d => {
      val oldWarehouse = deliveryToWarehouse(d - W).value
      deliveryToWarehouse(d - W) := W
      () => deliveryToWarehouse(d - W) := oldWarehouse
    },//:Long => () => Unit,

    reOptimizeVehicle= None,//:Option[Long => Option[Neighborhood]],

    vehicleToObjective= objPerWarehouse, //:Array[Objective],
    unroutedPenalty= constantObjective,//:Objective,
    globalObjective= obj,//:Objective,
    maxIt = 10,
    cycleFinderAlgoSelection= CycleFinderAlgoType.Mouthuy,//:CycleFinderAlgoType = CycleFinderAlgoType.Mouthuy,
    name ="VLSNForAssignAndThen")

  def vlsnForSwapAndThen(m : SwapMove) = {
    new VLSN(W: Int,
      initVehicleToRoutedNodesToMove = () => closeOfTwoWarehousesToNodeMap(m.idI, m.idJ, 20),
      initUnroutedNodesToInsert = () => unServedDelivery.value.map(i => i + W),
      nodeToRelevantVehicles = () => distanceToClosestCentroidMap.view.mapValues(_.map(_._1.valueInt).toList.filter(_ != W)).toMap, //() => Map[Long,Iterable[Long]],

      // puisqu'on fait plusieurs inserts de nodes différents sur le même véhicule.
      targetVehicleNodeToInsertNeighborhood = w => d => assignForInsertVLSN(w, d, "insert"), //:Long => Long => Neighborhood,
      targetVehicleNodeToMoveNeighborhood = w => d => assignForInsertVLSN(w, d, "move"), //:Long => Long => Neighborhood,
      nodeToRemoveNeighborhood = d => assignForInsertVLSN(W, d, "remove"), //:Long => Neighborhood,

      removeNodeAndReInsert = d => {
        val oldWarehouse = deliveryToWarehouse(d - W).value
        deliveryToWarehouse(d - W) := W
        () => deliveryToWarehouse(d - W) := oldWarehouse
      }, //:Long => () => Unit,

      reOptimizeVehicle = None, //:Option[Long => Option[Neighborhood]],

      vehicleToObjective = objPerWarehouse, //:Array[Objective],
      unroutedPenalty = constantObjective, //:Objective,
      globalObjective = obj, //:Objective,
      cycleFinderAlgoSelection = CycleFinderAlgoType.Mouthuy, //:CycleFinderAlgoType = CycleFinderAlgoType.Mouthuy,
      name = "VLSN"
    )
  }

  def assignDelivery(m : AssignMove) = AssignNeighborhood(deliveryToWarehouse,searchZone = () => kClosestStoresByStore(m.id),domain = (_,i) => distanceToClosestCentroid(i).map(c => c._1.valueInt))

  var lastDisplay = this.getWatch

  val search = bestSlopeFirst(List(
    profile(AssignNeighborhood(deliveryToWarehouse,"SwitchAssignedWarehouse",domain = (_,i) => distanceToClosestCentroid(i).map(c => c._1.valueInt))),
    profile(AssignNeighborhood(warehouseOpenArray,"OpenWarehouseAndLink") dynAndThen assignHalfTheClosestStores),
    profile(AssignNeighborhood(conditionalEdgesOpenArray,"SwitchEdge")),
    profile(vlsn),
    profile(swapWarehouses dynAndThen transferStores name "swapAndReassign"),
    profile(AssignNeighborhood(deliveryToWarehouse,"SwitchAssignedWarehouse",domain = (_,i) => distanceToClosestCentroid(i).map(c => c._1.valueInt)) dynAndThen assignDelivery),
    profile(swapStore))).onExhaustRestartAfter(RandomizeNeighborhood(warehouseOpenArray, () => W/5,"Randomize1"), 2, obj, restartFromBest = true) afterMove(
    if(lastDisplay + displayDelay <= this.getWatch){ //} && obj.value < bestDisplayedObj) {

    visual.redraw(openEdges.value,
      openWarehouses.value,
      TreeMap(deliveryToWarhouseMap.view.mapValues(e => if (e.value == W) -1 else e.valueInt).toIndexedSeq:_*),
      extraPath = List(),
      extraCentroids = (0 until W).toArray
    )
    lastDisplay = this.getWatch
  }) showObjectiveFunction(obj)

  search.verbose = 2
  search.doAllMoves(obj = obj)

  println(deliveryToWarehouse.foldLeft(true)((b : Boolean,d : CBLSIntVar) => openWarehouses.value.contains(d.valueInt) && b))


  println(unServedDelivery.value.map(v => s"$v - ${graph.coordinates(v)}"))

  println(openWarehouses)
  println(openWarehouses.value.toArray.length)

  visual.redraw(openEdges.value,
    openWarehouses.value,
    TreeMap(deliveryToWarhouseMap.view.mapValues(e => if (e.value == W) -1 else e.valueInt).toIndexedSeq:_*),
    extraPath = List(),
    extraCentroids = (0 until W).toArray
  )

  println(Array.tabulate(deliveryServedByWarehouse.length - 1)(i => deliveryServedByWarehouse(i).toString  + "-" + (objPerWarehouse(i).value + costForOpeningWarehouse * warehouseOpenArray(i).value) + "-" + ((objPerWarehouse(i).value + costForOpeningWarehouse * warehouseOpenArray(i).value) / (deliveryServedByWarehouse(i).value.iterator.length max 1))).mkString("\n"))

  println(obj)

  println(search.profilingStatistics)

}
