package oscar.cbls.business.routing.invariants

import oscar.cbls.warning
import oscar.cbls.algo.dll.{DLLStorageElement, DoublyLinkedList}
import oscar.cbls.algo.graph.{ConditionalGraph, Distance, RevisableAStar, RevisableDistance}
import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.model.RoutingConventionMethods
import oscar.cbls.core.computation.{CBLSIntVar, CBLSSetVar, ChangingSeqValue, ChangingSetValue, Domain, Invariant, InvariantHelper, SeqNotificationTarget, SeqUpdate, SeqUpdateAssign, SeqUpdateDefineCheckpoint, SeqUpdateInsert, SeqUpdateLastNotified, SeqUpdateMove, SeqUpdateRemove, SeqUpdateRollBackToCheckpoint, SeqValue, SetNotificationTarget, SetValue, Store}
import oscar.cbls.core.propagation.Checker

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

object RouteLengthOnConditionalGraph{

  def apply(routes:SeqValue,
            n:Int,
            v:Int,
            openConditions:SetValue,
            nodeInRoutingToNodeInGraph:Int => Int,
            graph:ConditionalGraph,
            underApproximatingDistance:(Int,Int) => Long,
            distanceIfNotConnected:Int,
            freeReturn:Boolean = false):RouteLengthOnConditionalGraph ={

    val store:Store = InvariantHelper.findModel(routes,openConditions)
    val vehicleToRouteLength = Array.tabulate(v)(vehicle => CBLSIntVar(store,0,name = "route_length_of_vehicle_" + vehicle))

    new RouteLengthOnConditionalGraph(routes:SeqValue,
      n:Int,
      v:Int,
      openConditions:SetValue,
      nodeInRoutingToNodeInGraph:Int => Int,
      graph:ConditionalGraph,
      underApproximatingDistance:(Int,Int) => Long,
      distanceIfNotConnected:Int, //do not put anything too big, or it will trigger some overflow
      vehicleToRouteLength,
      freeReturn)
  }
}

//TODO: some pair of nodes have their shortest path without condition.
// A O(1) routing should exist in such case, possibly even in the A* search itself,
// so could be used to speed up AStar in general

class RouteLengthOnConditionalGraph(routes:SeqValue,
                                    n:Int,
                                    v:Int,
                                    openConditions:SetValue,
                                    nodeInRoutingToNodeInGraph:Int => Int,
                                    graph:ConditionalGraph,
                                    underApproximatingDistance:(Int,Int) => Long,
                                    distanceIfNotConnected:Long, //do not put anything too big, or it will trigger some overflow
                                    val distancePerVehicle:Array[CBLSIntVar],
                                    freeReturn:Boolean = false)
  extends Invariant() with SeqNotificationTarget with SetNotificationTarget {

  //require(!freeReturn, "sorry free return is not implemented yet. ")
  require(v == distancePerVehicle.length)

  warning(
    openConditions.min == 0 && openConditions.max == graph.nbConditions - 1,
    s"RouteLengthOnConditionalGraph: openConditions should range on the conditions of the conditional graph; openConditions.domain:${openConditions.domain} nbConditions:${graph.nbConditions}")

  private val nbConditions = graph.nbConditions

  registerStaticAndDynamicDependency(openConditions)
  registerStaticAndDynamicDependency(routes)
  finishInitialization()
  for (i <- distancePerVehicle) i.setDefiningInvariant(this)

  // //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //there is no checkpoint at all.
  //a cached vehicle searcher is used here, and updated at each checkpoint.

  //the engine, used in all computations.
  //it was developed to be quickly restarteable
  private val aStarEngine = new RevisableAStar(graph: ConditionalGraph, underApproximatingDistance)

  //println(routes.toString() + v)
  private var vehicleSearcher:((IntSequence,Int)=>Int) = if(v == 1) (_,_) => 0 else
    RoutingConventionMethods.cachedVehicleReachingPosition(routes.value, v)

  //fast query for the AStar algo.
  private val isConditionalEdgeOpen: Array[Boolean] = Array.fill(openConditions.max + 1)(false)
  for(o <- openConditions.value){
    isConditionalEdgeOpen(o) = true
  }

  //max two of them.
  private val minNodeToAStarInfos: Array[QList[AStarInfo]] = Array.fill(n)(null)

  private val conditionToAStarInfo: Array[DoublyLinkedList[AStarInfo]] = Array.tabulate(nbConditions)(_ => new DoublyLinkedList[AStarInfo]())

  private var myNeededConditions:Option[CBLSSetVar] = None

  private var myRelevantConditions:Option[CBLSSetVar] = None

  //all the AStarInfo that are currently instantiated (and that can be deleted easily when an assign is taking place)
  private val allAStarInfo: DoublyLinkedList[AStarInfo] = new DoublyLinkedList[AStarInfo]()

  //initialization
  computeAndAffectValueFromScratch(routes.value)

  /**
    * @return the conditions that are actually used in the present state.
    */
  def neededConditions:CBLSSetVar = {
    myNeededConditions match{
      case Some(variable) => variable
      case None =>
        var neededConditionsAcc:Set[Int] = SortedSet.empty
        for(aStarInfo:AStarInfo <- allAStarInfo) {
          for (c <- aStarInfo.requiredConditions){
            neededConditionsAcc = neededConditionsAcc + c
          }
        }
        val toReturn = CBLSSetVar(model,neededConditionsAcc,Domain(0,graph.nbConditions-1), "neededConditions")
        myNeededConditions = Some(toReturn)
        toReturn
    }
  }

  def relevantConditions:CBLSSetVar =     {
    myRelevantConditions match{
      case Some(variable) => variable
      case None =>
        var relevantConditionsAcc:Set[Int] = SortedSet.empty
        for(aStarInfo:AStarInfo <- allAStarInfo){
          for(c <- aStarInfo.conditionsForRevision) {
            relevantConditionsAcc = relevantConditionsAcc + c
          }
        }
        val toReturn = CBLSSetVar(model,relevantConditionsAcc,Domain(0,graph.nbConditions-1), "relevantConditions")
        myRelevantConditions = Some(toReturn)
        toReturn
    }
  }

  // //////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  private def getAStarInfo(node1: Int, node2: Int): AStarInfo = {
    val (minNode, maxNode) = if (node1 < node2) (node1, node2) else (node2, node1)

    val allInfoOnMinNode = minNodeToAStarInfos(minNode) // at most two

    if (allInfoOnMinNode.head.maxNode == maxNode) {
      allInfoOnMinNode.head
    } else {
      //there are two of them, so we are the second one (of course)
      require(allInfoOnMinNode.tail.head.maxNode == maxNode)
      require(allInfoOnMinNode.tail.tail == null)
      allInfoOnMinNode.tail.head
    }
  }

  private def getDistanceForAStarResult(d: RevisableDistance):Long = {
    d match {
      case d: Distance =>
        d.distance
      case _ =>
        distanceIfNotConnected
    }
  }

  class AStarInfo(val minNode:Int,
                  val maxNode:Int,
                  result:RevisableDistance){

    override def toString: String = s"AStarInfo(minNode:$minNode maxNode:$maxNode result:$result)"

    private var myElements:QList[DLLStorageElement[AStarInfo]] = QList(allAStarInfo.addElem(this))

    for( c <- result.conditionsForRevisions) {
      if (conditionToAStarInfo(c).isEmpty) myRelevantConditions match {
        case Some(variable) => variable :+= c
        case None => ; //awfully expensive!
      }
      myElements = QList(conditionToAStarInfo(c).addElem(this), myElements)

      if (isConditionalEdgeOpen(c)) myNeededConditions match {
        case Some(neededConditionVar) => neededConditionVar :+= c
        case None => ;
      }
    }

    def conditionsForRevision:Iterable[Int] = result.conditionsForRevisions

    def requiredConditions:SortedSet[Int] = result.requiredConditions

    minNodeToAStarInfos(minNode) = QList(this,minNodeToAStarInfos(minNode))

    val distance:Long = getDistanceForAStarResult(result)

    /**
      * @return true if this info was valid and invalidated, false if it was already invalidated
      */
    def invalidate(): Unit = {
      //println("invalidate " + this)
      while(myElements != null){
        myElements.head.delete()
        myElements = myElements.tail
      }

      if(minNodeToAStarInfos(minNode).head == this){
        minNodeToAStarInfos(minNode) = minNodeToAStarInfos(minNode).tail
      }else{
        //there are two of them, so we are the second one (of course)
        require(minNodeToAStarInfos(minNode).tail.head == this)
        require(minNodeToAStarInfos(minNode).tail.tail == null)
        minNodeToAStarInfos(minNode) = QList(minNodeToAStarInfos(minNode).head)
      }

      //TODO: this is c log(c), by far too slow!!
      myNeededConditions match{
        case Some(neededConditionVar) =>
          for( c <- result.conditionsForRevisions) {
            if (!isConditionalEdgeOpen(c) || conditionToAStarInfo(c).isEmpty) {
              neededConditionVar :-= c
            }
          }
        case None => ;
      }

      myRelevantConditions match{
        case Some(relevantConditionVar) =>
          for( c <- result.conditionsForRevisions){
            if(conditionToAStarInfo(c).isEmpty){
              relevantConditionVar :-= c
            }
          }
        case None => ;
      }
    }
  }

  private def computeDistanceAndSaveItAll(fromNode:Int,toNode:Int): AStarInfo = {
    val (minNode,maxNode) = if(fromNode < toNode)(fromNode,toNode) else (toNode,fromNode)
    val result = aStarEngine.search(
      graph.nodes(nodeInRoutingToNodeInGraph(fromNode)),
      graph.nodes(nodeInRoutingToNodeInGraph(toNode)),
      isConditionalEdgeOpen,
      includePath = false)

    //it performs the registration automatically.
    new AStarInfo(minNode, maxNode, result)
  }

  private def dropAllAStarInfo(): Unit ={
    //we are forced to do this way because DLL are mutable things
    while(allAStarInfo.nonEmpty){
      allAStarInfo.head.invalidate()
    }
  }

  override def notifySetChanges(v: ChangingSetValue,
                                id: Int,
                                addedValues: Iterable[Int],
                                removedValues: Iterable[Int],
                                oldValue: SortedSet[Int],
                                newValue: SortedSet[Int]): Unit = {

    require(v == openConditions)

    var aStarToRecompute: List[AStarInfo] = Nil

    def recordTouchedCondition(closedCondition: Int): Unit ={
      val l = conditionToAStarInfo(closedCondition).toList
      for (a <- l) a.invalidate()
      require(conditionToAStarInfo(closedCondition).isEmpty)
      aStarToRecompute = l ::: aStarToRecompute
    }

    for(closedCondition <- removedValues){
      recordTouchedCondition(closedCondition)
      isConditionalEdgeOpen(closedCondition) = false
    }
    for(openCondition <- addedValues){
      recordTouchedCondition(openCondition)
      isConditionalEdgeOpen(openCondition) = true
    }

    //println("Astar To Recompute " + aStarToRecompute.mkString(";"))

    //now we have to recompute the hops, and update the corresponding vehicles
    for(aStar:AStarInfo <- aStarToRecompute){
      //println("Recomputing " + aStar)
     // println(routes.value.mkString(";"))
      //println(aStar.minNode)
      //find the vehicle
      val vehicle = vehicleSearcher(routes.value,routes.value.positionOfAnyOccurrence(aStar.minNode).get)

      //println("Impacted Vehicle " + vehicle)

      //recompute the hop, and save the AstarInfo for later notifications
      val newAStarInfo = computeDistanceAndSaveItAll(aStar.minNode,aStar.maxNode)

      //update the route length of the vehicle
      val oldDistance = aStar.distance
      val newDistance = newAStarInfo.distance
      distancePerVehicle(vehicle) :+= (newDistance - oldDistance)
    }
  }

  // //////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    *
    * @param routes a sequence of integers representing routes
    * @return the distance per vehicle or the total distance in a singleton array, according to the global "perVehicle" flag
    */
  private def computeAndAffectValueFromScratch(routes:IntSequence): Unit ={
    require(allAStarInfo.isEmpty)
    var currentPosition = routes.explorerAtAnyOccurrence(0).get
    var currentVehicle:Int = 0
    var currentLength:Long = 0

    while(currentPosition.next match{
      case None => //at the end of the current vehicle, which is the last one
        //compute the last hop
        val lastHopToComeBack = if (freeReturn) 0 else computeDistanceAndSaveItAll(currentPosition.value,v-1).distance
        currentLength += lastHopToComeBack
        distancePerVehicle(v-1) := currentLength
        false
      case Some(nextPosition) if nextPosition.value < v =>
        //at the end of the current vehicle; starting a new one
        val lastHopToComeBack = if (freeReturn) 0 else computeDistanceAndSaveItAll(currentPosition.value,currentVehicle).distance
        currentLength += lastHopToComeBack
        distancePerVehicle(currentVehicle) := currentLength

        currentPosition = nextPosition
        currentVehicle += 1
        currentLength = 0
        require(currentVehicle == nextPosition.value)
        true
      case Some(nextPosition) if nextPosition.value >= v =>
        //carry on the current vehicle
        val newHop = computeDistanceAndSaveItAll(currentPosition.value,nextPosition.value).distance
        currentLength += newHop
        currentPosition = nextPosition
        true
    }){}
  }

  // //////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit ={
    digestUpdates(changes)
  }

  private def digestUpdates(changes:SeqUpdate):Unit = {
    changes match {
      case SeqUpdateDefineCheckpoint(prev,isStarMode,checkpointLevel) =>
       // println("Define Checkpoint")
        //println(minNodeToAStarInfos(0).toList.mkString(";"))
        //we do not manage checkpoints at all
        digestUpdates(prev)

        //we update teh vehicle searcher, since many queries might be done on it.
        vehicleSearcher =
          if(v == 1) (_,_) => 0
          else RoutingConventionMethods.cachedVehicleReachingPosition(changes.newValue, v)

      case r@SeqUpdateRollBackToCheckpoint(checkpoint:IntSequence,checkpointLevel:Int) =>
        //println("RollBack")
        //we do not manage checkpoints at all
        digestUpdates(r.howToRollBack)

        //we update the vehicle searcher, since many queries might be done on it.
        vehicleSearcher =
          if(v == 1) (_,_) => 0
          else RoutingConventionMethods.cachedVehicleReachingPosition(changes.newValue, v)

      case x@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        //println("Move")
        //on which vehicle did we move?
        //also from --> to cannot include a vehicle start.
        digestUpdates(prev)
        if(x.isNop) return

        if(x.isSimpleFlip){
          //this is a simple flip

          val oldPrevFromValue = prev.newValue.valueAtPosition(fromIncluded - 1).get
          val oldSuccToValue = RoutingConventionMethods.routingSuccPos2Val(toIncluded,prev.newValue,v)

          val fromValue = x.fromValue
          val toValue = x.toValue

          //we do not care about order:-)
          val oldAStarBeforeMovedSegment = getAStarInfo(oldPrevFromValue,fromValue)

          val oldAStarAfterMovedSegmentDistance = if (freeReturn && oldSuccToValue < v) 0 else getAStarInfo(toValue,oldSuccToValue).distance

          val newAStarBeforeMovedSegment = computeDistanceAndSaveItAll(oldPrevFromValue,toValue)
          val newAStarAfterMovedSegmentOption = if (freeReturn && oldSuccToValue < v) None else Some(computeDistanceAndSaveItAll(fromValue,oldSuccToValue))
          val newAStarAfterMovedSegmentDistance = newAStarAfterMovedSegmentOption match {case None => 0; case Some(aStarInfo) => aStarInfo.distance}

          //for simple flip, there is no node cost to consider

          val vehicleOfMovedSegment = vehicleSearcher(prev.newValue, fromIncluded)

          distancePerVehicle(vehicleOfMovedSegment) :+= (
            newAStarBeforeMovedSegment.distance + newAStarAfterMovedSegmentDistance
              - (oldAStarBeforeMovedSegment.distance + oldAStarAfterMovedSegmentDistance))

        }else {
          //actually moving, not simple flip
          val oldPrevFromValue = prev.newValue.valueAtPosition(fromIncluded - 1).get
          val oldSuccToIfNoLoopOpt = prev.newValue.valueAtPosition(toIncluded + 1)
          val oldSuccToValue = oldSuccToIfNoLoopOpt match {
            case None => v - 1
            case Some(value) => if (value < v) value - 1 else value
          }

          val fromValue = x.fromValue
          val toValue = x.toValue
          val afterValue = x.afterValue

          val oldSuccAfterValue = RoutingConventionMethods.routingSuccPos2Val(after, prev.newValue, v)

          val oldAStarBeforeMovedSegment = getAStarInfo(oldPrevFromValue,fromValue)
          oldAStarBeforeMovedSegment.invalidate()

          val oldAStarAfterMovedSegmentDistance =
            if (freeReturn && oldSuccToValue < v)
              0
            else {
              val oldAStarAfterMovedSegment = getAStarInfo(toValue,oldSuccToValue)
              oldAStarAfterMovedSegment.invalidate()
              oldAStarAfterMovedSegment.distance
            }

          val oldAStarAfterAfterDistance =
            if (oldSuccAfterValue < v && freeReturn)
              0 else {
              val oldAStarAfterAfter = getAStarInfo(afterValue,oldSuccAfterValue)
              oldAStarAfterAfter.invalidate()
              oldAStarAfterAfter.distance
            }

          val newAStarBeforeMovedSegment = computeDistanceAndSaveItAll(afterValue,if(flip) toValue else fromValue)

          val newAStarAfterMovedSegmentOption = if (freeReturn && oldSuccAfterValue < v) None else Some(computeDistanceAndSaveItAll(if(flip) fromValue else toValue,oldSuccAfterValue))
          val newAStarAfterMovedSegmentDistance = newAStarAfterMovedSegmentOption match {case None => 0; case Some(aStarInfo) => aStarInfo.distance}
          val newAStarReplacingSegmentOption = if (freeReturn && oldSuccToValue < v) None else Some(computeDistanceAndSaveItAll(oldPrevFromValue,oldSuccToValue))
          val newAStarReplacingSegmentDistance = newAStarReplacingSegmentOption match {case None => 0; case Some(aStarInfo) => aStarInfo.distance}

          //per vehicle, there might be some node cost to consider
          val vehicleOfMovedSegment = vehicleSearcher(prev.newValue, fromIncluded)
          val targetVehicleOfMove = vehicleSearcher(prev.newValue, after)
          assert(vehicleOfMovedSegment == vehicleSearcher(prev.newValue,toIncluded))
          //println(oldSuccAfterValue + " -- " + oldSuccToValue + " -- " + oldPrevFromValue + " -- " + distancePerVehicle(vehicleOfMovedSegment))
          //println(newAStarReplacingSegmentDistance + " + " + newAStarBeforeMovedSegment.distance + " + " +  newAStarAfterMovedSegmentDistance
           // +  " - (" + (oldAStarBeforeMovedSegment.distance + " + " +  oldAStarAfterMovedSegmentDistance + " + " +  oldAStarAfterAfterDistance +")"))
          if (vehicleOfMovedSegment == targetVehicleOfMove) {
            //the segment is moved to the same vehicle, so we do not consider node cost here

            distancePerVehicle(vehicleOfMovedSegment) :+= (
              newAStarReplacingSegmentDistance + newAStarBeforeMovedSegment.distance + newAStarAfterMovedSegmentDistance
                - (oldAStarBeforeMovedSegment.distance + oldAStarAfterMovedSegmentDistance + oldAStarAfterAfterDistance))

          } else {
            //moving a segment to another vehicle, and per vehicle required.

            //summing the moved segment (this is slow, but it is requested to compute the cost per vehicle)
            val segmentLength = computeValueBetween(prev.newValue,
              vehicleOfMovedSegment,
              fromIncluded, fromValue,
              toIncluded,toValue)

            distancePerVehicle(vehicleOfMovedSegment) :+= (
              newAStarReplacingSegmentDistance - (oldAStarBeforeMovedSegment.distance + oldAStarAfterMovedSegmentDistance + segmentLength))

            distancePerVehicle(targetVehicleOfMove) :+= (
              newAStarBeforeMovedSegment.distance + segmentLength + newAStarAfterMovedSegmentDistance - oldAStarAfterAfterDistance)
          }
        }

      case SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        //println("Insert")
        digestUpdates(prev)

        val newSeq = changes.newValue

        val oldPrev = prev.newValue.valueAtPosition(pos-1).get

        val oldSucc =prev.newValue.valueAtPosition(pos) match{
          case None => v-1 //at the end
          case Some(oldSuccIfNoLoop) =>  if(oldSuccIfNoLoop < v) oldSuccIfNoLoop-1 else oldSuccIfNoLoop
        }

        val oldAStarDistance =
          if (freeReturn && oldSucc < v)
            0
          else {
            val oldAStar = getAStarInfo(oldPrev, oldSucc)
            oldAStar.invalidate()
            oldAStar.distance
          }

        val newDistanceBefore = computeDistanceAndSaveItAll(oldPrev,value).distance

        val newAStarAfterOption = if (freeReturn && oldSucc < v) None else Some(computeDistanceAndSaveItAll(value,oldSucc))
        val newDistanceAfter = newAStarAfterOption match {case None => 0; case Some(aStarInfo) => aStarInfo.distance}

        //println(oldPrev + " -- " + oldSucc + " -- " + value + " -- " + newDistanceBefore + " -- " + newDistanceAfter)

        //val newDistanceAfter = computeDistanceAndSaveItAll(oldSucc,value).distance

        val vehicle = vehicleSearcher(newSeq, pos)
        distancePerVehicle(vehicle) :+= (newDistanceBefore + newDistanceAfter - oldAStarDistance)

      case x@SeqUpdateRemove(positionOfDelete : Int, prev : SeqUpdate) =>
        //println("Remove")
        digestUpdates(prev)

        val removedValue = x.removedValue

        val oldPrevValue = prev.newValue.valueAtPosition(positionOfDelete-1).get
        val oldSuccValue = RoutingConventionMethods.routingSuccPos2Val(positionOfDelete, prev.newValue,v)

        //we do not care about order:-)
        val oldAStarBefore = getAStarInfo(oldPrevValue,removedValue)
        oldAStarBefore.invalidate()

        val oldAStarAfterDistance =
          if (freeReturn && oldSuccValue < v)
            0
          else {
            val oldAStarAfter = getAStarInfo(oldSuccValue,removedValue)
            oldAStarAfter.invalidate()
            oldAStarAfter.distance
          }

        val newDistance = if (freeReturn && oldSuccValue < v) 0 else computeDistanceAndSaveItAll(oldPrevValue,oldSuccValue).distance

        val vehicle = vehicleSearcher(prev.newValue,positionOfDelete)

        //println(oldPrevValue + " -- " + oldSuccValue + " -- " + oldAStarBefore.distance + " -- " + oldAStarAfterDistance + " -- " + distancePerVehicle(vehicle))

        distancePerVehicle(vehicle) :+= (newDistance - (oldAStarBefore.distance + oldAStarAfterDistance))


      case SeqUpdateLastNotified(value:IntSequence) =>
        require(value quickEquals routes.value)
      //we are starting from the previous value
      case SeqUpdateAssign(value : IntSequence) =>
        //impossible to go incremental
        dropAllAStarInfo()
        computeAndAffectValueFromScratch(value)
    }
  }

  @tailrec
  private def computeValueBetween(s:IntSequence, vehicle: Int, fromPosIncluded:Int, fromValueIncluded:Int, toPosIncluded:Int, toValueIncluded:Int):Long = {
    if(fromPosIncluded == toPosIncluded) 0L
    else if(fromPosIncluded < toPosIncluded) {
      var e = s.explorerAtPosition(fromPosIncluded).get
      var toReturn = 0L

      while (e.position < toPosIncluded) {
        val nextPos = e.next.get
        toReturn += getAStarInfo(e.value,nextPos.value).distance
        e = nextPos
      }
      toReturn
    }else{
      //this is symmetric
      computeValueBetween(s:IntSequence, vehicle: Int, toPosIncluded:Int, toValueIncluded:Int, fromPosIncluded:Int, fromValueIncluded:Int)
    }
  }

  override def checkInternals(c : Checker) : Unit = {
    check(c, routes.value)
  }

  private def check(c : Checker,s:IntSequence): Unit = {
    val debug = false
    //require(allAStarInfo.nonEmpty)
    if (debug)
      println("--------- Check ---------")
    var currentPosition = routes.value.explorerAtAnyOccurrence(0).get
    var currentVehicle:Int = 0
    var currentLength:Long = 0

    def checkHop(fromNode:Int,toNode:Int):Long = {
      val distance1 = getDistanceForAStarResult(aStarEngine.search(
        graph.nodes(nodeInRoutingToNodeInGraph(fromNode)),
        graph.nodes(nodeInRoutingToNodeInGraph(toNode)),
        isConditionalEdgeOpen,
        includePath = false))
      val distance2 = getDistanceForAStarResult(aStarEngine.search(
        graph.nodes(nodeInRoutingToNodeInGraph(toNode)),
        graph.nodes(nodeInRoutingToNodeInGraph(fromNode)),
        isConditionalEdgeOpen,
        includePath = false))

      val info = getAStarInfo(fromNode,toNode)
      val (minNode, maxNode) = if (fromNode < toNode) (fromNode, toNode) else (toNode, fromNode)

      require(minNode == info.minNode)
      require(maxNode == info.maxNode)
      require(distance1 == distance2, distance1 + "==" +  distance2)
      //TODO: did fail
      require(info.distance == distance1,
        s"${info.distance}==$distance1 info:$info")

      distance1
    }
    if (debug) {
      println(routes)
      println(distancePerVehicle.map(d => d.value).mkString(";"))
    }

    while(currentPosition.next match{
      case None => //at the end of the current vehicle, which is the last one
        //compute the last hop
        currentLength += (if (freeReturn) 0 else checkHop(currentPosition.value,v-1))
        require(distancePerVehicle(v-1).value == currentLength,
          s"Incremental Distance ${distancePerVehicle(v-1).value} -- From Scratch Distance $currentLength")
        false
      case Some(nextPosition) if nextPosition.value < v =>
        //at the end of the current vehicle; starting a new one
        val lastHopToComeBack = if (freeReturn) 0 else checkHop(currentPosition.value,currentVehicle)
        currentLength += lastHopToComeBack
        if (debug)
          println(s"Distance of Vehicle :${distancePerVehicle(currentVehicle).value}")
        require(distancePerVehicle(currentVehicle).value == currentLength,
          s"vehicle $currentVehicle Incremental Distance ${distancePerVehicle(currentVehicle).value} != From Scratch Distance $currentLength")

        currentPosition = nextPosition
        currentVehicle += 1
        currentLength = 0
        require(currentVehicle == nextPosition.value)
        true
      case Some(nextPosition) if nextPosition.value >= v =>
        //carry on the current vehicle
        val newHop = checkHop(currentPosition.value,nextPosition.value)
        currentLength += newHop
        currentPosition = nextPosition
        true
    }){}
    if (debug)
      println("------- End Check -------")
  }
}
