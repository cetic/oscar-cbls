package oscar.cbls.business.routing.invariants.naive

import oscar.cbls.core.computation.{Invariant,SeqNotificationTarget,SeqUpdate,ChangingSeqValue,SeqUpdateDefineCheckpoint,SeqUpdateRollBackToCheckpoint,SeqUpdateInsert,SeqUpdateMove,SeqUpdateRemove,SeqUpdateLastNotified,SeqUpdateAssign}
import oscar.cbls.business.routing.invariants.segments.{VehicleSegments,Segment,NewNode,PreComputedSubSequence,FlippedPreComputedSubSequence}
import oscar.cbls.algo.seq.{IntSequence,IntSequenceExplorer}
import oscar.cbls.business.routing.model.VehicleLocation
import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.magicArray.IterableMagicBoolArray
import oscar.cbls.core.propagation.Checker


/** Defines the abstract naive routing constraint
  * 
  * The constraint takes a type parameter '''U'''. It computes the value (of type '''U''') associated to each node using the function '''op'''. 
  * The function '''op''' (given by the user) shall be able to compute the value for the node '''to''' using the node '''from''' and the value
  * at the node '''from'''. The constraint is "naive" in a sens that it is not able to make symbolic calculus. But the constraint 
  * only recomputes what is necessary.
  * 
  * The constraint uses the a segment mecanism: after an update, the sequence is cutted into segements that represent the changes since
  * the last time the constraint had been notified.
  * 
  * The constraint is independant from the direction of the travel through the sequence (the sequence can be travelled forward 
  * or backward)
  * 
  * @param routes The input sequence of the constraint.
  * @param n The number of nodes of the problem
  * @param v The numver of vehicles of the problem
  * @param defaultValueForUnroutedNodes The default value associated to unrouted nodes
  * @param initValuePerVehicle The value at the begining of the vehicle (the begining of the vehicle depends on the travel
  * direction through the sequence)
  * @param op A function (('''from''','''to''','''valueFrom''') => '''valueTo''') that takes the node '''from''', the node '''to''' and the value
  * '''valueFrom''' associated to the node '''from''' and computes the value associated to the node '''to''' 
  * 
  **/


abstract class AbstractNaiveRoutingConstraint[U <: Any : Manifest](routes : ChangingSeqValue,
  n : Int,
  v : Int,
  defaultValueForUnroutedNodes : U,
  initValuePerVehicle : Array[U],
  op : (Int,Int, U) => U) extends Invariant with SeqNotificationTarget {

  // The invariants listen to the sequence routes
  registerStaticAndDynamicDependency(routes)
  finishInitialization()

  //A vehicle searcher for each vehicle
  private var vehicleSearcher : VehicleLocation = VehicleLocation((0 until v).toArray)

  // The array that contains the segments of the vehicle
  private val segmentsOfVehicle : Array[VehicleSegments] = Array.fill(v)(null)

  // A variable that saves the vehicle searcher at each checkpoint
  private var vehicleSearcherStack : QList[(Int,VehicleLocation)] = null

  // The current value at each nodes
  private val valueOfNodes : Array[U] = Array.fill(n)(defaultValueForUnroutedNodes)

  // // The value associated to the vehicle at the end of the route
  // private val vehicleValueAtEnd : Array[U] = Array.fill(n)(defaultValueForUnroutedNodes)

  // An array that contains the set of vehicle that has changed since the last notification
  private val changedVehicleSinceLastNotified = new IterableMagicBoolArray(v,false)

  for (vehicle <- 0 until v) updateValuesOfVehicleNodesFromScratch(vehicle,routes.newValue)

  /** Returns an Option[IntSequenceExplorer] on the next point of the route to consider
    * The next point depends on the direction of travel through the sequence
    * 
    * @param expl The sequence explorer at the current node of the segment
    * @param seg The current segment
    * @return an option explorer that is 
               - '''None''' if the current node is the end of the segment
               - '''Some(expl)''' else */


  protected def getNextPointOfSegmentToUpdate(expl : IntSequenceExplorer,seg : Segment) : Option[IntSequenceExplorer]

  /** Returns the first point to update on a segment (the first point depends on the direction of travel through the sequence)
    * 
    * @param seg The segment
    * @return The id of the first point to update
    */
  protected def firstPointToUpdateOnSegment(seg : Segment) : Int

  /** Returns the last point to update on a segment (the last point depends on the direction of travel through the sequence)
    * 
    * @param seg The segment
    * @return The id of the last point to update
    */
  protected def lastPointToUpdateOnSegment(seg : Segment) : Int

  /** Returns the order in which the segments have to be processed (the order depends on the direction of the travel through the sequence
    * 
    * @param segments The structure that contains the segments of a vehicle
    * @return The list of segment in the correct order
    */

  protected def ordonateSegments(segments : VehicleSegments) : QList[Segment]

  /** Implement this function in order to assign the relevant values in U to the oscar variable of your problem
    * 
    * @param node The index of the node
    * @param value The value associated to the node
    */

  protected def assignNodeValue(node : Int,value : U) : Unit

  /** Returns the array of value associated to each node
    */
  def getValueAtNodes : Array[U] = valueOfNodes

  /** Incrementally update the values of the node for a given vehicle
    * 
    * @param vehicle The vehicle to update
    * @param routes The current routes of the vehicle
    */

  private def updateValuesOfVehicleNodes(vehicle : Int,routes : IntSequence) : Unit = {
    // println(s"Update Vehicle Values of vehicle $vehicle")
    // println(segmentsOfVehicle(vehicle))
    val segmentList = ordonateSegments(segmentsOfVehicle(vehicle))
    // println(segmentList.mkString(", "))
    //println(valueOfNodes.mkString("\n"))
    val lastNode = updateSegmentListValues(segmentList,initValuePerVehicle(vehicle),vehicle,routes)
    // println(s"LastNode : $lastNode")
    //val lastValue = if (lastNode == vehicle) initValuePerVehicle(vehicle) else valueOfNodes(lastNode)
    //println(s"Last value : $lastValue")
    if (lastNode != vehicle)
      updateValueOfNode(vehicle,op(lastNode,vehicle,valueOfNodes(lastNode)))
    else
      updateValueOfNode(vehicle,initValuePerVehicle(vehicle))
    //vehicleValueAtEnd(vehicle) = op(lastNode,vehicle,valueOfNodes(lastNode))
  }


  /** Update the value associated to a node
    * 
    * @param node The node to update
    * @param value The new value of the node
    */
  private def updateValueOfNode(node : Int,value : U) : Unit = {
    // println(s"Update $node with $value")
    valueOfNodes(node) = value
    assignNodeValue(node : Int,value : U)
  }


  /** Recurcively updates the value the nodes of a segment list. The segments are updated in the order given by the list.
    * 
    * @param segments The list of segments
    * @param prevValue The value of the node before the segment
    * @param prevNode The last node before the segment
    * @param routes The current routes of the vehicles
    */
  private def updateSegmentListValues(segments : QList[Segment],prevValue : U,prevNode : Int,routes : IntSequence) : Int = {
    if (segments != null) {
      val currentSegment = segments.head
      //println(s"Treating segment : $currentSegment")
      currentSegment match {
        case NewNode(node) =>
          updateValueOfNode(node,op(prevNode,node,prevValue))
          //println(s"$prevNode -> $n (prevValue = $prevValue) : ${op(prevNode,node,prevValue)}")
          updateSegmentListValues(segments.tail,valueOfNodes(node),node,routes)
        case PreComputedSubSequence(_,_,_) =>
          val fstNode = firstPointToUpdateOnSegment(currentSegment)
          val currentValueOfFirstNode = valueOfNodes(fstNode)
          val newValueOfFirstNode = op(prevNode,fstNode,prevValue)
          //println(s"f($prevNode,$fstNode,$prevValue) = $newValueOfFirstNode")
          if (currentValueOfFirstNode != newValueOfFirstNode) {
            updateValueOfNode(fstNode,newValueOfFirstNode)
            updateSegmentValues(getNextPointOfSegmentToUpdate(routes.explorerAtFirstOccurrence(fstNode).get,currentSegment),currentSegment,fstNode)
          }
          val lastNode = lastPointToUpdateOnSegment(currentSegment)
          val lastValue = valueOfNodes(lastNode)
          updateSegmentListValues(segments.tail,lastValue,lastNode,routes)
        case FlippedPreComputedSubSequence(_,_,_) =>
          val fstNode = firstPointToUpdateOnSegment(currentSegment)
          val currentValueOfFirstNode = valueOfNodes(fstNode)
          val newValueOfFirstNode = op(prevNode,fstNode,prevValue)
          //println(s"$fstNode $currentValueOfFirstNode - $newValueOfFirstNode")
          updateValueOfNode(fstNode,newValueOfFirstNode)
          updateSegmentValues(getNextPointOfSegmentToUpdate(routes.explorerAtFirstOccurrence(fstNode).get,currentSegment),currentSegment,fstNode,forceRecompute = true)
          val lastNode = lastPointToUpdateOnSegment(currentSegment)
          val lastValue = valueOfNodes(lastNode)
          updateSegmentListValues(segments.tail,lastValue,lastNode,routes)
      }
    } else
        prevNode
  }


  /** Updates values of the vehicle nodes from scratch. It initiates the segments and travel through to update the nodes value
    * 
    * @param vehicle The vehicle for which the nodes need to be updated
    * @param routes The current routes of the vehicles
    */
  private def updateValuesOfVehicleNodesFromScratch(vehicle : Int,routes : IntSequence) : Unit = {


    initSegmentsOfVehicle(vehicle,routes)
    updateValueOfNode(vehicle,initValuePerVehicle(vehicle))
    if (segmentsOfVehicle(vehicle).segments != null) {
      val vehicleSegment = segmentsOfVehicle(vehicle).segments.head
      val explorer = routes.explorerAtFirstOccurrence(firstPointToUpdateOnSegment(vehicleSegment))
      val lastNode = updateSegmentValues(explorer,vehicleSegment,vehicle,true)
      if (lastNode.isDefined)
        updateValueOfNode(vehicle,op(lastNode.get,vehicle,valueOfNodes(lastNode.get)))
      //vehicleValueAtEnd(vehicle) = op(lastNode.get,vehicle,valueOfNodes(lastNode.get))
    }
  }

  /** Update the values of the nodes of one segment. It starts at the begining of the segment and ends either when it reaches the end of the segment or 
    * when it reaches a node where the new value is identical to the previous value (in this case, there is no need to recompute until the end of the
    * segment. A flag '''forceRecompute''' can force to compute until the end of the segment even if a identical value is reached
    * 
    * @param explOpt The IntSequenceExplorer option on the current node to update
    * @param s The Segment to explore
    * @param prevPoint The id of the previous node
    * @param forceRecompute A flag to force the update of the entire segment
    * @return returns the last point of the segment if the end of the segment has been reached (returns None instead)
    */

  private def updateSegmentValues(explOpt : Option[IntSequenceExplorer],s : Segment,prevPoint : Int,forceRecompute : Boolean = false) : Option[Int] = {
    explOpt match {
      case None => Some(prevPoint)
      case Some(expl) =>
        val currentPoint = expl.value
        val newValueOfPoint = op(prevPoint,currentPoint,valueOfNodes(prevPoint))
        //println(s"$prevPoint -> $currentPoint -> $newValueOfPoint (oldValue : ${valueOfNodes(currentPoint)})")
        if (forceRecompute || newValueOfPoint != valueOfNodes(currentPoint)) {
          //println("Update Value")
          updateValueOfNode(currentPoint,newValueOfPoint)
          //println(valueOfNodes(currentPoint))
          updateSegmentValues(getNextPointOfSegmentToUpdate(expl,s),s,currentPoint,forceRecompute)
        } else
            None
    }
  }


  /** Initialize the segments of the vehicles. At the initialisation, there is 0 or 1 segment (0 if the vehicle route is empty,
    * 1 otherwise).
    * 
    * @param vehicle The vehicle for which the segents are initialized
    * @param routes The current sequence
    */
  private def initSegmentsOfVehicle(vehicle : Int, routes : IntSequence) : Unit = {
    // println(routes)
    // println(s"Init vehicle : $vehicle")
    // println(s"$vehicleSearcher")
    val posOfVehicle = vehicleSearcher.startPosOfVehicle(vehicle)
    val (lastNodeOfVehicle,lastNodePos) : (Int,Int) =
      if (vehicle < v-1) {
        val lastNodePos = vehicleSearcher.startPosOfVehicle(vehicle + 1) - 1
        (routes.valueAtPosition(lastNodePos).get,lastNodePos)
      }
      else
        (routes.valueAtPosition(routes.size - 1).get,routes.size - 1)

    val fstNodePos = vehicleSearcher.startPosOfVehicle(vehicle) + 1

    //println(s"${vehicleSearcher.startPosOfVehicle(vehicle)} -- $fstNodePos -- $lastNodePos")

    val segmentList : QList[Segment] = if (fstNodePos <= lastNodePos) QList(PreComputedSubSequence(routes.valueAtPosition(fstNodePos).get,lastNodeOfVehicle,lastNodePos - posOfVehicle)) else null
    segmentsOfVehicle(vehicle) = VehicleSegments(segmentList,n,v)

    //println(segmentsOfVehicle(vehicle))

  }


  override def notifySeqChanges(r : ChangingSeqValue, d : Int,changes : SeqUpdate) : Unit = {

    // println("\n\n\n==============================================================================================")
    // println("== New Notify")
    // println("==============================================================================================")
    // println(r.value)
    // (0 until n).foreach(node => println(s"$node -> (value: ${valueOfNodes(node)})"))
    // println(changes)
    // println(changes.newValue)
    // (0 until v).foreach(vehicle => println(s"$vehicle --> ${segmentsOfVehicle(vehicle)}"))

    if (digestUpdates(changes)) {
      // println("INCREMENTAL")
      // println(s"Changed Vehicle : ${changedVehicleSinceLastNotified.indicesAtTrueAsQList.mkString(";")}")
      // (0 until v).foreach(vehicle => println(s"$vehicle --> ${segmentsOfVehicle(vehicle)}"))

      QList.qForeach(changedVehicleSinceLastNotified.indicesAtTrueAsQList,(vehicle : Int) => {
        updateValuesOfVehicleNodes(vehicle,r.newValue)
      })
    } else {
      // println("FROM SCRATCH")
      for (vehicle <- 0 until v) updateValuesOfVehicleNodesFromScratch(vehicle,r.newValue)
    }
  }

  /** Returns the vehicle searcher and the new vehicle searcher list corresponding to the roll back to a checkpoint level in the vehicle search list
    * 
    * @param checkPointLevel The level of teh checkpoint
    * @param vehicleSearcherList The list of vehicle searcher
    * @return A couple that contains the saved vehicle searcher and the new vehicle searcher list without the checkpoint with higher level
    */

  private def getCachedVehicleSearcher(checkPointLevel : Int,vehicleSearcherList : QList[(Int,VehicleLocation)]) : (VehicleLocation,QList[(Int,VehicleLocation)]) = {
    if (vehicleSearcherList == null) {
      throw new Error(s"CheckPoint Error : Impossible to go back to required checkpoint level ($checkPointLevel)")
    } else {
      if (vehicleSearcherList.head._1 == checkPointLevel) {
        (vehicleSearcherList.head._2,vehicleSearcherList)
      } else {
        getCachedVehicleSearcher(checkPointLevel,vehicleSearcherList.tail)
      }
    }
  }

  /** Digest the updates
    */

  private def digestUpdates(changes : SeqUpdate) : Boolean = {
    //println(changes)
    changes match {
      case SeqUpdateDefineCheckpoint(prev : SeqUpdate,checkpointLevel : Int) =>
        val prevUpdate = digestUpdates(prev)

        if (checkpointLevel == 0)
          vehicleSearcher = vehicleSearcher.regularize
        vehicleSearcherStack =
          if (checkpointLevel == 0)
            QList((0,vehicleSearcher),null)
          else
            QList((checkpointLevel,vehicleSearcher),vehicleSearcherStack)

        prevUpdate


      case r@SeqUpdateRollBackToCheckpoint(checkpoint: IntSequence, checkpointLevel: Int) =>

        // println(vehicleSearcherStack.mkString(";"))

        // println(s"Digest Update of RollBack ... (${r.howToRollBack})")


        val prevUpdate = digestUpdates(r.howToRollBack)
        // println("Digested !")
        // println(s"Go Back to checkPoint $checkpointLevel ${vehicleSearcherStack.mkString(";")}")


        val (newVehicleSearcher,newVehicleSearcherStack) = getCachedVehicleSearcher(checkpointLevel,vehicleSearcherStack)
        // vehicleSearcher = newVehicleSearcher
        // vehicleSearcherStack = newVehicleSearcherStack

        prevUpdate


      case sui@SeqUpdateInsert(value: Int, pos: Int, prev: SeqUpdate) =>
        val prevUpdate = digestUpdates(prev)

        if (prevUpdate) {
          val impactedVehicle = vehicleSearcher.vehicleReachingPosition(pos - 1)
          val vehiclePosition = vehicleSearcher.startPosOfVehicle(impactedVehicle)

          // println(sui)
          // println(impactedVehicle)


          changedVehicleSinceLastNotified(impactedVehicle) = true
          segmentsOfVehicle(impactedVehicle) = segmentsOfVehicle(impactedVehicle).insertSegments(QList[Segment](NewNode(value)),pos - 1,prev.newValue,vehiclePosition + 1)

        }

        vehicleSearcher = vehicleSearcher.push(sui.oldPosToNewPos)

        prevUpdate
      case sum@SeqUpdateMove(fromIncluded: Int, toIncluded: Int, after: Int, flip: Boolean, prev: SeqUpdate) =>
        val prevUpdate = digestUpdates(prev)
        val prevRoutes = prev.newValue

        val fromVehicle = vehicleSearcher.vehicleReachingPosition(fromIncluded)
        val fromVehiclePosition = vehicleSearcher.startPosOfVehicle(fromVehicle)
        val toVehicle = vehicleSearcher.vehicleReachingPosition(after)
        val sameVehicle = fromVehicle == toVehicle
        val toVehiclePosition = if (sameVehicle) fromVehiclePosition else vehicleSearcher.startPosOfVehicle(toVehicle)

        changedVehicleSinceLastNotified(fromVehicle) = true
        if (!sameVehicle)
          changedVehicleSinceLastNotified(toVehicle) = true

        val fromImpactedSegment = segmentsOfVehicle(fromVehicle)

        // println(fromImpactedSegment)

        val (listSegmentsAfterRemove, segmentsToRemove) =
          fromImpactedSegment.removeSubSegments(fromIncluded, toIncluded, prevRoutes,fromVehiclePosition + 1)

        // println(listSegmentsAfterRemove)

        val toImpactedSegment = if (sameVehicle) listSegmentsAfterRemove else segmentsOfVehicle(toVehicle)
        // println(s"toImpactedSegment : $toImpactedSegment")
        // If we are in same vehicle and we remove nodes to put them later in the route, the route length before insertion point has shortened
        val delta =
          if (!sameVehicle || after < fromIncluded) 0
          else toIncluded - fromIncluded + 1


        // Insert the sub-segments at his new position
        val listSegmentsAfterInsertion =
          if (flip)
            toImpactedSegment.insertSegments(segmentsToRemove.qMap(_.flip()).reverse, after, prevRoutes,toVehiclePosition + 1 , delta)
          else
            toImpactedSegment.insertSegments(segmentsToRemove, after, prevRoutes, toVehiclePosition + 1 , delta)

        // println(listSegmentsAfterInsertion)

        segmentsOfVehicle(toVehicle) = listSegmentsAfterInsertion
        if (!sameVehicle) segmentsOfVehicle(fromVehicle) = listSegmentsAfterRemove

        vehicleSearcher = vehicleSearcher.push(sum.oldPosToNewPos)

        prevUpdate
      case sur@SeqUpdateRemove(position: Int, prev: SeqUpdate) =>
        val prevUpdate = digestUpdates(prev)
        
        val prevRoutes = prev.newValue

        val impactedVehicle = vehicleSearcher.vehicleReachingPosition(position)
        val vehiclePos = vehicleSearcher.startPosOfVehicle(impactedVehicle)
        val impactedSegment = segmentsOfVehicle(impactedVehicle)
        val removedNode = prevRoutes.valueAtPosition(position).get
        //println(s"REMOVED: $removedNode")
        updateValueOfNode(removedNode,defaultValueForUnroutedNodes)
        changedVehicleSinceLastNotified(impactedVehicle) = true
        val (listSegmentAfterRemove, _) = impactedSegment.removeSubSegments(position, position, prevRoutes,vehiclePos + 1)

        segmentsOfVehicle(impactedVehicle) = listSegmentAfterRemove

        vehicleSearcher = vehicleSearcher.push(sur.oldPosToNewPos)

        prevUpdate

      case SeqUpdateLastNotified(value: IntSequence) =>
        for (vehicle <- 0 until v) initSegmentsOfVehicle(vehicle,value)
        changedVehicleSinceLastNotified.all_=(false)
        true

      case SeqUpdateAssign(value: IntSequence) =>
        vehicleSearcher = VehicleLocation(Array.tabulate(v)(vehicle => {
          value.positionOfAnyOccurrence(vehicle).get
        }))
        for (vehicle <- 0 until v) initSegmentsOfVehicle(vehicle,value)
        false

      case _ => // Not incremental
        false
    }
  }

  override def checkInternals(c: Checker): Unit = {
    // println("\n\n== Check Internals ")
    // println(routes.newValue)
    // (0 until n).foreach(node => println(s"$node -> (value : ${valueOfNodes(node)})"))
    val incrValues = valueOfNodes.clone()
    for (vehicle <- 0 until v) {
      updateValuesOfVehicleNodesFromScratch(vehicle,routes.newValue)
    }
    // println(valueOfNodes.mkString(";;"))
    for (node <- 0 until n) {
      require(valueOfNodes(node) == incrValues(node),s"For node $node, incremental value (${incrValues(node)}) != Value from scratch (${valueOfNodes(node)})")
    }
  }



}


