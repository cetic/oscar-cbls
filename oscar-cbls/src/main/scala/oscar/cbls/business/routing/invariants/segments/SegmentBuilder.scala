package oscar.cbls.business.routing.invariants.segments

import oscar.cbls.core.computation.{ChangingSeqValue,SeqUpdate,SeqUpdateDefineCheckpoint}
import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.magicArray.IterableMagicBoolArray
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.model.VehicleLocation

import scala.annotation.tailrec
import oscar.cbls.core.computation.SeqUpdateRollBackToCheckpoint
import oscar.cbls.algo.seq.IntSequenceExplorer

object VehicleSegments{
  def apply(segments: QList[Segment], n : Int,v : Int): VehicleSegments = new VehicleSegments(segments, n,v)
}



class VehicleSegments(val segments : QList[Segment],n : Int,v : Int)  {
  private def getValueAtPosition(pos: Int, routes: IntSequence): Int ={
    // routes.valueAtPosition(pos) match {
    //   case None => -1
    //   case Some(position) => position
    // }
    if(pos >= n) -1
    else {   // If we can't use the cache we use the explorer

      val explorer = routes.explorerAtPosition(pos)
      if (explorer.isDefined)
        explorer.get.value
      else
        -1
    }
  }


  /**
    * This method finds the impacted segment of the previous update.
    * The segment is found if the endNode is after or equal to the search position.
    *
    * @param pos the searched position
    * @param vehicle the vehicle in which we want to add a node
    * @return a tuple (impactedSegment: Segment, exploredSegments: Option[QList[Segment ] ], unexploredSegments: Option[QList[Segment ] ])
    */
  private def findImpactedSegment(pos: Int, initCounter: Int, segmentsToExplore: QList[Segment] = segments): (Segment, QList[Segment], QList[Segment], Int) ={
    @tailrec
    def checkSegment(segmentsToExplore: QList[Segment], counter: Int = initCounter, exploredSegments: QList[Segment] = null): (Segment, QList[Segment], QList[Segment], Int) ={
      require(segmentsToExplore != null, "Shouldn't happen, it means that the desired position is not within this vehicle route")
      val segment = segmentsToExplore.head
      val newCounter = counter + segment.length()
      if(newCounter >= pos)
        (segment, if(exploredSegments != null) exploredSegments.reverse else exploredSegments, segmentsToExplore.tail, counter)
      else
        checkSegment(segmentsToExplore.tail, newCounter, QList(segment,exploredSegments))
    }

    checkSegment(segmentsToExplore)
  }


  // def initSegment(routes : IntSequence,) = {
    

  // }


  /**
    * Remove Segments and Sub-Segment from the Segments List
    *
    * Find the Segments holding the specified positions and split them in two parts right after these positions.
    * If a position is at the end of a Segment, this Segment is not splitted.
    * The in-between segments are gathered as well
    * @param from From position
    * @param to To position
    * @return A ListSegments containing the segments containing or between from and to
    */
  def removeSubSegments(from: Int, to: Int, routes: IntSequence,searchStartPos : Int): (VehicleSegments, QList[Segment]) ={
    //val vehiclePos = vehicleSearcher.startPosOfVehicle(vehicle)
    //println(s"$searchStartPos ${this} -- $from -- $to")
    val (fromImpactedSegment, segmentsBeforeFromImpactedSegment, segmentsAfterFromImpactedSegment, currentCounter) =
      findImpactedSegment(from,searchStartPos-1)
    val (toImpactedSegment, tempSegmentsBetweenFromAndTo, segmentsAfterToImpactedSegment,_) =
      findImpactedSegment(to,currentCounter,QList(fromImpactedSegment,segmentsAfterFromImpactedSegment))

    val segmentsBetweenFromAndTo = QList.qDrop(tempSegmentsBetweenFromAndTo,1)
    
    // nodeBeforeFrom is always defined because it's at worst a vehicle node
    // val nodeBeforeFrom = getValueAtPosition(from -1,routes)
    // val fromNode = getValueAtPosition(from, routes)

    val explorerBeforeFrom = routes.explorerAtPosition(from-1).get
    val nodeBeforeFrom = explorerBeforeFrom.value
    val fromNode = explorerBeforeFrom.next match {
      case None => -1
      case Some(e) => e.value
    }

    // val toNode = getValueAtPosition(to, routes)
    // val nodeAfterTo = if(to+1 < n)getValueAtPosition(to+1, routes) else -1
    val explorerToNode = routes.explorerAtPosition(to).get
    val toNode = explorerToNode.value
    val nodeAfterTo = explorerToNode.next match {
      case None => -1
      case Some(e) => e.value
    }

    val lengthUntilFromImpactedSegment = QList.qFold[Segment,Int](segmentsBeforeFromImpactedSegment, (acc,item) => acc + item.length(),0)


    // We split the fromImpactedSegment in two part fromLeftResidue and toLeftResidue
    val fromLeftResidueLength = from - lengthUntilFromImpactedSegment - searchStartPos    // From is not part of the left residue
    val fromRightResidueLength = fromImpactedSegment.length() - fromLeftResidueLength
    val fromLeftAndRightResidue = fromImpactedSegment.splitAtNode(nodeBeforeFrom, fromNode, fromLeftResidueLength, fromRightResidueLength)
    val fromLeftResidue = fromLeftAndRightResidue._1
    val fromRightResidue = fromLeftAndRightResidue._2

    val (removedSegments, toRightResidue) =
      if(fromImpactedSegment == toImpactedSegment){
        // Same segment => We split the fromRightResidue in two parts removedSubSegment and toRightResidue
        if (nodeAfterTo < v)      // To is at the end of the vehicle route
          (QList(fromRightResidue), null)
        else {
          val toRightResidueLength = fromRightResidueLength - to + from - 1
          val removedSubSegmentLength = fromRightResidueLength - toRightResidueLength
          val removedSegmentAndRightResidue = fromRightResidue.splitAtNode(toNode, nodeAfterTo, removedSubSegmentLength, toRightResidueLength)
          (QList(removedSegmentAndRightResidue._1), removedSegmentAndRightResidue._2)
        }

      } else {
        var removedSegments = QList(fromRightResidue,segmentsBetweenFromAndTo)
        val lengthUntilToImpactedSegment =
          lengthUntilFromImpactedSegment + fromImpactedSegment.length() +
        QList.qFold[Segment,Int](segmentsBetweenFromAndTo, (acc,item) => acc + item.length(),0)

        // Diff segment => We split the toImpactedSegment in two parts toLeftResidue and toRightResidue
        val toLeftAndRightResidue =
          if(nodeAfterTo < v)
            (toImpactedSegment, null)
          else {
            val toLeftResidueLength = to - searchStartPos - lengthUntilToImpactedSegment + 1    // To is part of the left residue
            val toRightResidueLength = toImpactedSegment.length() - toLeftResidueLength
            toImpactedSegment.splitAtNode(toNode, nodeAfterTo, toLeftResidueLength, toRightResidueLength)
          }
        
        val toLeftResidue = toLeftAndRightResidue._1
        val toRightResidue = toLeftAndRightResidue._2

        removedSegments = QList.nonReversedAppend(removedSegments, QList(toLeftResidue))
        (removedSegments, toRightResidue)
      }

    var newSegments: QList[Segment] = segmentsAfterToImpactedSegment
    if(toRightResidue != null) newSegments = QList(toRightResidue, newSegments)
    if(fromLeftResidue != null) newSegments = QList(fromLeftResidue, newSegments)
    newSegments = QList.nonReversedAppend(segmentsBeforeFromImpactedSegment, newSegments)

    (VehicleSegments(newSegments,n,v),removedSegments)
  }

  /**
    * Insert a list of segments at the specified position
    * @param segmentsToInsert
    * @param afterPosition
    * @return
    */
  def insertSegments(segmentsToInsert: QList[Segment], afterPosition: Int, routes: IntSequence,searchStartPos : Int, delta: Int = 0): VehicleSegments ={
    //val searchStartPos = vehicleSearcher.startPosOfVehicle(vehicle)

    //println(s"$searchStartPos $this ${segmentsToInsert.mkString(",")} $delta")

    if (segments == null)
      VehicleSegments(segmentsToInsert,n,v)
    else {

      val (impactedSegment, segmentsBeforeImpactedSegment, segmentsAfterImpactedSegment,_) = findImpactedSegment(afterPosition - delta, searchStartPos-1)

      // val insertAfterNode = getValueAtPosition(afterPosition, routes)
      // val insertBeforeNode: Int = if(afterPosition+1 < n) getValueAtPosition(afterPosition+1, routes) else -1
      val explorerBeforeInsert = routes.explorerAtPosition(afterPosition).get
      val insertAfterNode = explorerBeforeInsert.value
      val insertBeforeNode = explorerBeforeInsert.next match {
        case None => -1
        case Some(e) => e.value
      }


      // We split the impacted segment in two parts (leftResidue and rightResidue)
      // 1° => Compute parts' length
      // 2° => Split the impacted segment
      val segmentsLengthBeforeImpactedSegment = QList.qFold[Segment,Int](segmentsBeforeImpactedSegment, (acc,item) => acc + item.length(),0)
      val leftRightResidue = if(insertBeforeNode < v){
        (impactedSegment, null)
      } else {
        val rightResidueLength = segmentsLengthBeforeImpactedSegment + impactedSegment.length() - afterPosition + searchStartPos - 1 + delta
        val leftResidueLength = impactedSegment.length() - rightResidueLength
        impactedSegment.splitAtNode(insertAfterNode, insertBeforeNode, leftResidueLength, rightResidueLength)
      }
      val leftResidue = leftRightResidue._1
      val rightResidue = leftRightResidue._2


      // Building the resulting QList starting at the end
      var newSegments: QList[Segment] = segmentsAfterImpactedSegment                     // Segments after impacted segment
      if(rightResidue != null) newSegments = QList(rightResidue, newSegments)               // add right residue
      newSegments = QList.nonReversedAppend(segmentsToInsert, newSegments)                  // prepend the segments to insert
      if(leftResidue != null) newSegments = QList(leftResidue, newSegments)                 // add left residue
      newSegments = QList.nonReversedAppend(segmentsBeforeImpactedSegment, newSegments)     // Prepend segments before impacted segments

      VehicleSegments(newSegments, n,v)
    }
  }

  override def toString: String ={
    s"Segments of vehicle : ${segments.mkString(", ")}"
  }
  
}

abstract sealed class Segment{
  /**
    * Split this Segment in two Segments right before the split node
    * If split node == start node, there will only be one Segment, the Segment itself
    * @return the left part of the splitted Segment (if exist) and the right part (starting at splitNode)
    */
  def splitAtNode(beforeSplitNode: Int, splitNode: Int, leftLength: Int, rightLength: Int): (Segment,Segment)

  def flip(): Segment

  def length(): Int

  def startNode(): Int

  def endNode(): Int
}

/**
  * This represents a subsequence starting at startNode and ending at endNode.
  * This subsequence was present in the global sequence when the pre-computation was performed
  * @param startNode the first node of the subsequence
  * @param endNode the last node of the subsequence
  */
case class PreComputedSubSequence(startNode:Int,
                                                  endNode:Int,
                                                  length: Int) extends Segment{
  override def toString: String = {
    s"PreComputedSubSequence (StartNode : $startNode EndNode : $endNode Length : $length)"
  }

  override def splitAtNode(beforeSplitNode: Int, splitNode: Int, leftLength: Int, rightLength: Int): (Segment,Segment) = {
    if(splitNode == startNode) (null,this)
    else if(beforeSplitNode == endNode) (this,null)
    else {
      (PreComputedSubSequence(startNode,beforeSplitNode,leftLength),
        PreComputedSubSequence(splitNode,endNode,rightLength))
    }
  }

  override def flip(): Segment = {
    FlippedPreComputedSubSequence(endNode, startNode, length)
  }
}

/**
  * This represents a subsequence starting at startNode and ending at endNode.
  * This subsequence was not present in the global sequence when the pre-computation was performed, but
  * the flippedd subsequence obtained by flippig it was present in the global sequence when the pre-computation was performed, but
  * @param startNode the first node of the subsequence knowing that the sequence is flipped (it was the last node of the subsequence when the pre-computation was performed)
  * @param endNode the last node of the subsequence knowing that the sequence is flipped (it was the first node of the subsequence when the pre-computation was performed)

  */
case class FlippedPreComputedSubSequence(startNode:Int,
                                                         endNode:Int,
                                                         length: Int) extends Segment{
  override def toString: String = {
    s"FlippedPreComputedSubSequence (StartNode : $startNode EndNode : $endNode Length : $length)"
  }

  override def splitAtNode(beforeSplitNode: Int, splitNode: Int, leftLength: Int, rightLength: Int): (Segment,Segment) = {
    if(splitNode == startNode) (null,this)
    else if(beforeSplitNode == endNode) (this,null)
    else {
      (FlippedPreComputedSubSequence(startNode,beforeSplitNode,leftLength),
        FlippedPreComputedSubSequence(splitNode,endNode,rightLength))
    }
  }

  override def flip(): Segment = {
    PreComputedSubSequence(endNode, startNode, length)
  }
}

/**
  * This represent that a node that was not present in the initial sequence when pre-computation was performed.
  * @param node
  */
case class NewNode(node:Int) extends Segment{
  override def toString: String = {
    s"NewNode - Node : $node"
  }

  override def splitAtNode(beforeSplitNode: Int, splitNode: Int, leftLength: Int, rightLength: Int): (Segment,Segment) = {
    require(beforeSplitNode == node || splitNode == node,s"Spliting $this between $beforeSplitNode and $splitNode is impossible")
    if (beforeSplitNode == node)
      (this, null)
    else
      (null, this)
  }

  override def flip(): Segment = {
    this
  }

  override def length(): Int = 1

  override def startNode(): Int = node

  override def endNode(): Int = node
}
