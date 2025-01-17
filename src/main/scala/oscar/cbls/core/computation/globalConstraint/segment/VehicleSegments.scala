// OscaR is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 2.1 of the License, or
// (at your option) any later version.
//
// OscaR is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License  for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with OscaR.
// If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html

package oscar.cbls.core.computation.globalConstraint.segment

import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer, RootIntSequenceExplorer}
import oscar.cbls.modeling.routing.VRP

import scala.annotation.tailrec

/** Companion object of the [[VehicleSegments]] class. */
object VehicleSegments {

  /** Returns a VehicleSegment that wrap a list of [[Segment]] and methods to update that list.
    *
    * @param segments
    *   The list of subsequences on which we can reuse precomputation.
    * @param v
    *   The number of vehicle in the associated route.
    */
  def apply(segments: List[Segment], v: Int): VehicleSegments = new VehicleSegments(segments, v)

  /** Returns a VehicleSegment that wrap a list of [[Segment]] and methods to update that list. This
    * instance contain only one segment covering all the input vehicle.
    *
    * @param vrp
    *   The routing problem instance that need some precomputation.
    * @param vehicle
    *   The vehicle we want to generate segment.
    */
  def apply(vrp: VRP, vehicle: Int): VehicleSegments = {
    val nodes = vrp.routeOfVehicle(vehicle)
    val seg   = PrecomputedSubSequence(vehicle, nodes.last, nodes.length)
    VehicleSegments(List(seg), vrp.v)
  }

  def apply(route: IntSequence, vehicle: Int, v: Int): VehicleSegments = {
    require(
      vehicle < v,
      s"Working with node (node $vehicle) that is not a vehicle (number of vehicles = $v)"
    )
    var nodes = List(vehicle)

    var exp = route.explorerAtAnyOccurrence(vehicle).get.next
    while (exp.position < route.size && exp.value >= v) {
      nodes = exp.value :: nodes
      exp = exp.next
    }
    val seg = PrecomputedSubSequence(vehicle, nodes.head, nodes.length)
    VehicleSegments(List(seg), v)
  }

  /** Moves subsegments from a vehicle to another.
    *
    * @param sourceVehiclePos
    *   The position of the source vehicle.
    * @param targetVehiclePos
    *   The position of the target vehicle.
    * @param sourceSegments
    *   The list of segments of the source vehicle.
    * @param targetSegments
    *   The list of segments of the target vehicle.
    * @param fromIncludedExp
    *   The explorer containing the start position of the segments to move.
    * @param toIncludedExp
    *   The explorer containing the end position of the segments to move.
    * @param afterPosExp
    *   The explorer containing the position after which insert the segments.
    * @param flip
    *   If the moved segments have to be flipped.
    * @return
    *   1. An updated list of segments for the source vehicle.
    *   1. An updated list of segments for the target vehicle.
    */
  def moveSegments(
    sourceVehiclePos: Int,
    targetVehiclePos: Int,
    sourceSegments: VehicleSegments,
    targetSegments: VehicleSegments,
    fromIncludedExp: IntSequenceExplorer,
    toIncludedExp: IntSequenceExplorer,
    afterPosExp: IntSequenceExplorer,
    flip: Boolean
  ): (VehicleSegments, VehicleSegments) = {

    val sameVehicle: Boolean = sourceVehiclePos == targetVehiclePos

    // Removes segments in the source vehicle
    val (fromSegmentsAfterMove, segmentsToMove): (VehicleSegments, List[Segment]) =
      sourceSegments.removeSubSegments(fromIncludedExp, toIncludedExp, sourceVehiclePos)

    val toImpactedSegments: VehicleSegments =
      if (sameVehicle) fromSegmentsAfterMove else targetSegments

    val lengthOfMovedSegments: Int = {
      // If we are in the same vehicle and if we are removing nodes to put them later,
      // the route length before the insertion point has been shortened.
      // We need to take it into account to insert these segment properly.
      if (sameVehicle && afterPosExp.position >= fromIncludedExp.position)
        toIncludedExp.position - fromIncludedExp.position + 1
      else 0
    }

    // Inserts the removed segment in the target vehicle
    val toSegmentsAfterMove: VehicleSegments =
      if (flip) {
        toImpactedSegments.insertSegments(
          segmentsToMove.map(_.flip()).reverse,
          afterPosExp,
          targetVehiclePos,
          lengthOfMovedSegments
        )
      } else {
        toImpactedSegments.insertSegments(
          segmentsToMove,
          afterPosExp,
          targetVehiclePos,
          lengthOfMovedSegments
        )
      }

    if (sameVehicle) (toSegmentsAfterMove, toSegmentsAfterMove)
    else (fromSegmentsAfterMove, toSegmentsAfterMove)

  }
}

/** Class wrapping a list of [[Segment]] and methods to update that list.
  *
  * @param segments
  *   The list of subsequences on which we can reuse precomputation.
  * @param v
  *   The number of vehicle in the associated route.
  */
class VehicleSegments(val segments: List[Segment], v: Int) {

  /** Finds the impacted segment by the previous update. The segment is found if the endNode is
    * after or equal to the search position.
    *
    * @param searchedPos
    *   The searched position.
    * @param impactedVehiclePos
    *   The position of the vehicle impacted by the change.
    * @param segmentsToExplore
    *   The list of segment to explore to find the impacted one.
    * @return
    *   A tuple containing:
    *   1. The impacted segment;
    *   1. The list of explored segments;
    *   1. The segment that were not explored to find the impacted one;
    *   1. The position of the start of the impacted segment.
    */
  private def findImpactedSegment(
    searchedPos: Int,
    impactedVehiclePos: Int,
    segmentsToExplore: List[Segment] = segments
  ): (Segment, List[Segment], List[Segment], Int) = {

    /** Checks if the first segment of the remaining segments to explore contains the searched
      * position. If yes, returns
      *   1. The impacted segment;
      *   1. The list of explored segments;
      *   1. The segment that were not explored to find the impacted one;
      *   1. The position of the start of the impacted segment.
      *
      * Else, explore the next segment in the list.
      */
    @tailrec
    def findSegmentInternal(
      segmentsToExplore: List[Segment],
      currentSegmentStartPosition: Int = impactedVehiclePos,
      exploredSegment: List[Segment] = List.empty
    ): (Segment, List[Segment], List[Segment], Int) = {
      require(
        segmentsToExplore.nonEmpty,
        "Should not happen. It means that the desired position is no within the vehicle route"
      )
      val currentSegment      = segmentsToExplore.head
      val nextSegmentStartPos = currentSegmentStartPosition + currentSegment.length()
      if (nextSegmentStartPos > searchedPos) {
        // If the next segment start after the searched position,
        // the current segment contains this position
        (
          currentSegment,
          if (exploredSegment.nonEmpty) exploredSegment.reverse else exploredSegment,
          segmentsToExplore.tail,
          currentSegmentStartPosition
        )
      } else {
        // Else, we need to explore the next segment
        findSegmentInternal(
          segmentsToExplore.tail,
          nextSegmentStartPos,
          currentSegment :: exploredSegment
        )
      }
    }

    findSegmentInternal(segmentsToExplore)
  }

  /** Remove Segments and SubSegment from the `segment` list.
    *
    * Finds the [[Segment]] holding the specified positions and splits them in two part right after
    * these positions. If a position is at the end of a Segment, this Segment is not split. The
    * in-between segments are gathered as well.
    *
    * @param fromExp
    *   The explorer containing `from` position.
    * @param toExp
    *   The explorer containing `to` position.
    * @param impactedVehiclePos
    *   The position of the impacted vehicle in the associated sequence.
    * @return
    *   1. A VehicleSegments with an updated list of segments.
    *   1. The list af the removed subsegments.
    */
  private def removeSubSegments(
    fromExp: IntSequenceExplorer,
    toExp: IntSequenceExplorer,
    impactedVehiclePos: Int
  ): (VehicleSegments, List[Segment]) = {

    // Finds the segment containing from position
    val (
      fromImpactedSegment,
      segmentsBeforeFromImpactedSegment,
      segmentsAfterFromImpactedSegment,
      startPositionOfFromImpactedSegment
    ) =
      findImpactedSegment(fromExp.position, impactedVehiclePos)

    // Finds the segment containing to position
    val (
      toImpactedSegment,
      segmentsBetweenFromAndToFromImpactedIncluded,
      segmentsAfterToImpactedSegment,
      startPositionOfToImpactedSegment
    ) =
      findImpactedSegment(
        toExp.position,
        startPositionOfFromImpactedSegment,
        // to position can be in the same segment that from
        fromImpactedSegment :: segmentsAfterFromImpactedSegment
      )

    // The last call to findImpactedSegment include the fromImpactedSegment in the explored segments.
    // We don't need this duplicate so we remove it.
    val segmentsBetweenFromAndTo: List[Segment] =
      if (segmentsBetweenFromAndToFromImpactedIncluded.nonEmpty)
        segmentsBetweenFromAndToFromImpactedIncluded.tail
      else List.empty

    // nodeBeforeFrom is always defined because it's at worst a vehicle node
    val nodeBeforeFrom = fromExp.prev.value
    val nodeAfterTo = toExp.next match {
      case _: RootIntSequenceExplorer => -1
      case e: IntSequenceExplorer     => e.value
    }

    // We split the fromImpactedSegment in two part fromLeftResidue and toLeftResidue
    val fromLeftResidueLength =
      fromExp.position - startPositionOfFromImpactedSegment // From is not a part of the left residue
    val fromRightResidueLength = fromImpactedSegment.length() - fromLeftResidueLength
    val (fromLeftResidue, fromRightResidue): (Option[Segment], Option[Segment]) =
      fromImpactedSegment.splitAtNode(
        nodeBeforeFrom,
        fromExp.value,
        fromLeftResidueLength,
        fromRightResidueLength
      )

    val (removedSegments, toRightResidue): (List[Segment], Option[Segment]) = {
      assert(fromRightResidue.isDefined, "No residue at right of from. Should not happen here")
      val toLeftResidueLength =
        toExp.position - startPositionOfToImpactedSegment + 1 // to is part of the left residue
      val toRightResidueLength = toImpactedSegment.length() - toLeftResidueLength

      if (fromImpactedSegment == toImpactedSegment) {
        // Same segment => We split the fromRightResidue into 2 parts: removedSubSegment and toRightResidue
        if (nodeAfterTo < v) // to is at the end of the vehicle route
          (List(fromRightResidue.get), None)
        else {

          val removedSubSegmentLength = fromRightResidueLength - toRightResidueLength

          val (removedSegment, rightResidue) = fromRightResidue.get.splitAtNode(
            toExp.value,
            nodeAfterTo,
            removedSubSegmentLength,
            toRightResidueLength
          )
          (List(removedSegment.get), rightResidue)
        }
      } else {
        var removedSeg: List[Segment] = fromRightResidue.get :: segmentsBetweenFromAndTo

        // We split the toImpactedSegment into 2 parts: toLeftResidue and toRightResidue
        val (toLeftResidue, toRightResidue): (Option[Segment], Option[Segment]) =
          if (nodeAfterTo < v) (Some(toImpactedSegment), None)
          else {
            toImpactedSegment.splitAtNode(
              toExp.value,
              nodeAfterTo,
              toLeftResidueLength,
              toRightResidueLength
            )
          }

        assert(toLeftResidue.isDefined, "No residue at left of to. Should not happen here")
        removedSeg = removedSeg ::: List(toLeftResidue.get)
        (removedSeg, toRightResidue)
      }
    }

    // Building the updated list of segments starting from then end

    // Segments after the impacted segments
    var newSegments: List[Segment] = segmentsAfterToImpactedSegment
    // Right residues of to position
    if (toRightResidue.isDefined) newSegments = toRightResidue.get :: newSegments
    // Left residues of from position
    if (fromLeftResidue.isDefined) newSegments = fromLeftResidue.get :: newSegments
    // Segments before impacted segments
    newSegments = segmentsBeforeFromImpactedSegment ::: newSegments

    (VehicleSegments(newSegments, v), removedSegments)
  }

  /** Removes a nodes from the list of segment.
    *
    * @param toRemoveExp
    *   The explorer containing the position of the node to remove.
    * @param impactedVehiclePos
    *   The position of the impacted vehicle in the associated sequence.
    * @return
    *   1. A VehicleSegments with an updated list of segments.
    *   1. The removed node as a list.
    */
  def removeNode(
    toRemoveExp: IntSequenceExplorer,
    impactedVehiclePos: Int
  ): (VehicleSegments, List[Segment]) = {
    removeSubSegments(toRemoveExp, toRemoveExp, impactedVehiclePos)
  }

  /** Inserts a list of segments after the given position.
    *
    * @param segmentsToInserts
    *   The segments to insert.
    * @param afterPosExp
    *   The explorer containing the position after which insert the segments.
    * @param impactedVehiclePos
    *   The position of the impacted vehicle in the associated sequence.
    * @param lengthOfMovedSegments
    *   The length of the segments to insert. This parameter is only useful when we are moving
    *   segments to a later position in the current vehicle's route.
    * @return
    *   A VehicleSegments with an updated list of segments.
    */
  private def insertSegments(
    segmentsToInserts: List[Segment],
    afterPosExp: IntSequenceExplorer,
    impactedVehiclePos: Int,
    lengthOfMovedSegments: Int = 0
  ): VehicleSegments = {

    if (segments.isEmpty) VehicleSegments(segmentsToInserts, v)
    else if (afterPosExp.position - lengthOfMovedSegments == impactedVehiclePos - 1)
      VehicleSegments(segmentsToInserts ::: segments, v)
    else {
      // Finds the segment impacted by the insert
      val (
        impactedSegment,
        segmentsBeforeImpactedSegment,
        segmentsAfterImpactedSegment,
        startPosOfImpactedSegment
      ) =
        findImpactedSegment(afterPosExp.position - lengthOfMovedSegments, impactedVehiclePos)

      val insertAfterNode = afterPosExp.value // the node after which make the insertion
      val insertBeforeNode = afterPosExp.next match { // the node before which make the insertion
        case _: RootIntSequenceExplorer => -1
        case e: IntSequenceExplorer     => e.value
      }

      // We split the impacted segment into 2 parts: leftResidue and rightResidue
      val (leftResidue, rightResidue): (Option[Segment], Option[Segment]) = {
        if (insertBeforeNode < v) (Some(impactedSegment), None)
        else {
          val leftResidueLength =
            afterPosExp.position - lengthOfMovedSegments - startPosOfImpactedSegment + 1
          val rightResidueLength = impactedSegment.length() - leftResidueLength
          impactedSegment.splitAtNode(
            insertAfterNode,
            insertBeforeNode,
            leftResidueLength,
            rightResidueLength
          )
        }
      }

      // Building the updated list of segments from the end

      // Segments after the impacted segment
      var newSegments: List[Segment] = segmentsAfterImpactedSegment
      // Right residue of the impacted segment
      if (rightResidue.isDefined) newSegments = rightResidue.get :: newSegments
      // Segments to insert
      newSegments = segmentsToInserts ::: newSegments
      // Left residue of the impacted segment
      if (leftResidue.isDefined) newSegments = leftResidue.get :: newSegments
      // Segments before the impacted one
      newSegments = segmentsBeforeImpactedSegment ::: newSegments

      VehicleSegments(newSegments, v)
    }
  }

  /** Insert a new node in the list of segments.
    *
    * @param insertedNode
    *   The node to insert.
    * @param afterPosExp
    *   The explorer containing the position after which insert the segments.
    * @param impactedVehiclePos
    *   The position of the impacted vehicle in the associated sequence.
    * @return
    *   A VehicleSegments with an updated list of segments.
    */
  def insertNode(
    insertedNode: Int,
    afterPosExp: IntSequenceExplorer,
    impactedVehiclePos: Int
  ): VehicleSegments = {
    val node = NewNode(insertedNode)
    insertSegments(List(node), afterPosExp, impactedVehiclePos)
  }

  override def toString: String =
    s"""
       |Segments of vehicle:
       |\t${segments.mkString("\n\t")}
       |""".stripMargin
}
