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

package oscar.cbls.lib.invariant.routing

import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.computation.seq._
import oscar.cbls.core.computation.{Invariant, Store}
import oscar.cbls.modeling.routing.VRS

/** Companion object of the [[RoutingConventionConstraint]] class. */
object RoutingConventionConstraint {

  /** Creates a RoutingConventionConstraint invariant.
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param vrs
    *   The routing structure on which the routing constraints must be respected.
    */
  def apply(model: Store, vrs: VRS): RoutingConventionConstraint =
    new RoutingConventionConstraint(model, vrs)

}

/** An invariant that checks if the routing conventions are respected
  *
  * In OscaR.cbls, the vehicle routes of a routing problem are represented by an IntSequence. This
  * representation follows the following conventions:
  *   - If the problem as `n` nodes and `v` vehicles, the nodes are identified from `0` to `n - 1`
  *     and the vehicles are identified from `0` to `v - 1`;
  *   - Each id can only appear once in the IntSequence;
  *   - The id of all the vehicle must always be in the IntSequence;
  *   - The id of all the vehicle must always be ordered from `0` to `v - 1`
  *   - The route of a vehicle starts at its id and ends at the next vehicle id.
  *
  * If the following conventions are not respected, the invariant raises an exception
  *
  * This class is automatically called by setting 'debug' parameter of class VRS at 'true'
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param vrs
  *   The routing structure on which the routing constraints must be respected.
  */
class RoutingConventionConstraint(model: Store, vrs: VRS)
    extends Invariant(model, Some("Routing convention"))
    with SeqNotificationTarget {

  private[this] var routedNodes: Array[Boolean] =
    Array.tabulate(vrs.n)(node => vrs.isRouted(node))
  // Used to check if a neighborhood do not lose the level 0 checkpoint.
  private[this] var checkpointAtLevel0: Option[IntSequence] = None
  private[this] var lastNotified: IntSequence               = vrs.routes.value()

  vrs.routes.registerStaticallyAndDynamicallyListeningElement(this)
  require(
    vrs.routes.pendingValue.unorderedContentNoDuplicate.count(_ < vrs.v) == vrs.v,
    s"The current route has not the expected number (${vrs.v}) of vehicles!"
  )

  require(
    !vrs.routes.pendingValue.unorderedContentNoDuplicate.exists(_ >= vrs.n),
    s"The current route has node bigger than n (${vrs.n})!"
  )
  checkVehicleOrder(vrs.routes.value())
  checkNoDuplicate(vrs.routes.value())

  /** Checks if, in the given sequence, the vehicles are positioned in increasing order. */
  def checkVehicleOrder(seq: IntSequence): Unit = {
    val positionOfVehicles = (0 until vrs.v)
      .map(vId => {
        require(
          seq.positionOfAnyOccurrence(vId).isDefined,
          s"""
             |The vehicles' depots are not all in the route!
             |Vehicle $vId is not inserted
           """.stripMargin
        )
        seq.positionOfAnyOccurrence(vId).get
      })
      .toArray
    for (vehicle <- 0 until vrs.v - 1) {
      require(
        positionOfVehicles(vehicle) < positionOfVehicles(vehicle + 1),
        s"""
           |The vehicles' depots are not sorted properly!
           |Their position should be strictly increasing.
           |Got:
           |""".stripMargin +
          positionOfVehicles.zipWithIndex
            .map(x => s"\tVehicle: ${x._2} - Position: ${x._1}")
            .mkString("\n")
      )
    }
  }

  override def notifySeqChanges(
    seqVariable: SeqVariable,
    contextualVarIndex: Int,
    changes: SeqUpdate
  ): Unit = {
    require(
      vrs.routes.value() equals lastNotified,
      "The last notified value is not equal to the route original value"
    )
    digestUpdate(changes)
    lastNotified = changes.newValue
  }

  override def checkInternals(): Unit = {}

  private[this] def checkRequirement(
    requirement: Boolean,
    errorMsg: String,
    prev: SeqUpdate
  ): Unit = {
    require(
      requirement,
      s"""
         |$errorMsg
         |Previous movements: $prev
         |""".stripMargin
    )
  }

  /** Checks if the given sequence has no duplicated values */
  private[this] def checkNoDuplicate(seq: IntSequence): Unit = {
    val nbOccurrence = seq.unorderedContentNoDuplicateWithNBOccurrences
    for ((_, nb) <- nbOccurrence) {
      require(
        nb == 1,
        s"""
           |Some nodes are duplicated!
           |Given sequence's occurrences:
           |$nbOccurrence
           |""".stripMargin
      )
    }
  }

  private[this] def digestUpdate(changes: SeqUpdate): Unit = {
    changes match {
      case SeqUpdateDefineCheckpoint(prev: SeqUpdate, checkpointLevel: Int) =>
        digestUpdate(prev)
        if (checkpointLevel == 0) checkpointAtLevel0 = Some(changes.newValue)

      case SeqUpdateInsert(value: Int, explorer: IntSequenceExplorer, prev: SeqUpdate) =>
        digestUpdate(prev)
        val errorMsg: String =
          s"""
             |Got:
             |\tInsert value: $value
             |\tInsert pos: ${explorer.position}
             |""".stripMargin
        checkRequirement(value >= vrs.v, s"Trying to insert a vehicle! $errorMsg", prev)
        checkRequirement(
          value < vrs.n,
          s"Trying to insert a node outside the domain! $errorMsg",
          prev
        )
        checkRequirement(!routedNodes(value), s"Node already inserted! $errorMsg", prev)

        routedNodes(value) = true
      case SeqUpdateRemove(pos: IntSequenceExplorer, prev: SeqUpdate) =>
        digestUpdate(prev)
        val removedValue = pos.value
        val errorMsg: String =
          s"""
             |Got:
             |\tRemove pos: $pos
             |""".stripMargin
        checkRequirement(removedValue >= vrs.v, s"Trying to remove a vehicle! $errorMsg", prev)

        routedNodes(removedValue) = false

      case sum @ SeqUpdateMove(fromPos, toPos, afterPose, _, prev) =>
        digestUpdate(prev)
        val errorMsg =
          s"""
             |Got:
             |From position: ${fromPos.position}
             |To position: ${toPos.position}
             |After position: ${afterPose.position}
             |""".stripMargin
        checkRequirement(
          !sum.movedValues.exists(_ < vrs.v),
          s"Trying to move a vehicle! $errorMsg",
          prev
        )

      case SeqUpdateRollBackToTopCheckpoint(
            checkpoint: IntSequence,
            howToRollback: SeqUpdate,
            checkpointLevel: Int,
            _
          ) =>
        if (checkpointLevel == 0) {
          require(
            checkpointAtLevel0.nonEmpty,
            "Trying to rollback to level 0 checkpoint but no one was saved"
          )
          require(
            checkpoint sameIdentity checkpointAtLevel0.get,
            "Trying to roll back to a level 0 checkpoint which is not the same that the saved one"
          )
        }
        rollbackUpdate(howToRollback)

      case SeqUpdateLastNotified(value: IntSequence) =>
        require(value equals lastNotified, "The last notified value is not the saved one.")
        require(
          value sameIdentity vrs.routes.value(),
          "The last notified value is not equal to the route original value"
        )

      case SeqUpdateReleaseTopCheckpoint(prev: SeqUpdate, _) => digestUpdate(prev)

      case SeqUpdateAssign(newSeq: IntSequence) =>
        val errorMsg =
          s"""
             |Got:
             |\tNew Seq: $newSeq
             |""".stripMargin
        require(
          newSeq.unorderedContentNoDuplicate.count(_ < vrs.v) == vrs.v,
          s"The assigned sequence has not the expected number (${vrs.v}) of vehicles! $errorMsg"
        )

        require(
          !newSeq.unorderedContentNoDuplicate.exists(_ >= vrs.n),
          s"The assigned sequence has node bigger than ${vrs.n}! $errorMsg"
        )
        checkVehicleOrder(newSeq)
        checkNoDuplicate(newSeq)
        routedNodes = Array.tabulate(vrs.n)(newSeq.contains)
    }
  }

  private[this] def rollbackUpdate(howToRollBack: SeqUpdate): Unit = {
    howToRollBack match {
      case SeqUpdateInsert(value: Int, _, prev: SeqUpdate) =>
        rollbackUpdate(prev)
        routedNodes(value) = true
      case SeqUpdateRemove(pos: IntSequenceExplorer, prev: SeqUpdate) =>
        rollbackUpdate(prev)
        val removedValue = pos.value
        routedNodes(removedValue) = false
      case sum: SeqUpdateMove                        => rollbackUpdate(sum.prev)
      case _: SeqUpdateLastNotified                  =>
      case surtc: SeqUpdateReleaseTopCheckpoint      => rollbackUpdate(surtc.prev)
      case surbttc: SeqUpdateRollBackToTopCheckpoint => rollbackUpdate(surbttc.howToRollBack)
      case x: SeqUpdate => require(requirement = false, s"Unexpected rollback: $x")
    }
  }

}
