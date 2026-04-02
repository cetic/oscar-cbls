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

import oscar.cbls.algo.magicArray.MagicBoolArray
import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.computation.Invariant
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.seq._
import oscar.cbls.modeling.routing.{StackedVehicleSearcher, VRS}

import scala.annotation.tailrec
import scala.collection.immutable.HashSet

/** Companion object of the [[NodePrecedence]] class. */
object NodePrecedence {

  /** Creates an invariant that maintains the number of violated precedences for all the routes in
    * the VRS.
    *
    * @param vrs
    *   The object that represents the Vehicle Routing Problem.
    * @param precedences
    *   An array of pairs describing the precedences.
    * @param name
    *   The (optional) name of the Invariant.
    */
  def apply(
    vrs: VRS,
    precedences: Array[(Int, Int)],
    name: String = "Precedence"
  ): NodePrecedence = {
    val output = IntVariable(vrs.store, 0L)
    new NodePrecedence(vrs, precedences, output, if (name == "") None else Some(name))
  }

}

/** Invariant that maintains the number of violated precedences for all the routes in the VRS.
  *
  * Given a list of pair `(beforeNode, afterNode)`, the precedence constraint is violated if:
  *   - `beforeNode` and `afterNode` are routed but `beforeNode` is positioned after `afterNode` ;
  *   - Or, `beforeNode` and `afterNode` are not on the same vehicle ;
  *   - Or, only one of these values is routed.
  *
  * Otherwise, the constraint is respected.
  *
  * @param vrs
  *   The object that represents the Vehicle Routing Problem.
  * @param precedences
  *   An array of pairs describing the precedences.
  * @param output
  *   Variable maintaining the number of violated precedences.
  * @param name
  *   The (optional) name of the Invariant.
  */
class NodePrecedence(
  vrs: VRS,
  precedences: Array[(Int, Int)],
  output: IntVariable,
  name: Option[String]
) extends Invariant(vrs.store, name)
    with SeqNotificationTarget {

  vrs.routes.registerStaticallyAndDynamicallyListeningElement(this)
  output.setDefiningInvariant(this)

  /** For each node `i`, returns the id's of the precedences where `i` is the `beforeNode`. */
  private[this] val beforeToPrecedences: Array[List[Int]] = Array.fill(vrs.n)(Nil)

  /** For each node `i`, returns the id's of the precedences where `i` is the `afterNode`. */
  private[this] val afterToPrecedences: Array[List[Int]] = Array.fill(vrs.n)(Nil)

  for (precedenceId: Int <- precedences.indices) {
    val (from, to): (Int, Int) = precedences(precedenceId)
    beforeToPrecedences(from) = precedenceId :: beforeToPrecedences(from)
    afterToPrecedences(to) = precedenceId :: afterToPrecedences(to)
  }

  /** For each precedence id `i`, returns if the precedence `i` is currently violated or not. */
  private[this] val isPrecedenceViolated: MagicBoolArray = MagicBoolArray(precedences.length)

  /** Contains the id's of the violated precedences. */
  private[this] var _violatedPrecedences: HashSet[Int] = HashSet.empty

  /** Stack containing data used to restore the invariant when rolling back to checkpoints. */
  private[this] val savedDataAtCheckpoints: SeqValueStack[SavedDataAtCheckpoint] =
    new SeqValueStack[SavedDataAtCheckpoint]()

  private[this] var vehicleSearcher: StackedVehicleSearcher =
    StackedVehicleSearcher(vrs.routes.value(), vrs.v)

  computeAndAffectViolationFromScratch(vrs.routes.value())

  /** Returns the number of violated precedences as an IntVariable. */
  def apply(): IntVariable = output

  /** Returns the violated precedences. */
  def violatedPrecedences: HashSet[(Int, Int)] =
    _violatedPrecedences.map(id => precedences(id))

  override def notifySeqChanges(
    seqVariable: SeqVariable,
    contextualVarIndex: Int,
    changes: SeqUpdate
  ): Unit = digestUpdates(changes)

  override def checkInternals(): Unit = {
    var expected         = 0
    val seq: IntSequence = vrs.routes.pendingValue
    val stateDescription: String =
      s"""|Route: $seq
          |Violated precedences: $violatedPrecedences
          |Precedences: ${precedences.mkString(", ")}
          |""".stripMargin

    for (precedenceId: Int <- precedences.indices) {
      val (before, after): (Int, Int) = precedences(precedenceId)
      if (checkPrecedenceViolation(seq, before, after)) {
        expected += 1
        require(
          isPrecedenceViolated(precedenceId),
          s"""${this.getClass.getSimpleName} failed.
             |Precedence $precedenceId: ($before, $after) should be considered violated.
             |""".stripMargin + stateDescription
        )
      } else {
        require(
          !isPrecedenceViolated(precedenceId),
          s"""${this.getClass.getSimpleName} failed.
             |Precedence $precedenceId: ($before, $after) should not be considered violated.
             |""".stripMargin + stateDescription
        )
      }
    }

    require(
      expected == output.pendingValue,
      s"""${this.getClass.getSimpleName} failed.
         |Expected number of violated precedences: $expected
         |Got: ${output.pendingValue}
         |""".stripMargin + stateDescription
    )

  }

  /** Checks if the given precedence is violated in the given route.
    *
    * @param seq
    *   The current route.
    * @param before
    *   The before node of the precedence.
    * @param after
    *   The after node of the precedence.
    * @return
    *   Whether the input precedence is violated.
    */
  private[this] def checkPrecedenceViolation(seq: IntSequence, before: Int, after: Int): Boolean = {
    seq.positionOfAnyOccurrence(before) match {
      case Some(beforePos) =>
        seq.positionOfAnyOccurrence(after) match {
          case Some(afterPos) =>
            if (beforePos > afterPos) true
            else {
              val vehicleOfBefore = vehicleSearcher.vehicleReachingPosition(beforePos)
              val vehicleOfAfter  = vehicleSearcher.vehicleReachingPosition(afterPos)
              if (vehicleOfBefore != vehicleOfAfter) true
              else false
            }
          case None => true // Only one part of the precedence is routed
        }
      case None =>
        seq.positionOfAnyOccurrence(after) match {
          case None    => false // The whole precedence is unrouted
          case Some(_) => true  // Only one part of the precedence is routed
        }
    }
  }

  /** Given a route, finds the violated precedence and updates the concerned variables. */
  private[this] def computeAndAffectViolationFromScratch(seq: IntSequence): Unit = {
    var total: Int = 0

    isPrecedenceViolated.all = false
    _violatedPrecedences = HashSet.empty

    for (id <- precedences.indices) {
      val (before, after) = precedences(id)
      if (checkPrecedenceViolation(seq, before, after)) {
        total += 1
        isPrecedenceViolated(id) = true
        _violatedPrecedences += id
      }
    }
    output := total
  }

  /** Used to update the output variables after an insert or a move.
    *
    * Each precedence is of the form `(beforeNode, afterNode)`. When this method is called, we know
    * one of these two nodes, called `knownNode`. According to the value of the `knownNodeIsBefore`
    * before, `knownNode` is considered as the `beforeNode` or the `afterNode` is the precedences to
    * check.
    *
    * @param precedencesToCheck
    *   List of the precedences' ids to check.
    * @param seq
    *   The new value of the route.
    * @param knownPos
    *   The position of the `knownNode` in the new value of the route.
    * @param targetVehicle
    *   The vehicle in which the new node or the moved segment is inserted.
    * @param knownNodeIsBefore
    *   Given a precedence `(beforeNode, afterNode)` is the `knownNode` is considered as the
    *   `beforeNode` or the `afterNode`.
    */
  @tailrec
  private[this] def updateViolationForInsertOrMove(
    precedencesToCheck: List[Int],
    seq: IntSequence,
    knownPos: Int,
    targetVehicle: Int,
    knownNodeIsBefore: Boolean
  ): Unit = {

    val expectedOrder: (Int, Int) => Boolean = if (knownNodeIsBefore) _ < _ else _ > _
    precedencesToCheck match {
      case Nil => ;
      case precedenceId :: tail =>
        val otherValueOfPrecedence: Int =
          if (knownNodeIsBefore) precedences(precedenceId)._2 else precedences(precedenceId)._1

        seq.positionOfAnyOccurrence(otherValueOfPrecedence) match {
          case None =>
            // Only one node of the precedence is routed. The precedence is violated
            if (!isPrecedenceViolated(precedenceId)) {
              output :+= 1
              isPrecedenceViolated(precedenceId) = true
              _violatedPrecedences += precedenceId
            }
          case Some(otherPos) =>
            if (!isPrecedenceViolated(precedenceId)) {
              // The precedence was not violated.
              // It becomes violated if the nodes are badly placed or not in the same vehicle.
              if (
                !expectedOrder(knownPos, otherPos)
                || targetVehicle != vehicleSearcher.vehicleReachingPosition(otherPos)
              ) {
                output :+= 1
                isPrecedenceViolated(precedenceId) = true
                _violatedPrecedences += precedenceId
              }
            } else {
              // The precedence was violated.
              // It is no more violated if the nodes are correctly placed and in the same vehicle.
              if (
                expectedOrder(knownPos, otherPos)
                && targetVehicle == vehicleSearcher.vehicleReachingPosition(otherPos)
              ) {
                output :-= 1
                isPrecedenceViolated(precedenceId) = false
                _violatedPrecedences -= precedenceId
              }
            }
        }
        updateViolationForInsertOrMove(tail, seq, knownPos, targetVehicle, knownNodeIsBefore)
    }
  }

  /** Used to update the output variables after a remove.
    *
    * Each precedence is of the form `(beforeNode, afterNode)`. When this method is called, we know
    * one of these two nodes, called `knownNode`. According to the value of the `knownNodeIsBefore`
    * before, `knownNode` is considered as the `beforeNode` or the `afterNode` is the precedences to
    * check.
    *
    * @param precedencesToCheck
    *   List of the precedences' ids to check.
    * @param seq
    *   The new value of the route.
    * @param knownNodeIsBefore
    *   Given a precedence `(beforeNode, afterNode)` is the `knownNode` is considered as the
    *   `beforeNode` or the `afterNode`.
    */
  @tailrec
  private[this] def updateViolationForRemove(
    precedencesToCheck: List[Int],
    seq: IntSequence,
    knownNodeIsBefore: Boolean
  ): Unit = {

    precedencesToCheck match {
      case Nil => ;
      case precedenceId :: tail =>
        val otherNode: Int =
          if (knownNodeIsBefore) precedences(precedenceId)._2 else precedences(precedenceId)._1
        if (!isPrecedenceViolated(precedenceId)) {
          // We are removing a routed node and the precedence is not violated.
          // The other node is still routed. The precedence becomes violated.
          isPrecedenceViolated(precedenceId) = true
          output :+= 1
          _violatedPrecedences += precedenceId
        } else if (seq.positionOfAnyOccurrence(otherNode).isEmpty) {
          // The precedence is violated because only one node is routed.
          // We are removing the remaining node. The precedence is no more violated.
          isPrecedenceViolated(precedenceId) = false
          output :-= 1
          _violatedPrecedences -= precedenceId
        }
        updateViolationForRemove(tail, seq, knownNodeIsBefore)
    }

  }

  private[this] def digestUpdates(changes: SeqUpdate): Unit = {
    changes match {
      case _: SeqUpdateLastNotified => ;

      case SeqUpdateAssign(newValue) =>
        vehicleSearcher = StackedVehicleSearcher(newValue, vrs.v)
        computeAndAffectViolationFromScratch(newValue)

      case insertUpdate @ SeqUpdateInsert(
            toInsert: Int,
            insertAfterPosExp: IntSequenceExplorer,
            prev: SeqUpdate
          ) =>
        digestUpdates(prev)

        val targetVehicle = vehicleSearcher.vehicleReachingPosition(insertAfterPosExp.position)
        vehicleSearcher = vehicleSearcher.push(insertUpdate.oldPosToNewPos)

        updateViolationForInsertOrMove(
          beforeToPrecedences(toInsert),
          insertUpdate.newValue,
          insertAfterPosExp.position + 1,
          targetVehicle,
          knownNodeIsBefore = true
        )

        updateViolationForInsertOrMove(
          afterToPrecedences(toInsert),
          insertUpdate.newValue,
          insertAfterPosExp.position + 1,
          targetVehicle,
          knownNodeIsBefore = false
        )

      case removeUpdate @ SeqUpdateRemove(toRemoveExp: IntSequenceExplorer, prev: SeqUpdate) =>
        digestUpdates(prev)

        vehicleSearcher = vehicleSearcher.push(removeUpdate.oldPosToNewPos)
        val removedValue = toRemoveExp.value
        updateViolationForRemove(
          beforeToPrecedences(removedValue),
          removeUpdate.newValue,
          knownNodeIsBefore = true
        )
        updateViolationForRemove(
          afterToPrecedences(removedValue),
          removeUpdate.newValue,
          knownNodeIsBefore = false
        )

      case moveUpdate @ SeqUpdateMove(
            fromPosExp: IntSequenceExplorer,
            _: IntSequenceExplorer,
            afterPosExp: IntSequenceExplorer,
            flip: Boolean,
            prev: SeqUpdate
          ) =>
        digestUpdates(prev)

        val targetVehicle: Int = vehicleSearcher.vehicleReachingPosition(afterPosExp.position)
        vehicleSearcher = vehicleSearcher.push(moveUpdate.oldPosToNewPos)
        val movedNodes = moveUpdate.movedValues

        var knownPos: Int =
          if (afterPosExp.position <= fromPosExp.position) afterPosExp.position // Moving downward
          else afterPosExp.position - movedNodes.length                         // Moving upward

        if (flip)
          knownPos += movedNodes.length + 1 // If flipped, we will iterate backwardly on the moved nodes

        for (movedNode <- movedNodes) {
          if (flip) knownPos -= 1 // Backward iteration
          else knownPos += 1      // Forward iteration

          updateViolationForInsertOrMove(
            beforeToPrecedences(movedNode),
            moveUpdate.newValue,
            knownPos,
            targetVehicle,
            knownNodeIsBefore = true
          )

          updateViolationForInsertOrMove(
            afterToPrecedences(movedNode),
            moveUpdate.newValue,
            knownPos,
            targetVehicle,
            knownNodeIsBefore = false
          )
        }

      case defineCheckpointUpdate @ SeqUpdateDefineCheckpoint(
            prev: SeqUpdate,
            checkpointLevel: Int
          ) =>
        digestUpdates(prev)
        // Saves the current number of violated precedences and their id's
        savedDataAtCheckpoints.pushToLevel(
          defineCheckpointUpdate.newValue,
          checkpointLevel,
          SavedDataAtCheckpoint(output.pendingValue, _violatedPrecedences)
        )

        vehicleSearcher = vehicleSearcher.defineCheckpoint()

      case SeqUpdateRollBackToTopCheckpoint(
            checkpoint: IntSequence,
            howToRollback: SeqUpdate,
            level: Int,
            _
          ) =>
        rollbackUpdates(howToRollback)
        assert(
          savedDataAtCheckpoints.stackLevel == level,
          s"Checkpoint levels are not coherent (sequence level: $level != this invariant " +
            s"level: ${savedDataAtCheckpoints.stackLevel})"
        )

        val savedData = savedDataAtCheckpoints.topValue(checkpoint)
        // Restores the violations
        isPrecedenceViolated.all = false
        for (precedenceId: Int <- savedData.violatedPrecedences)
          isPrecedenceViolated(precedenceId) = true

        output := savedData.violationAtCheckpoint
        this._violatedPrecedences = savedData.violatedPrecedences
        vehicleSearcher = vehicleSearcher.rollbackToTopCheckpoint()

      case releaseUpdate: SeqUpdateReleaseTopCheckpoint =>
        digestUpdates(releaseUpdate.prev)
        savedDataAtCheckpoints.pop()
        vehicleSearcher = vehicleSearcher.releaseTopCheckpoint()

      case x: SeqUpdate => throw new IllegalArgumentException(s"Unexpected update: $x")
    }
  }

  // When rolling back, we only need to digest the "release top checkpoint" to maintains
  // a good stack level and the other rollbacks. Other updates can be ignored.
  private[this] def rollbackUpdates(howToRollback: SeqUpdate): Unit = {
    howToRollback match {
      case release: SeqUpdateReleaseTopCheckpoint =>
        rollbackUpdates(release.prev)
        savedDataAtCheckpoints.pop()
        vehicleSearcher = vehicleSearcher.releaseTopCheckpoint()

      case SeqUpdateRollBackToTopCheckpoint(
            checkpoint: IntSequence,
            howToRollback: SeqUpdate,
            level: Int,
            _
          ) =>
        rollbackUpdates(howToRollback)
        assert(
          savedDataAtCheckpoints.stackLevel == level,
          s"Checkpoint levels are not coherent (sequence level: $level != this " +
            s"invariant level: ${savedDataAtCheckpoints.stackLevel})"
        )
        savedDataAtCheckpoints.popUntilLevel(checkpoint, level)
        vehicleSearcher = vehicleSearcher.rollbackToTopCheckpoint()

      case _: SeqUpdateLastNotified =>
      case x: SeqUpdateWithPrev     => rollbackUpdates(x.prev)
      case x: SeqUpdate => throw new IllegalArgumentException(s"Unexpected rollback: $x")

    }

  }

  /** Little case class to save the number of violated precedences and their id's. */
  private case class SavedDataAtCheckpoint(
    violationAtCheckpoint: Long,
    violatedPrecedences: HashSet[Int]
  )
}
