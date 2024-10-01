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

package oscar.cbls.core.computation.seq

import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.computation.{Invariant, KeyForRemoval, SavedValue, Store, Variable}

/** Companion object of SeqVariable
  */
object SeqVariable {

  /** Creates a new SeqVariable with the given parameters.
    *
    * @param model
    *   The propagation structure to which the element is attached.
    * @param initialValue
    *   The initial value of the SeqVariable.
    * @param name
    *   The (optional) name of the Variable.
    * @param maxPivotPerValuePercent
    *   The maximum number of [[oscar.cbls.algo.sequence.affineFunction.Pivot]] per 100 value in the
    *   IntSequence. When defining a new checkpoint, if this value is exceeded, a regularization is
    *   done.
    * @param isConstant
    *   If the variable is a constant.
    * @return
    *   A new SeqVariable
    */
  def apply(
    model: Store,
    initialValue: List[Int],
    name: String = "SeqVariable",
    maxPivotPerValuePercent: Int = 4,
    isConstant: Boolean = false
  ): SeqVariable = {
    new SeqVariable(model, initialValue, name, maxPivotPerValuePercent, isConstant)
  }

  /** Creates an empty new SeqVariable with the given parameters.
    *
    * @param model
    *   The propagation structure to which the element is attached.
    * @param name
    *   The (optional) name of the Variable.
    * @param maxPivotPerValuePercent
    *   The maximum number of [[oscar.cbls.algo.sequence.affineFunction.Pivot]] per 100 value in the
    *   IntSequence. When defining a new checkpoint, if this value is exceeded, a regularization is
    *   done.
    * @param isConstant
    *   If the variable is a constant.
    * @return
    *   A new SeqVariable
    */
  def empty(
    model: Store,
    name: String = "SeqVariable",
    maxPivotPerValuePercent: Int = 4,
    isConstant: Boolean = false
  ): Unit = {
    new SeqVariable(model, List.empty, name, maxPivotPerValuePercent, isConstant)
  }
}

/** Defines the [[oscar.cbls.core.computation.Variable]] that will encapsulate an
  * [[oscar.cbls.algo.sequence.IntSequence]] so that it can behave as a
  * [[oscar.cbls.core.propagation.PropagationElement]].
  *
  * This class provides the classical modification method (insert,move,remove) but not only. There
  * are also several methods allowing a checkpoint/rollback mechanism:
  *   - defineCurrentValueAsCheckpoint: registers the actual value as the top checkpoint,
  *   - rollbackToTopCheckpoint: cancel every modification since the top checkpoint,
  *   - releaseTopCheckpoint: remove the top checkpoint.
  *
  * A classical use of the SeqVariable:
  *   1. Exploration phase:
  *      i. define a new top checkpoint
  *      i. insert a new value
  *      i. propagate and check the result
  *      i. rollback to top checkpoint
  *      i. insert a new value
  *      i. propagate and check the result
  *      i. rollback to top checkpoint
  *      i. ...
  *   1. Commit phase (once you found the move you want):
  *      i. rollback and release all checkpoint
  *      i. do your insert/move/remove
  *
  * @param model
  *   The propagation structure to which the element is attached.
  * @param initialValue
  *   The initial value of the SeqVariable.
  * @param name
  *   The (optional) name of the Variable.
  * @param maxPivotPerValuePercent
  *   The maximum number of [[oscar.cbls.algo.sequence.affineFunction.Pivot]] per 100 value in the
  *   IntSequence. When defining a new checkpoint, if this value is exceeded, a regularization is
  *   done.
  * @param isConstant
  *   If the variable is a constant.
  */
class SeqVariable(
  model: Store,
  initialValue: List[Int],
  name: String,
  maxPivotPerValuePercent: Int,
  isConstant: Boolean
) extends Variable(model, isConstant) {
  override type NotificationTargetType = SeqNotificationTarget
  require(model != null)
  setDomain(Int.MinValue, Int.MaxValue)

  /*
  The actual value of this variable.
   For listening invariants this is the value of the variable until next propagation.
   */
  private var _value: IntSequence = {
    if (initialValue.isEmpty) IntSequence.empty()
    else IntSequence(initialValue)
  }
  // The pending value of this variable, as a stack of modification to be propagated.
  private var toNotify: SeqUpdate = SeqUpdateLastNotified(_value)

  /** Returns the pending value of this SeqVariable, the one that may be not propagated yet. */
  def pendingValue: IntSequence = toNotify.newValue

  def checkpointLevel: Int = levelOfTopCheckpoint

  override def registerDynamicallyListeningElement(target: SeqNotificationTarget, indexToRecallAtNotification: Int): KeyForRemoval[(SeqNotificationTarget, Int)] = {
    doRegisterDynamicallyListeningElement(target,indexToRecallAtNotification)
  }

  override def registerStaticallyAndDynamicallyListeningElement(
    propagationElement: Invariant with SeqNotificationTarget,
    indexToRecallAtNotification: Int
  ): KeyForRemoval[(SeqNotificationTarget, Int)] =
    doRegisterStaticallyAndDynamicallyListeningElement(
      propagationElement,
      indexToRecallAtNotification
    )

  override def name(): String = name

  /** Propagates up to this variable (if needed) and returns the new value of this SeqVariable.
    *
    * If this variable is a decision variable, meaning that no invariant control this variable, no
    * propagation are triggered.
    *
    * @return
    *   The new value of this SeqVariable.
    */
  def value(): IntSequence = {
    val propagating = model.propagating
    if (isADecisionVariable && !propagating) return toNotify.newValue
    if (!propagating) model.propagate(Some(this))
    _value
  }

  // Check if the explorer is on the right IntSequence
  private def checkExplorerForMove(explorer: IntSequenceExplorer, explorerName: String): Unit = {
    require(
      toNotify.newValue.sameIdentity(explorer.intSequence),
      s"$explorerName must explore the current IntSequence." +
        s"\nShould be ${toNotify.newValue} with token : ${toNotify.newValue.token}." +
        s"\nGot${explorer.intSequence} with token : ${explorer.intSequence.token}."
    )
  }

  private def checkConstant(): Unit = require(!isConstant, "Can not modify a constant SeqVariable")

  /** Inserts a value after a given position.
    *
    * @param value
    *   The value to insert.
    * @param insertAfterPositionExplorer
    *   The position after which to insert the value, as an explorer.
    * @param seqAfter
    *   Optionally (if known), the resulting IntSequence.
    */
  def insertAfterPosition(
    value: Int,
    insertAfterPositionExplorer: IntSequenceExplorer,
    seqAfter: Option[IntSequence] = None
  ): Unit = {
    checkConstant()
    checkExplorerForMove(insertAfterPositionExplorer, "InsertAfterPositionExplorer")

    recordPerformedIncrementalUpdate(seqAfter match {
      case Some(sa) => (prev, _) => SeqUpdateInsert(value, insertAfterPositionExplorer, prev, sa)
      case None =>
        (prev, newSeq) =>
          if (newSeq == null) SeqUpdateInsert(value, insertAfterPositionExplorer, prev)
          else SeqUpdateInsert(value, insertAfterPositionExplorer, prev, newSeq)
    })
    scheduleForPropagation()
  }

  /** Removes the value at a given position.
    *
    * @param removePositionExplorer
    *   The position where to remove the value, as an explorer.
    * @param seqAfter
    *   Optionally (if known), the resulting IntSequence.
    */
  def remove(
    removePositionExplorer: IntSequenceExplorer,
    seqAfter: Option[IntSequence] = None
  ): Unit = {
    checkConstant()
    checkExplorerForMove(removePositionExplorer, "RemovePositionExplorer")

    recordPerformedIncrementalUpdate(seqAfter match {
      case Some(sa) => (prev, _) => SeqUpdateRemove(removePositionExplorer, prev, sa)
      case None =>
        (prev, newSeq) =>
          if (newSeq == null) SeqUpdateRemove(removePositionExplorer, prev)
          else SeqUpdateRemove(removePositionExplorer, prev, newSeq)
    })
    scheduleForPropagation()
  }

  /** Moves and flip (or not) the subsequence defined by the two position (included) after the given
    * position.
    *
    * @param fromIncludedPositionExplorer
    *   The start position of the subsequence, as and explorer.
    * @param toIncludedPositionExplorer
    *   The end position of the subsequence, as an explorer.
    * @param afterPositionExplorer
    *   The position after which the subsequence will be moved, as an explorer.
    * @param flip
    *   Whether the subsequence must be flipped or not.
    * @param seqAfter
    *   Optionally (if known), the resulting IntSequence.
    */
  def move(
    fromIncludedPositionExplorer: IntSequenceExplorer,
    toIncludedPositionExplorer: IntSequenceExplorer,
    afterPositionExplorer: IntSequenceExplorer,
    flip: Boolean,
    seqAfter: Option[IntSequence] = None
  ): Unit = {
    checkConstant()
    checkExplorerForMove(fromIncludedPositionExplorer, "FromIncludedPositionExplorer")
    checkExplorerForMove(toIncludedPositionExplorer, "ToIncludedPositionExplorer")
    checkExplorerForMove(afterPositionExplorer, "AfterPositionExplorer")

    val fromPos  = fromIncludedPositionExplorer.position
    val toPos    = toIncludedPositionExplorer.position
    val afterPos = afterPositionExplorer.position

    require(
      fromPos <= toPos,
      s"fromPosition should be before or equals to toPosition. " +
        s"Got from : $fromPos to : $toPos"
    )
    require(
      afterPos < fromPos || afterPos > toPos,
      s"afterPosition should be before fromPosition or after to toPosition. " +
        s"Got from : $fromPos to : $toPos after : $afterPos"
    )

    if (!(afterPos == fromPos - 1 && (fromPos == toPos || !flip))) {
      recordPerformedIncrementalUpdate(seqAfter match {
        case Some(sa) =>
          (prev, _) =>
            SeqUpdateMove(
              fromIncludedPositionExplorer,
              toIncludedPositionExplorer,
              afterPositionExplorer,
              flip,
              prev,
              sa
            )
        case None =>
          (prev, newSeq) =>
            if (newSeq == null)
              SeqUpdateMove(
                fromIncludedPositionExplorer,
                toIncludedPositionExplorer,
                afterPositionExplorer,
                flip,
                prev
              )
            else
              SeqUpdateMove(
                fromIncludedPositionExplorer,
                toIncludedPositionExplorer,
                afterPositionExplorer,
                flip,
                prev,
                newSeq
              )
      })

      scheduleForPropagation()
    }
  }

  /** Flips the subsequence defined by the two position (included).
    *
    * @param fromIncludedPositionExplorer
    *   The start position of the subsequence, as and explorer.
    * @param toIncludedPositionExplorer
    *   The end position of the subsequence, as an explorer.
    * @param seqAfter
    *   Optionally (if known), the resulting IntSequence.
    */
  def flip(
    fromIncludedPositionExplorer: IntSequenceExplorer,
    toIncludedPositionExplorer: IntSequenceExplorer,
    seqAfter: Option[IntSequence] = None
  ): Unit = {
    checkConstant()
    move(
      fromIncludedPositionExplorer,
      toIncludedPositionExplorer,
      fromIncludedPositionExplorer.prev,
      flip = true,
      seqAfter
    )
  }

  /** Swaps and flip (or not) two segments of the sequence defined by their two included positions.
    *
    * @param firstSegmentStartPositionExplorer
    *   The start position of the first segment, as and explorer.
    * @param firstSegmentEndPositionExplorer
    *   The end position of the first segment, as an explorer.
    * @param flipFirstSegment
    *   Whether the first segment must be flipped or not.
    * @param secondSegmentStartPositionExplorer
    *   The start position of the second segment, as and explorer.
    * @param secondSegmentEndPositionExplorer
    *   The end position of the second segment, as an explorer.
    * @param flipSecondSegment
    *   Whether the second segment must be flipped or not.
    * @param seqAfter
    *   Optionally (if known), the resulting IntSequence.
    */
  def swapSegments(
    firstSegmentStartPositionExplorer: IntSequenceExplorer,
    firstSegmentEndPositionExplorer: IntSequenceExplorer,
    flipFirstSegment: Boolean,
    secondSegmentStartPositionExplorer: IntSequenceExplorer,
    secondSegmentEndPositionExplorer: IntSequenceExplorer,
    flipSecondSegment: Boolean,
    seqAfter: Option[IntSequence] = None
  ): Unit = {
    checkConstant()
    checkExplorerForMove(firstSegmentStartPositionExplorer, "FirstSegmentStartPositionExplorer")
    checkExplorerForMove(firstSegmentEndPositionExplorer, "FirstSegmentEndPositionExplorer")
    checkExplorerForMove(secondSegmentStartPositionExplorer, "SecondSegmentStartPositionExplorer")
    checkExplorerForMove(secondSegmentEndPositionExplorer, "SecondSegmentEndPositionExplorer")

    val firstStartPosition  = firstSegmentStartPositionExplorer.position
    val firstEndPosition    = firstSegmentEndPositionExplorer.position
    val secondStartPosition = secondSegmentStartPositionExplorer.position
    val secondEndPosition   = secondSegmentEndPositionExplorer.position

    require(
      firstStartPosition <= firstEndPosition,
      s"Segment start position can not be after segment end position. " +
        s"Got start = $firstStartPosition end = $firstEndPosition"
    )
    require(
      secondStartPosition <= secondEndPosition,
      s"Segment start position can not be after segment end position. " +
        s"Got start = $secondStartPosition end = $secondEndPosition"
    )
    require(
      !(firstEndPosition >= secondStartPosition && firstEndPosition <= secondEndPosition) &&
        !(secondEndPosition >= firstStartPosition && secondEndPosition <= firstEndPosition),
      s"Swapped segments can not overlap themselves. " +
        s"\nGot positions : firstStart = $firstStartPosition firstEnd = $firstEndPosition " +
        s"secondStart = $secondStartPosition secondEnd = $secondEndPosition"
    )

    if (firstEndPosition == secondStartPosition - 1) {
      // segments are contiguous, we only need to move the second segment
      // and, if necessary flip the first one
      if (flipFirstSegment) {
        flip(firstSegmentStartPositionExplorer, firstSegmentEndPositionExplorer)
        move(
          toNotify.newValue.explorerAtPosition(secondSegmentStartPositionExplorer.position).get,
          toNotify.newValue.explorerAtPosition(secondSegmentEndPositionExplorer.position).get,
          toNotify.newValue.explorerAtPosition(firstSegmentStartPositionExplorer.prev.position).get,
          flipSecondSegment,
          seqAfter
        )
      } else {
        move(
          secondSegmentStartPositionExplorer,
          secondSegmentEndPositionExplorer,
          firstSegmentStartPositionExplorer.prev,
          flipSecondSegment,
          seqAfter
        )
      }
    } else if (firstEndPosition < secondStartPosition) {
      // move the lowest segment upward just before the second one (so that its indices do not change)
      move(
        firstSegmentStartPositionExplorer,
        firstSegmentEndPositionExplorer,
        secondSegmentStartPositionExplorer.prev,
        flipFirstSegment,
        seqAfter
      )
      // then bring the upward one lower
      move(
        toNotify.newValue.explorerAtPosition(secondSegmentStartPositionExplorer.position).get,
        toNotify.newValue.explorerAtPosition(secondSegmentEndPositionExplorer.position).get,
        toNotify.newValue.explorerAtPosition(firstSegmentStartPositionExplorer.prev.position).get,
        flipSecondSegment,
        seqAfter
      )
    } else {
      // swap them
      swapSegments(
        secondSegmentStartPositionExplorer,
        secondSegmentEndPositionExplorer,
        flipSecondSegment,
        firstSegmentStartPositionExplorer,
        firstSegmentEndPositionExplorer,
        flipFirstSegment,
        seqAfter
      )
    }
  }

  /** Registers a new incremental update.
    *
    * Generates and stores the new update in toNotify and performedSinceTopCheckpoint.
    * @param generateNewIncrementalUpdate
    *   A function implemented by the caller that generates the new update.
    */
  private final def recordPerformedIncrementalUpdate(
    generateNewIncrementalUpdate: (SeqUpdate, IntSequence) => SeqUpdate
  ): Unit = {
    // toNotify and performedSinceTopCheckpoint aren't the same.
    toNotify = generateNewIncrementalUpdate(toNotify, null)
    if (performedSinceTopCheckpoint != null) {
      performedSinceTopCheckpoint =
        generateNewIncrementalUpdate(performedSinceTopCheckpoint, toNotify.newValue)
    } else {
      require(
        levelOfTopCheckpoint == -1,
        "A checkpoint has been defined but the performedSinceTopCheckpoint stack has not been initiated"
      )
    }
  }

  /** Sets the new value of this SeqVariable.
    *
    * @param seq
    *   The new value.
    */
  def :=(seq: IntSequence): Unit = setValue(seq)

  /** Assigns a new value to this SeqVariable.
    *
    * This is a non-incremental operation. You cannot have some non-propagated changes.
    *
    * @param newIntSequence
    *   The new IntSequence value.
    */
  def setValue(newIntSequence: IntSequence): Unit = {
    checkConstant()
    require(
      levelOfTopCheckpoint == -1,
      "Sequences cannot be assigned when a checkpoint has been defined"
    )
    if (!(toNotify.newValue sameIdentity newIntSequence)) {
      toNotify = SeqUpdateAssign(newIntSequence)
      scheduleForPropagation()
    }
  }

  /** Returns the value of the top checkpoint */
  def topCheckpointLevel: Int = levelOfTopCheckpoint

  // The last defined checkpoint (can be null if no checkpoint is defined)
  private[this] var topCheckpoint: IntSequence = _
  private[this] var levelOfTopCheckpoint: Int  = -1
  // What has been performed since last checkpoint definition
  private[this] var performedSinceTopCheckpoint: SeqUpdate = _
  /* Stack of checkpoint and their update, does not include top checkpoint
  The tuple is as follows:
  - checkpointValue
  - the update that led to this value from the previous checkpoint
   */
  private[this] var checkpointStackNotTop: List[(IntSequence, SeqUpdate)] = List.empty

  /** Defines the current value as a new checkpoint. */
  def defineCurrentValueAsCheckpoint(seqAfter: Option[IntSequence] = None): IntSequence = {
    checkConstant()
    toNotify =
      SeqUpdateDefineCheckpoint(toNotify, maxPivotPerValuePercent, levelOfTopCheckpoint + 1)

    if (topCheckpoint != null) {
      checkpointStackNotTop = (topCheckpoint, performedSinceTopCheckpoint) :: checkpointStackNotTop
    }
    topCheckpoint = seqAfter.getOrElse(toNotify.newValue)
    levelOfTopCheckpoint += 1
    performedSinceTopCheckpoint = SeqUpdateLastNotified(topCheckpoint)

    scheduleForPropagation()
    topCheckpoint
  }

  /** Rollback to the top checkpoint.
    *
    * Either the SeqVariable is already propagated ==> we need to specify how to rollback, i.e.,
    * which operation has to be made to be back at the previous state.
    *
    * Or it wasn't propagated, then we just drop the last updates.
    *
    * @param checkingCheckpoint
    *   An optional checkpoint just to check if the calling neighborhood/method is at the same level
    *   as the SeqVariable.
    */
  def rollbackToTopCheckpoint(checkingCheckpoint: Option[IntSequence] = None): Unit = {
    checkConstant()
    require(
      topCheckpoint != null,
      "Cannot rollback to top checkpoint since no checkpoint has been defined"
    )
    if (checkingCheckpoint.nonEmpty)
      require(
        checkingCheckpoint.get sameIdentity topCheckpoint,
        s"The checking checkpoint does not quick equals the top checkpoint." +
          s"\nDo they contain the same elements? " +
          s"${checkingCheckpoint.get equals topCheckpoint} " +
          s"\ncheckingCheckpoint:${checkingCheckpoint.get} \ntopCheckpoint:$topCheckpoint"
      )

    rollbackSimplification(toNotify, topCheckpoint) match {
      case CheckpointReachedNotRemoved(newToNotify: SeqUpdate) =>
        // Checkpoint value could be found in toNotify, and updates after it were removed so we don't have to do anything
        // Reset performedSinceTopCheckpoint and update toNotify
        performedSinceTopCheckpoint = SeqUpdateLastNotified(topCheckpoint)
        toNotify = newToNotify

      case NoSimplificationPerformed =>
        // In this case, the checkpoint was already notified, and possibly some moves were performed from it.
        // We cannot simply pop updates, we have to add rollBack instructions

        val howToRollBack = performedSinceTopCheckpoint
          .reverseThis(expectedValueAfterFullReverse = topCheckpoint)
          .appendThisTo(toNotify)
        toNotify = SeqUpdateRollBackToTopCheckpoint(
          topCheckpoint,
          howToRollBack = howToRollBack,
          level = levelOfTopCheckpoint,
          toNotify
        )

        performedSinceTopCheckpoint = SeqUpdateLastNotified(topCheckpoint)
        scheduleForPropagation()
    }

    require(
      toNotify.newValue sameIdentity topCheckpoint,
      s"The new value, after rollback does not quickEquals the top checkpoint " +
        s"\nNew value : ${toNotify.newValue}  \n Top Checkpoint : $topCheckpoint"
    )
  }

  /** Releases and returns the top checkpoint.
    *
    * Used when exploring neighborhood. For instance, after exploring OnePointMove, we need to
    * release the top checkpoint.
    *
    * '''NOTE : you need to roll back to top checkpoint first.'''
    *
    * @return
    *   The current top checkpoint value
    */
  def releaseTopCheckpoint(): IntSequence = {
    checkConstant()
    require(topCheckpoint != null, "No checkpoint defined to release")
    require(toNotify.newValue equals topCheckpoint, "You must be at top checkpoint to release it")

    toNotify = toNotify match {
      // We just rolled back, simply add this new instruction
      case _: SeqUpdateRollBackToTopCheckpoint =>
        SeqUpdateReleaseTopCheckpoint(toNotify, toNotify.newValue)
      // We are at topCheckpoint and it was not yet released.
      // It seems we are releasing a checkpoint after which no modification where done.
      case _: SeqUpdateReleaseTopCheckpoint =>
        SeqUpdateReleaseTopCheckpoint(toNotify, toNotify.newValue)
      // We are at top checkpoint level, just drop the checkpoint definition
      case dc: SeqUpdateDefineCheckpoint =>
        dc.prev
      // The rollback instruction of the checkpoint definition are already commited, add this new instruction
      case ln: SeqUpdateLastNotified =>
        require(
          ln.newValue equals topCheckpoint,
          "Cannot release checkpoint since last notified value is not at top checkpoint value"
        )
        SeqUpdateReleaseTopCheckpoint(toNotify, topCheckpoint)
    }
    checkpointStackNotTop = checkpointStackNotTop match {
      case newTop :: tail =>
        require(levelOfTopCheckpoint > 0)
        topCheckpoint = newTop._1
        performedSinceTopCheckpoint = newTop._2
        tail
      case Nil =>
        // there is no upper checkpoint
        require(levelOfTopCheckpoint == 0)
        topCheckpoint = null
        performedSinceTopCheckpoint = null
        Nil
    }
    levelOfTopCheckpoint -= 1
    scheduleForPropagation()
    topCheckpoint
  }

  /** Tries to find the last update corresponding to the searched checkpoint.
    *
    * Pops update until a checkpoint definition or a non-incremental update is reached. Then there
    * are several scenarios:
    *   - We reached the searchedCheckpoint definition ==> It was not propagated yet, just drop
    *     everything until then.
    *   - We reached a rollbackToTopCheckpoint instruction.
    *     - It matches our searched checkpoint ==> The checkpoint definition was already propagated
    *       but we already have a howToRollback instruction, just drop everything until then.
    *     - It does not match our searched checkpoint ==> can't happen since we should have reached
    *       a releaseTopCheckpoint before.
    *   - We reached a releaseTopCheckpoint instruction ==> it's definition was already propagated,
    *     our targeted checkpoint is even further ==> no simplification.
    *   - We reached a LastNotify update.
    *     - Either it has our targeted checkpoint value, we can drop everything until then.
    *     - Or there is no simplification.
    *
    * Note : We can not reach an seqUpdateAssign because assignation is only performed when no
    * checkpoint are defined. And we need a checkpoint definition to roll back to it, so we must
    * reach one before an assignation.
    *
    * @param updates
    *   The latest not propagated updates.
    * @param searchedCheckpoint
    *   The targeted checkpoint.
    * @return
    *   CleaningResult according to the performed cleaning.
    */
  private def rollbackSimplification(
    updates: SeqUpdate,
    searchedCheckpoint: IntSequence
  ): CleaningResult = {

    def isSimpleMove(seqUpdate: SeqUpdateWithPrev): Boolean = {
      seqUpdate.isInstanceOf[SeqUpdateInsert] ||
      seqUpdate.isInstanceOf[SeqUpdateMove] ||
      seqUpdate.isInstanceOf[SeqUpdateRemove]
    }

    updates match {
      case update: SeqUpdateWithPrev if isSimpleMove(update) =>
        rollbackSimplification(update.prev, searchedCheckpoint) match {
          case NoSimplificationPerformed =>
            if (searchedCheckpoint sameIdentity updates.newValue)
              CheckpointReachedNotRemoved(updates)
            else NoSimplificationPerformed
          case x => x
        }

      case SeqUpdateLastNotified(value: IntSequence) =>
        if (value sameIdentity searchedCheckpoint)
          CheckpointReachedNotRemoved(updates)
        else NoSimplificationPerformed

      case SeqUpdateDefineCheckpoint(_, _) =>
        require(
          updates.newValue sameIdentity searchedCheckpoint,
          s"Reaching a SeqUpdateDefinedCheckpoint while roll backing, it's value should be the targeted checkpoint.\n  " +
            s"Same identity : false \t Same value : ${updates.newValue equals searchedCheckpoint}\n" +
            s"Reached define checkpoint value : ${updates.newValue} \n" +
            s"Targeted checkpoint : $searchedCheckpoint"
        )
        CheckpointReachedNotRemoved(updates)

      case SeqUpdateRollBackToTopCheckpoint(
            checkpointValue: IntSequence,
            _: SeqUpdate,
            _: Int,
            _: SeqUpdate
          ) =>
        if (checkpointValue sameIdentity searchedCheckpoint)
          CheckpointReachedNotRemoved(updates)
        else {
          require(requirement = false, "Can't happen, see method comment for more information")
          NoSimplificationPerformed
        }

      case _ => NoSimplificationPerformed
    }
  }

  protected[core] override def performPropagation(): Unit = {
    val dynListElements = getDynamicallyListeningElements
    dynListElements.foreach { case (inv: SeqNotificationTarget, index: Int) =>
      inv.notifySeqChanges(this, index, toNotify)
    }
    _value = toNotify.newValue
    toNotify = SeqUpdateLastNotified(_value)
  }

  /** Creates a clone of this SeqVariable.
    *
    * By using the [[SeqIdentityInvariant]], each update of this variable will be applied to the
    * clone.
    *
    * @param maxDepth
    *   The maximum depth of the IntSequence, aka, the maximum number of
    *   [[oscar.cbls.algo.sequence.StackedUpdateIntSequence]] before committing them.
    * @return
    *   A clone of this variable.
    */
  def createClone(maxDepth: Int = 50): SeqVariable = {
    val clone = new SeqVariable(
      model,
      this.value().toList,
      s"clone_of_$name",
      maxPivotPerValuePercent,
      isConstant
    )
    SeqIdentityInvariant(model, this, clone)
    clone
  }

  /** Saves the state of this variable */
  override def save(): SavedValue = SeqSavedValue(this)

  override def setDomain(min: Long, max: Long): Unit = {
    require(
      min >= Int.MinValue && max <= Int.MaxValue,
      "The maximum domain for SeqVariable is Integer range"
    )
    super.setDomain(min, max)
  }

  override def checkInternals(): Unit = {
    require(
      this.value().toList equals toNotify.newValue.toList,
      s"Pending value of $name is not equal to toNotify value : " +
        s"\nShould be : ${this.value().toList} \nGot ${toNotify.newValue.toList}"
    )
    require(
      toNotify.isInstanceOf[SeqUpdateLastNotified],
      s"To notify value of $name should be of type SeqUpdateLastNotified but got :\ntoNotify:$toNotify"
    )
  }

  override def toString: String = {
    s"$name :\n " +
      s"Current value : ${toNotify.newValue}\n" +
      s"To notify : $toNotify\n" +
      s"Checkpoint level : $levelOfTopCheckpoint\n"
  }
}

private abstract class CleaningResult

private case class CheckpointReachedNotRemoved(newToNotify: SeqUpdate) extends CleaningResult()

private case object NoSimplificationPerformed extends CleaningResult
