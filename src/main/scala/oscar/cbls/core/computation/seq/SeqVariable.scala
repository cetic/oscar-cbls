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
import oscar.cbls.core.computation.{SavedValue, Store, Variable}

class SeqVariable(
  model: Store,
  initialValues: List[Int],
  name: String = "SeqVariable",
  maxPivotPerValuePercent: Int = 4,
  isConstant: Boolean = false
) extends Variable(model, isConstant) {
  require(model != null)

  // Latest propagated value
  private var mOldValue: IntSequence = IntSequence(initialValues)
  // Stack of notification to notify at next propagation
  private var toNotify: SeqUpdate = SeqUpdateLastNotified(mOldValue)

  /** Returns the new value of this SeqVariable */
  def newValue: IntSequence = toNotify.newValue

  def name: String = name

  /** Propagates up to this variable (if needed) and returns the new value of this SeqVariable.
    *
    * If this variable is a decision variable, meaning that no invariant control this variable, no
    * propagation are triggered.
    *
    * @return
    *   The new value of this SeqVariable.
    */
  def value: IntSequence = {
    if (model == null) return mOldValue
    val propagating = model.propagating
    if (isADecisionVariable && !propagating) return toNotify.newValue
    if (!propagating) model.propagate(this)
    mOldValue
  }

  // Check if the explorer is on the right IntSequence
  private def checkExplorerForMove(explorer: IntSequenceExplorer, explorerName: String): Unit = {
    require(
      toNotify.newValue.quickEquals(explorer.intSequence),
      s"$explorerName must explore the current IntSequence." +
        s"\nShould be ${toNotify.newValue}.\nGot ${explorer.intSequence}"
    )
  }

  /** Inserts a value after a given position.
    *
    * @param value
    *   The value to insert
    * @param insertAfterPositionExplorer
    *   The position after which to insert the value, as an explorer
    * @param seqAfter
    *   Optionally (if known), the resulting IntSequence
    */
  def insertAfterPosition(
    value: Int,
    insertAfterPositionExplorer: IntSequenceExplorer,
    seqAfter: Option[IntSequence] = None
  ): Unit = {
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
    *   The position where to remove the value, as an explorer
    * @param seqAfter
    *   Optionally (if known), the resulting IntSequence
    */
  def remove(
    removePositionExplorer: IntSequenceExplorer,
    seqAfter: Option[IntSequence] = None
  ): Unit = {
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
    *   The start position of the subsequence, as and explorer
    * @param toIncludedPositionExplorer
    *   The end position of the subsequence, as an explorer
    * @param afterPositionExplorer
    *   The position after which the subsequence will be moved, as an explorer
    * @param flip
    *   Whether or not the subsequence must be flipped
    * @param seqAfter
    *   Optionally (if known), the resulting IntSequence
    */
  def move(
    fromIncludedPositionExplorer: IntSequenceExplorer,
    toIncludedPositionExplorer: IntSequenceExplorer,
    afterPositionExplorer: IntSequenceExplorer,
    flip: Boolean,
    seqAfter: Option[IntSequence] = None
  ): Unit = {
    checkExplorerForMove(fromIncludedPositionExplorer, "FromIncludedPositionExplorer")
    checkExplorerForMove(toIncludedPositionExplorer, "ToIncludedPositionExplorer")
    checkExplorerForMove(afterPositionExplorer, "AfterPositionExplorer")
    require(
      fromIncludedPositionExplorer.position <= toIncludedPositionExplorer.position,
      s"fromPosition should be before or equals to toPosition. " +
        s"Got from : ${fromIncludedPositionExplorer.position} to : ${toIncludedPositionExplorer.position}"
    )
    require(
      afterPositionExplorer.position < fromIncludedPositionExplorer.position ||
        afterPositionExplorer.position >= toIncludedPositionExplorer.position,
      s"afterPosition should be before fromPosition or greater or equals to toPosition. " +
        s"Got from : ${fromIncludedPositionExplorer.position} to : ${toIncludedPositionExplorer.position} " +
        s"after : ${afterPositionExplorer.position}"
    )

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

  /** Flips the subsequence defined by the two position (included)
    *
    * @param fromIncludedPositionExplorer
    *   The start position of the subsequence, as and explorer
    * @param toIncludedPositionExplorer
    *   The end position of the subsequence, as an explorer
    * @param seqAfter
    *   Optionally (if known), the resulting IntSequence
    */
  def flip(
    fromIncludedPositionExplorer: IntSequenceExplorer,
    toIncludedPositionExplorer: IntSequenceExplorer,
    seqAfter: Option[IntSequence] = None
  ): Unit = {
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
    *   The start position of the first segment, as and explorer
    * @param firstSegmentEndPositionExplorer
    *   The end position of the first segment, as an explorer
    * @param flipFirstSegment
    *   Whether or not the first segment must be flipped
    * @param secondSegmentStartPositionExplorer
    *   The start position of the second segment, as and explorer
    * @param secondSegmentEndPositionExplorer
    *   The end position of the second segment, as an explorer
    * @param flipSecondSegment
    *   Whether or not the second segment must be flipped
    * @param seqAfter
    *   Optionally (if known), the resulting IntSequence
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
      move(
        secondSegmentStartPositionExplorer,
        secondSegmentEndPositionExplorer,
        firstSegmentStartPositionExplorer.prev,
        flipSecondSegment,
        seqAfter
      )
    } else if (firstEndPosition < secondStartPosition) {
      // move lowest segment upward just before the second one (so that its indices do not change)
      move(
        firstSegmentStartPositionExplorer,
        firstSegmentEndPositionExplorer,
        secondSegmentStartPositionExplorer.prev,
        flipFirstSegment,
        seqAfter
      )
      // them bring the upward one lower
      move(
        secondSegmentStartPositionExplorer,
        secondSegmentEndPositionExplorer,
        firstSegmentStartPositionExplorer.prev,
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

  @inline
  /** Registers a new incremental update
    *
    * Generates and stores the new update in toNotify and performedSinceTopCheckpoint
    * @param generateNewIncrementalUpdate
    *   A function implemented by the caller that generates the new update
    */
  private final def recordPerformedIncrementalUpdate(
    generateNewIncrementalUpdate: (SeqUpdate, IntSequence) => SeqUpdate
  ): Unit = {
    // toNotify and performedSinceTopCheckpoint aren't the same.
    toNotify = generateNewIncrementalUpdate(toNotify, null)
    performedSinceTopCheckpoint =
      generateNewIncrementalUpdate(performedSinceTopCheckpoint, toNotify.newValue)
  }

  def :=(seq: IntSequence): Unit = setValue(seq)

  /** Assigns a new value to this SeqVariable.
    *
    * This is a non-incremental operation. You cannot have some non-propagated changes.
    *
    * @param newIntSequence
    *   The new IntSequence value
    */
  def setValue(newIntSequence: IntSequence): Unit = {
    require(
      performedSinceTopCheckpoint == null &&
        levelOfTopCheckpoint == -1,
      "Sequences cannot be assigned when a checkpoint has been defined"
    )

    if (!(toNotify.newValue quickEquals newIntSequence)) {
      toNotify = SeqUpdateAssign(newIntSequence)
      scheduleForPropagation()
    }
  }

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

  /** Defines the current value as a new checkpoint */
  def defineCurrentValueAsCheckpoint(): IntSequence = {
    toNotify =
      SeqUpdateDefineCheckpoint(toNotify, maxPivotPerValuePercent, levelOfTopCheckpoint + 1)

    if (topCheckpoint != null) {
      checkpointStackNotTop = (topCheckpoint, performedSinceTopCheckpoint) :: checkpointStackNotTop
    }
    topCheckpoint = toNotify.newValue
    levelOfTopCheckpoint += 1
    performedSinceTopCheckpoint = SeqUpdateLastNotified(topCheckpoint)

    scheduleForPropagation()
    toNotify.newValue
  }

  def rollbackToTopCheckpoint(checkpoint: IntSequence): Unit = {
    require(
      checkpoint quickEquals topCheckpoint,
      s"given checkpoint not quick equals to my top checkpoint; equal=${checkpoint equals topCheckpoint} checkpoint:$checkpoint my topCheckpoint:$topCheckpoint"
    )

    rollbackSimplification(toNotify, topCheckpoint) match {
      case CheckpointDeclarationReachedAndRemoved(_: SeqUpdate) =>
        require(false, "Should not append")
      case CheckpointReachedNotRemoved(newToNotify: SeqUpdate) =>
        // Checkpoint value could be found in toNotify, and updated after it were removed so we don't have to do anything
        // Reset performedSinceTopCheckpoint and update toNotify
        performedSinceTopCheckpoint = SeqUpdateLastNotified(topCheckpoint)
        toNotify = newToNotify

      case NoSimplificationPerformed =>
        // In this case, the checkpoint was already notified, and possibly some moves were performed from it.
        // We cannot simply pop updates, we have to add rollBack instructions

        val howToRollBack = performedSinceTopCheckpoint
          .reverseThis(expectedValueAfterFullReverse = topCheckpoint)
          .appendThisTo(toNotify.explicitHowToRollBack())
        toNotify = SeqUpdateRollBackToCheckpoint(
          checkpoint,
          howToRollBack = howToRollBack,
          level = levelOfTopCheckpoint
        )

        performedSinceTopCheckpoint = SeqUpdateLastNotified(topCheckpoint)
        scheduleForPropagation()
    }

    require(
      toNotify.newValue quickEquals checkpoint,
      s"${toNotify.newValue} not quickEquals $checkpoint"
    )
  }

  /** Releases the top checkpoint.
    *
    * Used when exploring neighborhood. For instance, after exploring OnePointMove, we need to
    * release the top checkpoint.
    */
  def releaseTopCheckpoint(): Unit = {
    require(topCheckpoint != null, "No checkpoint defined to release")
    require(levelOfTopCheckpoint >= 0)

    removeCheckpointDeclarationIfPresent(toNotify, topCheckpoint) match {
      case CheckpointDeclarationReachedAndRemoved(newToNotify: SeqUpdate) =>
        toNotify = newToNotify
      case CheckpointReachedNotRemoved(_: SeqUpdate) =>
        require(requirement = false, "Unexpected internal result")
      case NoSimplificationPerformed =>
      // Checkpoint propagated, nothing to do
    }

    // Two cases, currently at checkpoint 0 or higher than 0
    checkpointStackNotTop match {
      case newTop :: tail =>
        require(levelOfTopCheckpoint > 0)
        checkpointStackNotTop = tail
        topCheckpoint = newTop._1
        performedSinceTopCheckpoint =
          if (performedSinceTopCheckpoint != null && newTop._2 != null)
            performedSinceTopCheckpoint.appendThisTo(newTop._2)
          else null
        levelOfTopCheckpoint -= 1
      case Nil =>
        // there is no upper checkpoint
        require(levelOfTopCheckpoint == 0)
        levelOfTopCheckpoint = -1
        topCheckpoint = null
        performedSinceTopCheckpoint = null
    }
  }

  /** Tries to find the last update corresponding to the searched checkpoint.
    *
    * Pops update until a checkpoint definition or a non-incremental update is reached. Then there
    * are 4 scenarios :
    *   - The sequence was not propagated ==> we should reach top checkpoint
    *   - The sequence was propagated ==> top checkpoint is now in a SeqUpdateLastNotify we should
    *     reach it
    *   - There was an assignation (not incremental) ==> we can't go further back, so either it's
    *     the target value or we cannot simplify the rollback
    *   - We want to roll back multiple times and the sequence was propagated before roll-backing
    *     \==> A SeqUpdateRollBack is already present,
    *
    * @param updates
    *   The latest not propagated updates
    * @param searchedCheckpoint
    *   The targeted checkpoint
    * @return
    *   CleaningResult according to the performed cleaning
    */
  private def rollbackSimplification(
    updates: SeqUpdate,
    searchedCheckpoint: IntSequence
  ): CleaningResult = {
    updates match {
      case SeqUpdateInsert(_: Int, _: IntSequenceExplorer, prev: SeqUpdate) =>
        rollbackSimplification(prev, searchedCheckpoint) match {
          case NoSimplificationPerformed =>
            if (searchedCheckpoint quickEquals updates.newValue)
              CheckpointReachedNotRemoved(updates)
            else NoSimplificationPerformed
          case x => x
        }

      case SeqUpdateMove(
            _: IntSequenceExplorer,
            _: IntSequenceExplorer,
            _: IntSequenceExplorer,
            _: Boolean,
            prev: SeqUpdate
          ) =>
        rollbackSimplification(prev, searchedCheckpoint) match {
          case NoSimplificationPerformed =>
            if (searchedCheckpoint quickEquals updates.newValue)
              CheckpointReachedNotRemoved(updates)
            else NoSimplificationPerformed
          case x => x
        }

      case SeqUpdateRemove(_: IntSequenceExplorer, prev: SeqUpdate) =>
        rollbackSimplification(prev, searchedCheckpoint) match {
          case NoSimplificationPerformed =>
            if (searchedCheckpoint quickEquals updates.newValue)
              CheckpointReachedNotRemoved(updates)
            else NoSimplificationPerformed
          case x => x
        }

      case SeqUpdateAssign(value: IntSequence) =>
        if (value quickEquals searchedCheckpoint)
          CheckpointReachedNotRemoved(updates)
        else NoSimplificationPerformed

      case SeqUpdateLastNotified(value: IntSequence) =>
        if (value quickEquals searchedCheckpoint)
          CheckpointReachedNotRemoved(updates)
        else NoSimplificationPerformed

      case SeqUpdateDefineCheckpoint(_, _) =>
        require(
          updates.newValue quickEquals searchedCheckpoint,
          s"The targeted checkpoint must (in this case) be equal to the latest defined checkpoint. " +
            s"Latest defined checkpoint ${updates.newValue} got targeted checkpoint $searchedCheckpoint"
        )
        CheckpointReachedNotRemoved(updates)

      case SeqUpdateRollBackToCheckpoint(checkpointValue: IntSequence, _: SeqUpdate, _: Int) =>
        if (checkpointValue quickEquals searchedCheckpoint)
          CheckpointReachedNotRemoved(updates)
        else NoSimplificationPerformed

      case _ => NoSimplificationPerformed
    }
  }

  /** Removes the search checkpoint from the updates stack.
    *
    * @param updates
    *   The updates stack
    * @param searchedCheckpoint
    *   The searched checkpoint
    * @return
    *   The cleaning result
    */
  private def removeCheckpointDeclarationIfPresent(
    updates: SeqUpdate,
    searchedCheckpoint: IntSequence
  ): CleaningResult = {
    updates match {
      case SeqUpdateInsert(value: Int, pos: IntSequenceExplorer, prev: SeqUpdate) =>
        removeCheckpointDeclarationIfPresent(prev, searchedCheckpoint) match {
          case NoSimplificationPerformed => NoSimplificationPerformed
          case CheckpointDeclarationReachedAndRemoved(newPrev) =>
            CheckpointDeclarationReachedAndRemoved(
              SeqUpdateInsert(value, pos, newPrev, updates.newValue)
            )
          case _ => throw new Error("unexpected match")
        }

      case SeqUpdateMove(
            fromIncluded: IntSequenceExplorer,
            toIncluded: IntSequenceExplorer,
            after: IntSequenceExplorer,
            flip: Boolean,
            prev: SeqUpdate
          ) =>
        removeCheckpointDeclarationIfPresent(prev, searchedCheckpoint) match {
          case NoSimplificationPerformed => NoSimplificationPerformed
          case CheckpointDeclarationReachedAndRemoved(newPrev) =>
            CheckpointDeclarationReachedAndRemoved(
              SeqUpdateMove(fromIncluded, toIncluded, after, flip, newPrev, updates.newValue)
            )
          case _ => throw new Error("unexpected match")
        }

      case SeqUpdateRemove(position: IntSequenceExplorer, prev: SeqUpdate) =>
        removeCheckpointDeclarationIfPresent(prev, searchedCheckpoint) match {
          case NoSimplificationPerformed => NoSimplificationPerformed
          case CheckpointDeclarationReachedAndRemoved(newPrev) =>
            CheckpointDeclarationReachedAndRemoved(
              SeqUpdateRemove(position, newPrev, updates.newValue)
            )
          case _ => throw new Error("unexpected match")
        }

      case SeqUpdateAssign(_: IntSequence) => NoSimplificationPerformed

      case SeqUpdateLastNotified(_: IntSequence) => NoSimplificationPerformed

      case SeqUpdateDefineCheckpoint(prev: SeqUpdate, _: Int) =>
        require(
          updates.newValue quickEquals searchedCheckpoint,
          "require fail on quick equals: " + updates.newValue + "should== " + searchedCheckpoint
        )
        CheckpointDeclarationReachedAndRemoved(prev)

      case SeqUpdateRollBackToCheckpoint(_: IntSequence, _: SeqUpdate, _: Int) =>
        NoSimplificationPerformed

      case _ => NoSimplificationPerformed
    }
  }

  @inline
  override protected def performPropagation(): Unit = {
    val dynListElements = getDynamicallyListeningElements
    dynListElements.foreach(pe => {
      val inv: SeqNotificationTarget = pe.asInstanceOf[SeqNotificationTarget]
      inv.notifySeqChanges(this, pe.id, toNotify)
    })
    mOldValue = toNotify.newValue
    toNotify = SeqUpdateLastNotified(mOldValue)
  }

  /** Creates a clone of this SeqVariable.
    *
    * Through the SeqIdentityInvariant, each update of this variable will be applied to the clone.
    *
    * @param maxDepth
    * @return
    *   A clone of this variable
    */
  def createClone(maxDepth: Int = 50): SeqVariable = {
    val clone = new SeqVariable(
      model,
      this.value.toList,
      s"clone_of_$name",
      maxPivotPerValuePercent,
      isConstant
    )
    SeqIdentityInvariant(this, clone)
    clone
  }

  /** Save the state of this variable */
  override def save(): SavedValue = SeqSavedValue(this)

  override def checkInternals(): Unit = {
    require(this.value.toList equals toNotify.newValue.toList)
    require(this.toNotify.isInstanceOf[SeqUpdateLastNotified], Some(s"toNotify:$toNotify"))
  }
}

private abstract class CleaningResult

private class SimplificationPerformed extends CleaningResult

private case class CheckpointDeclarationReachedAndRemoved(newToNotify: SeqUpdate)
    extends SimplificationPerformed()

private case class CheckpointReachedNotRemoved(newToNotify: SeqUpdate)
    extends SimplificationPerformed()

private case object NoSimplificationPerformed extends CleaningResult
