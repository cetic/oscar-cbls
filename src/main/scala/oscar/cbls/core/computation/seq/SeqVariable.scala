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
import oscar.cbls.core.computation.{Invariant, SavedValue, Store, Variable}
import oscar.cbls.core.propagation.PropagationStructure

class SeqVariable(
  model: Store,
  initialValues: List[Int],
  name: String = "SeqVariable",
  maxPivotPerValuePercent: Int = 4,
  isConstant: Boolean = false
) extends Variable(model, isConstant) {
  override type NotificationTargetType = SeqNotificationTarget
  require(model != null)
  setDomain(Int.MinValue, Int.MaxValue)

  // Latest propagated value
  private var mOldValue: IntSequence = {
    if (initialValues.isEmpty) IntSequence.empty()
    else IntSequence(initialValues)
  }
  // Stack of notification to notify at next propagation
  private var toNotify: SeqUpdate = SeqUpdateLastNotified(mOldValue)

  /** Returns the new value of this SeqVariable */
  def newValue: IntSequence = toNotify.newValue

  override def name(): String = name

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
    if (!propagating) model.propagate(Some(this))
    mOldValue
  }

  // Check if the explorer is on the right IntSequence
  private def checkExplorerForMove(explorer: IntSequenceExplorer, explorerName: String): Unit = {
    require(
      toNotify.newValue.sameIdentity(explorer.intSequence),
      s"$explorerName must explore the current IntSequence." +
        s"\nShould be ${toNotify.newValue} with token : ${toNotify.newValue.token}.\nGot ${explorer.intSequence} with token : ${explorer.intSequence.token}"
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
      // move lowest segment upward just before the second one (so that its indices do not change)
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
      levelOfTopCheckpoint == -1,
      "Sequences cannot be assigned when a checkpoint has been defined"
    )
    if (!(toNotify.newValue sameIdentity newIntSequence)) {
      if (!toNotify.isInstanceOf[SeqUpdateLastNotified] && !toNotify.isInstanceOf[SeqUpdateAssign])
        performPropagation()
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
  def defineCurrentValueAsCheckpoint(seqAfter: Option[IntSequence] = None): IntSequence = {
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
    * Either the SeqVariable is already propagated ==> we need to specify how to rollback. Meaning
    * which operation has to be made to be back at the previous state.
    *
    * or it wasn't propagated, then we just drop the last updates
    *
    * @param checkingCheckpoint
    *   An optional checkpoint just to check if the calling neighborhood/method is at the same level
    *   as the SeqVariable
    */
  def rollbackToTopCheckpoint(checkingCheckpoint: Option[IntSequence] = None): Unit = {
    require(topCheckpoint != null, "Can not rollback to top checkpoint since no checkpoint has been defined")
    if (checkingCheckpoint.nonEmpty)
      require(
        checkingCheckpoint.get sameIdentity topCheckpoint,
        s"The checking checkpoint does not quick equals the top checkpoint \nDo they contain the same elements ? " +
          s"${checkingCheckpoint.get equals topCheckpoint} \ncheckingCheckpoint:${checkingCheckpoint.get} \ntopCheckpoint:$topCheckpoint"
      )

    rollbackSimplification(toNotify, topCheckpoint) match {
      case CheckpointDeclarationReachedAndRemoved(_: SeqUpdate) =>
        require(false, "Should not append")
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
      s"The new value, after rollback does not quickEquals the top checkpoint \nNew value : ${toNotify.newValue}  \n Top Checkpoint : $topCheckpoint"
    )
  }

  /** Releases the top checkpoint.
    *
    * Used when exploring neighborhood. For instance, after exploring OnePointMove, we need to
    * release the top checkpoint.
    */
  def releaseTopCheckpoint(): IntSequence = {
    require(topCheckpoint != null, "No checkpoint defined to release")
    require(levelOfTopCheckpoint >= 0)
    require(toNotify.newValue equals topCheckpoint, "You must be at top checkpoint to release it")

    // Two cases, currently at checkpoint 0 or higher than 0

    toNotify = toNotify match {
      case _: SeqUpdateRollBackToTopCheckpoint =>
        SeqUpdateReleaseTopCheckPoint(toNotify, toNotify.newValue)
      case dc: SeqUpdateDefineCheckpoint =>
        dc.prev
      case ln: SeqUpdateLastNotified =>
        require(
          ln.value equals topCheckpoint,
          "Cannot release checkpoint since last notified value is not at top checkpoint value"
        )
        SeqUpdateReleaseTopCheckPoint(toNotify, topCheckpoint)
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
            if (searchedCheckpoint sameIdentity updates.newValue)
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
            if (searchedCheckpoint sameIdentity updates.newValue)
              CheckpointReachedNotRemoved(updates)
            else NoSimplificationPerformed
          case x => x
        }

      case SeqUpdateRemove(_: IntSequenceExplorer, prev: SeqUpdate) =>
        rollbackSimplification(prev, searchedCheckpoint) match {
          case NoSimplificationPerformed =>
            if (searchedCheckpoint sameIdentity updates.newValue)
              CheckpointReachedNotRemoved(updates)
            else NoSimplificationPerformed
          case x => x
        }

      case SeqUpdateAssign(value: IntSequence) =>
        if (value sameIdentity searchedCheckpoint)
          CheckpointReachedNotRemoved(updates)
        else NoSimplificationPerformed

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
            prev: SeqUpdate
          ) =>
        if (checkpointValue sameIdentity searchedCheckpoint)
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
          updates.newValue sameIdentity searchedCheckpoint,
          "require fail on quick equals: " + updates.newValue + "should== " + searchedCheckpoint
        )
        CheckpointDeclarationReachedAndRemoved(prev)

      case SeqUpdateRollBackToTopCheckpoint(
            _: IntSequence,
            _: SeqUpdate,
            _: Int,
            prev: SeqUpdate
          ) =>
        NoSimplificationPerformed

      case _ => NoSimplificationPerformed
    }
  }

  @inline
  override def performPropagation(): Unit = {
    val dynListElements = getDynamicallyListeningElements
    dynListElements.foreach {
      case (inv: SeqNotificationTarget, index: Int) =>
        inv.notifySeqChanges(this, index, toNotify)
      case (x: Invariant, _) =>
        throw new IllegalArgumentException(
          s"The listening Invariant ($x) does not extend SeqNotificationTarget, therefore no notification can be send to it."
        )
    }
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
    SeqIdentityInvariant(model, this, clone)
    clone
  }

  /** Save the state of this variable */
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
      this.value.toList equals toNotify.newValue.toList,
      s"Pending value of $name is not equal to toNotify value : " +
        s"\nShould be : ${this.value.toList} \nGot ${toNotify.newValue.toList}"
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

private class SimplificationPerformed extends CleaningResult

private case class CheckpointDeclarationReachedAndRemoved(newToNotify: SeqUpdate)
    extends SimplificationPerformed()

private case class CheckpointReachedNotRemoved(newToNotify: SeqUpdate)
    extends SimplificationPerformed()

private case object NoSimplificationPerformed extends CleaningResult
