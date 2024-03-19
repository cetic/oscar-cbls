package oscar.cbls.core.computation.seq

import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.computation.{SavedValue, Store, Variable}

class SeqVariable(
  model: Store,
  initialValues: List[Int],
  name: Option[String] = None,
  maxPivotPerValuePercent: Int = 4,
  isConstant: Boolean = false
) extends Variable(model, isConstant) {
  require(model != null)

  private var mOldValue: IntSequence = IntSequence(initialValues)
  private var toNotify: SeqUpdate    = SeqUpdateLastNotified(mOldValue)
  def newValue: IntSequence = toNotify.newValue

  override def checkInternals(): Unit = {
    require(this.value.toList equals toNotify.newValue.toList)
    require(this.toNotify.isInstanceOf[SeqUpdateLastNotified], Some(s"toNotify:$toNotify"))
  }

  def name: String = name.getOrElse("SeqVariable")

  def value: IntSequence = {
    if (model == null) return mOldValue
    val propagating = model.propagating
    if (definingInvariant == null && !propagating) return toNotify.newValue
    if (!propagating) model.propagate(this)
    mOldValue
  }

  private def checkExplorerForMove(explorer: IntSequenceExplorer, explorerName: String): Unit = {
    require(
      toNotify.newValue.quickEquals(explorer.intSequence),
      s"$explorerName must explore the current IntSequence." +
        s"\nShould be ${toNotify.newValue}.\nGot ${explorer.intSequence}"
    )
  }

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

  def flip(
    fromIncludedPositionExplorer: IntSequenceExplorer,
    toIncludedPositionExplorer: IntSequenceExplorer
  ): Unit = {
    move(
      fromIncludedPositionExplorer,
      toIncludedPositionExplorer,
      fromIncludedPositionExplorer.prev,
      flip = true
    )
  }

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

  def swapSegments(
    firstSegmentStartPositionExplorer: IntSequenceExplorer,
    firstSegmentEndPositionExplorer: IntSequenceExplorer,
    flipFirstSegment: Boolean,
    secondSegmentStartPositionExplorer: IntSequenceExplorer,
    secondSegmentEndPositionExplorer: IntSequenceExplorer,
    flipSecondSegment: Boolean
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
        flipSecondSegment
      )
    } else if (firstEndPosition < secondStartPosition) {
      // move lowest segment upward just before the second one (so that its indices do not change)
      move(
        firstSegmentStartPositionExplorer,
        firstSegmentEndPositionExplorer,
        secondSegmentStartPositionExplorer.prev,
        flipFirstSegment
      )
      // them bring the upward one lower
      move(
        secondSegmentStartPositionExplorer,
        secondSegmentEndPositionExplorer,
        firstSegmentStartPositionExplorer.prev,
        flipSecondSegment
      )
    } else {
      // swap them
      swapSegments(
        secondSegmentStartPositionExplorer,
        secondSegmentEndPositionExplorer,
        flipSecondSegment,
        firstSegmentStartPositionExplorer,
        firstSegmentEndPositionExplorer,
        flipFirstSegment
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

  def setValue(seq: IntSequence): Unit = {
    require(
      performedSinceTopCheckpoint == null &&
        levelOfTopCheckpoint == -1,
      "Sequences cannot be assigned when a checkpoint has been defined"
    )

    if (!(toNotify.newValue quickEquals seq)) {
      toNotify = SeqUpdateAssign(seq)
      scheduleForPropagation()
    }
  }

  // stack does not include top checkpoint
  // tuple is as follows:
  //  checkpointValue,
  //  the update that led to this value from the previous checkpoint

  private[this] var levelOfTopCheckpoint: Int = -1

  // can be null if no checkpoint
  private[this] var topCheckpoint: IntSequence                            = null
  private[this] var checkpointStackNotTop: List[(IntSequence, SeqUpdate)] = List.empty

  // what has been performed on the newValue after the current checkpoint (not maintained if checkpoint is circle mode, or if the number of updates gets too large)
  private[this] var performedSinceTopCheckpoint: SeqUpdate = null

  /** to define the current value as a checkpoint
    */
  def defineCurrentValueAsCheckpoint(): IntSequence = {
    // we do not use the record function because it also records stuff for the checkpoint stack
    toNotify =
      SeqUpdateDefineCheckpoint(toNotify, maxPivotPerValuePercent, levelOfTopCheckpoint + 1)

    if (topCheckpoint != null) {
      checkpointStackNotTop = (topCheckpoint, performedSinceTopCheckpoint) :: checkpointStackNotTop
    }
    topCheckpoint = toNotify.newValue // this one was regularized if needed, btw
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

  /** releases the top checkpoint
    * @note
    *   You do not need to be at the top checkpoint value to call this, you can do it later no
    *   worries.
    */
  def releaseTopCheckpoint(): Unit = {
    require(topCheckpoint != null, "No checkpoint defined to release")
    require(levelOfTopCheckpoint >= 0)

    // the checkpoint might not have been communicated yet, so we look for newValue, since we are at the checkpoint.
    val checkPointWipedOut =
      removeCheckpointDeclarationIfPresent(toNotify, topCheckpoint) match {
        case CheckpointDeclarationReachedAndRemoved(newToNotify: SeqUpdate) =>
          toNotify = newToNotify
          true
        case CheckpointReachedNotRemoved(newToNotify: SeqUpdate) =>
          require(false, "unexpected internal result")
          false
        case NoSimplificationPerformed =>
          // there is nothing to do here because the checkpoint has been communicated anyway,
          false
      }

    checkpointStackNotTop match {
      case top :: tail =>
        require(levelOfTopCheckpoint > 0)
        checkpointStackNotTop = tail
        topCheckpoint = top._1
        performedSinceTopCheckpoint =
          if (performedSinceTopCheckpoint != null && top._2 != null)
            performedSinceTopCheckpoint.appendThisTo(top._2)
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

  def releaseTopCheckpointsToLevel(level: Int, included: Boolean): Unit = {
    if (included) {
      while (levelOfTopCheckpoint >= level) {
        releaseTopCheckpoint()
      }
    } else {
      while (levelOfTopCheckpoint > level) {
        releaseTopCheckpoint()
      }
    }
  }

  @inline
  final protected def performSeqPropagation(): Unit = {
    val dynListElements = getDynamicallyListeningElements
    dynListElements.foreach(pe => {
      val inv: SeqNotificationTarget = pe.asInstanceOf[SeqNotificationTarget]
      inv.notifySeqChanges(this, pe.id, toNotify)
    })
    mOldValue = toNotify.newValue
    toNotify = SeqUpdateLastNotified(mOldValue)
  }

  def :=(seq: IntSequence): Unit = {
    setValue(seq)
    scheduleForPropagation()
  }

  def createClone(maxDepth: Int = 50): SeqVariable = {
    val clone = new SeqVariable(
      model,
      this.value.toList,
      Some(s"clone_of_${this.name}"),
      maxPivotPerValuePercent,
      isConstant
    )
    IdentitySeqInvariant(this, clone)
    clone
  }

  // CHECKPOINT STUFF

  private def removeAllCheckpointDefinitionAboveOrEqualLevel(
    updates: SeqUpdate,
    level: Int
  ): SeqUpdate = {
    updates match {
      case i @ SeqUpdateInsert(value: Int, pos: IntSequenceExplorer, prev: SeqUpdate) =>
        val newPrev = removeAllCheckpointDefinitionAboveOrEqualLevel(prev, level)
        if (newPrev == prev) updates
        else SeqUpdateInsert(value, pos, newPrev, i.newValue)

      case m @ SeqUpdateMove(
            fromIncluded: IntSequenceExplorer,
            toIncluded: IntSequenceExplorer,
            after: IntSequenceExplorer,
            flip: Boolean,
            prev: SeqUpdate
          ) =>
        val newPrev = removeAllCheckpointDefinitionAboveOrEqualLevel(prev, level)
        if (newPrev == prev) updates
        else SeqUpdateMove(fromIncluded, toIncluded, after, flip, newPrev, m.newValue)

      case r @ SeqUpdateRemove(position: IntSequenceExplorer, prev: SeqUpdate) =>
        val newPrev = removeAllCheckpointDefinitionAboveOrEqualLevel(prev, level)
        if (newPrev == prev) updates
        else SeqUpdateRemove(position, newPrev, r.newValue)

      case _: SeqUpdateAssign =>
        updates

      case _: SeqUpdateLastNotified =>
        updates

      case d @ SeqUpdateDefineCheckpoint(prev, defineLevel) =>
        if (level == defineLevel) {
          // this checkpoint def should be removed, and we know that there is no checkpoint with a level higher than this one later on
          prev
        } else if (defineLevel > level) {
          // checkpoint should be removed, and there might be checkpoints non communicated with level higher than this one, so we recurse
          removeAllCheckpointDefinitionAboveOrEqualLevel(prev, level)
        } else {
          // checkpoint should not be removed, and we do not need to pursue checkpoint cleaning
          d
        }

      case _: SeqUpdateRollBackToCheckpoint =>
        // we leave it, it is a leaf anyway
        updates

      case x =>
        throw new Error(s"Unhanded match $x")
    }
  }

  abstract class CleaningResult

  class SimplificationPerformed(val cleaned: SeqUpdate) extends CleaningResult

  private case class CheckpointDeclarationReachedAndRemoved(newToNotify: SeqUpdate)
      extends SimplificationPerformed(newToNotify)

  private case class CheckpointReachedNotRemoved(newToNotify: SeqUpdate)
      extends SimplificationPerformed(newToNotify)

  private case object NoSimplificationPerformed extends CleaningResult

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
            s"Latest defined checkpoint ${updates.newValue} got targeted checkpoint ${searchedCheckpoint}"
        )
        CheckpointReachedNotRemoved(updates)

      case SeqUpdateRollBackToCheckpoint(checkpointValue: IntSequence, _: Int) =>
        if (checkpointValue quickEquals searchedCheckpoint)
          CheckpointReachedNotRemoved(updates)
        else NoSimplificationPerformed

      case _ =>
        NoSimplificationPerformed // Default case
    }
  }

  def removeCheckpointDeclarationIfPresent(
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

      case SeqUpdateAssign(value: IntSequence) =>
        NoSimplificationPerformed

      case SeqUpdateLastNotified(value: IntSequence) =>
        // check for equality
        NoSimplificationPerformed

      case SeqUpdateDefineCheckpoint(prev: SeqUpdate, level: Int) =>
        // here
        require(
          updates.newValue quickEquals searchedCheckpoint,
          "require fail on quick equals: " + updates.newValue + "should== " + searchedCheckpoint
        )
        CheckpointDeclarationReachedAndRemoved(prev)

      case SeqUpdateRollBackToCheckpoint(checkpointValue: IntSequence, level: Int) =>
        NoSimplificationPerformed

      case _ =>
        NoSimplificationPerformed // Default case
    }
  }

  // TODO
  /** Save the state of this variable */
  override def save(): SavedValue = ???

  /** this is the propagation method that should be overridden by propagation elements. notice that
   * it is only called in a propagation wave if: 1L: it has been registered for propagation since
   * the last time it was propagated 2L: it is included in the propagation wave: partial
   * propagation wave do not propagate all propagation elements; it only propagates the ones that
   * come in the predecessors of the targeted propagation element overriding this method is
   * optional, so an empty body is provided by default
   */
  override def performPropagation(): Unit = ???
}
