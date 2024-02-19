package oscar.cbls.core.computation.seq

import oscar.cbls.algo.sequence.IntSequence
import oscar.cbls.core.computation.{Store, Variable}

class SeqVariable(model: Store, initialValues: List[Int], name: Option[String] = None, isConstant: Boolean = false) extends Variable(model,isConstant) {
  require(model != null)

  private val mOldValue: IntSequence = IntSequence(initialValues)
  private var toNotify: SeqUpdate = SeqUpdateLastNotified(mOldValue)

  override def checkInternals(): Unit ={
    require(this.value.toList equals this.newValue.toList)
    require(this.toNotify.isInstanceOf[SeqUpdateLastNotified], Some(s"toNotify:$toNotify"))
  }

  override def createCheckpoint: VariableCheckpoint =
    SeqVarCheckpoint(this,this.defineCurrentValueAsCheckpoint())

  def name: String = name.getOrElse("SeqVariable")

  def value: IntSequence = {
    if (model == null) return mOldValue
    val propagating = model.propagating
    if (definingInvariant == null && !propagating) return toNotify.newValue
    if(!propagating) model.propagate(this)
    mOldValue
  }

  protected def insertAtPosition(value:Int,pos:Int): Unit ={
    require(pos <= toNotify.newValue.size)
    require(pos >= 0)
    recordPerformedIncrementalUpdate((prev,newSeq) =>
      if(newSeq == null) SeqUpdateInsert(value,pos,prev)
      else SeqUpdateInsert(value,pos,prev,newSeq))
    //model.notifyChanged()
  }

  protected def insertAtPosition(value:Int,pos:Int,seqAfter:IntSequence): Unit ={
    require(pos <= toNotify.newValue.size)
    require(pos >= 0)
    recordPerformedIncrementalUpdate((prev,_) => SeqUpdateInsert(value,pos,prev,seqAfter))
    //notifyChanged()
  }

  protected def remove(position:Int): Unit ={
    require(toNotify.newValue.size > position && position >=0,
      s"removing at position $position size is ${newValue.size}")
    recordPerformedIncrementalUpdate((prev,newSeq) =>
      if(newSeq == null) SeqUpdateRemove(position, prev)
      else SeqUpdateRemove(position, prev,newSeq))
    notifyChanged()
  }

  protected def remove(position:Int,seqAfter:IntSequence): Unit ={
    require(toNotify.newValue.size > position && position >=0,
      s"removing at position $position size is ${newValue.size}")
    recordPerformedIncrementalUpdate((prev,_) => SeqUpdateRemove(position,prev,seqAfter))
    notifyChanged()
  }

  protected def flip(fromIncludedPosition:Int,toIncludedPosition:Int): Unit ={
    move(fromIncludedPosition,toIncludedPosition,fromIncludedPosition-1,true)
  }

  //-1 for first position
  protected def move(fromIncludedPosition:Int,toIncludedPosition:Int,afterPosition:Int,flip:Boolean): Unit ={
    require(toNotify.newValue.size > toIncludedPosition)
    require(toNotify.newValue.size > afterPosition,
      s"toNotify.newValue.size(=${toNotify.newValue.size}) > afterPosition(=$afterPosition)")
    require(0 <= fromIncludedPosition,
      s"move with fromIncludedPosition=$fromIncludedPosition")
    require(-1<=afterPosition)
    require(fromIncludedPosition <= toIncludedPosition,
      s"fromIncludedPosition=$fromIncludedPosition should <= toIncludedPosition=$toIncludedPosition")
    require(
      afterPosition < fromIncludedPosition || afterPosition > toIncludedPosition,
      s"afterPosition=$afterPosition cannot be between fromIncludedPosition=$fromIncludedPosition and toIncludedPosition=$toIncludedPosition")

    recordPerformedIncrementalUpdate((prev,newSeq) =>
      if(newSeq == null) SeqUpdateMove(fromIncludedPosition,toIncludedPosition,afterPosition,flip,prev)
      else SeqUpdateMove(fromIncludedPosition,toIncludedPosition,afterPosition,flip,prev,newSeq))

    notifyChanged()
  }

  //-1 for first position
  protected def move(fromIncludedPosition:Int,toIncludedPosition:Int,afterPosition:Int,flip:Boolean,seqAfter:IntSequence): Unit ={

    require(toNotify.newValue.size > fromIncludedPosition)
    require(toNotify.newValue.size > toIncludedPosition)
    require(toNotify.newValue.size > afterPosition)
    require(0 <= fromIncludedPosition)
    require(0<=toIncludedPosition)
    require(-1<=afterPosition)
    require(fromIncludedPosition <= toIncludedPosition)

    recordPerformedIncrementalUpdate((prev,_) =>
      SeqUpdateMove(fromIncludedPosition,toIncludedPosition,afterPosition,flip,prev,seqAfter))

    notifyChanged()
  }

  protected def swapSegments(firstSegmentStartPosition:Int,
                             firstSegmentEndPosition:Int,
                             flipFirstSegment:Boolean,
                             secondSegmentStartPosition:Int,
                             secondSegmentEndPosition:Int,
                             flipSecondSegment:Boolean): Unit ={

    require(firstSegmentStartPosition <= firstSegmentEndPosition)
    require(secondSegmentStartPosition <= secondSegmentEndPosition)

    if (firstSegmentEndPosition == secondSegmentStartPosition - 1) {
      // segments are contiguous, we only need to move the second segment
      move(secondSegmentStartPosition,secondSegmentEndPosition,firstSegmentStartPosition-1,flipSecondSegment)

    }else if(firstSegmentEndPosition < secondSegmentStartPosition){
      //do it

      //move lowest segment upward just before the second one (so that its indices do not change)
      move(firstSegmentStartPosition,firstSegmentEndPosition,secondSegmentStartPosition-1,flipFirstSegment)

      //them bring the upward one lower
      move(secondSegmentStartPosition,secondSegmentEndPosition,firstSegmentStartPosition-1,flipSecondSegment)

    }else{
      require(secondSegmentEndPosition < firstSegmentStartPosition)
      //swap them
      swapSegments(
        secondSegmentStartPosition, secondSegmentEndPosition, flipSecondSegment,
        firstSegmentStartPosition, firstSegmentEndPosition, flipFirstSegment)
    }
  }

  @inline
  private final def recordPerformedIncrementalUpdate(updatefct:(SeqUpdate,IntSequence) => SeqUpdate): Unit ={
    //for notification recording
    toNotify = updatefct(toNotify,null)

    if(performedSinceTopCheckpoint != null) {
      performedSinceTopCheckpoint = updatefct(performedSinceTopCheckpoint, toNotify.newValue)
    }else{
      //if it is null, it means that no checkpoint was declared.
      require(currentCheckpointLevel == -1)
    }
  }

  protected [computation] def setValue(seq:IntSequence): Unit ={
    require(
      performedSinceTopCheckpoint == null &&
        !toNotify.anyCheckpointDefinition &&
        levelOfTopCheckpoint == -1,
      "Sequences cannot be assigned when a checkpoint has been defined")

    if(!(this.newValue quickEquals seq)) {
      toNotify = SeqUpdateAssign(seq)
      notifyChanged()
    }
  }

  // checkpoint management values

  //This section of code is for maintaining the checkpoint stack.
  //stack does not include top checkpoint
  //triplet is as follows:
  //  checkpointValue,
  //  the update that led to this value from the previous checkpoint
  //  true if star mode, false if circle mode

  //this is about the performed stuff, on the neighborhood side and also covers the notified side
  private[this] var levelOfTopCheckpoint:Int = -1
  def currentCheckpointLevel = levelOfTopCheckpoint

  //can be null if no checkpoint
  private[this] var topCheckpoint : IntSequence = null
  private[this] var checkpointStackNotTop : List[(IntSequence, SeqUpdate)] = List.empty

  //what has been performed on the newValue after the current checkpoint (not maintained if checkpoint is circle mode, or if the number of updates gets too large)
  private[this] var performedSinceTopCheckpoint : SeqUpdate = null

  def getTopCheckpoint : IntSequence = topCheckpoint

  /**
   * to define the current value as a checkpoint
   */
  protected def defineCurrentValueAsCheckpoint(): IntSequence = {
    //we do not use the record function because it also records stuff for the checkpoint stack
    toNotify =
      SeqUpdateDefineCheckpoint(toNotify, maxPivotPerValuePercent, levelOfTopCheckpoint+1)

    if(topCheckpoint != null){
      checkpointStackNotTop = (topCheckpoint,performedSinceTopCheckpoint) :: checkpointStackNotTop
    }
    topCheckpoint = toNotify.newValue //this one was regularized if needed, btw
    levelOfTopCheckpoint += 1

    performedSinceTopCheckpoint = SeqUpdateLastNotified(topCheckpoint)

    notifyChanged()
    toNotify.newValue
  }

  protected def rollbackToTopCheckpoint(checkpoint : IntSequence): Unit ={

    require(checkpoint quickEquals topCheckpoint,
      s"given checkpoint not quick equals to my top checkpoint; equal=${checkpoint equals topCheckpoint} checkpoint:$checkpoint my topCheckpoint:$topCheckpoint")

    popToNotifyUntilCheckpointDeclaration(toNotify,topCheckpoint,removeDeclaration = false) match{
      case CheckpointDeclarationReachedAndRemoved(newToNotify:SeqUpdate) =>
        //error, we asked removeDeclaration = false
        throw new Error("unexpected result")
      case SeqUpdatesCleanedUntilQuickEqualValueReachedCheckpointDeclarationNotRemoved(newToNotify:SeqUpdate) =>
        //checkpoint value could be found in toNotify, and updated after it were removed so we don't have to do anything
        require(newToNotify.newValue quickEquals checkpoint,
          s"${newToNotify.newValue} not quickEquals $checkpoint")

        //we are at the checkpoint declaration, and it has not been communicated yet,
        // so we know that this is already scheduled for propagation unless it has never been scheduled because there was nothing to communicate
        require(this.isScheduled || toNotify.isInstanceOf[SeqUpdateLastNotified])
        performedSinceTopCheckpoint = SeqUpdateLastNotified(topCheckpoint)

        toNotify = newToNotify

      case NoSimplificationPerformed =>
        //in this case, the checkpoint was already notified, and possibly some moves were performed from it.
        require(!toNotify.anyCheckpointDefinition)

        //checkpoint value could not be found in sequence, we have to add rollBack instructions
        //It also means that the checkpoint was communicated to the listening side

        val howToRollBack = performedSinceTopCheckpoint.reverseThis(newValueForThisAfterFullReverse = topCheckpoint).appendThisTo(toNotify.explicitHowToRollBack())
        toNotify = SeqUpdateRollBackToCheckpoint(
          checkpoint,
          howToRollBack = howToRollBack,
          level = levelOfTopCheckpoint)

        performedSinceTopCheckpoint = SeqUpdateLastNotified(topCheckpoint)
        notifyChanged()
    }

    require(toNotify.newValue quickEquals checkpoint,
      s"${toNotify.newValue} not quickEquals $checkpoint")
  }

  /**
   * releases the top checkpoint
   * @note You do not need to be at the top checkpoint value to call this, you can do it later no worries.
   */
  protected def releaseTopCheckpoint(): Unit ={
    require(topCheckpoint != null, "No checkpoint defined to release")
    require(levelOfTopCheckpoint >= 0)

    //the checkpoint might not have been communicated yet, so we look for newValue, since we are at the checkpoint.
    val checkPointWipedOut =
      removeCheckpointDeclarationIfPresent(toNotify,topCheckpoint) match{
        case CheckpointDeclarationReachedAndRemoved(newToNotify:SeqUpdate) =>
          //we wipe out this checkpoint from history
          toNotify = newToNotify
          true //checkpointWipedout
        case SeqUpdatesCleanedUntilQuickEqualValueReachedCheckpointDeclarationNotRemoved(newToNotify:SeqUpdate) =>
          throw new Error("unexpected internal result")
          false //checkpointWipedout
        case NoSimplificationPerformed =>
          //there is nothing to do here because the checkpoint has been communicated anyway,
          // and we are in the latest checkpoint fashion where checkpoints are implicitly released when a new one is communicated
          false //checkpoint not WipedOut
      }

    //in all cases, we must pop the checkpoint from the checkpoint stack since it is working on the NewValues
    checkpointStackNotTop match{
      case top :: tail =>
        require(levelOfTopCheckpoint > 0)
        checkpointStackNotTop = tail
        topCheckpoint = top._1
        performedSinceTopCheckpoint = if(performedSinceTopCheckpoint != null && top._2!= null) performedSinceTopCheckpoint.appendThisTo(top._2) else null
        levelOfTopCheckpoint -= 1
      case Nil =>
        //there is no upper checkpoint
        require(levelOfTopCheckpoint == 0)
        levelOfTopCheckpoint = -1
        topCheckpoint = null
        performedSinceTopCheckpoint = null
    }
  }

  protected def releaseTopCheckpointsToLevel(level:Int,included:Boolean): Unit ={
    if(included) {
      while (levelOfTopCheckpoint >= level) {
        releaseTopCheckpoint()
      }
    }else{
      while (levelOfTopCheckpoint > level) {
        releaseTopCheckpoint()
      }
    }
  }

  @inline
  final protected def performSeqPropagation(): Unit ={
    val dynListElements = getDynamicallyListeningElements
    val headPhantom = dynListElements.headPhantom
    var currentElement = headPhantom.next

    while (currentElement != headPhantom) {
      val e = currentElement.elem
      val inv : SeqNotificationTarget = e._1.asInstanceOf[SeqNotificationTarget]
      assert({
        this.model.notifiedInvariant = inv.asInstanceOf[Invariant]; true
      })
      inv.notifySeqChanges(this, e._2, toNotify)
      assert({
        this.model.notifiedInvariant = null; true
      })

      //we go to the next to be robust against invariant that change their dependencies when notified
      //this might cause crash because dynamicallyListenedInvariants is a mutable data structure
      currentElement = currentElement.next
    }

    mOldValue = toNotify.newValue
    toNotify = SeqUpdateLastNotified(mOldValue)
  }

  protected def :=(seq:IntSequence): Unit ={
    setValue(seq)
    notifyChanged()
  }

  def createClone(maxDepth:Int=50):CBLSSeqVar = {
    val clone = new CBLSSeqVar(model,this.value,this.maxValue,s"clone_of_${this.name}",maxPivotPerValuePercent,maxDepth)
    IdentitySeq(this,clone)
    clone
  }

  // CHECKPOINT STUFF

  private def removeAllCheckpointDefinitionAboveOrEqualLevel(updates:SeqUpdate, level:Int):SeqUpdate = {
    updates match {
      case i@SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        val newPrev = removeAllCheckpointDefinitionAboveOrEqualLevel(prev, level)
        if(newPrev == prev) updates
        else SeqUpdateInsert(value, pos, newPrev,i.newValue)

      case m@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        val newPrev = removeAllCheckpointDefinitionAboveOrEqualLevel(prev, level)
        if(newPrev == prev) updates
        else SeqUpdateMove(fromIncluded, toIncluded, after, flip, newPrev, m.newValue)

      case r@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        val newPrev = removeAllCheckpointDefinitionAboveOrEqualLevel(prev, level)
        if(newPrev == prev) updates
        else SeqUpdateRemove(position, newPrev, r.newValue)

      case _:SeqUpdateAssign =>
        updates

      case _:SeqUpdateLastNotified =>
        updates

      case d@SeqUpdateDefineCheckpoint(prev, defineLevel) =>
        if(level == defineLevel) {
          //this checkpoint def should be removed, and we know that there is no checkpoint with a level higher than this one later on
          prev
        }else if (defineLevel > level){
          //checkpoint should be removed, and there might be checkpoints non communicated with level higher than this one, so we recurse
          removeAllCheckpointDefinitionAboveOrEqualLevel(prev, level)
        }else{
          //checkpoint should not be removed, and we do not need to pursue checkpoint cleaning
          d
        }

      case _:SeqUpdateRollBackToCheckpoint =>
        //we leave it, it is a leaf anyway
        updates

      case x =>
        throw new Error(s"Unhanded match $x")
    }
  }

  abstract class CleaningResult

  class SimplificationPerformed(val cleaned:SeqUpdate)
    extends CleaningResult

  case class CheckpointDeclarationReachedAndRemoved(newToNotify:SeqUpdate)
    extends SimplificationPerformed(newToNotify)

  case class SeqUpdatesCleanedUntilQuickEqualValueReachedCheckpointDeclarationNotRemoved(newToNotify:SeqUpdate)
    extends SimplificationPerformed(newToNotify)

  case object NoSimplificationPerformed
    extends CleaningResult

  /**
   * pops the updates until the searched checkpoint is reached, base on token comparison
   * @param updates
   * @return CleaningResult according to the performed cleaning
   */
  private def popToNotifyUntilCheckpointDeclaration(updates:SeqUpdate,
                                                    searchedCheckpoint:IntSequence,
                                                    removeDeclaration:Boolean):CleaningResult = {
    updates match {
      case SeqUpdateInsert(value:Int,pos:Int,prev:SeqUpdate) =>
        popToNotifyUntilCheckpointDeclaration(prev,searchedCheckpoint,removeDeclaration) match{
          case NoSimplificationPerformed =>
            if (searchedCheckpoint quickEquals updates.newValue)
              SeqUpdatesCleanedUntilQuickEqualValueReachedCheckpointDeclarationNotRemoved(updates)
            else NoSimplificationPerformed
          case x => x
        }

      case SeqUpdateMove(fromIncluded:Int,toIncluded:Int,after:Int,flip:Boolean,prev:SeqUpdate) =>
        popToNotifyUntilCheckpointDeclaration(prev,searchedCheckpoint,removeDeclaration) match{
          case NoSimplificationPerformed =>
            if (searchedCheckpoint quickEquals updates.newValue)
              SeqUpdatesCleanedUntilQuickEqualValueReachedCheckpointDeclarationNotRemoved(updates)
            else NoSimplificationPerformed
          case x => x
        }

      case SeqUpdateRemove(position:Int,prev:SeqUpdate) =>
        popToNotifyUntilCheckpointDeclaration(prev,searchedCheckpoint,removeDeclaration) match{
          case NoSimplificationPerformed =>
            if (searchedCheckpoint quickEquals updates.newValue)
              SeqUpdatesCleanedUntilQuickEqualValueReachedCheckpointDeclarationNotRemoved(updates)
            else NoSimplificationPerformed
          case x => x
        }

      case SeqUpdateAssign(value:IntSequence) =>
        if(value quickEquals searchedCheckpoint)
          SeqUpdatesCleanedUntilQuickEqualValueReachedCheckpointDeclarationNotRemoved(updates)
        else NoSimplificationPerformed

      case SeqUpdateLastNotified(value:IntSequence) =>
        //check for equality
        if(value quickEquals searchedCheckpoint)
          SeqUpdatesCleanedUntilQuickEqualValueReachedCheckpointDeclarationNotRemoved(updates)
        else NoSimplificationPerformed

      case SeqUpdateDefineCheckpoint(prev,level) =>
        //here
        //TODO: not sure that this is the same checkpoint
        require(updates.newValue quickEquals searchedCheckpoint,
          s"require fail on quick equals (equals=${updates.newValue equals searchedCheckpoint}): ${updates.newValue}should== $searchedCheckpoint")

        if(removeDeclaration) {
          CheckpointDeclarationReachedAndRemoved(prev)
        }else{
          SeqUpdatesCleanedUntilQuickEqualValueReachedCheckpointDeclarationNotRemoved(updates)
        }

      case SeqUpdateRollBackToCheckpoint(checkpointValue:IntSequence, level:Int) =>
        if(checkpointValue quickEquals searchedCheckpoint)
          SeqUpdatesCleanedUntilQuickEqualValueReachedCheckpointDeclarationNotRemoved(updates)
        else NoSimplificationPerformed

      case _ =>
        NoSimplificationPerformed // Default case
    }
  }

  def removeCheckpointDeclarationIfPresent(updates:SeqUpdate,searchedCheckpoint:IntSequence):CleaningResult = {
    updates match{
      case SeqUpdateInsert(value:Int,pos:Int,prev:SeqUpdate) =>
        removeCheckpointDeclarationIfPresent(prev,searchedCheckpoint) match{
          case NoSimplificationPerformed => NoSimplificationPerformed
          case CheckpointDeclarationReachedAndRemoved(newPrev) =>
            CheckpointDeclarationReachedAndRemoved(
              SeqUpdateInsert(value,pos,newPrev,updates.newValue))
          case _ => throw new Error("unexpected match")
        }

      case SeqUpdateMove(fromIncluded:Int,toIncluded:Int,after:Int,flip:Boolean,prev:SeqUpdate) =>
        removeCheckpointDeclarationIfPresent(prev,searchedCheckpoint) match{
          case NoSimplificationPerformed => NoSimplificationPerformed
          case CheckpointDeclarationReachedAndRemoved(newPrev) =>
            CheckpointDeclarationReachedAndRemoved(
              SeqUpdateMove(fromIncluded,toIncluded,after,flip,newPrev,updates.newValue)
            )
          case _ => throw new Error("unexpected match")
        }

      case SeqUpdateRemove(position:Int,prev:SeqUpdate) =>
        removeCheckpointDeclarationIfPresent(prev,searchedCheckpoint) match{
          case NoSimplificationPerformed => NoSimplificationPerformed
          case CheckpointDeclarationReachedAndRemoved(newPrev) =>
            CheckpointDeclarationReachedAndRemoved(
              SeqUpdateRemove(position,newPrev,updates.newValue)
            )
          case _ => throw new Error("unexpected match")
        }

      case SeqUpdateAssign(value:IntSequence) =>
        NoSimplificationPerformed

      case SeqUpdateLastNotified(value:IntSequence) =>
        //check for equality
        NoSimplificationPerformed

      case SeqUpdateDefineCheckpoint(prev:SeqUpdate, level:Int) =>
        //here
        require(updates.newValue quickEquals searchedCheckpoint,
          "require fail on quick equals: " + updates.newValue + "should== " + searchedCheckpoint)
        CheckpointDeclarationReachedAndRemoved(prev)

      case SeqUpdateRollBackToCheckpoint(checkpointValue:IntSequence,level:Int) =>
        NoSimplificationPerformed

      case _ =>
        NoSimplificationPerformed // Default case
    }
  }
}