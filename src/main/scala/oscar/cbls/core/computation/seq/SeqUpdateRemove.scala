package oscar.cbls.core.computation.seq

import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer}

object SeqUpdateRemove {

  def apply(removePosAsExplorer : IntSequenceExplorer, prev : SeqUpdate):SeqUpdate = {
    apply(removePosAsExplorer,prev,prev.newValue.remove(removePosAsExplorer, fast = true))
  }

  def apply(removePosAsExplorer : IntSequenceExplorer, prev : SeqUpdate, seq:IntSequence):SeqUpdate = {
    prev match {
      case SeqUpdateInsert(insertedValue:Int,insertAfterPositionExplorer:IntSequenceExplorer,insertPrev:SeqUpdate)
        if insertPrev.newValue quickEquals seq => insertPrev
      case _ => new SeqUpdateRemove(removePosAsExplorer,prev,seq)
    }
  }

  /**
   *
   * @param r
   * @return position,prev
   */
  def unapply(r:SeqUpdateRemove):Option[(IntSequenceExplorer,SeqUpdate)] = Some(r.explorerAtRemovePosition,r.prev)
}

class SeqUpdateRemove(val explorerAtRemovePosition:IntSequenceExplorer, prev:SeqUpdate, seq:IntSequence)
  extends SeqUpdateWithPrev(prev,seq){

  assert(seq equals prev.newValue.remove(explorerAtRemovePosition,fast=true),"wrong promize on seq value when building SeqUpdateRemove")

  val removedValue:Int = seq match{
    case d:RemovedIntSequence if explorerAtRemovePosition == d.positionOfDelete && (d.seq quickEquals prev.newValue) => d.removedValue
    case _ => prev.newValue.valueAtPosition(explorerAtRemovePosition).head}


  override protected[computation] def reverseThis(newValueForThisAfterFullReverse: IntSequence, nextOp: SeqUpdate): SeqUpdate = {
    prev.reverseThis(newValueForThisAfterFullReverse, SeqUpdateInsert(removedValue, explorerAtRemovePosition, nextOp, prev.newValue))
  }

  override protected[computation] def appendThisTo(previousUpdates: SeqUpdate): SeqUpdate = {
    SeqUpdateRemove(explorerAtRemovePosition,prev.appendThisTo(previousUpdates),seq)
  }

  override protected[computation] def explicitHowToRollBack(): SeqUpdate = {
    SeqUpdateRemove(explorerAtRemovePosition,prev.explicitHowToRollBack(),seq)
  }

  override def oldPosToNewPos(oldPos : Int) : Option[Int] = {
    if (oldPos == explorerAtRemovePosition) None
    else if (oldPos < explorerAtRemovePosition) Some(oldPos)
    else Some(oldPos-1)
  }

  override def newPos2OldPos(newPos : Int) : Option[Int] = {
    if(newPos < explorerAtRemovePosition) Some(newPos)
    else Some(newPos +1)
  }

  override protected[computation] def regularize(maxPivot:Int) : SeqUpdate =
    SeqUpdateRemove(explorerAtRemovePosition,prev,seq.regularizeToMaxPivot(maxPivot))

  override def toString : String =
    s"SeqUpdateRemove(value:$removedValue position:$explorerAtRemovePosition prev:$prev)"
}