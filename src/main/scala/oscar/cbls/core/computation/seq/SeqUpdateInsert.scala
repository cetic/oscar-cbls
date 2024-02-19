package oscar.cbls.core.computation.seq

import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer}

object SeqUpdateInsert {
  def apply(value : Int, insertAfterPositionExplorer : IntSequenceExplorer, prev : SeqUpdate, seq : IntSequence) : SeqUpdate = {
    prev match {
      //we compare the seq here because seq equality is used for checkpointing stuff to annihilate the moves
      case x@SeqUpdateRemove(removedPositionExplorer : IntSequenceExplorer, prevOfDelete : SeqUpdate)
        if prevOfDelete.newValue quickEquals seq => prevOfDelete
      case _ => new SeqUpdateInsert(value,insertAfterPositionExplorer,prev,seq)
    }
  }

  /**
   * @param value
   * @param pos the position of the insert, what comes upwards ad at this position is moved by one pos upwards
   * @param prev
   * @return
   */
  def apply(value : Int, insertAfterPositionExplorer : IntSequenceExplorer, prev : SeqUpdate) : SeqUpdate = {
    prev match {
      //here, since there is no seq given, we compare on the move itself to anihilate the moves
      case x@SeqUpdateRemove(removedPositionExplorer : IntSequenceExplorer, prevOfDelete : SeqUpdate)
        if insertAfterPositionExplorer.position+1 == removedPositionExplorer.position && value == x.removedValue => prevOfDelete
      case _ => new SeqUpdateInsert(value,insertAfterPositionExplorer,prev,prev.newValue.insertAfterPosition(value, insertAfterPositionExplorer, fast = true))
    }
  }

  /**
   * @param i
   * @return value, position, prev
   */
  def unapply(i:SeqUpdateInsert):Option[(Int,IntSequenceExplorer,SeqUpdate)] = Some(i.value,i.insertAfterPositionExplorer,i.prev)
}

//after is -1 for start position
class SeqUpdateInsert(val value: Int, val insertAfterPositionExplorer: IntSequenceExplorer, prev:SeqUpdate, seq:IntSequence)
  extends SeqUpdateWithPrev(prev:SeqUpdate, seq){

  lazy val insertionPos: Int = insertAfterPositionExplorer.next.position

  override protected[computation] def reverseThis(newValueForThisAfterFullReverse: IntSequence, nextOp:SeqUpdate): SeqUpdate = {
    prev.reverseThis(newValueForThisAfterFullReverse, SeqUpdateRemove(insertAfterPositionExplorer.next.position,nextOp,prev.newValue))
  }

  override protected[computation] def appendThisTo(previousUpdates: SeqUpdate): SeqUpdate = {
    SeqUpdateInsert(value: Int, insertAfterPositionExplorer: IntSequenceExplorer, prev.appendThisTo(previousUpdates), seq)
  }

  override protected[computation] def explicitHowToRollBack(): SeqUpdate = {
    SeqUpdateInsert(value: Int, insertAfterPositionExplorer: IntSequenceExplorer, prev.explicitHowToRollBack(), seq)
  }

  override def oldPosToNewPos(oldPos : Int) : Option[Int] = {
    if (oldPos < insertAfterPositionExplorer.next.position) Some(oldPos)
    else Some(oldPos + 1)
  }

  override def newPos2OldPos(newPos : Int) : Option[Int] = {
    if(newPos == insertAfterPositionExplorer.next.position) None
    else if (newPos < pos) Some(newPos)
    else Some(newPos-1)
  }

  override protected[computation] def regularize(maxPivot:Int) : SeqUpdate =
    SeqUpdateInsert(value,pos,prev,seq.regularizeToMaxPivot(maxPivot))

  override def toString : String = s"SeqUpdateInsert(value:$value position:$pos prev:$prev)"
}
