package oscar.cbls.core.computation.seq

import oscar.cbls.algo.sequence.IntSequence

object SeqUpdateDefineCheckpoint {

  def apply(prev: SeqUpdate, maxPivotPerValuePercent: Int, level: Int):SeqUpdateDefineCheckpoint = {
    val doRegularize = level == 0
    val newPrev = if(doRegularize) prev.regularize(maxPivotPerValuePercent) else prev
    new SeqUpdateDefineCheckpoint(newPrev, maxPivotPerValuePercent, level)
  }

  def unapply(u:SeqUpdateDefineCheckpoint):Option[(SeqUpdate,Int)] = Some(u.prev,u.level)
}

class SeqUpdateDefineCheckpoint(prev: SeqUpdate, maxPivotPerValuePercent: Int, val level: Int) extends SeqUpdateWithPrev(prev, prev.newValue) {

  override protected[computation] def reverseThis(newValueForThisAfterFullReverse: IntSequence, nextOp: SeqUpdate): SeqUpdate = {
    require(nextOp.newValue quickEquals this.newValue)
    prev.reverseThis(newValueForThisAfterFullReverse, SeqUpdateDefineCheckpoint(nextOp, maxPivotPerValuePercent, level))
  }

  override protected[computation] def appendThisTo(previousUpdates: SeqUpdate): SeqUpdate = {
    SeqUpdateDefineCheckpoint(prev.appendThisTo(previousUpdates), maxPivotPerValuePercent,level)
  }

  override protected[computation] def explicitHowToRollBack(): SeqUpdate = {
    SeqUpdateDefineCheckpoint(prev.explicitHowToRollBack(), maxPivotPerValuePercent,level)
  }

  protected[computation] def regularize(maxPivot:Int) : SeqUpdate = this

  // TODO : Is it the right way ?
  def oldPosToNewPos(oldPos : Int) : Option[Int] = throw new Error("SeqUpdateDefineCheckpoint should not be queried for delta on moves")

  def newPos2OldPos(newPos : Int) : Option[Int] = throw new Error("SeqUpdateDefineCheckpoint should not be queried for delta on moves")

  override def toString : String = s"SeqUpdateDefineCheckpoint(level:$level prev:$prev)"
}
