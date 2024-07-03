package oscar.cbls.core.computation.seq

import oscar.cbls.algo.sequence.IntSequence

object SeqUpdateReleaseTopCheckPoint {
  def apply(prev: SeqUpdate, seq: IntSequence): SeqUpdateReleaseTopCheckPoint = {
    new SeqUpdateReleaseTopCheckPoint(prev, seq)
  }

  def unapply(seqUpdateReleaseTopCheckPoint: SeqUpdateReleaseTopCheckPoint): Option[(SeqUpdate, IntSequence)] ={
    Some((seqUpdateReleaseTopCheckPoint.prev,seqUpdateReleaseTopCheckPoint.newValue))
  }

}

class SeqUpdateReleaseTopCheckPoint(prev: SeqUpdate, seq: IntSequence) extends SeqUpdateWithPrev(prev,seq) {

  override def oldPosToNewPos(oldPos: Int): Option[Int] = ???

  override def newPos2OldPos(newPos: Int): Option[Int] = ???

  override protected[seq] def reverseThis(expectedValueAfterFullReverse: IntSequence, updatesAlreadyReversed: SeqUpdate): SeqUpdate = ???

  override protected[seq] def appendThisTo(previousUpdates: SeqUpdate): SeqUpdate = ???

  override protected[seq] def regularize(maxPivot: Int): SeqUpdate = SeqUpdateReleaseTopCheckPoint(prev, seq.regularizeToMaxPivot(maxPivot))

  override def toString: String =
    s"SeqUpdateReleaseTopCheckPoint(checkpoint:$seq prev:$prev)"
}
