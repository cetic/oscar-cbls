package oscar.cbls.core.computation.seq

object SeqUpdateMove{
  def apply(fromIncluded:Int,toIncluded:Int,after:Int,flip:Boolean,prev:SeqUpdate):SeqUpdateMove =
    new SeqUpdateMove(fromIncluded,toIncluded,after,flip,prev,prev.newValue.moveAfter(fromIncluded,toIncluded,after,flip,fast=true))

  def apply(fromIncluded:Int,toIncluded:Int,after:Int,flip:Boolean,prev:SeqUpdate,seq:IntSequence):SeqUpdate = {
    prev match{
      case u:SeqUpdateMove if u.prev.newValue quickEquals seq => u.prev
      case _ => new SeqUpdateMove(fromIncluded,toIncluded,after, flip, prev, seq)
    }
  }

  /**
   * @param move
   * @return fromIncluded,toIncluded,after,flip,prev
   */
  def unapply(move:SeqUpdateMove):Option[(Int,Int,Int,Boolean,SeqUpdate)] =
    Some(move.fromIncluded,move.toIncluded,move.after,move.flip,move.prev)
}

class SeqUpdateMove(val fromIncluded:Int,val toIncluded:Int,val after:Int, val flip:Boolean, prev:SeqUpdate, seq:IntSequence)
  extends SeqUpdateWithPrev(prev,seq){

  assert(seq equals prev.newValue.moveAfter(fromIncluded,toIncluded,after,flip,fast=true),
    s"given seq=$seq should be ${prev.newValue.moveAfter(fromIncluded,toIncluded,after,flip,fast=true)}")

  def isSimpleFlip:Boolean = after+1 == fromIncluded && flip
  def isNop:Boolean = after+1 == fromIncluded && !flip
  //TODO: find someting faster.
  def fromValue:Int = prev.newValue.valueAtPosition(fromIncluded).head
  def toValue:Int = prev.newValue.valueAtPosition(toIncluded).head
  def afterValue:Int = prev.newValue.valueAtPosition(after).head
  def moveDownwards:Boolean = fromIncluded > after
  def moveUpwards:Boolean = fromIncluded < after
  def nbPointsInMovedSegment:Int = toIncluded - fromIncluded + 1

  def movedValuesSet: SortedSet[Int] = prev.newValue.valuesBetweenPositionsSet(fromIncluded,toIncluded)
  def movedValuesQList: QList[Int] = prev.newValue.valuesBetweenPositionsQList(fromIncluded,toIncluded)

  override protected[computation] def reverseThis(newValueForThisAfterFullReverse: IntSequence, nextOp: SeqUpdate): SeqUpdate = {
    val (intFromIncluded,intToIncluded) = if(flip) (toIncluded,fromIncluded) else (fromIncluded,toIncluded)

    prev.reverseThis(newValueForThisAfterFullReverse,
      SeqUpdateMove(
        oldPosToNewPosNoOopt(intFromIncluded),
        oldPosToNewPosNoOopt(intToIncluded),
        oldPosToNewPosNoOopt(fromIncluded-1),
        flip,
        nextOp,
        prev.newValue))
  }

  override protected[computation] def appendThisTo(previousUpdates: SeqUpdate): SeqUpdate = {
    SeqUpdateMove(fromIncluded,toIncluded,after,flip, prev.appendThisTo(previousUpdates), seq:IntSequence)
  }

  override protected[computation] def explicitHowToRollBack(): SeqUpdate = {
    SeqUpdateMove(fromIncluded,toIncluded,after,flip, prev.explicitHowToRollBack(), seq:IntSequence)
  }

  assert({seq match{case m:MovedIntSequence => m.localBijection.checkBijection() case _ => ;};true})

  //TODO: find O(1) solution
  private var localBijection:PiecewiseLinearBijectionNaive = null

  private def ensureBijection(): Unit ={
    if(localBijection == null) {
      localBijection = seq match{
        case m:MovedIntSequence
          if ((m.seq quickEquals prev.newValue) && m.startPositionIncluded == fromIncluded
            && m.endPositionIncluded == toIncluded && m.moveAfterPosition == after && m.flip == flip) =>
          m.localBijection
        case _ => MovedIntSequence.bijectionForMove(fromIncluded, toIncluded, after, flip)
      }
    }
  }

  @inline
  private def oldPosToNewPosNoOopt(oldPos : Int) : Int = {
    MovedIntSequence.oldPosToNewPos(oldPos : Int, fromIncluded:Int, toIncluded:Int, after:Int, flip:Boolean)
  }

  //TODO transposer çà dans IntSequece.MovedIntSequence
  override def oldPosToNewPos(oldPos : Int) : Option[Int] = {
    Some(oldPosToNewPosNoOopt(oldPos : Int))
  }

  override def newPos2OldPos(newPos : Int) : Option[Int] = {
    ensureBijection()
    Some(localBijection.forward(newPos))
  }

  override protected[computation] def regularize(maxPivot:Int) : SeqUpdate =
    SeqUpdateMove(fromIncluded,toIncluded,after,flip,prev,seq.regularizeToMaxPivot(maxPivot))

  override def toString : String =
    s"SeqUpdateMove(fromIncluded:$fromIncluded toIncluded:$toIncluded after:$after flip:$flip prev:$prev)"
}