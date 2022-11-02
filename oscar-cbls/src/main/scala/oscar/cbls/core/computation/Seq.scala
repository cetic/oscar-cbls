/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
package oscar.cbls.core.computation

import oscar.cbls.algo.fun.PiecewiseLinearBijectionNaive
import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.{ConcreteIntSequence, IntSequence, MovedIntSequence, RemovedIntSequence}
import oscar.cbls.core.propagation.Checker

import scala.collection.immutable.SortedSet

/*
 * Checkpoints must be defined and released manually by neighborhoods
 * you can release a checkpoint when not at the checkpoint
 * but you must respect the stack model of checkpoint
 * checkpoint releases are not included in the notifications,
 * but they can easily be inferred from the roll back and the defines.
 */

sealed trait SeqValue extends Value{
  def value:IntSequence
  def domain:Domain
  def min:Int = domain.min.toInt
  def max:Int = domain.max.toInt
  def name:String
  override final def valueString: String = value.toString
}

object SeqValue{
  implicit def tist2IntSeqVar(a:List[Int]):SeqValue = CBLSSeqConst(IntSequence(a))
}

sealed abstract class SeqUpdate(val newValue:IntSequence){
  protected[computation] def reverseThis(newValueForThisAfterFullReverse:IntSequence, nextOp:SeqUpdate = SeqUpdateLastNotified(this.newValue)):SeqUpdate
  protected[computation] def appendThisTo(previousUpdates:SeqUpdate):SeqUpdate
  protected[computation] def explicitHowToRollBack():SeqUpdate

  protected[computation] def regularize(maxPivot:Int):SeqUpdate

  def depth:Int

  final def anyCheckpointDefinition:Boolean = highestLevelOfDeclaredCheckpoint != -1
  def anyRollBack:Boolean

  /**the level of he highest declared checkpoint in this sequpdate nd its predecessors.
   * -1 if no declared checkpoints
   * */
  def highestLevelOfDeclaredCheckpoint:Int
}

sealed abstract class SeqUpdateWithPrev(val prev:SeqUpdate,newValue:IntSequence) extends SeqUpdate(newValue) {

  def anyRollBack:Boolean = prev.anyRollBack

  val highestLevelOfDeclaredCheckpoint: Int = prev.highestLevelOfDeclaredCheckpoint

  def oldPosToNewPos(oldPos:Int):Option[Int]
  def newPos2OldPos(newPos:Int):Option[Int]
  override val depth:Int = {
    val pd = prev.depth
    if(pd >=0) pd+1 else pd-1
  }
}

object SeqUpdateInsert {
  def apply(value : Int, pos : Int, prev : SeqUpdate, seq : IntSequence) : SeqUpdate = {
    prev match {
      //we compare the seq here because seq equality is used for checkpointing stuff to annihilate the moves
      case x@SeqUpdateRemove(removedPosition : Int, prevOfDelete : SeqUpdate)
        if prevOfDelete.newValue quickEquals seq => prevOfDelete
      case _ => new SeqUpdateInsert(value,pos,prev,seq)
    }
  }

  /**
   * @param value
   * @param pos the position of the insert, what comes upwards ad at this position is moved by one pos upwards
   * @param prev
   * @return
   */
  def apply(value : Int, pos : Int, prev : SeqUpdate) : SeqUpdate = {
    prev match {
      //here, since there is no seq given, we compare on the move itself to anihilate the moves
      case x@SeqUpdateRemove(removedPosition : Int, prevOfDelete : SeqUpdate)
        if pos == removedPosition && value == x.removedValue => prevOfDelete
      case _ => new SeqUpdateInsert(value,pos,prev,prev.newValue.insertAtPosition(value, pos, fast = true))
    }
  }

  /**
   * @param i
   * @return value, position, prev
   */
  def unapply(i:SeqUpdateInsert):Option[(Int,Int,SeqUpdate)] = Some(i.value,i.pos,i.prev)
}

//after is -1 for start position
class SeqUpdateInsert(val value: Int, val pos: Int, prev:SeqUpdate, seq:IntSequence)
  extends SeqUpdateWithPrev(prev:SeqUpdate, seq){
  assert(seq equals prev.newValue.insertAtPosition(value,pos,fast=true))

  override protected[computation] def reverseThis(newValueForThisAfterFullReverse: IntSequence, nextOp:SeqUpdate): SeqUpdate = {
    prev.reverseThis(newValueForThisAfterFullReverse, SeqUpdateRemove(pos,nextOp,prev.newValue))
  }

  override protected[computation] def appendThisTo(previousUpdates: SeqUpdate): SeqUpdate = {
    SeqUpdateInsert(value: Int, pos: Int, prev.appendThisTo(previousUpdates), seq)
  }

  override protected[computation] def explicitHowToRollBack(): SeqUpdate = {
    SeqUpdateInsert(value: Int, pos: Int, prev.explicitHowToRollBack(), seq)
  }

  override def oldPosToNewPos(oldPos : Int) : Option[Int] = {
    if (oldPos < pos) Some(oldPos)
    else Some(oldPos + 1)
  }

  override def newPos2OldPos(newPos : Int) : Option[Int] = {
    if(newPos == pos) None
    else if (newPos < pos) Some(newPos)
    else Some(newPos-1)
  }

  override protected[computation] def regularize(maxPivot:Int) : SeqUpdate =
    SeqUpdateInsert(value,pos,prev,seq.regularizeToMaxPivot(maxPivot))

  override def toString : String = s"SeqUpdateInsert(value:$value position:$pos prev:$prev)"
}

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

object SeqUpdateRemove {

  def apply(position : Int, prev : SeqUpdate):SeqUpdate = {
    apply(position,prev,prev.newValue.delete(position, fast = true))
  }

  def apply(position : Int, prev : SeqUpdate, seq:IntSequence):SeqUpdate = {
    prev match {
      case SeqUpdateInsert(insertedValue:Int,insertPos:Int,insertPrev:SeqUpdate)
        if insertPrev.newValue quickEquals seq => insertPrev
      case _ => new SeqUpdateRemove(position,prev,seq)
    }
  }

  /**
   *
   * @param r
   * @return position,prev
   */
  def unapply(r:SeqUpdateRemove):Option[(Int,SeqUpdate)] = Some(r.position,r.prev)
}

class SeqUpdateRemove(val position:Int,prev:SeqUpdate,seq:IntSequence)
  extends SeqUpdateWithPrev(prev,seq){

  assert(seq equals prev.newValue.delete(position,fast=true),"wrong promize on seq value when building SeqUpdateRemove")

  val removedValue:Int = seq match{
    case d:RemovedIntSequence if position == d.positionOfDelete && (d.seq quickEquals prev.newValue) => d.removedValue
    case _ => prev.newValue.valueAtPosition(position).head}


  override protected[computation] def reverseThis(newValueForThisAfterFullReverse: IntSequence, nextOp: SeqUpdate): SeqUpdate = {
    prev.reverseThis(newValueForThisAfterFullReverse, SeqUpdateInsert(removedValue, position, nextOp, prev.newValue))
  }

  override protected[computation] def appendThisTo(previousUpdates: SeqUpdate): SeqUpdate = {
    SeqUpdateRemove(position,prev.appendThisTo(previousUpdates),seq)
  }

  override protected[computation] def explicitHowToRollBack(): SeqUpdate = {
    SeqUpdateRemove(position,prev.explicitHowToRollBack(),seq)
  }

  override def oldPosToNewPos(oldPos : Int) : Option[Int] = {
    if (oldPos == position) None
    else if (oldPos < position) Some(oldPos)
    else Some(oldPos-1)
  }

  override def newPos2OldPos(newPos : Int) : Option[Int] = {
    if(newPos < position) Some(newPos)
    else Some(newPos +1)
  }

  override protected[computation] def regularize(maxPivot:Int) : SeqUpdate =
    SeqUpdateRemove(position,prev,seq.regularizeToMaxPivot(maxPivot))

  override def toString : String =
    s"SeqUpdateRemove(value:$removedValue position:$position prev:$prev)"
}

case class SeqUpdateAssign(value:IntSequence) extends SeqUpdate(value){

  override def anyRollBack: Boolean = false

  val highestLevelOfDeclaredCheckpoint: Int = -1

  override protected[computation] def reverseThis(newValueForThisAfterFullReverse: IntSequence, nextOp: SeqUpdate): SeqUpdate = {
    SeqUpdateAssign(newValueForThisAfterFullReverse)
  }

  override protected[computation] def appendThisTo(previousUpdates: SeqUpdate): SeqUpdate = this

  override protected[computation] def explicitHowToRollBack(): SeqUpdate = this

  override protected[computation] def regularize(maxPivot:Int) : SeqUpdate =
    SeqUpdateAssign(value.regularizeToMaxPivot(maxPivot))

  override def depth : Int = -1
}

case class SeqUpdateLastNotified(value:IntSequence) extends SeqUpdate(value){

  override def anyRollBack: Boolean = false

  override def highestLevelOfDeclaredCheckpoint: Int = -1

  override protected[computation] def reverseThis(newValueForThisAfterFullReverse: IntSequence, nextOp: SeqUpdate): SeqUpdate = {
    require(newValueForThisAfterFullReverse quickEquals this.newValue,
      s"not proper reverse target on $this target:$newValueForThisAfterFullReverse")
    nextOp
  }

  override protected[computation] def appendThisTo(previousUpdates: SeqUpdate): SeqUpdate = {
    require(this.newValue quickEquals previousUpdates.newValue, "illegal append operation; values do not match")
    previousUpdates
  }

  override protected[computation] def explicitHowToRollBack(): SeqUpdate = this

  override protected[computation] def regularize(maxPivot:Int) : SeqUpdate = SeqUpdateLastNotified(value.regularizeToMaxPivot(maxPivot))

  override def depth : Int = 0

}

object SeqUpdateDefineCheckpoint{

  def apply(prev: SeqUpdate, maxPivotPerValuePercent: Int, level: Int):SeqUpdateDefineCheckpoint = {
    val doRegularize = level == 0
    val newPrev = if(doRegularize) prev.regularize(maxPivotPerValuePercent) else prev
    new SeqUpdateDefineCheckpoint(newPrev, maxPivotPerValuePercent, level)
  }

  def unapply(u:SeqUpdateDefineCheckpoint):Option[(SeqUpdate,Int)] = Some(u.prev,u.level)
}

/**
 * @param mprev
 * @param maxPivotPerValuePercent
 * @param level the first checkpoint to be declared is 0, the second in stack is 1
 */
class SeqUpdateDefineCheckpoint(mprev: SeqUpdate, maxPivotPerValuePercent: Int, val level: Int)
  extends SeqUpdateWithPrev(mprev,mprev.newValue){

  override val highestLevelOfDeclaredCheckpoint: Int = prev.highestLevelOfDeclaredCheckpoint max level


  override protected[computation] def reverseThis(newValueForThisAfterFullReverse: IntSequence, nextOp: SeqUpdate): SeqUpdate = {
    require(nextOp.newValue quickEquals this.newValue)
    prev.reverseThis(newValueForThisAfterFullReverse, SeqUpdateDefineCheckpoint(nextOp, maxPivotPerValuePercent, level))
  }

  override protected[computation] def appendThisTo(previousUpdates: SeqUpdate): SeqUpdate = {
    SeqUpdateDefineCheckpoint(mprev.appendThisTo(previousUpdates), maxPivotPerValuePercent,level)
  }

  override protected[computation] def explicitHowToRollBack(): SeqUpdate = {
    SeqUpdateDefineCheckpoint(mprev.explicitHowToRollBack(), maxPivotPerValuePercent,level)
  }

  protected[computation] def regularize(maxPivot:Int) : SeqUpdate = this

  def oldPosToNewPos(oldPos : Int) : Option[Int] = throw new Error("SeqUpdateDefineCheckpoint should not be queried for delta on moves")

  def newPos2OldPos(newPos : Int) : Option[Int] = throw new Error("SeqUpdateDefineCheckpoint should not be queried for delta on moves")

  override def toString : String = s"SeqUpdateDefineCheckpoint(level:$level prev:$mprev)"
}

object SeqUpdateRollBackToCheckpoint{
  def apply(checkpointValue:IntSequence, howToRollBack:SeqUpdate, level:Int):SeqUpdateRollBackToCheckpoint = {
    new SeqUpdateRollBackToCheckpoint(checkpointValue, howToRollBack, level)
  }

  def unapply(u:SeqUpdateRollBackToCheckpoint):Option[(IntSequence,Int)] = Some(u.checkpointValue,u.level)
}

class SeqUpdateRollBackToCheckpoint(val checkpointValue:IntSequence, val howToRollBack:SeqUpdate, val level:Int)
  extends SeqUpdate(checkpointValue){

  override def anyRollBack: Boolean = true

  override def highestLevelOfDeclaredCheckpoint: Int = -1

  override protected[computation] def regularize(maxPivot:Int) : SeqUpdate = this


  override protected[computation] def reverseThis(newValueForThisAfterFullReverse: IntSequence, nextOp: SeqUpdate): SeqUpdate = {
    throw new Error("cannot reverse SeqUpdateRollBackToCheckpoint")
  }

  override protected[computation] def appendThisTo(previousUpdates: SeqUpdate): SeqUpdate = {
    this
  }

  override protected[computation] def explicitHowToRollBack(): SeqUpdate = {
    howToRollBack
  }

  override def toString : String =
    s"SeqUpdateRollBackToCheckpoint(level:$level checkpoint:$checkpointValue)"

  override def depth : Int = 0
}

/**
 * this is the thing you must implement to listen to any ChangingSeqValue.
 * you will be notified about seqChanges through this interface
 * notice that you will always be notified of checkpoint-related changes.
 * Invariants must only consider one checkpoint, since they are never notified about checkpoint release,
 * only about newly defined checkpoints.
 * if you decide not to handle checkpoint, you will anyway be notified about rollbacks, but the rollback actually
 * includes incremental changes info, so you can go for incremental changes in this way.
 *
 * notice that checkpoint definition is sent as any other update (although it is identity operator)
 */
trait SeqNotificationTarget {
  def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit
}

class CBLSSeqConst(override val value:ConcreteIntSequence) extends SeqValue{
  override def domain : Domain = value.largestValue match{case None => Domain.empty case Some(v) => Domain(0 , v)}
  override def name : String = value.toString
}

object CBLSSeqConst{
  implicit def seq2SeqValue(seq: IntSequence): CBLSSeqConst = new CBLSSeqConst(seq.regularize())
  implicit def seq2SeqConst(seq: IntSequence): CBLSSeqConst = new CBLSSeqConst(seq.regularize())

  def apply(seq:IntSequence):CBLSSeqConst = new CBLSSeqConst(seq.regularize())
}

class CBLSSeqVar(givenModel:Store,
                 initialValue:IntSequence,
                 val maxVal:Int = Int.MaxValue,
                 n: String = null,
                 maxPivotPerValuePercent:Int = 4,
                 maxHistorySize:Int = 50)
  extends ChangingSeqValue(initialValue, maxVal, maxPivotPerValuePercent, maxHistorySize) with Variable{
  require(domain.min == 0)
  require(givenModel != null)

  model = givenModel

  override def checkInternals(c : Checker): Unit ={
    c.check(this.value.toList equals this.newValue.toList)
    c.check(this.toNotify.isInstanceOf[SeqUpdateLastNotified], Some(s"toNotify:$toNotify"))
  }

  override def name: String = if (n == null) defaultName else n

  /**
   * inserts the value at the position in the sequence, and shifts the tail by one position accordingly
   * @param value the inserted value
   * @param pos the position where the value is located after the insert is completed
   */
  override def insertAtPosition(value:Int,pos:Int): Unit ={
    super.insertAtPosition(value,pos)
  }

  /**
   * inserts the value at the position in the sequence, and shifts the tail by one position accordingly
   * @param value the inserted value
   * @param pos the position where the value is located after the insert is completed
   * @param seqAfter the sequence after the insert if performed. if you have it you can set it here, for speed
   */
  override def insertAtPosition(value:Int,pos:Int,seqAfter:IntSequence): Unit ={
    super.insertAtPosition(value,pos,seqAfter)
  }

  /**
   * removes the value at the given position in the sequence, and shifts the tail by one position accordingly
   * @param position the position where the value is removed
   */
  override def remove(position:Int): Unit ={
    super.remove(position)
  }

  /**
   * removes the value at the given position in the sequence, and shifts the tail by one position accordingly
   * @param position the position where the value is removed
   * @param seqAfter the sequence after the remove if performed. if you have it you can set it here, for speed
   */
  override def remove(position:Int,seqAfter:IntSequence): Unit ={
    super.remove(position,seqAfter)
  }

  override def move(fromIncludedPosition:Int,toIncludedPosition:Int,afterPosition:Int,flip:Boolean): Unit ={
    super.move(fromIncludedPosition,toIncludedPosition,afterPosition,flip)
  }

  override def move(fromIncludedPosition:Int,toIncludedPosition:Int,afterPosition:Int,flip:Boolean,seqAfter:IntSequence): Unit ={
    super.move(fromIncludedPosition,toIncludedPosition,afterPosition,flip,seqAfter)
  }

  override def flip(fromIncludedPosition:Int,toIncludedPosition:Int): Unit ={
    super.flip(fromIncludedPosition,toIncludedPosition)
  }

  override def swapSegments(firstSegmentStartPosition : Int, firstSegmentEndPosition : Int, flipFirstSegment : Boolean,
                            secondSegmentStartPosition : Int, secondSegmentEndPosition : Int, flipSecondSegment : Boolean) : Unit =
    super.swapSegments(
      firstSegmentStartPosition, firstSegmentEndPosition, flipFirstSegment,
      secondSegmentStartPosition, secondSegmentEndPosition, flipSecondSegment)

  override def setValue(seq:IntSequence): Unit ={super.setValue(seq)}

  override  def :=(seq:IntSequence): Unit ={super.setValue(seq)}

  override def defineCurrentValueAsCheckpoint():IntSequence = {
    super.defineCurrentValueAsCheckpoint()
  }

  override def rollbackToTopCheckpoint(checkpoint:IntSequence): Unit ={
    super.rollbackToTopCheckpoint(checkpoint)
  }

  override def releaseTopCheckpoint(): Unit ={
    super.releaseTopCheckpoint()
  }

  override protected def releaseTopCheckpointsToLevel(level : Int, included:Boolean): Unit ={
    super.releaseTopCheckpointsToLevel(level,included)
  }

  def <==(i: SeqValue): Unit ={IdentitySeq(i,this)}

  override def performPropagation(): Unit ={performSeqPropagation()}
}

object CBLSSeqVar{
  implicit val ord:Ordering[CBLSSetVar] = (o1: CBLSSetVar, o2: CBLSSetVar) => o1.compare(o2)
}

class ChangingSeqValueSnapShot(val uniqueId:Int,val savedValue:IntSequence) extends AbstractVariableSnapShot(uniqueId){

  override protected def doRestore(m:Store) : Unit = {
    val seqVar = m.getSeqVar(uniqueId)
    val currentValue =  seqVar.value
    if(! (currentValue quickEquals savedValue)){
      seqVar := savedValue
    }
  }

  override def valueString(): String = "[" + savedValue.mkString(",") + "]"

  override protected def doRestoreWithoutCheckpoints(m: Store): Unit = {
    val seqVar = m.getSeqVar(uniqueId)
    val currentValue =  seqVar.value
    val topCheckPoint = seqVar.getTopCheckpoint
    if (topCheckPoint != null) {
      seqVar.releaseTopCheckpoint()
    }
    if (! (currentValue quickEquals savedValue)) {
      seqVar := savedValue
    }
  }

  override def makeIndependentSerializable: IndependentSerializableAbstractVariableSnapshot =
    IndependentSerializableChangingSeqValueSnapShot(uniqueId:Int, savedValue.toArray)
}


case class IndependentSerializableChangingSeqValueSnapShot(uniqueId: Int, savedValues: Array[Int])
  extends IndependentSerializableAbstractVariableSnapshot {
  override def makeLocal: AbstractVariableSnapShot = new ChangingSeqValueSnapShot(uniqueId:Int,IntSequence(savedValues))
}

/**
 * this is an abstract implementation with placeholders for checkpoint management stuff
 * There are three implementation of checkpoint stuff: all,latest,topMost
 * @param initialValue
 * @param maxValue
 * @param maxPivotPerValuePercent
 * @param maxHistorySize
 */
abstract class ChangingSeqValue(initialValue: Iterable[Int], val maxValue: Int, val maxPivotPerValuePercent: Int, val maxHistorySize:Int)
  extends AbstractVariable with SeqValue{

  override def snapshot : ChangingSeqValueSnapShot = new ChangingSeqValueSnapShot(this.uniqueID,this.value)

  def valueAtSnapShot(s:Solution):IntSequence = s(this) match{
    case s:ChangingSeqValueSnapShot => s.savedValue
    case _ => throw new Error(s"cannot find value of $this in snapshot")}

  private var mOldValue:IntSequence = IntSequence(initialValue)

  protected[computation] var toNotify:SeqUpdate = SeqUpdateLastNotified(mOldValue)

  override def domain : Domain = Domain(0,maxValue)
  override def max : Int = maxValue
  override def min : Int = 0

  override def value: IntSequence = {
    if (model == null) return mOldValue
    val propagating = model.propagating
    if (definingInvariant == null && !propagating) return toNotify.newValue
    if(!propagating) model.propagate(this)
    mOldValue
  }

  def newValue:IntSequence = {
    assert(model.checkExecutingInvariantOK(definingInvariant),
      s"variable [$this] queried for latest val by non-controlling invariant")
    toNotify.newValue
  }

  override def toString:String = s"$name:=${if(model.propagateOnToString) value else toNotify.newValue}"

  def toStringNoPropagate: String = s"$name:=${toNotify.newValue}"

  protected def insertAtPosition(value:Int,pos:Int): Unit ={
    require(pos <= toNotify.newValue.size)
    require(pos >= 0)
    recordPerformedIncrementalUpdate((prev,newSeq) =>
      if(newSeq == null) SeqUpdateInsert(value,pos,prev)
      else SeqUpdateInsert(value,pos,prev,newSeq))
    notifyChanged()
  }

  protected def insertAtPosition(value:Int,pos:Int,seqAfter:IntSequence): Unit ={
    require(pos <= toNotify.newValue.size)
    require(pos >= 0)
    recordPerformedIncrementalUpdate((prev,_) => SeqUpdateInsert(value,pos,prev,seqAfter))
    notifyChanged()
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

/** this is a special case of invariant that has a single output variable, that is a Seq
 * @author renaud.delandtsheer@cetic.be
 */
abstract class SeqInvariant(initialValue:IntSequence,
                            maxValue:Int = Int.MaxValue,
                            maxPivotPerValuePercent:Int = 10,
                            maxHistorySize:Int = 10)
  extends ChangingSeqValue(initialValue, maxValue:Int, maxPivotPerValuePercent, maxHistorySize)
    with Invariant{

  override def definingInvariant: Invariant = this
  override def isControlledVariable:Boolean = true
  override def isDecisionVariable:Boolean = false

  override def model: Store = propagationStructure.asInstanceOf[Store]

  override def hasModel:Boolean = schedulingHandler != null

  private var customName:String = null
  /**use this if you want to give a particular name to this concept, to be used in toString*/
  def setName(n:String):SeqInvariant = {
    customName = n
    this
  }

  override final def name: String = if(customName == null) this.getClass.getSimpleName else customName

  override final def performPropagation(): Unit ={
    performInvariantPropagation()
    performSeqPropagation()
  }
}

object IdentitySeq{
  def apply(fromValue:SeqValue,toValue:CBLSSeqVar): Unit ={
    fromValue match{
      case c:CBLSSeqConst => toValue := c.value
      case c:ChangingSeqValue => new IdentitySeq(c,toValue)
    }
  }
}

class IdentitySeq(fromValue:ChangingSeqValue, toValue:CBLSSeqVar)
  extends Invariant
    with SeqNotificationTarget{

  registerStaticAndDynamicDependency(fromValue)
  toValue.setDefiningInvariant(this)
  finishInitialization()

  toValue := fromValue.value

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    assert(v == fromValue)
    digestChanges(changes)
  }

  private var checkPointStackNotTop:List[IntSequence] = List.empty

  private var topCheckpoint:IntSequence = null
  private var levelTopCheckpoint:Int = -1

  private def popTopCheckpoint(): Unit ={
    checkPointStackNotTop match{
      case cp :: tail =>
        topCheckpoint = cp
        checkPointStackNotTop = tail
        assert(levelTopCheckpoint + 1 == checkPointStackNotTop.size)
        levelTopCheckpoint -= 1
      case _ =>
        topCheckpoint = null
        levelTopCheckpoint = -1
    }
  }

  private def pushTopCheckpoint(newCheckpoint:IntSequence): Unit ={
    if(topCheckpoint != null) {
      checkPointStackNotTop = topCheckpoint :: checkPointStackNotTop
    }
    topCheckpoint = newCheckpoint
    levelTopCheckpoint += 1
  }

  def digestChanges(changes:SeqUpdate): Unit ={
    changes match{
      case SeqUpdateInsert(value:Int,pos:Int,prev:SeqUpdate) =>
        digestChanges(prev)
        toValue.insertAtPosition(value,pos,changes.newValue)
      case SeqUpdateMove(fromIncluded:Int,toIncluded:Int,after:Int,flip:Boolean,prev:SeqUpdate) =>
        digestChanges(prev)
        toValue.move(fromIncluded,toIncluded,after,flip,changes.newValue)
      case SeqUpdateRemove(position:Int,prev:SeqUpdate) =>
        digestChanges(prev)
        toValue.remove(position,changes.newValue)
      case SeqUpdateAssign(s) =>
        while(levelTopCheckpoint >=0){
          toValue.releaseTopCheckpoint()
          popTopCheckpoint()
        }
        toValue := s
      case SeqUpdateLastNotified(value:IntSequence) =>
        //nothing to do here
        assert(value equals toValue.newValue)
      case SeqUpdateRollBackToCheckpoint(value:IntSequence,level:Int) =>
        //roll back might free some checkpoints implicitly
        while(level < levelTopCheckpoint){
          toValue.releaseTopCheckpoint()
          popTopCheckpoint()
        }
        require(level == levelTopCheckpoint)
        require(value quickEquals topCheckpoint,
          s"fail on quick equals equals=${value.toList equals topCheckpoint.toList} value:$value topCheckpoint:$topCheckpoint")
        toValue.rollbackToTopCheckpoint(value)
      case SeqUpdateDefineCheckpoint(prev:SeqUpdate,level:Int) =>
        digestChanges(prev)
        while(level <= levelTopCheckpoint){
          toValue.releaseTopCheckpoint()
          popTopCheckpoint()
        }
        require(changes.newValue quickEquals prev.newValue)
        pushTopCheckpoint(changes.newValue)
        toValue.defineCurrentValueAsCheckpoint()

      case _ =>
      // Default case, do nothing
    }
  }

  override def checkInternals(c:Checker): Unit ={
    c.check(toValue.newValue.toList equals fromValue.newValue.toList,
      Some(s"IdentitySeq: toValue.value=${toValue.value} should equals fromValue.value=${fromValue.value}"))
  }
}

class SeqCheckpointedValueStack[@specialized T]{
  private[this] var checkpointStackNotTop:List[(IntSequence,T)] = List.empty
  private[this] var _topCheckpoint:IntSequence = null
  private[this] var _outputAtTopCheckpoint:T = null.asInstanceOf[T]
  private[this] var checkpointStackLevel:Int = -1

  private def popCheckpointStackToLevel(level:Int,included:Boolean): Unit ={
    if(included){
      while(checkpointStackLevel>=level) {
        popCheckpoint()
      }
    }else{
      while(checkpointStackLevel>level) {
        popCheckpoint()
      }
    }
  }

  private def popCheckpoint(): Unit ={
    require(checkpointStackLevel >=0)
    if(checkpointStackLevel>0){
      val top = checkpointStackNotTop.head
      checkpointStackNotTop = checkpointStackNotTop.tail
      _topCheckpoint = top._1
      _outputAtTopCheckpoint = top._2
    }else{
      _topCheckpoint = null
      _outputAtTopCheckpoint = null.asInstanceOf[T]
    }
    checkpointStackLevel -= 1
  }

  def outputAtTopCheckpoint(checkpoint:IntSequence):T = {
    require(topCheckpoint quickEquals checkpoint,
      s"topCheckpoint:$topCheckpoint not quickEquals checkpoint:$checkpoint")
    _outputAtTopCheckpoint
  }

  def topCheckpoint:IntSequence = _topCheckpoint

  def defineTopCheckpoint(checkpoint:IntSequence,savedValue:T): Unit ={
    if(checkpointStackLevel>=0){
      checkpointStackNotTop = (_topCheckpoint,_outputAtTopCheckpoint) :: checkpointStackNotTop
    }
    _topCheckpoint = checkpoint
    _outputAtTopCheckpoint = savedValue
    checkpointStackLevel += 1
  }

  def rollBackAndOutputValue(checkpoint:IntSequence,checkpointLevel:Int):T = {
    popCheckpointStackToLevel(checkpointLevel,false)
    outputAtTopCheckpoint(checkpoint)
  }

  def defineCheckpoint(checkpoint:IntSequence,checkpointLevel:Int,savedValue:T): Unit ={
    require(checkpointLevel <= checkpointStackLevel+1)
    require(checkpointLevel >= 0)
    popCheckpointStackToLevel(checkpointLevel,true)
    defineTopCheckpoint(checkpoint:IntSequence,savedValue:T)
  }
}
