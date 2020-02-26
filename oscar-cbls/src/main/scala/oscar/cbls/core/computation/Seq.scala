package oscar.cbls.core.computation

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

import oscar.cbls.algo.fun.PiecewiseLinearBijectionNaive
import oscar.cbls.algo.seq.{ConcreteIntSequence, IntSequence, MovedIntSequence, RemovedIntSequence}
import oscar.cbls.core.propagation.{Checker, ErrorChecker}

import scala.language.implicitConversions


/*
 *checkpoints must be defined and released manually by neighborhoods
 * you can release a checkpoint when not at the checkpoint
 * but you must respect the stack model of checkpoint
 * checkpoint releases are not included in the notifications,
 * but they can easily be inferred from the roll back and the defines.
 */



sealed trait SeqValue extends Value{
  def value:IntSequence
  def domain:Domain
  def min = domain.min
  def max = domain.max
  def name:String
  override final def valueString: String = value.toString
}

object SeqValue{
  implicit def tist2IntSeqVar(a:List[Long]):SeqValue = CBLSSeqConst(IntSequence(a))
}


//TODO: when instantiating moves, we must always check that they cannot be anihilated.
//basically, move instantiation should proceed through obects that perfor such anihilation automatically, and based on move features, not on quichEquals.

sealed abstract class SeqUpdate(val newValue:IntSequence){
  protected[computation] def reverse(target:IntSequence, from:SeqUpdate):SeqUpdate
  protected[computation] def regularize(maxPivot:Int):SeqUpdate
  protected[computation] def prepend(u:SeqUpdate):SeqUpdate
  protected[computation] def pruneTo(target:IntSequence):SeqUpdate

  def depth:Int

  final def anyCheckpointDefinition:Boolean = highestLevelOfDeclaredCheckpoint != -1L

  /**the level of he highest declared checkpoint in this sequpdate nd its predecessors.
    * -1L if no declared checkpoints
    * */
  def highestLevelOfDeclaredCheckpoint:Int
}

sealed abstract class SeqUpdateWithPrev(val prev:SeqUpdate,newValue:IntSequence) extends SeqUpdate(newValue) {

  val highestLevelOfDeclaredCheckpoint = prev.highestLevelOfDeclaredCheckpoint

  def oldPosToNewPos(oldPos:Int):Option[Int]
  def newPos2OldPos(newPos:Int):Option[Int]
  override val depth:Int = {
    val pd = prev.depth
    if(pd >=0L) pd+1 else pd-1
  }

  override protected[computation] def pruneTo(target : IntSequence) : SeqUpdate = {
    val newPrev = prev.pruneTo(target)
    if(newPrev != null) newPrev
    else if (target quickEquals this.newValue) this
    else null
  }
}

object SeqUpdateInsert {
  def apply(value : Long, pos : Int, prev : SeqUpdate, seq : IntSequence) : SeqUpdate = {
    prev match {
      //we compare the seq here because seq equality is used for checkpointing stuff to anihilate the moves
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
  def apply(value : Long, pos : Int, prev : SeqUpdate) : SeqUpdate = {
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
  def unapply(i:SeqUpdateInsert):Option[(Long,Int,SeqUpdate)] = Some(i.value,i.pos,i.prev)
}


//after is -1L for start position
class SeqUpdateInsert(val value:Long,val pos:Int,prev:SeqUpdate, seq:IntSequence)
  extends SeqUpdateWithPrev(prev:SeqUpdate, seq){
  assert(seq equals prev.newValue.insertAtPosition(value,pos,fast=true))

  override protected[computation] def reverse(target:IntSequence, newPrev:SeqUpdate) : SeqUpdate = {
    if(newPrev.newValue quickEquals target) newPrev
    else prev.reverse(target,SeqUpdateRemove(pos,newPrev,prev.newValue))
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

  override protected[computation] def prepend(u : SeqUpdate) : SeqUpdate =
    SeqUpdateInsert(value, pos, prev.prepend(u), seq)

  override def toString : String = "SeqUpdateInsert(value:" + value + " position:" + pos + " prev:" + prev + ")"
}

object SeqUpdateMove{
  def apply(fromIncluded:Int,toIncluded:Int,after:Int,flip:Boolean,prev:SeqUpdate):SeqUpdateMove =
    new SeqUpdateMove(fromIncluded,toIncluded,after,flip,prev,prev.newValue.moveAfter(fromIncluded,toIncluded,after,flip,fast=true))

  def apply(fromIncluded:Int,toIncluded:Int,after:Int,flip:Boolean,prev:SeqUpdate,seq:IntSequence):SeqUpdate = {
    assert(seq equals prev.newValue.moveAfter(fromIncluded,toIncluded,after,flip,fast=true), "found bug :)")
    prev match{
      case u:SeqUpdateMove if u.prev.newValue quickEquals seq => u.prev
      case _ => new SeqUpdateMove(fromIncluded,toIncluded,after, flip, prev, seq)
    }
  }

  /**
    *
    * @param move
    * @return fromIncluded,toIncluded,after,flip,prev
    */
  def unapply(move:SeqUpdateMove):Option[(Int,Int,Int,Boolean,SeqUpdate)] =
    Some(move.fromIncluded,move.toIncluded,move.after,move.flip,move.prev)
}


class SeqUpdateMove(val fromIncluded:Int,val toIncluded:Int,val after:Int, val flip:Boolean, prev:SeqUpdate, seq:IntSequence)
  extends SeqUpdateWithPrev(prev,seq){

  assert(seq equals prev.newValue.moveAfter(fromIncluded,toIncluded,after,flip,fast=true),
    "given seq=" + seq + " should be " +  prev.newValue.moveAfter(fromIncluded,toIncluded,after,flip,fast=true))

  def isSimpleFlip:Boolean = after+1 == fromIncluded && flip
  def isNop = after+1 == fromIncluded && !flip
  def fromValue:Long = prev.newValue.valueAtPosition(fromIncluded).head
  def toValue:Long = prev.newValue.valueAtPosition(toIncluded).head
  def afterValue:Long = prev.newValue.valueAtPosition(after).head
  def moveDownwards:Boolean = fromIncluded > after
  def moveUpwards:Boolean = fromIncluded < after
  def nbPointsInMovedSegment = toIncluded - fromIncluded + 1

  def movedValuesSet = prev.newValue.valuesBetweenPositionsSet(fromIncluded,toIncluded)
  def movedValuesQList = prev.newValue.valuesBetweenPositionsQList(fromIncluded,toIncluded)

  override protected[computation] def reverse(target:IntSequence, newPrev:SeqUpdate) : SeqUpdate = {

    val (intFromIncluded,intToIncluded) = if(flip) (toIncluded,fromIncluded) else (fromIncluded,toIncluded)

    prev.reverse(target,new SeqUpdateMove(
      oldPosToNewPosNoOopt(intFromIncluded),
      oldPosToNewPosNoOopt(intToIncluded),
      oldPosToNewPosNoOopt(fromIncluded-1),
      flip,
      newPrev,
      prev.newValue))
  }

  assert({seq match{case m:MovedIntSequence => m.localBijection.checkBijection() case _ => ;};true})

  //TODO: find O(1L) solution
  private var localBijection:PiecewiseLinearBijectionNaive = null
  private def ensureBijection(){
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

  override protected[computation] def prepend(u : SeqUpdate) : SeqUpdate =
    SeqUpdateMove(fromIncluded,toIncluded,after,flip,prev.prepend(u),seq)

  override def toString : String =
    "SeqUpdateMove(fromIncluded:" + fromIncluded +
      " toIncluded:" + toIncluded +
      " after:" + after+
      " flip:" + flip +
      " prev:" + prev + ")"
}

object SeqUpdateRemove {

  def apply(position : Int, prev : SeqUpdate):SeqUpdate = {
    apply(position,prev,prev.newValue.delete(position, fast = true))
  }

  def apply(position : Int, prev : SeqUpdate, seq:IntSequence):SeqUpdate = {
    prev match {
      case SeqUpdateInsert(insertedValue:Long,insertPos:Int,insertPrev:SeqUpdate)
        if insertPrev.newValue quickEquals seq //comparison must be on quickequals since this is the stuff used for checkpoint cleaning
      => insertPrev
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

  val removedValue:Long = seq match{
    case d:RemovedIntSequence if position == d.positionOfDelete && (d.seq quickEquals prev.newValue) => d.removedValue
    case _ => prev.newValue.valueAtPosition(position).head}

  override protected[computation] def reverse(target:IntSequence, newPrev:SeqUpdate) : SeqUpdate = {
    if(newPrev.newValue quickEquals target) newPrev
    else prev.reverse(target,SeqUpdateInsert(removedValue, position, newPrev, prev.newValue))
  }

  override def oldPosToNewPos(oldPos : Int) : Option[Int] = {
    if (oldPos == position) None
    else if (oldPos < position) Some(oldPos)
    else Some(oldPos-1)
  }

  override def newPos2OldPos(newPos : Int) : Option[Int] = {
    if(newPos <= position) Some(newPos)
    else Some(newPos +1)
  }

  override protected[computation] def regularize(maxPivot:Int) : SeqUpdate =
    SeqUpdateRemove(position,prev,seq.regularizeToMaxPivot(maxPivot))

  override protected[computation] def prepend(u : SeqUpdate) : SeqUpdate =
    SeqUpdateRemove(position,prev.prepend(u),seq)

  override def toString : String =  "SeqUpdateRemove(value:" + removedValue + " position:" + position + " prev:" + prev + ")"
}

case class SeqUpdateAssign(value:IntSequence) extends SeqUpdate(value){

  val highestLevelOfDeclaredCheckpoint = -1

  override protected[computation] def reverse(target : IntSequence, newPrev:SeqUpdate) : SeqUpdate = {
    if (target quickEquals this.newValue) newPrev
    else SeqUpdateAssign (target)
  }

  override protected[computation] def regularize(maxPivot:Int) : SeqUpdate =
    SeqUpdateAssign(value.regularizeToMaxPivot(maxPivot))

  override protected[computation] def prepend(u : SeqUpdate) : SeqUpdate = {
    assert(!u.anyCheckpointDefinition)
    this
  }

  override def depth : Int = -1

  override protected[computation] def pruneTo(target : IntSequence) : SeqUpdate =
    if(target quickEquals this.newValue) this
    else null
}

case class SeqUpdateLastNotified(value:IntSequence) extends SeqUpdate(value){

  override def highestLevelOfDeclaredCheckpoint = -1

  override protected[computation] def reverse(target : IntSequence, newPrev:SeqUpdate) : SeqUpdate = {
    require(target quickEquals this.newValue,"not proper reverse target on " + this + " target:" + target)
    if (target quickEquals this.newValue) newPrev
    else SeqUpdateAssign (target)
  }

  override protected[computation] def regularize(maxPivot:Int) : SeqUpdate = SeqUpdateLastNotified(value.regularizeToMaxPivot(maxPivot))

  override protected[computation] def prepend(u : SeqUpdate) : SeqUpdate = {
    require(u.newValue quickEquals value, "error on prepend; prepending " + u + " expected:" + this.newValue + " eq:" + (u.newValue.toList equals this.newValue.toList))
    u
  }

  override def depth : Int = 0

  override protected[computation] def pruneTo(target : IntSequence) : SeqUpdate = if (this.value quickEquals target) this else null
}

object SeqUpdateDefineCheckpoint{

  def apply(prev:SeqUpdate,activeCheckpoint:Boolean, maxPivotPerValuePercent:Int,doRegularize:Boolean,level:Int):SeqUpdateDefineCheckpoint = {
    new SeqUpdateDefineCheckpoint(prev,activeCheckpoint, maxPivotPerValuePercent,doRegularize,level)
  }

  def unapply(u:SeqUpdateDefineCheckpoint):Option[(SeqUpdate,Boolean,Int)] = Some(u.prev,u.activeCheckpoint,u.level)
}

/**
  * @param mprev
  * @param activeCheckpoint
  * @param maxPivotPerValuePercent
  * @param doRegularize
  * @param level the first checkpoint to be declared is 0L, the second in stack is 1L
  */
class SeqUpdateDefineCheckpoint(mprev:SeqUpdate,val activeCheckpoint:Boolean, maxPivotPerValuePercent:Int,val doRegularize:Boolean, val level:Int)
  extends SeqUpdateWithPrev(mprev,if(doRegularize) mprev.newValue.regularizeToMaxPivot(maxPivotPerValuePercent) else mprev.newValue){

  override val highestLevelOfDeclaredCheckpoint = prev.highestLevelOfDeclaredCheckpoint max level

  protected[computation]  def reverse(target : IntSequence, from : SeqUpdate) : SeqUpdate = mprev.reverse(target,from)

  protected[computation] def regularize(maxPivot:Int) : SeqUpdate = this

  def oldPosToNewPos(oldPos : Int) : Option[Int] = throw new Error("SeqUpdateDefineCheckpoint should not be queried for delta on moves")

  def newPos2OldPos(newPos : Int) : Option[Int] = throw new Error("SeqUpdateDefineCheckpoint should not be queried for delta on moves")

  protected[computation] def prepend(u : SeqUpdate) : SeqUpdate = {
    SeqUpdateDefineCheckpoint(mprev.prepend(u), activeCheckpoint, maxPivotPerValuePercent, doRegularize,level)
  }

  override def toString : String = "SeqUpdateDefineCheckpoint(level:" + level + " prev:" + mprev + ")"
}

object SeqUpdateRollBackToCheckpoint{
  def apply(checkpointValue:IntSequence,howToRollBackfct:()=>SeqUpdate,level:Int):SeqUpdateRollBackToCheckpoint = {
    new SeqUpdateRollBackToCheckpoint(checkpointValue, howToRollBackfct,level)
  }

  def unapply(u:SeqUpdateRollBackToCheckpoint):Option[(IntSequence,Int)] = Some(u.checkpointValue,u.level)
}

class SeqUpdateRollBackToCheckpoint(val checkpointValue:IntSequence,howToRollBackFct:()=>SeqUpdate, val level:Int)
  extends SeqUpdate(checkpointValue){

  override def highestLevelOfDeclaredCheckpoint = -1

  override protected[computation] def regularize(maxPivot:Int) : SeqUpdate = this

  override protected[computation] def reverse(target : IntSequence, newPrev:SeqUpdate) : SeqUpdate = {
    if (target quickEquals this.newValue) newPrev
    else SeqUpdateAssign (target)
  }

  override protected[computation] def prepend(u : SeqUpdate) : SeqUpdate = {
    assert(!u.anyCheckpointDefinition)
    this
  }

  //TODO: there might still be overflows during howToRollBack?!
  private var reversedInstructions:SeqUpdate = null
  def howToRollBack:SeqUpdate = {
    if (reversedInstructions != null) reversedInstructions
    else {
      reversedInstructions = howToRollBackFct()
      reversedInstructions
    }
  }

  override def toString : String =
    "SeqUpdateRollBackToCheckpoint(level:" + level + " checkpoint:" + checkpointValue + ")" //+ " howTo:" +  howToRollBack + ")"

  override def depth : Int = 0

  override protected[computation] def pruneTo(target : IntSequence) : SeqUpdate =
    if(target quickEquals this.newValue) this
    else null
}

/**
  * this is the thing you must implement to listen to any ChangingSeqValue.
  * you will be notified about seqChanges through this interface
  * notice that you will always be notified of checkpoint-related changes.
  * Invariants must only consider one hcackpoint, since they are never notified about checkpoint release,
  * only about newly defined checkpoints.
  * if you decide not to handle checkpoint, you will anyway be notified about rollbacks, but the rollback actualy
  * includes incremental changes info, so you can go for incremental changes in this way.
  *
  * notice that checkpoint definition is sent as any other update (although it is identity operator)
  */
trait SeqNotificationTarget {
  def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit
}

class CBLSSeqConst(override val value:ConcreteIntSequence) extends SeqValue{
  override def domain : Domain = value.largestValue match{case None => Domain.empty case Some(v) => Domain(0L , v)}
  override def name : String = value.toString
}

object CBLSSeqConst{
  implicit def seq2SeqValue(seq: IntSequence): CBLSSeqConst = new CBLSSeqConst(seq.regularize())
  implicit def seq2SeqConst(seq: IntSequence): CBLSSeqConst = new CBLSSeqConst(seq.regularize())

  def apply(seq:IntSequence):CBLSSeqConst = new CBLSSeqConst(seq.regularize())
}

class CBLSSeqVar(givenModel:Store,
                 initialValue:IntSequence,
                 val maxVal:Long = Long.MaxValue,
                 n: String = null,
                 maxPivotPerValuePercent:Int = 4,
                 maxHistorySize:Int = 50)
  extends ChangingSeqValue(initialValue, maxVal, maxPivotPerValuePercent, maxHistorySize) with Variable{
  require(domain.min == 0L)
  require(givenModel != null)

  model = givenModel

  override def checkInternals(c : Checker){
    c.check(this.value.toList equals this.newValue.toList)
    c.check(this.toNotify.isInstanceOf[SeqUpdateLastNotified], Some("toNotify:" + toNotify))
  }

  override def name: String = if (n == null) defaultName else n

  /**
    * inserts the value at the postion in the sequence, and shifts the tail by one position accordingly
    * @param value the inserted value
    * @param pos the position where the value is located afer the insert is completed
    */
  override def insertAtPosition(value:Long,pos:Int){
    super.insertAtPosition(value,pos)
  }

  /**
    * inserts the value at the postion in the sequence, and shifts the tail by one position accordingly
    * @param value the inserted value
    * @param pos the position where the value is located afer the insert is completed
    * @param seqAfter the sequence after the insert if performed. if you have it you can set it here, for speed
    */
  override def insertAtPosition(value:Long,pos:Int,seqAfter:IntSequence){
    super.insertAtPosition(value,pos,seqAfter)
  }

  /**
    * removes the value at the given position in the sequence, and shifts the tail by one position accordingly
    * @param position the position where the value is removed
    */
  override  def remove(position:Int){
    super.remove(position)
  }

  /**
    * removes the value at the given position in the sequence, and shifts the tail by one position accordingly
    * @param position the position where the value is removed
    * @param seqAfter the sequence after the remove if performed. if you have it you can set it here, for speed
    */
  override  def remove(position:Int,seqAfter:IntSequence){
    super.remove(position,seqAfter)
  }

  /**
    *
    * @param fromIncludedPosition
    * @param toIncludedPosition
    * @param afterPosition
    * @param flip
    */
  override def move(fromIncludedPosition:Int,toIncludedPosition:Int,afterPosition:Int,flip:Boolean){
    super.move(fromIncludedPosition,toIncludedPosition,afterPosition,flip)
  }

  override def move(fromIncludedPosition:Int,toIncludedPosition:Int,afterPosition:Int,flip:Boolean,seqAfter:IntSequence){
    super.move(fromIncludedPosition,toIncludedPosition,afterPosition,flip,seqAfter)
  }

  override def flip(fromIncludedPosition:Int,toIncludedPosition:Int){
    super.flip(fromIncludedPosition,toIncludedPosition)
  }

  override def swapSegments(firstSegmentStartPosition : Int, firstSegmentEndPosition : Int, flipFirstSegment : Boolean,
                            secondSegmentStartPosition : Int, secondSegmentEndPosition : Int, flipSecondSegment : Boolean) : Unit =
    super.swapSegments(
      firstSegmentStartPosition, firstSegmentEndPosition, flipFirstSegment,
      secondSegmentStartPosition, secondSegmentEndPosition, flipSecondSegment)

  override  def setValue(seq:IntSequence) {super.setValue(seq)}

  override  def :=(seq:IntSequence) {super.setValue(seq)}

  override def defineCurrentValueAsCheckpoint(checkPointIsActive:Boolean):IntSequence = {
    super.defineCurrentValueAsCheckpoint(checkPointIsActive:Boolean)
  }

  override def rollbackToTopCheckpoint(checkpoint:IntSequence) {
    super.rollbackToTopCheckpoint(checkpoint)
  }

  override def releaseTopCheckpoint(){
    super.releaseTopCheckpoint()
  }

  override protected def releaseTopCheckpointsToLevel(level : Int, included:Boolean){
    super.releaseTopCheckpointsToLevel(level,included)
  }

  def <==(i: SeqValue) {IdentitySeq(i,this)}



  override def performPropagation(){performSeqPropagation()}
}

object CBLSSeqVar{
  implicit val ord:Ordering[CBLSSetVar] = new Ordering[CBLSSetVar]{
    def compare(o1: CBLSSetVar, o2: CBLSSetVar) = o1.compare(o2)
  }
}

class ChangingSeqValueSnapShot(val variable:ChangingSeqValue,val savedValue:IntSequence) extends AbstractVariableSnapShot(variable){
  override protected def doRestore() : Unit = {variable.asInstanceOf[CBLSSeqVar] := savedValue}
}

/**
  * this is an abstract implementation with placeholders for checkpoint management stuff
  * There are three implementation of ceckpoint stuff: all,latest,topMost
  * @param initialValue
  * @param maxValue
  * @param maxPivotPerValuePercent
  * @param maxHistorySize
  */
abstract class ChangingSeqValue(initialValue: Iterable[Long], val maxValue: Long, val maxPivotPerValuePercent: Int, val maxHistorySize:Int)
  extends AbstractVariable with SeqValue{

  override def snapshot : ChangingSeqValueSnapShot = new ChangingSeqValueSnapShot(this,this.value)

  def valueAtSnapShot(s:Snapshot):IntSequence = s(this) match{
    case s:ChangingSeqValueSnapShot => s.savedValue
    case _ => throw new Error("cannot find value of " + this + " in snapshot")}

  private var mOldValue:IntSequence = IntSequence(initialValue)
  protected[computation] var toNotify:SeqUpdate = SeqUpdateLastNotified(mOldValue)

  override def domain : Domain = Domain(0L,maxValue)
  override def max : Long = maxValue
  override def min : Long = 0L

  override def value: IntSequence = {
    if (model == null) return mOldValue
    val propagating = model.propagating
    if (definingInvariant == null && !propagating) return toNotify.newValue
    if(!propagating) model.propagate(this)
    mOldValue
  }

  def newValue:IntSequence = {
    assert(model.checkExecutingInvariantOK(definingInvariant),"variable [" + this
      + "] queried for latest val by non-controlling invariant")
    toNotify.newValue
  }

  override def toString:String = name + ":=" + (if(model.propagateOnToString) value else toNotify.newValue)

  def toStringNoPropagate: String = name + ":=" + toNotify.newValue


  /*
gestion des checkpoints
-------------------------------------

toNotify

notifiedCheckpointStack

incremental
assign
checkpoint
rollBack
freeCheckpoint



* incremental
store into toNotify

* assign
check toNotify has Zero checkpoint toNotify (or this is an error)
store into toNotify (erases toNotify)

* new checkpoint
storeIntoToNotify
(checkpoints must be freed explicitely, so no need to search and destroy)

quel est le niveau de ce checkpoint?
il y a un copteur de niveau: currentCheckpointLevel
qui augmente de 1L chaue fois uq'on en déclare 1L et qui diminue de 1L à chaque release.  et c'est tout.


* freeCheckpoint
//maybe not at the actual checkpoint!
pop toNotify until checkpoint declaration removed (other updates are left in place, before and after)
nothing else todo, the free is not communicated in this way, it is inferred on the other side because a heckpoint is communicated that overridess it.


* rollBack
//pop toNotify until checkpoint declaration reached (and do not remove it)
if reached, tonotify is cleaned (rollBack anihilation)
if not reached, override the toNotify

what if some checkpoint declaration where here???
you can only roll back to the top checkpoint,
so higher checkpoint should have been freed before
the checkpoint itself might still be declared there
checkpoints below might still be there, but then the checkpoint declaration is also there, and popping toNotify will not remove them


Quand est-ce qu'on calcule le howto??
quand on déclare un rollBack
et que le rollBack n'est pas anihilé
un rollBack est d'office la première instruction d'un toNotify puisqu'il override tous les rucs incrémentaux)
donc son howTo est d'office basé sur ce qui a été notifié depuis le top checkpoint notifié
et donc on enregistre ce qui est notifié depuis les checkpoins, et pas ce qui est effectué
donc on doit enregistrer ce qu iest notifié, et maintenir la notified checkpoint stack, avec le notifiedSinceTopCheckpoint
et cette stack doit être mise à jour au moment de la notification.

 */




  protected def insertAtPosition(value:Long,pos:Int){
    assert(pos <= toNotify.newValue.size)
    assert(pos >= 0L)
    recordPerformedIncrementalUpdate((prev,newSeq) =>
      if(newSeq == null) SeqUpdateInsert(value,pos,prev)
      else SeqUpdateInsert(value,pos,prev,newSeq))
    notifyChanged()
  }

  protected def insertAtPosition(value:Long,pos:Int,seqAfter:IntSequence){
    assert(pos <= toNotify.newValue.size)
    assert(pos >= 0L)
    recordPerformedIncrementalUpdate((prev,_) => SeqUpdateInsert(value,pos,prev,seqAfter))
    // println(" notify insert " + toNotify)
    notifyChanged()
  }

  protected def remove(position:Int){
    require(toNotify.newValue.size > position && position >=0L,
      "removing at position " + position + " size is " + newValue.size)
    recordPerformedIncrementalUpdate((prev,newSeq) =>
      if(newSeq == null) SeqUpdateRemove(position, prev)
      else SeqUpdateRemove(position, prev,newSeq))
    //println(" notify remove " + toNotify)
    notifyChanged()
  }

  protected def remove(position:Int,seqAfter:IntSequence){
    require(toNotify.newValue.size > position && position >=0L, "removing at position " + position + " size is " + newValue.size)
    recordPerformedIncrementalUpdate((prev,_) => SeqUpdateRemove(position,prev,seqAfter))
    //println(" notify remove " + toNotify)
    notifyChanged()
  }

  protected def flip(fromIncludedPosition:Int,toIncludedPosition:Int){
    move(fromIncludedPosition,toIncludedPosition,fromIncludedPosition-1,true)
  }

  //-1L for first position
  protected def move(fromIncludedPosition:Int,toIncludedPosition:Int,afterPosition:Int,flip:Boolean){
    //println("seq.move(fromIncludedPosition:" + fromIncludedPosition + " toIncludedPosition:" + toIncludedPosition +" afterPosition:" + afterPosition + " flip:" + flip+ ")")
    require(toNotify.newValue.size > toIncludedPosition)
    require(toNotify.newValue.size > afterPosition, "toNotify.newValue.size(=" + toNotify.newValue.size + ") > afterPosition(=" + afterPosition + ")")
    require(0L <= fromIncludedPosition,"move with fromIncludedPosition=" + fromIncludedPosition)
    require(-1L<=afterPosition)
    require(fromIncludedPosition <= toIncludedPosition, "fromIncludedPosition=" + fromIncludedPosition + "should <= toIncludedPosition=" + toIncludedPosition)

    require(
      afterPosition < fromIncludedPosition || afterPosition > toIncludedPosition,
      "afterPosition=" + afterPosition + " cannot be between fromIncludedPosition=" + fromIncludedPosition + " and toIncludedPosition=" + toIncludedPosition)

    recordPerformedIncrementalUpdate((prev,newSeq) =>
      if(newSeq == null) SeqUpdateMove(fromIncludedPosition,toIncludedPosition,afterPosition,flip,prev)
      else SeqUpdateMove(fromIncludedPosition,toIncludedPosition,afterPosition,flip,prev,newSeq))

    //println("notified move " + toNotify)
    notifyChanged()
  }

  //-1L for first position
  protected def move(fromIncludedPosition:Int,toIncludedPosition:Int,afterPosition:Int,flip:Boolean,seqAfter:IntSequence){

    require(toNotify.newValue.size > fromIncludedPosition)
    require(toNotify.newValue.size > toIncludedPosition)
    require(toNotify.newValue.size > afterPosition)
    require(0L <= fromIncludedPosition)
    require(0L<=toIncludedPosition)
    require(-1L<=afterPosition)
    require(fromIncludedPosition <= toIncludedPosition)

    recordPerformedIncrementalUpdate((prev,_) =>
      SeqUpdateMove(fromIncludedPosition,toIncludedPosition,afterPosition,flip,prev,seqAfter))

    //println("notified move " + toNotify)
    notifyChanged()
  }

  protected def swapSegments(firstSegmentStartPosition:Int,
                             firstSegmentEndPosition:Int,
                             flipFirstSegment:Boolean,
                             secondSegmentStartPosition:Int,
                             secondSegmentEndPosition:Int,
                             flipSecondSegment:Boolean){

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
  private final def recordPerformedIncrementalUpdate(updatefct:(SeqUpdate,IntSequence) => SeqUpdate) {
    //for notification recording
    toNotify = updatefct(toNotify,null)

    if(performedSinceTopCheckpoint != null) {
      performedSinceTopCheckpoint = updatefct(performedSinceTopCheckpoint, toNotify.newValue)
    }else{
      //if it is null, it means that no checkpoint was declared.
      require(currentCheckpointLevel == -1L)
    }
  }

  protected [computation] def setValue(seq:IntSequence){
    require(
      performedSinceTopCheckpoint == null &&
        !toNotify.anyCheckpointDefinition &&
        levelOfTopCheckpoint == -1L,
      "Sequences cannot be assigned when a checkpoint has been defined")

    toNotify = SeqUpdateAssign(seq)

    notifyChanged()
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
  private[this] var topCheckpointIsStarMode : Boolean = false
  private[this] var checkpointStackNotTop : List[(IntSequence, SeqUpdate, Boolean)] = List.empty

  //what has been performed on the newValue after the current checkpoint (not maintained if checkpoint is circle mode, or if the number of updates gets too large)
  private[this] var performedSinceTopCheckpoint : SeqUpdate = null

  def getTopCheckpoint : IntSequence = topCheckpoint
  def getTopCheckpointIsStarMode : Boolean = topCheckpointIsStarMode

  /**
    * to define the current value as a checkpoint
    * the checkpoint can be used in a star mode or circle mode exploration.
    *
    * for star mode,
    *    the rollBack will lead to O(1L) roll back instructions, and stacked updates will be used in between
    * for circle mode,
    *    the roll back will lead to computing the reversed list of instructions to bring the value backto the checkpoint.
    *    if this list is bigger than maxHistorySize, it will be replaced with an assign
    *    Furthermore, the moves will not use the stacked updates; only concrete updates
    * @param starModeExploration true for a star mode exploration, false for a circle mode exploration
    * @return
    */
  protected def defineCurrentValueAsCheckpoint(starModeExploration : Boolean) : IntSequence = {
    //println("notify define checkpoint " + this.toNotify.newValue)

    //previous checkpoints might be in the toNotify list.
    //we must search and destroy them for all checkpoint declaration whose level is >= level of the defined checkpoint
    //notice that we do not need to search passed a checkpoint whose level is < level of the defined checkpoint

    //we do not use the record function because it also records stuff for the checkpoint stack
    toNotify =
        SeqUpdateDefineCheckpoint(
          toNotify,
          starModeExploration,
          maxPivotPerValuePercent,
          doRegularize = levelOfTopCheckpoint == -1,
          levelOfTopCheckpoint+1)

    if(topCheckpoint != null){
      checkpointStackNotTop = (topCheckpoint,performedSinceTopCheckpoint,topCheckpointIsStarMode) :: checkpointStackNotTop
    }
    topCheckpointIsStarMode = starModeExploration
    topCheckpoint = toNotify.newValue //this one was regularized if needed, btw
    levelOfTopCheckpoint += 1
    if(starModeExploration) {
      performedSinceTopCheckpoint = SeqUpdateLastNotified(topCheckpoint)
    }else{
      performedSinceTopCheckpoint = null
    }

    notifyChanged()
    toNotify.newValue
  }

  protected def rollbackToTopCheckpoint(checkpoint : IntSequence){

    // println("ChangingSeqValue got rollback to top checkpoint my level:" + levelOfTopCheckpoint)
    require(checkpoint quickEquals topCheckpoint,
      "given checkpoint not quickequal to my top checkpoint; equal=" +
        (checkpoint equals topCheckpoint) + " checkpoint:" + checkpoint + " my topCheckpoint:" + topCheckpoint)

    popToNotifyUntilCheckpointDeclaration(toNotify,topCheckpoint,removeDeclaration = false) match{
      case CheckpointDeclarationReachedAndRemoved(newToNotify:SeqUpdate) =>
        //error, we asked removeDeclaration = false
        throw new Error("unexpected result")
      case SeqUpdatesCleanedUntilQuickEqualValueReachedCheckpointDeclarationNotRemoved(newToNotify:SeqUpdate) =>
        //checkpoint value could be found in toNotify, and updatsd after it were removed so we don't have to do anything
        require(newToNotify.newValue quickEquals checkpoint,newToNotify.newValue + "not quickEquals " + checkpoint)

        //we are at the checkpoint declaration, and it has not been communicated yet,
        // so we know that this is already scheduled for propagation unless it has never been scheduled because there was nothing to communicate
        require(this.isScheduled || toNotify.isInstanceOf[SeqUpdateLastNotified])
        performedSinceTopCheckpoint = SeqUpdateLastNotified(topCheckpoint)

        toNotify = newToNotify

      case NoSimplificationPerformed =>
        //in this case, the checkpoint was already notified, and possibly some moves were performed from it.
        assert(!toNotify.anyCheckpointDefinition)
        //checkpoint value could not be found in sequence, we have to add rollBack instructions
        //It also means that the checkpoint was communicated to the listening side
        if(topCheckpointIsStarMode){
          //we are in star mode, so we can do a rollBack
          //we must do it incrementally, or through rollBack update

          assert(performedSinceTopCheckpoint.reverse(topCheckpoint,toNotify).newValue equals checkpoint)

          //the purpose of transferring performedSinceTopCheckpoint to a tmp value if to ensure that the function created
          // herebelow is not affected by a change in the variable, as it is modified later on
          val tmp = performedSinceTopCheckpoint
          val tmpToNotify = toNotify
          //println("performedSinceTopCheckpoint:" + performedSinceTopCheckpoint)
          //we specify a roll back and give the instructions that must be undone, just in case.
          toNotify = SeqUpdateRollBackToCheckpoint(
            checkpoint,
            () => {tmp.reverse(checkpoint,tmpToNotify)},
            level = levelOfTopCheckpoint)

        }else{
          // what if there is a checkpoint that has not been communicated in the toNotify?
          // this is not possible because we roll back to the topmost checkpoint, and if we reach this point,
          // the declaration of this topmost checkpoint is not in toNotify,
          // so it has already been ommunicated, and therefore, the other checkpoint also have.
          // top checkpoint is circle mode. roll back is performed through assign.
          toNotify = SeqUpdateAssign(checkpoint)
        }

        notifyChanged()
    }

    //in all case, we are at the checkpoint, so set it to LastNotified if active
    if(performedSinceTopCheckpoint != null)
      performedSinceTopCheckpoint = SeqUpdateLastNotified(topCheckpoint)

    require(toNotify.newValue quickEquals checkpoint,toNotify.newValue + "not quickEquals " + checkpoint)
    //println("notified of rollBack toNotify after:" + toNotify + " currentCheckpoint:" + topCheckpoint + " performedSinceTopCheckpoint:" + performedSinceTopCheckpoint)
  }

  /**
    * releases the top checkpoint
    * @note You do not need to be at the top checkpoint value to call this, you can do it later no worries.
    */
  protected def releaseTopCheckpoint() {
    require(topCheckpoint != null, "No checkpoint defined to release")
    require(levelOfTopCheckpoint >= 0L)

    //  println("changing seq got release top checkpoint current level is: " + levelOfTopCheckpoint)

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
        require(levelOfTopCheckpoint > 0L)
        checkpointStackNotTop = tail
        topCheckpoint = top._1
        topCheckpointIsStarMode = top._3
        performedSinceTopCheckpoint = if(performedSinceTopCheckpoint != null && top._2!= null) performedSinceTopCheckpoint.prepend(top._2) else null
        levelOfTopCheckpoint -= 1
      case Nil =>
        //there is no upper checkpoint
        require(levelOfTopCheckpoint == 0L)
        levelOfTopCheckpoint = -1
        topCheckpoint = null
        performedSinceTopCheckpoint = null
        topCheckpointIsStarMode = false
    }
  }

  protected def releaseTopCheckpointsToLevel(level:Int,included:Boolean){
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
  final protected def performSeqPropagation(){
    val dynListElements = getDynamicallyListeningElements
    val headPhantom = dynListElements.headPhantom
    var currentElement = headPhantom.next

    if(topCheckpoint!= null && !topCheckpointIsStarMode) toNotify = toNotify.regularize(maxPivotPerValuePercent)

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

  protected def :=(seq:IntSequence){
    setValue(seq)
    notifyChanged()
  }

  def createClone(maxDepth:Int=50):CBLSSeqVar = {
    val clone = new CBLSSeqVar(model,this.value,this.maxValue,"clone_of_" + this.name,maxPivotPerValuePercent,maxDepth)
    IdentitySeq(this,clone)
    clone
  }

  // CHECKPOINT STUFF






  private def removeAllCheckpointDefinitionAboveOrEqualLevel(updates:SeqUpdate, level:Int):SeqUpdate = {
    updates match {
      case i@SeqUpdateInsert(value : Long, pos : Int, prev : SeqUpdate) =>
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

      case d@SeqUpdateDefineCheckpoint(prev, isStarMode, defineLevel) =>
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
    updates match{
      case SeqUpdateInsert(value:Long,pos:Int,prev:SeqUpdate) =>
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

      case SeqUpdateDefineCheckpoint(prev,isStarMode,level) =>
        //here
        //TODO: not sure that this is the same checkpoint
        require(updates.newValue quickEquals searchedCheckpoint,
          "require fail on quick equals (equals=" + (updates.newValue equals searchedCheckpoint) + "): " + updates.newValue + "should== " + searchedCheckpoint)

        if(removeDeclaration) {
          CheckpointDeclarationReachedAndRemoved(prev)
        }else{
          SeqUpdatesCleanedUntilQuickEqualValueReachedCheckpointDeclarationNotRemoved(updates)
        }
      case SeqUpdateRollBackToCheckpoint(checkpointValue:IntSequence, level:Int) =>
        NoSimplificationPerformed
    }
  }

  def removeCheckpointDeclarationIfPresent(updates:SeqUpdate,searchedCheckpoint:IntSequence):CleaningResult = {
    updates match{
      case SeqUpdateInsert(value:Long,pos:Int,prev:SeqUpdate) =>
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

      case SeqUpdateDefineCheckpoint(prev:SeqUpdate,isActive:Boolean,level:Int) =>
        //here
        require(updates.newValue quickEquals searchedCheckpoint,
          "require fail on quick equals: " + updates.newValue + "should== " + searchedCheckpoint)
        CheckpointDeclarationReachedAndRemoved(prev)
      case SeqUpdateRollBackToCheckpoint(checkpointValue:IntSequence,level:Int) =>
        NoSimplificationPerformed
    }
  }
}

/** this is a special case of invariant that has a single output variable, that is a Seq
  * @author renaud.delandtsheer@cetic.be
  */
abstract class SeqInvariant(initialValue:IntSequence,
                            maxValue:Long = Long.MaxValue,
                            maxPivotPerValuePercent:Int = 10,
                            maxHistorySize:Int = 10)
  extends ChangingSeqValue(initialValue, maxValue:Long, maxPivotPerValuePercent, maxHistorySize)
    with Invariant{

  override def definingInvariant: Invariant = this
  override def isControlledVariable:Boolean = true
  override def isDecisionVariable:Boolean = false

  override def model = propagationStructure.asInstanceOf[Store]

  override def hasModel:Boolean = schedulingHandler != null

  private var customName:String = null
  /**use this if you want to give a particular name to this concept, to be used in toString*/
  def setName(n:String):SeqInvariant = {
    customName = n
    this
  }

  override final def name: String = if(customName == null) this.getClass.getSimpleName else customName

  override final def performPropagation(){
    performInvariantPropagation()
    performSeqPropagation()
  }
}

object IdentitySeq{
  def apply(fromValue:SeqValue,toValue:CBLSSeqVar){
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

  private def popTopCheckpoint(){
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

  private def pushTopCheckpoint(newCheckpoint:IntSequence){
    if(topCheckpoint != null) {
      checkPointStackNotTop = topCheckpoint :: checkPointStackNotTop
    }
    topCheckpoint = newCheckpoint
    levelTopCheckpoint += 1
  }

  def digestChanges(changes:SeqUpdate){
    changes match{
      case SeqUpdateInsert(value:Long,pos:Int,prev:SeqUpdate) =>
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
        require(value quickEquals topCheckpoint, "fail on quick equals equals=" + (value.toList equals topCheckpoint.toList)+ " value:" + value + " topCheckpoint:" + topCheckpoint)
        toValue.rollbackToTopCheckpoint(value)
      case SeqUpdateDefineCheckpoint(prev:SeqUpdate,activeCheckpoint:Boolean,level:Int) =>
        digestChanges(prev)
        // println("IdentitySeq got define checkpoint level=" + level + " my level=" + levelTopCheckpoint)
        while(level <= levelTopCheckpoint){
          toValue.releaseTopCheckpoint()
          popTopCheckpoint()
        }
        require(changes.newValue quickEquals prev.newValue)
        pushTopCheckpoint(changes.newValue)
        toValue.defineCurrentValueAsCheckpoint(activeCheckpoint)
    }
  }

  override def checkInternals(c:Checker){
    c.check(toValue.newValue.toList equals fromValue.newValue.toList,
      Some("IdentitySeq: toValue.value=" + toValue.value + " should equal fromValue.value=" + fromValue.value))
  }
}
/*
/**
  *  roll backs to checkpoints above the top of the stack are translated into a linear set of instructions,
  *  so that hte topmost checkpoint is actually managed in a circle fashion, although it is not declared so.
  *  internal mechanisms ensure that the stacked representation of IntSequence does not grow monotonically with each move
  * @param fromValue
  * @param toValue
  * @param maxStack
  */
class IdentitySeqTopMostCheckpointCircleAbove(fromValue:ChangingSeqValue, toValue:CBLSSeqVar, maxStack:Long)
  extends Invariant
    with SeqNotificationTarget{

  registerStaticAndDynamicDependency(fromValue)
  toValue.setDefiningInvariant(this)
  finishInitialization()

  toValue := fromValue.value

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit {
    assert(v == fromValue)
    digestChanges(changes)
  }

  private var checkPointStackNotTop:List[IntSequence] = List.empty
  private var topCheckpoint:IntSequence = null

  private var levelTopCheckpoint:Long = -1L

  private def popTopCheckpoint(){
    checkPointStackNotTop match{
      case cp :: tail =>
        topCheckpoint = cp
        checkPointStackNotTop = tail
        assert(levelTopCheckpoint +1L == checkPointStackNotTop.size)
        levelTopCheckpoint -= 1L
      case _ =>
        topCheckpoint = null
        levelTopCheckpoint = -1L
    }
  }

  private def pushTopCheckpoint(newCheckpoint:IntSequence){
    if(topCheckpoint != null) {
      checkPointStackNotTop = topCheckpoint :: checkPointStackNotTop
      topCheckpoint = newCheckpoint
      levelTopCheckpoint += 1L
    }else{
      topCheckpoint = newCheckpoint
      levelTopCheckpoint += 1L
    }
  }

  def digestChanges(changes:SeqUpdate){
    changes match{
      case SeqUpdateInsert(value:Long,pos:Long,prev:SeqUpdate) =>
        digestChanges(prev)
        toValue.insertAtPosition(value,pos,changes.newValue)
      case SeqUpdateMove(fromIncluded:Long,toIncluded:Long,after:Long,flip:Boolean,prev:SeqUpdate) =>
        digestChanges(prev)
        toValue.move(fromIncluded,toIncluded,after,flip,changes.newValue)
      case SeqUpdateRemove(position:Long,prev:SeqUpdate) =>
        digestChanges(prev)
        toValue.remove(position,changes.newValue)
      case SeqUpdateAssign(s) =>
        toValue.setValue(s)
      case SeqUpdateLastNotified(value:IntSequence) =>
        //nothing to do here
        assert(value equals toValue.newValue)
      case r@SeqUpdateRollBackToCheckpoint(value:IntSequence,level:Long) =>
        while(level > levelTopCheckpoint){
          if(levelTopCheckpoint <= maxStack) {
            toValue.releaseTopCheckpoint()
          }
          popTopCheckpoint()
        }
        require(value quickEquals topCheckpoint)
        if(levelTopCheckpoint > maxStack) {
          //this checkpoint has not been communicated, so we translate with the howTo
          digestChanges(r.howToRollBack)
        }else{
          //we perform a roll back to the topmost communicated checkpoint
          //and after this roll back, we perform the moves that come back to this point
          toValue.rollbackToTopCheckpoint(value)
        }
      case SeqUpdateDefineCheckpoint(prev:SeqUpdate,activeCheckpoint:Boolean,level:Long) =>
        digestChanges(prev)
        while(level > levelTopCheckpoint){
          if(levelTopCheckpoint <= maxStack) {
            toValue.releaseTopCheckpoint()
          }
          popTopCheckpoint()
        }
        require(level == levelTopCheckpoint +1L)
        pushTopCheckpoint(changes.newValue)
        require(level == levelTopCheckpoint)
        if(levelTopCheckpoint <= maxStack) {
          toValue.defineCurrentValueAsCheckpoint(activeCheckpoint)
        }
    }
  }

  override def checkInternals(c:Checker){
    c.check(toValue.value equals fromValue.value,
      Some("IdentitySeqTopMostCheckpointCircleAbove: toValue.value=" +toValue.value + " should equal fromValue.value=" + fromValue.value))
  }
}
*/
class SeqCheckpointedValueStack[@specialized T]{
  private[this] var checkpointStackNotTop:List[(IntSequence,T)] = List.empty
  private[this] var _topCheckpoint:IntSequence = null
  private[this] var _outputAtTopCheckpoint:T = null.asInstanceOf[T]
  private[this] var checkpointStackLevel:Int = -1

  private def popCheckpointStackToLevel(level:Int,included:Boolean){
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

  private def popCheckpoint(){
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
    require(topCheckpoint quickEquals checkpoint, "topCheckpoint:" + topCheckpoint + " not quickEquals checkpoint:" + checkpoint)
    _outputAtTopCheckpoint
  }

  def topCheckpoint:IntSequence = _topCheckpoint

  def defineTopCheckpoint(checkpoint:IntSequence,savedValue:T){
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

  def defineCheckpoint(checkpoint:IntSequence,checkpointLevel:Int,savedValue:T){
    require(checkpointLevel <= checkpointStackLevel+1)
    require(checkpointLevel >= 0)
    popCheckpointStackToLevel(checkpointLevel,true)
    defineTopCheckpoint(checkpoint:IntSequence,savedValue:T)
  }
}