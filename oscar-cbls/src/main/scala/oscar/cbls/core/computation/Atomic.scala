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
/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Renaud De Landtsheer
  ******************************************************************************/
package oscar.cbls.core.computation

import oscar.cbls.core.propagation.{Checker, PropagationElement}

/** this is something that has an integer value.
  * this value can be queried, and invariants can be posted on it,
  * and it can be used on the righ hand of <== operator
  */
sealed trait AtomicValue[T] extends Value{
  def value: T

  def name:String
  override def valueString: String = s"$value"
}

/**An AtomicVar is a variable managed by the [[oscar.cbls.core.computation.Store]] whose type is integer.
  *
  * initialDomain is the domain value of the variable. Some invariants exploit this value to declare fixed size arrays
  * @param initialValue is the value of the variable
  */
abstract class ChangingAtomicValue[T](initialValue:T)
  extends AbstractVariable with AtomicValue[T]{

  override def snapshot : ChangingAtomicValueSnapShot[T] = new ChangingAtomicValueSnapShot(this,this.value)
  def valueAtSnapShot(s:Snapshot):T = s(this) match{case s:ChangingAtomicValueSnapShot[T] => s.savedValue case _ => throw new Error("cannot find value of " + this + " in snapshot")}

  private[this] var mNewValue: T = initialValue
  private[this] var mOldValue = mNewValue

  override def toString: String = {
    if(model != null && model.propagateOnToString) s"$name:=$value" else s"$name:=$mNewValue"
  }

  override def toStringNoPropagate = s"$name:=$mNewValue"

  @inline
  def setValue(v:T): Unit ={
    if (v != mNewValue){
      mNewValue = v
      notifyChanged()
    }
  }

  override def value: T = {
    if (model == null) return mNewValue
    val propagating = model.propagating
    if (definingInvariant == null && !propagating) return mNewValue //the new value, actually!
    if(!propagating) model.propagate(this)
    mOldValue
  }

  def newValue:T = {
    assert(model.checkExecutingInvariantOK(definingInvariant),
      s"variable [$this] queried for latest val by non-controlling invariant")
    mNewValue
  }

  override def performPropagation(): Unit ={performAtomicPropagation()}

  final protected def performAtomicPropagation(): Unit ={
    if(mOldValue!=mNewValue){
      val old=mOldValue
      mOldValue=mNewValue  //TODO: the change should be made AFTER the notification

      val dynListElements = getDynamicallyListeningElements
      val headPhantom = dynListElements.headPhantom
      var currentElement = headPhantom.next
      while(currentElement != headPhantom){
        val e = currentElement.elem
        performNotificationToListeningInv(e._1,e._2,old,mNewValue)
        //we go to the next to be robust against invariant that change their dependencies when notified
        //this might cause crash because dynamicallyListenedInvariants is a mutable data structure
        currentElement = currentElement.next
      }
    }
  }

  def performNotificationToListeningInv(inv:PropagationElement,id:Int,oldVal:T,newVal:T): Unit

  override def checkInternals(c:Checker): Unit ={
    c.check(mOldValue == mNewValue)
  }

  protected def :=(v: T): Unit ={
    setValue(v)
  }

  def compare(that: ChangingAtomicValue[T]): Long = {
    assert(this.uniqueID != -1L, s"cannot compare non-registered PropagationElements this: [$this] that: [$that]")
    assert(that.uniqueID != -1L, s"cannot compare non-registered PropagationElements this: [$this] that: [$that]")
    this.uniqueID - that.uniqueID
  }
}

class ChangingAtomicValueSnapShot[T](val variable:ChangingAtomicValue[T],val savedValue:T) extends AbstractVariableSnapShot(variable){
  override protected def doRestore() : Unit = {variable.asInstanceOf[CBLSAtomicVar[T]] := savedValue}
}

/**An IntVar is a variable managed by the [[oscar.cbls.core.computation.Store]] whose type is integer.
  *
  * @param givenModel is the model in s-which the variable is declared, can be null if the variable is actually a constant, see [[oscar.cbls.core.computation.CBLSIntConst]]
  * initialDomain is the domain value of the variable. Some invariants exploit this value to declare fixed size arrays
  * @param initialValue is the initial value of the variable
  * @param n is the name of the variable, used for pretty printing only. if not set, a default will be used, based on the variable number
  */
abstract class CBLSAtomicVar[T](givenModel: Store, initialValue: T, n: String = null)
  extends ChangingAtomicValue(initialValue) with Variable{

  require(givenModel != null)

  model = givenModel

  override def name: String = if (n == null) defaultName else n

  override def :=(v: T): Unit ={
    setValue(v)
  }

  /**this operator swaps the value of two IntVar*/
  def :=:(v:CBLSAtomicVar[T]): Unit ={
    val a:T = v.value
    v:=this.value
    this := a
  }

  /**this operator swaps the value of two IntVar*/
  def swap(v: CBLSAtomicVar[T]): Unit ={
    this :=: v
  }
}

/**
  * An AtomicConst is an [[oscar.cbls.core.computation.CBLSAtomicVar]] that has a constant value.
  * It has no associated model, as there is no need to incorporate it into any propagation process.
  * notice that you should not attempt to create a CBLSAtomicConst directly; use the companion object for an efficient memoïzation
  * @param value: the value of the constant
  * @author renaud.delandtsheer@cetic.be
  */
class CBLSAtomicConst[T](override val value:T)
  extends AtomicValue[T]{
  override def toString:String = s"$value"
  override def name: String = value.toString
}

object CBLSAtomicConst{
  //def apply(a:Long) = new CBLSIntConst(a)
  def apply[T](a:T) = {
    new CBLSAtomicConst[T](a)
  }
}

/** this is a special case of invariant that has a single output variable, that is an IntVar
  * @author renaud.delandtsheer@cetic.be
  */
abstract class AtomicInvariant[T](initialValue:T = 0L)
  extends ChangingAtomicValue[T](initialValue)
    with Invariant{

  override def definingInvariant: Invariant = this
  override def isControlledVariable:Boolean = true
  override def isDecisionVariable:Boolean = false

  override def model: Store = propagationStructure.asInstanceOf[Store]

  override def hasModel:Boolean = schedulingHandler != null

  private var customName:String = null
  /**use this if you want to give a particular name to this concept, to be used in toString*/
  def setName(n:String):AtomicInvariant[T] = {
    customName = n
    this
  }

  override final def name: String = if(customName == null) this.getClass.getSimpleName else customName

  override final def performPropagation(): Unit ={
    performInvariantPropagation()
    performAtomicPropagation()
  }
}
