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

import oscar.cbls.core.propagation.Checker
import oscar.util.RandomGenerator

import scala.collection.mutable.{Map => MMap}

/** this is something that has an integer value.
  * this value can be queried, and invariants can be posted on it,
  * and it can be used on the righ hand of <== operator
  */
sealed trait IntValue extends Value{
  def value: Long
  def valueInt: Int
  def domain:Domain
  def min: Long = domain.min
  def minInt: Int = longToInt(min)
  def max: Long = domain.max
  def maxInt: Int = longToInt(max)

  def name:String
  override def valueString: String = s"$value"
  def restrictDomain(d:Domain): Unit

  def longToInt(value:Long):Int = {
    val i = value.toInt
    if (i != value) throw new ArithmeticException(s"integer overflow:$value")
    i
  }
}

object IntValue {

  implicit def intArray2IntValueArray(a: Array[Long]): Array[CBLSIntConst] = a.map(CBLSIntConst(_))

  implicit def toFunction(i: IntValue): () => Long = () => i.value

  implicit val ord: Ordering[IntValue] = (o1: IntValue, o2: IntValue) => {
    (o1, o2) match {
      case (a: CBLSIntConst, b: CBLSIntConst) => a.value compare b.value
      case (a: ChangingIntValue, b: ChangingIntValue) => a.uniqueID - b.uniqueID
      case (_: CBLSIntConst, _: ChangingIntValue) => -1
      case (_: ChangingIntValue, _: CBLSIntConst) => 1
    }
  }
}

trait IntNotificationTarget{
  def notifyIntChanged(v: ChangingIntValue, id: Int, oldVal: Long, newVal: Long): Unit
}

trait ShortIntNotificationTarget{
  def notifyIntChanged(v: ChangingIntValue, id: Int, oldVal: Int, newVal: Int): Unit
}

/**An IntVar is a variable managed by the [[oscar.cbls.core.computation.Store]] whose type is integer.
  *
  * @param initialDomain is the domain value of the variable. Some invariants exploit this value to declare fixed size arrays
  * @param initialValue is the value of the variable
  */
abstract class ChangingIntValue(initialValue:Long, initialDomain:Domain)
  extends AbstractVariable with IntValue{

  assert(initialDomain.contains(initialValue),
    s"$initialValue is not in the domain of ${this.name}($initialDomain). This might indicate an integer overflow.")

  override def snapshot : ChangingIntValueSnapShot = new ChangingIntValueSnapShot(this,this.value)
  def valueAtSnapShot(s:Snapshot):Long = s(this) match{case s:ChangingIntValueSnapShot => s.savedValue case _ => throw new Error("cannot find value of " + this + " in snapshot")}

  private[this] var privatedomain:Domain = initialDomain
  private[this] var mNewValue: Long = initialValue
  private[this] var mOldValue = mNewValue

  def domain:Domain = privatedomain

  def restrictDomain(d:Domain): Unit = {
    privatedomain = privatedomain.intersect(d)
    require(privatedomain.contains(mNewValue),
      s"you are restricting a domain and the value is not in this domain; value:$mNewValue newDomain:$d")
  }

  //Unions the domain with d
  def relaxDomain(d:Domain): Unit = {
    require(this.getStaticallyListeningElements.isEmpty,
      "you cannot relax the domain when someone is already listening to you")
    privatedomain = privatedomain.union(d)
  }

  def inDomain(v:Long): Boolean = privatedomain.contains(v)
  def domainSize:Long = privatedomain.size

  override def toString: String = {
    if (model != null && model.propagateOnToString)
      s"$name:=$value"
    else s"$name:=$mNewValue"
  }
  override def toStringNoPropagate = s"$name:=$mNewValue"

  @inline
  def setValue(v:Long): Unit ={
    if (v != mNewValue){
      assert(domain.contains(v),
        s"$v is not in the domain of $this($min..$max). This might indicate an integer overflow.")
      mNewValue = v
      notifyChanged()
    }
  }

  def adjustToDomain(v:Long):Long = {
    (v max this.min) min this.max
  }
  def adjustToDomainModulo(v:Long):Long = {
    val modVal = Math.max(domain.max - domain.min,1)
    val adjusted = (v - domain.min) % modVal
    adjusted + domain.min
  }

  override def value: Long = {
    if (model == null) return mNewValue
    val propagating = model.propagating
    if (definingInvariant == null && !propagating) return mNewValue //the new value, actually!
    if(!propagating) model.propagate(this)
    mOldValue
  }

  override def valueInt: Int = {
    longToInt(value)
  }

  def newValue:Long = {
    assert(model.checkExecutingInvariantOK(definingInvariant),
      s"variable [$this] queried for latest val by non-controlling invariant")
    mNewValue
  }

  def newValueInt:Int = {
    longToInt(newValue)
  }

  override def performPropagation(): Unit = {performIntPropagation()}

  final protected def performIntPropagation(): Unit ={
    if(mOldValue!=mNewValue){
      val old=mOldValue
      mOldValue=mNewValue  //TODO: the change should be made AFTER the notification

      val dynListElements = getDynamicallyListeningElements
      val headPhantom = dynListElements.headPhantom
      var currentElement = headPhantom.next
      while(currentElement != headPhantom){
        val e = currentElement.elem
        e._1 match {
          case intInvariant: IntNotificationTarget => {
            assert({this.model.notifiedInvariant=intInvariant.asInstanceOf[Invariant]; true})
            intInvariant.notifyIntChanged(this, e._2, old, mNewValue)
            assert({this.model.notifiedInvariant=null; true})
          }
          case shortIntInvariant: ShortIntNotificationTarget => {
            assert({this.model.notifiedInvariant=shortIntInvariant.asInstanceOf[Invariant]; true})
            shortIntInvariant.notifyIntChanged(this, e._2, longToInt(old), longToInt(mNewValue))
            assert({this.model.notifiedInvariant=null; true})
          }
        }
        
        //we go to the next to be robust against invariant that change their dependencies when notified
        //this might cause crash because dynamicallyListenedInvariants is a mutable data structure
        currentElement = currentElement.next
      }
    }
  }

  override def checkInternals(c:Checker): Unit ={
    c.check(mOldValue == mNewValue)
  }

  protected def :=(v: Long): Unit = {
    setValue(v)
  }

  protected def :+=(v: Long): Unit = {
    setValue(v + mNewValue)
  }

  protected def :*=(v: Long): Unit = {
    setValue(v * mNewValue)
  }

  protected def :-=(v:Long): Unit = {
    setValue(mNewValue - v)
  }

  /** increments the variable by one
    */
  protected def ++(): Unit = {
    setValue(1L + mNewValue)
  }

  def createClone:CBLSIntVar = {
    val clone = new CBLSIntVar(model, this.value, this.domain, "clone of ${this.name}")
    clone <== this
    clone
  }

  def compare(that: ChangingIntValue): Long = {
    assert(this.uniqueID != -1L, s"cannot compare non-registered PropagationElements this: [$this] that: [$that]")
    assert(that.uniqueID != -1L, s"cannot compare non-registered PropagationElements this: [$this] that: [$that]")
    this.uniqueID - that.uniqueID
  }
}

object ChangingIntValue{
  implicit val ord:Ordering[ChangingIntValue] = (o1: ChangingIntValue, o2: ChangingIntValue) => o1.compare(o2)
}

class ChangingIntValueSnapShot(val variable:ChangingIntValue,val savedValue:Long) extends AbstractVariableSnapShot(variable){
  override protected def doRestore() : Unit = {variable.asInstanceOf[CBLSIntVar] := savedValue}
}

/**An IntVar is a variable managed by the [[oscar.cbls.core.computation.Store]] whose type is integer.
  *
  * @param givenModel is the model in s-which the variable is declared, can be null if the variable is actually a constant, see [[oscar.cbls.core.computation.CBLSIntConst]]
  * @param initialDomain is the domain value of the variable. Some invariants exploit this value to declare fixed size arrays
  * @param initialValue is the initial value of the variable
  * @param n is the name of the variable, used for pretty printing only. if not set, a default will be used, based on the variable number
  */
class CBLSIntVar(givenModel: Store, initialValue: Long, initialDomain:Domain, n: String = null)
  extends ChangingIntValue(initialValue,initialDomain) with Variable{
  
  require(givenModel != null)

  model = givenModel

  override def name: String = if (n == null) defaultName else n

  override def :=(v: Long): Unit ={
    setValue(v)
  }

  def assignWithAdjust (v: Long): Unit ={
    setValue(adjustToDomain(v))
  }

  def incrementWithAdjust (v: Long): Unit ={
    setValue(adjustToDomain(v+newValue))
  }

  def assignWithModuloAdjust (v: Long): Unit ={
    setValue(adjustToDomainModulo(v))
  }

  override def :+=(v: Long): Unit ={
    setValue(v + newValue)
  }

  override def :*=(v: Long): Unit ={
    setValue(v * newValue)
  }

  override def :-=(v:Long): Unit ={
    setValue(newValue - v)
  }

  /** increments the variable by one
    */
  override def ++(): Unit ={
    setValue(1L + newValue)
  }

  /**this operator swaps the value of two IntVar*/
  def :=:(v:CBLSIntVar): Unit ={
    val a:Long = v.value
    v:=this.value
    this := a
  }

  /**this operator swaps the value of two IntVar*/
  def swap(v: CBLSIntVar): Unit ={
    this :=: v
  }

  def <==(i: IntValue): Unit ={IdentityInt(this,i)}

  def randomize(): Unit ={
    if(this.max != this.min) {
      require(this.max - this.min < Int.MaxValue, "The domain is too wide to take a random value")
      this := this.min + RandomGenerator.nextInt((this.max - this.min).toInt)
    }
  }
}

object CBLSIntVar{

  def apply(model: Store, value:Long = 0L, d:Domain = FullRange, name:String = null) =
    new CBLSIntVar(model, value, d, name)

  implicit val ord:Ordering[CBLSIntVar] = (o1: CBLSIntVar, o2: CBLSIntVar) => o1.compare(o2)
}

/**
 * An IntConst is an [[oscar.cbls.core.computation.CBLSIntVar]] that has a constant value.
 * It has no associated model, as there is no need to incorporate it into any propagation process.
 * notice that you should not attempt to create a CBLSIntConst directly; use the companion object for an efficient memoïzation
* @param value: the value of the constant
* @author renaud.delandtsheer@cetic.be
*/
class CBLSIntConst(override val value:Long)
  extends IntValue{
  override def valueInt: Int = {
    require(value <= Int.MaxValue, "The constant value is higher than Int.MaxValue")
    value.toInt
  }
  override def toString:String = s"$value"
  override def domain: SingleValueDomain = SingleValueDomain(value)
  override def min: Long = value
  override def max: Long = value
  override def name = value.toString
  override def restrictDomain(d:Domain): Unit ={
    require(d.contains(value))
  }
}

object CBLSIntConst{
  implicit def int2IntValue(a: Long): IntValue = apply(a)
  implicit def int2IntConst(a: Long): CBLSIntConst = apply(a)
  //def apply(a:Long) = new CBLSIntConst(a)
  val constMap = MMap.empty[Long,CBLSIntConst]
  def apply(a:Long) = {
    if(constMap.contains(a))constMap(a)
    else{
      val res = new CBLSIntConst(a)
      constMap(a) = res
      res
    }
  }
}

/** this is a special case of invariant that has a single output variable, that is an IntVar
  * @author renaud.delandtsheer@cetic.be
  */
abstract class IntInvariant(initialValue:Long = 0L, initialDomain:Domain = FullRange)
  extends ChangingIntValue(initialValue, initialDomain)
  with Invariant{

  override def definingInvariant: Invariant = this
  override def isControlledVariable:Boolean = true
  override def isDecisionVariable:Boolean = false

  override def model: Store = propagationStructure.asInstanceOf[Store]

  override def hasModel:Boolean = schedulingHandler != null

  private var customName:String = null
  /**use this if you want to give a particular name to this concept, to be used in toString*/
  def setName(n:String):IntInvariant = {
    customName = n
    this
  }

  override final def name: String = if(customName == null) this.getClass.getSimpleName else customName

  override final def performPropagation(): Unit ={
    performInvariantPropagation()
    performIntPropagation()
  }
}

object IdentityInt{
  def apply(toValue:CBLSIntVar, fromValue:IntValue): Unit ={
    fromValue match{
      case c:CBLSIntConst => toValue := c.value
      case c:ChangingIntValue => new IdentityInt(toValue, c)
    }
  }
}

/** an invariant that is the identity function
  * @author renaud.delandtsheer@cetic.be
  */
class IdentityInt(toValue:CBLSIntVar, fromValue:IntValue) extends Invariant with IntNotificationTarget{
  registerStaticAndDynamicDependency(fromValue)
  toValue.setDefiningInvariant(this)
  finishInitialization()

  toValue := fromValue.value

  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Long, NewVal: Long): Unit = {
    toValue := NewVal
  }

  override def checkInternals(c:Checker): Unit ={
    c.check(toValue.value == fromValue.value)
  }
}
