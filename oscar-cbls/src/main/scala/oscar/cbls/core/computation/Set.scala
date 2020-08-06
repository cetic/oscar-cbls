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

import oscar.cbls.algo.dll._
import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.rb.RedBlackTreeMap
import oscar.cbls.core.propagation._

import scala.collection.immutable.SortedSet

sealed trait SetValue extends Value{
  def value: SortedSet[Int]
  def domain:Domain
  def min:Int = domain.min.toInt
  def max:Int = domain.max.toInt
  def name:String
  override final def valueString: String = "{" + value.mkString(",") + "}"
}

object SetValue{
  implicit def intSet2IntSetVar(a:SortedSet[Int]):SetValue = CBLSSetConst(a)
  implicit def toFunction(i:SetValue):()=>SortedSet[Int] = () => i.value
}

class ChangingSetValueSnapShot(val variable:ChangingSetValue,val savedValue:SortedSet[Int]) extends AbstractVariableSnapShot(variable){
  override protected def doRestore() : Unit = {variable.asInstanceOf[CBLSSetVar] := savedValue}
}

class ValueWisePropagationWaveIdentifier()
abstract class ChangingSetValue(initialValue:SortedSet[Int], initialDomain:Domain)
  extends AbstractVariable with SetValue{
  require(initialDomain.max <= Int.MaxValue, s"The max value of the domain is lesser or equal to Int.MaxValue. Got : ${initialDomain.max}")
  private var privatedomain:Domain = initialDomain
  private var m_NewValue: SortedSet[Int] = initialValue
  private var OldValue:SortedSet[Int] = m_NewValue
  private[this] var domainSizeDiv10 = privatedomain.size/10
  def domain:Domain = privatedomain

  override def snapshot : ChangingSetValueSnapShot = new ChangingSetValueSnapShot(this,this.value)
  def valueAtSnapShot(s:Snapshot):SortedSet[Int] = s(this) match{case s:ChangingSetValueSnapShot => s.savedValue case _ => throw new Error("cannot find value of " + this + " in snapshot")}

  /**this must be protected because invariants might rework this after isntanciation
    * for CBLSVars, no problems*/
  protected def restrictDomain(d:Domain): Unit ={
    privatedomain = privatedomain.intersect(d)
    domainSizeDiv10 = privatedomain.size/10
  }

  override def toString:String = name + ":={" +
    (if(model == null || model.propagateOnToString) value
    else m_NewValue).mkString(",") + "}"

  /** this method is a toString that does not trigger a propagation.
    * use this when debugging your software.
    * you should specify to your IDE to render variable objects using this method isntead of the toString method
    * @return a string similar to the toString method
    */
  def toStringNoPropagate: String = name + ":={" + m_NewValue.foldLeft("")(
    (acc,intval) => if(acc.equalsIgnoreCase("")) ""+intval else acc+","+intval) + "}"

  //mechanism that manage key with value changes
  private val listeningElementsNonValueWise:DoublyLinkedList[(PropagationElement,Int)] = getDynamicallyListeningElements.permaFilter({case (pe,id) => id != Int.MinValue})

  override protected[core] def registerDynamicallyListeningElementNoKey(listening : PropagationElement, i : Int) : Unit = {
    super.registerDynamicallyListeningElementNoKey(listening, i)
  }

  def instrumentKeyToValueWiseKey(key:KeyForElementRemoval):ValueWiseKey = {
    createValueWiseMechanicsIfNeeded()
    new ValueWiseKey(key,this,null)
  }

  /**The values that have bee impacted since last propagation was performed.
    * null if set was assigned
    */
  private[this] var addedValues:QList[Int] = null
  private[this] var removedValues:QList[Int] = null
  private[this] var nbTouched:Int = 0

  protected def insertValue(v:Int): Unit ={
    if (!m_NewValue.contains(v)) insertValueNotPreviouslyIn(v)
  }

  protected def insertValueNotPreviouslyIn(v:Int): Unit ={
    if (nbTouched != -1){
      addedValues = QList(v,addedValues)
      nbTouched += 1
      if(nbTouched > domainSizeDiv10) nbTouched = -1
    }
    m_NewValue +=v
    notifyChanged()
  }

  protected def deleteValue(v:Int): Unit ={
    if (m_NewValue.contains(v)) deleteValuePreviouslyIn(v)
  }

  protected def deleteValues(values:Iterable[Int]): Unit ={
    values.foreach(deleteValue)
  }

  protected def insertValues(values:Iterable[Int]): Unit ={
    values.foreach(insertValue)
  }

  protected def deleteValuePreviouslyIn(v:Int): Unit ={
    if (nbTouched != -1){
      removedValues = QList(v,removedValues)
      nbTouched += 1
      if(nbTouched > domainSizeDiv10) nbTouched = -1
    }
    m_NewValue -=v
    notifyChanged()
  }

  /**We suppose that the new value is not the same as the actual value.
    * otherwise, there is a huge waste of time.
    * @param v the new value to set to the variable
    */
  protected def setValue(v:SortedSet[Int]): Unit ={
    removedValues = null
    addedValues = null
    nbTouched = -1
    m_NewValue = v
    notifyChanged()
  }

  private def createValueWiseMechanicsIfNeeded(): Unit ={
    if(valueToValueWiseKeys == null){
      require(Int.MinValue <= domain.min, "when using valueWise mechanism, the domain of sets should have a min >= Int.MinValue")

      offsetForValueWiseKey = domain.min.toInt
      val nbValues = this.domain.max.toInt - this.domain.min.toInt + 1
      require(Int.MinValue <= nbValues, "when using valueWise mechanism, the domain of sets should be reasonably small")

      valueToValueWiseKeys = Array.tabulate(nbValues)(_ => new DoublyLinkedList[ValueWiseKey]())
    }
  }
  private[this] var valueToValueWiseKeys:Array[DoublyLinkedList[ValueWiseKey]] = null
  private[this] var offsetForValueWiseKey:Int = Int.MaxValue

  @inline
  def addToValueWiseKeys(key:ValueWiseKey,value:Int):DLLStorageElement[ValueWiseKey] = {
    valueToValueWiseKeys(value - offsetForValueWiseKey).addElem(key)
  }

  @inline
  private def valueWiseKeysAtValue(value:Int):DoublyLinkedList[ValueWiseKey] = valueToValueWiseKeys(value - offsetForValueWiseKey)

  override def performPropagation(): Unit ={performSetPropagation()}

  @inline
  final protected def performSetPropagation(): Unit ={
    if(getDynamicallyListeningElements.isEmpty){
      //no need to do it gradually
      OldValue=m_NewValue
    }else{
      val (addedValues,deletedValues):(Iterable[Int],Iterable[Int]) = if (nbTouched == -1) {
        //need to call every listening one, so gradual approach required
        if(m_NewValue == OldValue) (List.empty,List.empty) else (m_NewValue.diff(OldValue),OldValue.diff(m_NewValue))

      }else {
        //TODO: this is slow, and it delives TreeSet. TreeSet are slow fo valueWise notifications
        //we have the set of values that have been touched (added or deleted)
        //but we need to check for each opf them if they have been both added and deleted
        var addedUnique = SortedSet.empty[Int] ++ this.addedValues
        var removedUnique = SortedSet.empty[Int] ++ this.removedValues
        for(inter <- addedUnique.intersect(removedUnique)){
          val inNew = m_NewValue.contains(inter)
          val inOld = OldValue.contains(inter)
          if(!inOld || inNew) {
            removedUnique = removedUnique - inter
          }
          if(!inNew || inOld) {
            addedUnique = addedUnique - inter
          }
        }
        (addedUnique,removedUnique)
      }

      assert((OldValue ++ addedValues -- deletedValues).equals(m_NewValue))

      if(addedValues.nonEmpty || deletedValues.nonEmpty) {
        //notifying the PE that listen to the whole set
        val dynListElements = listeningElementsNonValueWise
        val headPhantom = dynListElements.phantom
        var currentElement = headPhantom.next
        while (currentElement != headPhantom) {
          val e = currentElement.elem
          val inv : SetNotificationTarget = e._1.asInstanceOf[SetNotificationTarget]
          assert({
            this.model.notifiedInvariant = inv.asInstanceOf[Invariant]; true
          })
          inv.notifySetChanges(this, e._2, addedValues, deletedValues, OldValue, m_NewValue)
          assert({
            this.model.notifiedInvariant = null; true
          })
          //we go to the next to be robust against invariant that change their dependencies when notified
          //this might cause crash because dynamicallyListenedInvariants is a mutable data structure
          currentElement = currentElement.next
        }

        if(valueToValueWiseKeys != null) {
          val currentValueWisePropagationWaveIdentifier = new ValueWisePropagationWaveIdentifier()

          notifyForValues(addedValues, addedValues, deletedValues, currentValueWisePropagationWaveIdentifier)
          notifyForValues(deletedValues, addedValues, deletedValues, currentValueWisePropagationWaveIdentifier)
        }

      }
      //puis, on fait une affectation en plus, pour garbage collecter l'ancienne structure de donnees.
      OldValue=m_NewValue
    }
    this.addedValues = null
    this.removedValues = null
    nbTouched = 0
  }

  @inline  //This method is awfully slow, ad we do not know why
  private def notifyForValues(values : Iterable[Int],addedValues:Iterable[Int], deletedValues:Iterable[Int], currentValueWisePropagationWaveIdentifier:ValueWisePropagationWaveIdentifier): Unit ={
    val valuesIt = values.iterator
    while(valuesIt.hasNext){
      val value = valuesIt.next()
      val valueWiseKeys = valueWiseKeysAtValue(value)
      val headPhantom = valueWiseKeys.phantom
      var currentElement : DLLStorageElement[ValueWiseKey] = headPhantom.next
      while (currentElement != headPhantom) {
        val e : ValueWiseKey = currentElement.elem
        if(e.currentValueWisePropagationWaveIdentifier != currentValueWisePropagationWaveIdentifier) {
          e.currentValueWisePropagationWaveIdentifier = currentValueWisePropagationWaveIdentifier
          val target = e.target
          assert({
            this.model.notifiedInvariant = target.asInstanceOf[Invariant]
            true
          })
          target.notifySetChanges(this, Int.MinValue, addedValues, deletedValues, OldValue, m_NewValue)
          assert({
            this.model.notifiedInvariant = null
            true
          })
        }
        //we go to the next to be robust against invariant that change their dependencies when notified
        //this might cause crash because dynamicallyListenedInvariants is a mutable data structure
        currentElement = currentElement.next
      }
    }
  }

  def value:SortedSet[Int] = getValue(false)

  protected def newValue:SortedSet[Int] = getValue(true)

  private def getValue(NewValue:Boolean=false):SortedSet[Int] = {
    if (NewValue){
      assert(model.checkExecutingInvariantOK(definingInvariant),
        s"variable [$this] queried for latest val by non-controlling invariant")
      m_NewValue
    }else{
      if (model == null) return m_NewValue
      //if (definingInvariant == null && !model.propagating) return Value
      model.propagate(this)
      OldValue
    }
  }

  /**We suppose that the new value is not the same as the actual value.
    * otherwise, there is a huge waste of time.
    * @param v the new value to set to the variable
    */
  protected def :=(v:SortedSet[Int]): Unit = {setValue(v)}

  protected def :+=(i:Int): Unit = {this.insertValue(i)}
  protected def :-=(i:Int): Unit = {this.deleteValue(i)}

  def createClone:CBLSSetVar = {
    val clone = new CBLSSetVar(model, this.value, this.domain, s"clone of ${this.name}")
    clone <== this
    clone
  }

  override def checkInternals(c:Checker): Unit ={
    assert(this.definingInvariant == null || OldValue.intersect(m_NewValue).size == m_NewValue.size,
      s"internal error: Value: $m_NewValue OldValue: $OldValue")
  }
}
trait SetNotificationTarget extends PropagationElement{
  /**
   * this method will be called just before the variable "v" is actually updated.
   *
   * @param v
   * @param id d is always MinValue when notified for a valueWiseKey
   * @param addedValues
   * @param removedValues
   * @param oldValue
   * @param newValue
   */
  def notifySetChanges(v: ChangingSetValue, id: Int, addedValues: Iterable[Int], removedValues: Iterable[Int], oldValue: SortedSet[Int], newValue: SortedSet[Int]): Unit

  def registerDynamicValueWiseDependency(s:SetValue):ValueWiseKey = {
    s match{
      case c:ChangingSetValue =>
        val key = registerDynamicallyListenedElement(c,Int.MinValue)
        val valueWiseKey = c.instrumentKeyToValueWiseKey(key)
        valueWiseKey.target = this
        valueWiseKey
      case _ =>
        DoNothingValueWiseKey
    }
  }
}

class ValueWiseKey(originalKey:KeyForElementRemoval,setValue:ChangingSetValue,var target:SetNotificationTarget){

  val sizeOfSet:Int = setValue.max - setValue.min + 1
  val offset:Int = setValue.min
  var currentValueWisePropagationWaveIdentifier:ValueWisePropagationWaveIdentifier = null

  def performRemove(): Unit ={
    //remove all values in the focus of this key
    for(i <- valueToKey.values){
       i.delete()
    }
    originalKey.performRemove()
  }

  val minValue:Int = setValue.min
  val maxValue:Int = setValue.max

  var valueToKey:RedBlackTreeMap[DLLStorageElement[ValueWiseKey]] = RedBlackTreeMap.empty
  //val valueToKeyArray = Array.fill[DLLStorageElement[ValueWiseKey]](sizeOfSet)(null)

  def addToKey(value:Int): Unit ={
    if(!(minValue <= value  && value <= maxValue)) return
    val intValue = value
    require(!valueToKey.contains(intValue))
    valueToKey = valueToKey.insert(intValue,setValue.addToValueWiseKeys(this,intValue))
  }

  def removeFromKey(value:Int): Unit ={
    if(!(minValue <= value  && value <= maxValue)) return
    val intValue = value
    val k = valueToKey.get(intValue).get
    k.delete()
    valueToKey = valueToKey.remove(intValue)
  }
}

case object DoNothingValueWiseKey extends ValueWiseKey(DummyKeyForElementRemoval,null,null){
  override def addToKey(value : Int): Unit = {}

  override def removeFromKey(value : Int): Unit = {}
}

object ChangingSetValue{
  implicit val ord:Ordering[ChangingSetValue] = (o1: ChangingSetValue, o2: ChangingSetValue) => o1.compare(o2)
}

/**An IntSetVar is a variable managed by the [[oscar.cbls.core.computation.Store]] whose type is set of integer.
  * @param givenModel is the model in s-which the variable is declared, can be null if the variable is actually a constant, see [[oscar.cbls.core.computation.CBLSSetConst]]
  * @param initialDomain is the domain value of the variable. Some invariants exploit this value to declare fixed size arrays
  * @param initialValue is the initial value of the variable
  * @param n is the name of the variable, used for pretty printing only. if not set, a default will be used, based on the variable number
  * */
class CBLSSetVar(givenModel: Store, initialValue: SortedSet[Int], initialDomain:Domain, n: String = null)
  extends ChangingSetValue(initialValue, initialDomain) with Variable{

  require(givenModel != null)

  model = givenModel

  override def restrictDomain(d:Domain): Unit = super.restrictDomain(d)

  override def name: String = if (n == null) defaultName else n

  override def :=(v:SortedSet[Int]): Unit = {setValue(v)}

  override def :+=(i:Int): Unit = {this.insertValue(i)}
  override def :-=(i:Int): Unit = {this.deleteValue(i)}

  def <==(i: SetValue): Unit = {IdentitySet(this,i)}

  override def insertValue(v : Int) : Unit = super.insertValue(v)

  override def insertValueNotPreviouslyIn(v : Int) : Unit = super.insertValueNotPreviouslyIn(v)

  override def deleteValue(v : Int) : Unit = super.deleteValue(v)

  override def deleteValues(values : Iterable[Int]) : Unit = super.deleteValues(values)

  override def insertValues(values : Iterable[Int]) : Unit = super.insertValues(values)

  override def deleteValuePreviouslyIn(v : Int) : Unit = super.deleteValuePreviouslyIn(v)

  /** We suppose that the new value is not the same as the actual value.
    * otherwise, there is a huge waste of time.
    * @param v the new value to set to the variable
    */
  override def setValue(v : SortedSet[Int]) : Unit = super.setValue(v)

  override def value : SortedSet[Int] = super.value
}

object CBLSSetVar{
  //this conversion is forbidden because we inserted the new grammar.
  //implicit def toIntSet(v:IntSetVar):SortedSet[Int] = v.value

  def apply(s:Store, v:Iterable[Int] = List.empty, d:Domain=FullIntRange, name:String="") = {
    val emptySet:SortedSet[Int] = SortedSet.empty
    new CBLSSetVar(s, emptySet ++ v, d, name)
  }

  implicit val ord:Ordering[CBLSSetVar] = (o1: CBLSSetVar, o2: CBLSSetVar) => o1.compare(o2)
}

object CBLSSetConst {
  def apply(value : SortedSet[Int]) = new CBLSSetConst(value)
}

/**
 * An IntSetConst is an IntSetVar that has a constant value, defined by a set of integer.
 * It has no associated model, as there is no need to incorporate it into any propagation process.
 * @param value: the value of the constant
 * @author renaud.delandtsheer@cetic.be
 * */
class CBLSSetConst(override val value:SortedSet[Int])
  extends SetValue{
  override def toString:String = "Set{" + value.mkString(",") + "}"
  override def domain:Domain = DomainRange(value.min,value.max)
  override val min: Int = if (value.isEmpty) Int.MaxValue else value.min
  override val max: Int = if(value.isEmpty) Int.MinValue else value.max
  override def name: String = toString
}

/*
* @author renaud.delandtsheer@cetic.be
 */
abstract class SetInvariant(initialValue:SortedSet[Int] = SortedSet.empty,
                            initialDomain:Domain = DomainRange(0, Int.MaxValue))
  extends ChangingSetValue(initialValue, initialDomain)
  with Invariant {

  override def definingInvariant: Invariant = this
  override def isControlledVariable:Boolean = true
  override def isDecisionVariable:Boolean = false

  override def model: Store = propagationStructure.asInstanceOf[Store]
  override def hasModel:Boolean = schedulingHandler != null

  private var customName:String = null
  /**use this if you want to give a particular name to this concept, to be used in toString*/
  def setName(n:String):SetInvariant = {
    customName = n
    this
  }

  override final def name: String = if(customName == null) this.getClass.getSimpleName else customName

  override final def performPropagation(): Unit ={
    performInvariantPropagation()
    performSetPropagation()
  }
}

object IdentitySet{
  def apply(toValue:CBLSSetVar, fromValue:SetValue): Unit ={
    fromValue match{
      case c:CBLSSetConst => toValue := c.value
      case c:ChangingSetValue => new IdentitySet(toValue, c)
    }
  }
}

/** an invariant that is the identity function
  * @author renaud.delandtsheer@cetic.be
  */
class IdentitySet(toValue:CBLSSetVar, fromValue:ChangingSetValue)
  extends Invariant
  with SetNotificationTarget{

  registerStaticAndDynamicDependency(fromValue)
  toValue.setDefiningInvariant(this)
  finishInitialization()

  toValue := fromValue.value

  override def notifySetChanges(v: ChangingSetValue, id: Int, addedValues: Iterable[Int], removedValues: Iterable[Int], oldValue: SortedSet[Int], newValue: SortedSet[Int]) : Unit = {
    assert(v == this.fromValue)
    for(added <- addedValues)toValue.insertValueNotPreviouslyIn(added)
    for(deleted <- removedValues) toValue.deleteValuePreviouslyIn(deleted)
  }

  override def checkInternals(c:Checker): Unit ={
    c.check(toValue.value equals fromValue.value)
  }
}
