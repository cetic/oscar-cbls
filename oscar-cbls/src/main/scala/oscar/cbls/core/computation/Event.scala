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

import scala.collection.immutable.SortedSet

//TODO: porter à la nouvelle architecture.

object Event{

  def apply(v:Variable,
            action: =>Unit):Event = {
    val toreturn = new Event(v,null,null)
    toreturn.setAction(() => action)
    //    if (intaction != null) toreturn.setIntAction(intaction)
    //   if (intsetaction != null) toreturn.setIntSetAction(intsetaction)
    toreturn
  }

  def apply(v:Variable,
            action: =>Unit,
            ModifiedVars:Iterable[Variable]):Event = {
    val toreturn = new Event(v,null,ModifiedVars)
    toreturn.setAction(() => action)
    //    if (intaction != null) toreturn.setIntAction(intaction)
    //   if (intsetaction != null) toreturn.setIntSetAction(intsetaction)
    toreturn
  }

  /**this is an event, which is used to run dedicated code when the value of some variable changes.
    * It can also impact on the value of other variable, although it is not recommended
    * to implement invariants as events, because you cannot have the delta.
    * @param v the variable whose change will trigger the execution of action
    */
  def apply(v:Variable, w:Variable,
            intaction:Int=>Unit):Event = {
    val toreturn = new Event(v,w,null)
    if (intaction != null) toreturn.setIntAction(intaction)
    toreturn
  }

  /**this is an event, which is used to run dedicated code when the value of some variable changes.
    * It can also impact on the value of other variable, although it is not recommended
    * to implement invariants as events, because you cannot have the delta.
    * @param v the variable whose change will trigger the execution of action
    * @param ModifiedVars the variables that could be modified by the Event
    */
  def apply(v:Variable, w:Variable,
            intaction:Int=>Unit,
            ModifiedVars:Iterable[Variable]):Event = {
    val toreturn = new Event(v,w,ModifiedVars)
    if (intaction != null) toreturn.setIntAction(intaction)
    toreturn
  }

  def apply(v:Variable,
            intaction:Int=>Unit,
            ModifiedVars:Iterable[Variable]):Event = {
    val toreturn = new Event(v,null,ModifiedVars)
    if (intaction != null) toreturn.setIntAction(intaction)
    toreturn
  }

  def apply(v:IntValue,
            intaction:Int=>Unit):Event = {
    val toreturn = new Event(v,null,null)
    if (intaction != null) toreturn.setIntAction(intaction)
    toreturn
  }

  /*  def apply(v:Variable, w:Variable,
              action: =>Unit = null,
              intaction:Long=>Unit = null,
              intsetaction:SortedSet[Long]=>Unit = null,
              intintaction: (Long,Long)=>Unit = null,
              intsetintsetaction:(SortedSet[Long],SortedSet[Long]) => Unit = null,
              intsetintaction:(SortedSet[Long],Long) => Unit = null,
              intintsetaction:(Long,SortedSet[Long]) => Unit = null,
              ModifiedVars:Iterable[Variable] = null):Event = {
      val toreturn = new Event(v,w,ModifiedVars)
      toreturn.setAction((_:Unit) => {action})
      if (intaction != null) toreturn.setIntAction(intaction)
      if (intsetaction != null) toreturn.setIntSetAction(intsetaction)
      if (intintaction!=null) toreturn.setintintaction(intintaction)
      if (intsetintsetaction!=null) toreturn.setintsetintsetaction(intsetintsetaction)
      if (intsetintaction!=null) toreturn.setintsetintaction(intsetintaction)
      if (intintsetaction!=null) toreturn.setintintsetaction(intintsetaction)
      toreturn
    }
    */
}

/**Use the apply method in the companion object for building this*/
class Event(v:Value, w:Variable, ModifiedVars:Iterable[Variable])
  extends Invariant with IntNotificationTarget with SetNotificationTarget{

  //unfortunately, it is not possible to pass a type "=>Unit" as parameter to a case class.

  private var action: ()=>Unit=null
  private var actionIntParam: Int=>Unit = null
  private var actionIntSetParam: SortedSet[Int] => Unit = null

  private var oldIntv = 0
  private var oldIntSetv:SortedSet[Int] = SortedSet.empty
  private var oldIntw = 0
  private var oldIntSetw:SortedSet[Int] = SortedSet.empty

  private var intintaction: (Int,Int) => Unit = null
  private var intsetintsetaction:(SortedSet[Int],SortedSet[Int]) => Unit = null
  private var intsetintaction:(SortedSet[Int],Int) => Unit = null
  private var intintsetaction:(Int,SortedSet[Int]) => Unit = null

  def setAction(action: ()=>Unit): Unit ={
    this.action = action
  }

  def setIntAction(action: Int=>Unit): Unit ={
    this.actionIntParam = action
    oldIntv = v.asInstanceOf[IntValue].valueInt
  }

  def setIntSetAction(action: SortedSet[Int] => Unit): Unit ={
    this.actionIntSetParam = action
    oldIntSetv = v.asInstanceOf[CBLSSetVar].value
  }

  def setintintaction(intintaction: (Int,Int)=>Unit): Unit ={
    this.intintaction = intintaction
    this.oldIntv = v.asInstanceOf[CBLSIntVar].valueInt
    this.oldIntw = w.asInstanceOf[CBLSIntVar].valueInt
  }

  def setintsetintsetaction(intsetintsetaction:(SortedSet[Int],SortedSet[Int]) => Unit): Unit ={
    this.intsetintsetaction = intsetintsetaction
    this.oldIntSetv = v.asInstanceOf[CBLSSetVar].value
    this.oldIntSetw = w.asInstanceOf[CBLSSetVar].value
  }

  def setintsetintaction(intsetintaction:(SortedSet[Int],Int) => Unit): Unit ={
    this.intsetintaction = intsetintaction
    this.oldIntSetv = v.asInstanceOf[CBLSSetVar].value
    this.oldIntw = w.asInstanceOf[CBLSIntVar].valueInt
  }

  def setintintsetaction(intintsetaction:(Int,SortedSet[Int]) => Unit): Unit ={
    this.intintsetaction = intintsetaction
    this.oldIntv = v.asInstanceOf[CBLSIntVar].valueInt
    this.oldIntSetw = w.asInstanceOf[CBLSSetVar].value
  }

  registerStaticAndDynamicDependency(v)
  if(w!=null)  registerStaticAndDynamicDependency(w)
  finishInitialization()
  if (ModifiedVars != null)
    for(variable <- ModifiedVars){variable.setDefiningInvariant(this)}

  override def notifyIntChanged(v: ChangingIntValue, i: Int, OldVal: Long, NewVal: Long): Unit ={
    scheduleForPropagation()
  }

  override def notifySetChanges(v: ChangingSetValue, id: Int, addedValues: Iterable[Int], removedValues: Iterable[Int], oldValue: SortedSet[Int], newValue: SortedSet[Int]): Unit = {
    scheduleForPropagation()
  }

  override def performInvariantPropagation(): Unit ={
    if (action != null) action()

    if (actionIntParam!= null){
      actionIntParam(oldIntv)
    }
    if (actionIntSetParam != null){
      actionIntSetParam(oldIntSetv)
    }
    if(intintaction!=null){
      intintaction(oldIntv,oldIntw)
    }
    if (intsetintsetaction!=null){
      intsetintsetaction(oldIntSetv,oldIntSetw)
    }
    if (intsetintaction!=null){
      intsetintaction(oldIntSetv,oldIntw)
    }
    if (intintsetaction != null){
      intintsetaction(oldIntv,oldIntSetw)
    }

    //updating internal vars

    if (actionIntParam!= null){
      oldIntv = v.asInstanceOf[IntValue].valueInt
    }
    if (actionIntSetParam != null){
      oldIntSetv = v.asInstanceOf[CBLSSetVar].value
    }
    if(intintaction!=null){
      oldIntv = v.asInstanceOf[CBLSIntVar].valueInt
      oldIntw = w.asInstanceOf[CBLSIntVar].valueInt
    }
    if (intsetintsetaction!=null){
      oldIntSetv = v.asInstanceOf[CBLSSetVar].value
      oldIntSetw = w.asInstanceOf[CBLSSetVar].value
    }
    if (intsetintaction!=null){
      oldIntSetv = v.asInstanceOf[CBLSSetVar].value
      oldIntw = w.asInstanceOf[CBLSIntVar].valueInt
    }
    if (intintsetaction != null){
      oldIntv = v.asInstanceOf[CBLSIntVar].valueInt
      oldIntSetw = w.asInstanceOf[CBLSSetVar].value
    }
  }

  override def checkInternals(c: Checker): Unit = c.check(true, Some("Event.checkInternals"))

}
