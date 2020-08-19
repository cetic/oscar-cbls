/*
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.cbls.lib.invariant.logic

import oscar.cbls.core.computation._
import oscar.cbls.core.propagation.Checker

/**
  * And(vars)
  *
  * outputs true (0L) iff all vars are true (0L) otherwise false (sum(vars))
  *
  * Calculates false as the sum of all vars.
  * This could be done differently to get a violation function that behaves differently,
  * we could, for example, compute the squared error, average, or median value.
  * But sum will do for now.
  *
  * @param vars is an iterable of IntVars representing booleans that are true iff 0L, otherwise false.
  * @author gustav.bjordal@it.uu.se
  */
case class And(vars: Iterable[IntValue])
  extends IntInvariant(
    vars.foldLeft(0L)((a: Long, b: IntValue) => a + b.value),
    Domain(0 ,vars.foldLeft(0L)((acc, intvar) => DomainHelper.safeAdd(acc, intvar.max))))
    with IntNotificationTarget{

  for (v <- vars) registerStaticAndDynamicDependency(v)
  finishInitialization()

  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Long, NewVal: Long): Unit = {
    this :+= NewVal - OldVal
  }

  override def checkInternals(c: Checker): Unit = {
    c.check(this.value == vars.foldLeft(0L)((acc, intvar) => acc + intvar.value),
      Some("output.value == vars.foldLeft(0L)((acc,intvar) => acc+intvar.value)"))
  }
}

/**
  * Or(vars)
  *
  * outputs true (0L) iff any vars are true (0L) otherwise false (min(vars))
  *
  * Calculates false as the minimum value of vars.
  * This could be done differently to get a violation function that behaves differently,
  * we could, for example, return 0L if true and otherwise compute the squared error, average, or median value.
  * But min will do for now.
  *
  * @param vars is an iterable of IntVars representing booleans that are true iff 0L, otherwise false.
  * @author gustav.bjordal@it.uu.se
  */
case class Or(vars: Array[IntValue])
  extends IntInvariant(
    if(vars.exists(_.value == 0L)) { 0L } else { vars.foldLeft(0L)((a: Long, b: IntValue) => a + b.value)/vars.length },
    Domain(0L ,vars.foldLeft(0L)((acc, intvar) => DomainHelper.safeAdd(acc, intvar.max))))
    with IntNotificationTarget{

  for (v <- vars) registerStaticAndDynamicDependency(v)
  finishInitialization()

  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Long, NewVal: Long): Unit = {
    if(vars.exists(_.value == 0L)) {
      this := 0L
    }else{
      this := vars.foldLeft(0L)((a: Long, b: IntValue) => a + b.value) /vars.length
    }
  }

  override def checkInternals(c: Checker): Unit = {
    c.check(this.value == vars.foldLeft(0L)((acc, intvar) => acc + intvar.value),
      Some("output.value == vars.foldLeft(0L)((acc,intvar) => acc+intvar.value)"))
  }
}

/**
  * bool2int(var)
  *
  * outputs 1L iff var is true (0L) otherwise 0L
  *
  * Warning: Note that this breaks the representation of booleans, where 0L is true.
  *
  * @param v an IntValue
  * @author gustav.bjordal@it.uu.se
  */
case class Bool2Int(v: IntValue)
  extends IntInvariant(if(v.value == 0L) 1L else 0L, 0 to 1)
    with IntNotificationTarget{

  registerStaticAndDynamicDependency(v)
  finishInitialization()

  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Long, NewVal: Long): Unit = {
    this := (if(NewVal == 0L) 1L else 0L)
  }
}

/**
  * boolLE(a,b)
  *
  * outputs true (0L) iff a <= b otherwise false (a)
  *
  *
  * @param a IntValue
  * @param b IntValue
  * @author gustav.bjordal@it.uu.se
  */
case class BoolLEInv(a: IntValue, b:IntValue)
  extends IntInvariant(if(a.value > 0L && b.value == 0L) a.value else 0L, Domain(0L , a.max))
    with IntNotificationTarget{

  registerStaticAndDynamicDependency(a)
  registerStaticAndDynamicDependency(b)
  finishInitialization()

  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Long, NewVal: Long): Unit = {
    this :=
      (if(v == a){
        if(NewVal > 0L && b.value == 0L){
          NewVal+1L
        }else{
          0L
        }
      }else{
        if(NewVal == 0L && a.value >0L){
          a.value+1L
        }else{
          0L
        }
      }) / 2L
  }
}

/**
  * boolLT(a,b)
  *
  * outputs true (0L) iff a < b otherwise false (a+1L)
  *
  *
  * @param a IntValue
  * @param b IntValue
  * @author gustav.bjordal@it.uu.se
  */
case class BoolLTInv(a: IntValue, b:IntValue)
  extends IntInvariant(if(a.value > 0L) a.value else if(b.value == 0L) 1L else 0L, Domain(0 ,a.max))
    with IntNotificationTarget{

  registerStaticAndDynamicDependency(a)
  registerStaticAndDynamicDependency(b)
  finishInitialization()

  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Long, NewVal: Long): Unit = {
    this :=
      (if(v == a){
        if(NewVal == 0L && b.value>0L)
          0L
        else
          (NewVal + b.value + 1L)
      }else{
        if(NewVal >0L && a.value == 0L)
          0L
        else
          (NewVal + a.value + 1L)
      }) / 2L

  }
}

/**
  * xor(a,b)
  *
  * outputs true (0L) iff a != b otherwise false (min(a,b)+1L)
  *
  *
  * @param a IntValue
  * @param b IntValue
  * @author gustav.bjordal@it.uu.se
  */
case class XOR(a: IntValue, b:IntValue)
  extends IntInvariant(if((a.value > 0L && b.value > 0L) || (a.value == 0L && b.value == 0L)) (a.value + b.value + 1L) else 0L,
    Domain(0 ,Math.max(a.max,b.max)))
    with IntNotificationTarget{

  registerStaticAndDynamicDependency(a)
  registerStaticAndDynamicDependency(b)
  finishInitialization()

  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Long, NewVal: Long): Unit = {
    if((NewVal > 0L && OldVal == 0L) || (NewVal == 0L && OldVal > 0L)){
      if(this.value > 0L){
        this := 0L
      }else {
        if (v == a) {
          this := (NewVal + b.value + 1L)/2L
        } else {
          this := (a.value + NewVal + 1L)/2L
        }
      }
    }
  }
}

/**
  * xor(var)
  *
  * outputs true (0L) iff a != b otherwise false (min(a,b)+1L)
  *
  *
  * @param vars  array of IntValue
  * @author gustav.bjordal@it.uu.se
  */
case class XORArray(vars: Array[IntValue])
  extends IntInvariant((vars.foldLeft(1L)((acc,v) => if(v.value > 0L) acc+1L else acc)%2L)*vars.length,
    Domain(0L,vars.foldLeft(0L)((acc,v)=>acc+v.max)))
    with IntNotificationTarget{

  registerStaticAndDynamicDependencyArrayIndex(vars)
  finishInitialization()

  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Long, NewVal: Long): Unit = {
    if((NewVal > 0L && OldVal == 0L) || (NewVal == 0L && OldVal > 0L)){
      if(this.value > 0L){
        this := 0L
      }else {
        this := vars.foldLeft(Long.MaxValue)((acc,v) => if(v.value>0L) v.value + acc else acc)+1L
      }
    }
  }
}

object XOR{
  def apply(vars: Array[IntValue]) = new XORArray(vars)
}

object BoolNE{
  def apply(a: IntValue, b:IntValue) = new XOR(a,b)
}

/**
  * not(a)
  *
  * outputs true (0L) iff a > 0L otherwise false (1L)
  *
  *
  * @param a IntValue
  * @author gustav.bjordal@it.uu.se
  */
case class Not(a: IntValue)
  extends IntInvariant(if(a.value > 0L) 0L else 1L,
    0 to 1)
    with IntNotificationTarget{

  registerStaticAndDynamicDependency(a)
  finishInitialization()

  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Long, NewVal: Long): Unit = {
    this := (if(NewVal > 0L) 0L else 1L)
  }
}
