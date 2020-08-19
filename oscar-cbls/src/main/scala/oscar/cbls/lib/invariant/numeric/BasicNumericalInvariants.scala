/**
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
/**
 * *****************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 *            Yoann Guyot
 * ****************************************************************************
 */
package oscar.cbls.lib.invariant.numeric

import oscar.cbls.core.computation.{CBLSIntConst, ChangingIntValue, Domain, DomainHelper, DomainRange, IntInvariant, IntNotificationTarget, IntValue, InvariantHelper, SetValue, ShortIntNotificationTarget, Store}
import oscar.cbls.core.propagation.Checker
import oscar.cbls.lib.invariant.logic.{Int2Int, IntInt2Int}

object Sum {
  def apply(vars: Array[IntValue], cond: SetValue):IntValue = {
    if(vars.length==0) CBLSIntConst(0)
    else SumElements(vars, cond)
  }
  def apply(vars: Iterable[IntValue]):IntValue = {
    if(vars.isEmpty) CBLSIntConst(0)
    else new Sum(vars)
  }
  def apply(vars: Array[Long], cond: SetValue):IntValue = SumConstants(vars, cond)
}

object Prod {
  def apply(vars: Iterable[IntValue]) = new Prod(vars)
  def apply(vars: Array[IntValue], cond: SetValue) = ProdElements(vars, cond)
  def apply(vars: Array[Long], cond: SetValue) = ProdConstants(vars, cond)
}

/**
 * sum(vars)
 * @param vars is an iterable of IntVars
 * @author renaud.delandtsheer@cetic.be
 */
class Sum(vars: Iterable[IntValue])
  extends IntInvariant(
    vars.foldLeft(0L)((a: Long, b: IntValue) => a + b.value),
    Domain(vars.foldLeft(0L)((acc, intvar) => DomainHelper.safeAdd(acc, intvar.min)) , vars.foldLeft(0L)((acc, intvar) => DomainHelper.safeAdd(acc, intvar.max))))
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
 * linear(vars, coeffs)
 * @param vars is an iterable of IntVars
 * @param coeffs is an Indexed Sequence of Long
 * @author renaud.delandtsheer@cetic.be
 * @author jean-noel.monette@it.uu.se
 * */
class Linear(vars: Iterable[IntValue], coeffs: IndexedSeq[Long])
  extends IntInvariant(
    vars.zip(coeffs).foldLeft(0L)((acc, intvar) => acc + intvar._1.value*intvar._2),
    Domain(vars.zip(coeffs).foldLeft(0L)((acc, intvar) => DomainHelper.safeAdd(acc,DomainHelper2.getMinProd(intvar._1.min,intvar._1.max,intvar._2,intvar._2))) ,
      vars.zip(coeffs).foldLeft(0L)((acc, intvar) => DomainHelper.safeAdd(acc,DomainHelper2.getMaxProd(intvar._1.min,intvar._1.max,intvar._2,intvar._2)))))
    with IntNotificationTarget {

  //coeffs needs to be indexed as we need to access it be index from the index of vars (as given in notifyIntChanged)
  //TODO: There is still the risk of adding plus and minus "infinity" and get absurd results. But at least we avoid overflows...

  vars.zipWithIndex.foreach(vi => registerStaticAndDynamicDependency(vi._1,vi._2))
  finishInitialization()

  override def notifyIntChanged(v: ChangingIntValue, idx: Int, OldVal: Long, NewVal: Long): Unit = {
    this :+= (NewVal - OldVal) * coeffs(idx)
  }

  override def checkInternals(c: Checker): Unit = {
    c.check(this.value == vars.zip(coeffs).foldLeft(0L)((acc, intvar) => acc + intvar._1.value*intvar._2),
      Some("output.value == vars.zip(coeff).foldLeft(0L)((acc, intvar) => acc + intvar._1.value*intvar._2)"))
  }
}

/**
 * nvalue(x)
 *
 * @param x is an iterable of IntVars
 * @author juropolach@gmail.com
 * */
class Nvalue(x: Iterable[IntValue]) extends
  IntInvariant(1,DomainRange(1,x.map(_.maxInt).max - x.map(_.minInt).min + 1)) with ShortIntNotificationTarget{

  registerStaticAndDynamicDependencyAllNoID(x)
  finishInitialization()

  private val (minValueOfX,maxValueOfX) = InvariantHelper.getMinMaxBoundsShort(x)

  private val offset: Int = -minValueOfX

  private val N = maxValueOfX + offset
  private val range = 0 to N

  private val ValueCount: Array[Int] = (for (i <- 0 to N) yield 0).toArray

  this := 0

  for (element <- x){
    ValueCount(element.valueInt + offset) += 1
    if (ValueCount(element.valueInt + offset) == 1) {this :+= 1}
  }

  @inline
  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Int, NewVal: Int): Unit = {
    ValueCount(OldVal + offset) -= 1
    ValueCount(NewVal + offset) += 1

    if (ValueCount(OldVal + offset) == 0) {this :-= 1}
    if (ValueCount(NewVal + offset) == 1) {this :+= 1}
  }

  override def checkInternals(c: Checker): Unit = {
    val MyValueCount: Array[Int] = (for (i <- 0 to N) yield 0).toArray
    val Distinct: Int = 0
    for (element <- x){
      MyValueCount(element.valueInt + offset) += 1
      if (MyValueCount(element.valueInt + offset) == 1) {this :+= 1}
    }
    for (v <- range) {
      c.check(ValueCount(v) == MyValueCount(v),
        Some(s"ValueCount($v) (${ValueCount(v)}) == MyValueCount($v) (${MyValueCount(v)})"))
    }
    c.check(Distinct == this.valueInt,
      Some(s"Count of distinct values in $x ($Distinct) == output.value (${this.valueInt})"))
  }
}

/**
 * sum(vars) where vars is vars that have been added to the sum through addTerm
 * @param model the store
 * @author renaud.delandtsheer@cetic.be
 */
class ExtendableSum(model: Store, domain: Domain)
  extends IntInvariant(initialDomain = domain)
    with IntNotificationTarget{

  finishInitialization(model)

  def addTerm(i: IntValue): Unit = {
    registerStaticAndDynamicDependency(i)
    this :+= i.value
  }

  def addTerms(is: Iterable[IntValue]): Unit = {
    for (i <- is) {
      addTerm(i)
    }
  }

  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Long, NewVal: Long): Unit = {
    this :+= NewVal - OldVal
  }

  override def checkInternals(c: Checker): Unit = {
    if(this.getDynamicallyListenedElements != null) {
      c.check(this.value == this.getDynamicallyListenedElements.foldLeft(0L)((acc, intvar) => acc + intvar.asInstanceOf[IntValue].value),
        Some("output.value == vars.foldLeft(0L)((acc,intvar) => acc+intvar.value)"))
    }
  }
}

/**
 * prod(vars)
 * @param vars is a set of IntVars
 * @author renaud.delandtsheer@cetic.be
 */
class Prod(vars: Iterable[IntValue])
  extends IntInvariant
    with IntNotificationTarget{
  assert(vars.size > 0L, "Invariant prod declared with zero vars to multiply")

  for (v <- vars) registerStaticAndDynamicDependency(v)
  finishInitialization()

  var NullVarCount: Long = vars.count(v => v.value == 0L)
  var NonNullProd: Long = vars.foldLeft(1L)((acc, intvar) => if (intvar.value == 0L) { acc } else { acc * intvar.value })

  if (NullVarCount != 0L) {
    this := 0L
  } else {
    this := NonNullProd
  }

  //TODO: find better bound, this is far too much??
  restrictDomain({
    val (myMin,myMax) = vars.foldLeft((1L,1L))((acc, intvar) => (DomainHelper2.getMinProd(acc._1, acc._2, intvar.min, intvar.max),
      DomainHelper2.getMaxProd(acc._1, acc._2, intvar.min, intvar.max)))
    Domain(-myMax , myMax)})

  @inline
  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Long, NewVal: Long): Unit = {
    assert(OldVal != NewVal)
    if (OldVal == 0L && NewVal != 0L) {
      NullVarCount -= 1L
      NonNullProd *= NewVal
    } else if (OldVal != 0L && NewVal == 0L) {
      NullVarCount += 1L
      NonNullProd = NonNullProd / OldVal
    } else {
      NonNullProd = NonNullProd / OldVal
      NonNullProd = NonNullProd * NewVal
    }
    if (NullVarCount == 0L) {
      this := NonNullProd
    } else {
      this := 0L
    }
  }

  override def checkInternals(c: Checker): Unit = {
    var prod = 1L
    for (v <- vars) prod *= v.value
    c.check(this.value == prod,
      Some(s"output.value (${this.value}) == prod ($prod)"))
  }
}

/**
 * a^b
 * where a, b are IntValue
 *
 * @author gustav.bjordal@it.uu.se
 */
case class Pow(a: IntValue, b: IntValue)
  extends IntInt2Int(a, b, if (DomainHelper2.isSafePow(a,b))
    (l,r) => Math.pow(l,r).toInt
  else (l: Long, r: Long) => DomainHelper2.safePow(l,r),
    Domain(DomainHelper2.safePow(a.min, b.min) , DomainHelper2.safePow(a.max, b.max)))

/**
 * left - right
 * where left, right, and output are IntVar
 * @author renaud.delandtsheer@cetic.be
 */
case class Minus(left: IntValue, right: IntValue)
  extends IntInt2Int(left, right, if(DomainHelper2.isSafeSub(left,right))
    (l,r) => l - r
  else (l: Long, r: Long) => DomainHelper2.safeSub(l,r),
    Domain(DomainHelper2.safeSub(left.min, right.max) , DomainHelper2.safeSub(left.max, right.min))) {
  assert(left != right)
}

/** max(0L,left-right+offset)
 *  Used in LA and LEA constraints.
 *  @author jean-noel.monette@it.uu.se
 */
case class MinusOffsetPos(left:IntValue, right:IntValue, offset: Long)
  extends IntInt2Int(left,right, if(DomainHelper2.isSafeSub(left,right))
    (l,r) => 0L.max(l - r + offset)
  else (l: Long, r: Long) => 0L.max(DomainHelper2.safeSub(l,r)+offset),
    Domain(0L , 0L.max(DomainHelper2.safeAdd(DomainHelper2.safeSub(left.max, right.min),offset))))

/**
 * abs(left - right)
 * where left, right, and output are IntVar
 * @author renaud.delandtsheer@cetic.be
 * @author jean-noel.monette@it.uu.se
 * */
case class Dist(left: IntValue, right: IntValue)
  extends IntInt2Int(left, right,
    if(DomainHelper2.isSafeSub(left,right))
      (l,r) => (l - r).abs
    else (l: Long, r: Long) => DomainHelper2.safeSub(l,r).abs,
    Domain({val v = DomainHelper2.safeSub(left.min, right.max); (if (v <= 0L) 0L else v)} ,
      DomainHelper2.safeSub(left.max, right.min).max(DomainHelper2.safeSub(right.max,left.min)))) {
  assert(left != right)
}

/**
 * Invariant to maintain the violation of a reified constraint.
 * Assumes b takes values 0 to 1L
 * @author jean-noel.monette@it.uu.se
 * */
case class ReifViol(b: IntValue, v:IntValue) extends IntInt2Int(b,v, (b,v) => {if(v!=0L) b else 1L-b},Domain(0L , 1L)){
  assert(b.min>=0L && b.max<=1L)
}

/**
 * left + right
 * where left, right, and output are IntVar
 * @author renaud.delandtsheer@cetic.be
 */
case class Sum2(left: IntValue, right: IntValue)
  extends IntInt2Int(left, right, if(DomainHelper2.isSafeAdd(left,right))
    (l,r) => l + r
  else (l: Long, r: Long) => DomainHelper2.safeAdd(l,r),
    Domain(DomainHelper.safeAdd(left.min, right.min) , DomainHelper.safeAdd(left.max, right.max)))
//TODO: Should return simply left if right is the constant zero. (use a companion object)

/**
 * left * right
 * where left, right, and output are IntVar
 * @author renaud.delandtsheer@cetic.be
 */
case class Prod2(left: IntValue, right: IntValue)
  extends IntInt2Int(left, right, if(DomainHelper2.isSafeMult(left,right))
    (l,r) => l * r
  else (l: Long, r: Long) => DomainHelper2.safeMult(l,r),
    Domain(DomainHelper2.getMinProd2(left, right) , DomainHelper2.getMaxProd2(left, right)))

/**
 * left / right
 * where left, right, and output are IntVar
 * do not set right to zero, as usual...
 * @author renaud.delandtsheer@cetic.be
 */
case class Div(left: IntValue, right: IntValue)
  extends IntInt2Int(left, right, (l: Long, r: Long) => l / r,
    Domain(DomainHelper2.getMinDiv(left, right) , DomainHelper2.getMaxDiv(left, right)))
/**
 * left / right
 * where left, right, and output are IntVar
 * do not set right to zero, as usual...
 * @author renaud.delandtsheer@cetic.be
 */
case class Mod(left: IntValue, right: IntValue)
  extends IntInt2Int(left, right,
    (l: Long, r: Long) => l - r * (l / r),
    Domain(0L , Math.min(left.max, right.max)))

/**
 * abs(v) (absolute value)
 * where output and v are IntVar
 * @author renaud.delandtsheer@cetic.be
 */
case class Abs(v: IntValue)
  extends Int2Int(v, (x: Long) => x.abs,
    Domain((if (v.min <= 0L) 0L else v.min) , v.max.max(-v.min)))

/**
 * abs(v) (absolute value)
 * where output and v are IntVar
 * @author renaud.delandtsheer@cetic.be
 */
case class Square(v: IntValue)
  extends Int2Int(v, (x: Long) => x*x,
    Domain(if (v.min <= 0) 0 else ((v.min)*(v.min)), (v.max.max(-v.min))*(v.max.max(-v.min))))

case class Sqrt(v: IntValue)
  extends Int2Int(v, (x: Long) => math.sqrt(x).floor.toLong,
    Domain(math.sqrt(v.min).floor.toLong,math.sqrt(v.max).floor.toLong)){
  require(v.min >= 0)
}

/**
 * This invariant implements a step function. Values higher than pivot are mapped to ifval
 * values lower or equal to pivot are mapped to elseval
 * @author renaud.delandtsheer@cetic.be, suggested by Jean-Noël Monette
 *
 * @param x the IntVar parameter of the invariant
 * @param pivot the pivot value
 * @param thenVal the value returned when x > pivot
 * @param elseVal the value returned when x <= pivot
 */
case class Step(x: IntValue, pivot: Long = 0L, thenVal: Long = 1L, elseVal: Long = 0L)
  extends Int2Int(x, (a: Long) => if (a > pivot) thenVal else elseVal,
    Domain(math.min(thenVal,elseVal) , math.max(thenVal,elseVal)))

/**
 * This invariant implements the identity function within the min-max range.
 * values lower tham min result to min
 * values higher tham max result to max
 * @author renaud.delandtsheer@cetic.be
 * @param x
 * @param minBound
 * @param maxBound
 */
case class Bound(x: IntValue, minBound: Long, maxBound: Long)
  extends Int2Int(x, (a: Long) => if (a < minBound) minBound else if (a > maxBound) maxBound else a,
    Domain(math.max(minBound, x.min) , math.min(maxBound, x.max)))

/**
 * @author Gustav Björdal
 */
object DomainHelper2 {
  def getMinDiv(left: IntValue, right: IntValue): Long = {
    val maxVal = if (right.max == 0L) { -1L } else { right.max }
    val minVal = if (right.min == 0L) { 1L } else { right.min }
    Math.min(left.min / maxVal, Math.min(left.min / minVal,
      Math.min(left.max / maxVal, left.max / minVal)))
  }

  def getMaxDiv(left: IntValue, right: IntValue): Long = {
    val maxVal = if (right.max == 0L) { -1L } else { right.max }
    val minVal = if (right.min == 0L) { 1L } else { right.min }
    Math.max(left.min / maxVal, Math.max(left.min / minVal,
      Math.max(left.max / maxVal, left.max / minVal)))
  }

  // Unfortunately all of these options need to be checked. For example if left has the domain -1L0..0 and right has the domain 3..5 then
  // the min value would be -50L and the max value would be 0. But if the domains were -1L0..0 and -1L0..0 then the min would be 0L and max 10L0.
  // So basically all combinations of the domains min and max could yield the new min and max, as the ugly code below indicates. 
  def getMinProd2(left: IntValue, right: IntValue): Long = {
    Math.min(safeMult(left.min, right.min), Math.min(safeMult(left.min, right.max),
      Math.min(safeMult(left.max, right.min), safeMult(left.max, right.max))))
  }

  def getMinProd(lm:Long,lM:Long,rm:Long,rM:Long): Long = {
    Math.min(safeMult(lm, rm), Math.min(safeMult(lm, rM), Math.min(safeMult(lM,rm), safeMult(lM,rM))))
  }

  def getMaxProd2(left: IntValue, right: IntValue): Long = {
    Math.max(safeMult(left.min, right.min), Math.max(safeMult(left.min, right.max),
      Math.max(safeMult(left.max, right.min), safeMult(left.max, right.max))))
  }

  def getMaxProd(lm:Long,lM:Long,rm:Long,rM:Long): Long = {
    Math.max(safeMult(lm, rm), Math.max(safeMult(lm, rM), Math.max(safeMult(lM,rm), safeMult(lM,rM))))
  }

  def isSafeAdd(x: IntValue, y:IntValue): Boolean = {
    x.max + y.max <= Long.MaxValue && x.min + y.min >= Long.MinValue
  }

  def isSafeSub(x: IntValue, y:IntValue): Boolean = {
    x.max - y.min <= Long.MaxValue && x.min - y.max >= Long.MinValue
  }

  def isSafeMult(x:IntValue,y:IntValue): Boolean = {
    val m1 = x.max * y.max
    val m2 = x.max * y.min
    val m3 = x.min * y.max
    val m4 = x.min * y.min
    math.max(math.max(m1,m2), math.max(m3,m4)) <= Long.MaxValue && math.min(math.min(m1,m2), math.min(m3,m4)) >= Long.MinValue
  }

  def isSafePow(x: IntValue, y:IntValue): Boolean = {
    Math.pow(x.max, y.max) <= Long.MaxValue/10L
  }

  //Safe addition
  def safeAdd(x: Long, y: Long): Long = {
    if (x.toLong + y.toLong > Long.MaxValue) {
      Long.MaxValue
    } else if (x.toLong + y.toLong < Long.MinValue) {
      Long.MinValue
    } else {
      x + y
    }
  }

  //Safe subtraction
  def safeSub(x: Long, y: Long): Long = {
    if (x.toLong - y.toLong > Long.MaxValue) {
      Long.MaxValue
    } else if (x.toLong - y.toLong < Long.MinValue) {
      Long.MinValue
    } else {
      x - y
    }
  }

  //Safe multiplication
  def safeMult(x: Long, y: Long): Long = {
    if (x.toLong * y.toLong > Long.MaxValue) {
      Long.MaxValue
    } else if (x.toLong * y.toLong < Long.MinValue) {
      Long.MinValue
    } else {
      x * y
    }
  }

  //Safe multiplication
  def safePow(x: Long, y: Long): Long = {
    if (Math.pow(x,y) > Long.MaxValue/10L || Math.pow(x,y).isInfinity) {
      Long.MaxValue/10L
    } else {
      Math.pow(x,y).toInt
    }
  }
  //Division of integers is always safe.
}
