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
 * ****************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 * ****************************************************************************
 */
package oscar.cbls.lib.constraint

import oscar.cbls.core.computation.{CBLSIntVar, ChangingIntValue, Domain, IntInvariant, IntNotificationTarget, IntValue, Invariant, InvariantHelper, Value}
import oscar.cbls.core.constraint.Constraint
import oscar.cbls.core.propagation.Checker
import oscar.cbls.lib.invariant.logic.{BoolLEInv, BoolLTInv}
import oscar.cbls.lib.invariant.numeric.{Dist, MinusOffsetPos, ReifViol}

import scala.math.abs

/**
 * implements left <= right
 * @author renaud.delandtsheer@cetic.be
 */
protected class LEA(val left: IntValue, val right: IntValue) extends Constraint {
  val model = InvariantHelper.findModel(left, right)
  registerConstrainedVariables(left, right)

  /**
   * the violation is Max(0L,right-left)
   */
  override val violation =
    MinusOffsetPos(left,right,0L).setName(this.getClass.getSimpleName + ".violation")
    //Max2(0L, left - right).setName(this.getClass().getSimpleName() + ".violation")

  /**
   * The violation of each variable is equal to the global violation of the constraint
   */
  override def violation(v: Value): IntValue = { if (left == v || right == v) violation else 0L }

  override def checkInternals(c: Checker): Unit = {
    val diff = left.value - right.value
    c.check(violation.value == (if (diff <= 0L) 0L else diff),
      Some(s"Violation.value (${violation.value}) == (if (left.value - right.value ($diff) <= 0L) 0L else $diff)"))
  }
}

/**
 * implements left <= right
 * @author  Renaud De Landtsheer rdl@cetic.be
 */
case class LE(l: IntValue, r: IntValue) extends LEA(l, r)

/**
 * implements left >= right
 * it is just a parameter swap of [[LE]]
 * @author renaud.delandtsheer@cetic.be
 */
case class GE(l: IntValue, r: IntValue) extends LEA(r, l)

/**
 * implements left < right
 * @author renaud.delandtsheer@cetic.be
 */
protected class LA(val left: IntValue, val right: IntValue) extends Constraint {
  registerConstrainedVariables(left, right)

  /**
   * the violation is Max(0L,left - right + 1L)
   */
  override val violation =
    MinusOffsetPos(left,right,1L).setName(this.getClass.getSimpleName + ".violation")
    //TODO: If the constraint is always satisfied, given the domains, should set to a constant invariant. 
    //Max2(0L, left - right + 1L)

  /**
   * The violation of each variable is equal to the global violation of the constraint
   */
  override def violation(v: Value): IntValue = { if (left == v || right == v) violation else 0L }

  override def checkInternals(c: Checker): Unit = {
    val diff = left.value - right.value
    c.check(violation.value == (if (diff < 0L) 0L else diff + 1L),
      Some(s"Violation.value (${violation.value}) == (if (left.value - right.value ($diff) < 0L) 0L else ${diff + 1L})"))
  }
}

/**
 * implements left < right
 * it is just a parameter swap of [[L]]
 * @author renaud.delandtsheer@cetic.be
 */
case class L(l: IntValue, r: IntValue) extends LA(l, r)

/**
 * implements left > right
 * @author  Renaud De Landtsheer rdl@cetic.be
 */
case class G(l: IntValue, r: IntValue) extends LA(r, l)

/**
 * implements left != right
 * @author renaud.delandtsheer@cetic.be
 */
case class NE(left: IntValue, right: IntValue) extends Invariant with Constraint with IntNotificationTarget{
  registerConstrainedVariables(left, right)
  registerStaticAndDynamicDependenciesNoID(left, right)
  finishInitialization()

  /** the violation is 1L if the variables are equal, 0L otherwise*/
  override val violation: CBLSIntVar = CBLSIntVar(model, if (left.value == right.value) 1L else 0L, Domain(0L ,1L), "equals")

  violation.setDefiningInvariant(this)

  @inline
  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Long, NewVal: Long): Unit = {
    violation := (if (left.value == right.value) 1L else 0L)
  }

  /** the violation is 1L if the variables are equal, 0L otherwise*/
  override def violation(v: Value): IntValue = { if (left == v || right == v) violation else 0L }

  override def checkInternals(c: Checker): Unit = {
    c.check(violation.value == (if (left.value == right.value) 1L else 0L),
      Some(s"Violation.value (${violation.value}) == (if (left.value (${left.value}) == right.value (${right.value})) 1L else 0L)"))
  }
}

/**
 * constraints left == right
 * this is considered as a primitive constraint and used in the [[Constraint]]
 * class, so that it is part of the core instead of the library
 * @author renaud.delandtsheer@cetic.be
 */
case class EQ(left: IntValue, right: IntValue) extends Constraint {

  registerConstrainedVariables(left, right)

  override val violation = Dist(left, right)

  override def violation(v: Value):IntValue = { if (left == v || right == v) violation else 0L }

  override def checkInternals(c: Checker): Unit = {
    val myViolation = abs(left.value - right.value)
    c.check(violation.value == (if (left.value == right.value) 0L else myViolation),
      Some(s"Violation.value (${violation.value}) == (if (left.value (${left.value}) == right.value (${right.value})) 0L else $myViolation)"))
  }
}

case class BoolEQ(a: IntValue, b: IntValue) extends Constraint {

  registerConstrainedVariables(a, b)

  override val violation = BoolEQInv(a, b)

  override def violation(v: Value):IntValue = { if (a == v || b == v) violation else 0L }
  //override def violation(v: Value) = { if (a == v) Max2(0L,Minus(violation,b)) else if (b == v) Max2(0L,Minus(violation,a)) else 0L }
}

/**
  * BoolEQInv(a,b)
  *
  * ouputs true (0L) iff a == b otherwise violation is equal to (max(a,b)+1L)
  *
  *
  * @param a IntValue
  * @param b IntValue
  * @author gustav.bjordal@it.uu.se
  */
case class BoolEQInv(a: IntValue, b:IntValue)
  extends IntInvariant(if((a.value > 0L && b.value > 0L) || (a.value == 0L && b.value == 0L)) 0L else a.value+b.value+1L,
                       Domain(0L,Math.max(a.max,b.max)))
    with IntNotificationTarget{

  registerStaticAndDynamicDependency(a)
  registerStaticAndDynamicDependency(b)
  finishInitialization()

  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Long, NewVal: Long): Unit = {
    val other = if(a==v) b else a
    if((NewVal>0L && other.value >0L) || (NewVal==0L && other.value ==0L)){
      this := 0L
    }else{
      this := (NewVal + other.value + 1L) / 2L
    }
  }
}

case class BoolLE(a: IntValue, b: IntValue) extends Constraint {

  registerConstrainedVariables(a, b)

  override val violation = BoolLEInv(a, b)

  override def violation(v: Value):IntValue = { if (a == v || b == v) violation else 0L }
}

case class BoolLT(a: IntValue, b: IntValue) extends Constraint {

  registerConstrainedVariables(a, b)

  override val violation = BoolLTInv(a, b)

  override def violation(v: Value):IntValue = { if (a == v || b == v) violation else 0L }
}


/**
 * constraints b <=> c, i.e., b is true iff c is satisfied.
 * @author jean-noel.monette@it.uu.se 
 */
case class Reif(b: IntValue, c: Constraint) extends Constraint { 
  registerConstrainedVariables(b, c.violation)
  override val violation = ReifViol(b,c.violation)
  override def violation(v: Value):IntValue =  { if (b == v || c.violation == v) violation else 0L }
  //TODO: Check internals
}
