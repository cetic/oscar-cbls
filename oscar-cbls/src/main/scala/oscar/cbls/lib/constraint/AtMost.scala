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
/******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Renaud De Landtsheer
  ******************************************************************************/
package oscar.cbls.lib.constraint

import oscar.cbls._
import oscar.cbls.core.computation.{IntValue, Value}
import oscar.cbls.core.constraint.Constraint
import oscar.cbls.core.propagation.Checker
import oscar.cbls.lib.invariant.logic.DenseCount
import oscar.cbls.lib.invariant.minmax.Max2
import oscar.cbls.lib.invariant.numeric.Sum

import scala.collection.immutable.SortedMap

/**Implements the AtMost constraint on IntVar.
  * There is a set of bounds, defined in the parameter bound as pair (value,bound).
  * The variables should be such that there is at most ''bound'' of them which have the value ''value''.
  *
  * @param variables the variables that should be bounded, only one occurrence of each variable is tolerated
  * @param bounds map(value,bound) the bounds on the variables.
  *               We use a map to ensure that there is no two bounds on the same value.
  * @author renaud.delandtsheer@cetic.be
  */
case class AtMost(variables:Iterable[IntValue], bounds:SortedMap[Int, IntValue]) extends Constraint {
   assert(variables.size < Long.MaxValue)

  registerConstrainedVariables(variables)
  registerConstrainedVariables(bounds.values)

  private val countInvariant = DenseCount.makeDenseCount(variables.toArray)
  private val offset:Int = countInvariant.offset
  private val valueCount = countInvariant.counts //v => #occurrence of v+offset in variables

  private val noViolation:IntValue = 0
  private val violationByVal:Array[IntValue] = Array.tabulate(valueCount.length)(_ => noViolation)

  for((value,bound) <- bounds){
    violationByVal(value+offset) = Max2(noViolation,valueCount(value+offset) - bound)
  }

  //the violation of each input variable
  private val Violations:SortedMap[IntValue,IntValue] = {
    def accumulate(acc:SortedMap[IntValue,IntValue], variable:IntValue, violation:IntValue):SortedMap[IntValue,IntValue] =
      acc + (acc.get(variable) match{
            case Some(oldViolation) => (variable,(violation + oldViolation).setName(violation.name))
            case None => (variable,violation)})

    val violationForArray = variables.foldLeft(SortedMap.empty[IntValue,IntValue])(
      (acc,intvar) => accumulate(acc,intvar, violationByVal.element(intvar + offset))
    )
    bounds.foldLeft(violationForArray)(
      (acc,boundAndVariable) => {
        val viol = violationByVal(boundAndVariable._1 +offset)
        accumulate(acc,boundAndVariable._2, viol)
      })
  }

  /**The violation of the constraint is the sum on all bound of the number of variable that are in excess.
    * the number of variable in excess is the max between zero and
    * (the number of variable that have the value of the bound minus the bound).
    */
  val violation = Sum(bounds.keys.map(bound => violationByVal(bound+offset)))

  /**The violation of a variable is zero if its value is not the one of a bound.
    * If the variable has the value of a bound, its violation is the number of variable in excess for that bound.
    */
  override def violation(v: Value):IntValue = {
    Violations(v.asInstanceOf[IntValue])
  }

  override def checkInternals(c: Checker): Unit = {
    var checkBounds:SortedMap[Long, Long] = SortedMap.empty
    for(i <- bounds.keys) checkBounds += ((i,0L))
    for (v <- variables) if (checkBounds.isDefinedAt(v.value)) checkBounds += ((v.value,checkBounds(v.value) +1L))

    for (v <- variables){
      /*The violation of a variable is zero if its value is not the one of a bound.
        * If the variable has the value of a bound, its violation is the number of variable in excess for that bound.
        */
      val violationOfV = violation(v)
      val expectedViolation =
        if (checkBounds.isDefinedAt(v.value)) 0.max((checkBounds(v.value) - bounds(v.valueInt).value).toInt)
        else 0L
      c.check(violationOfV.value == expectedViolation, Some("" + violationOfV + " == expectedViolation (" + expectedViolation + ")"))
    }

    /*The violation of the constraint is the sum on all bound of the number of variable that are in excess.
      * the number of variable in excess is the max between zero and
      * (the number of variable that have the value of the bound minus the bound).*/
    var summedViolation = 0L
    for(i <- bounds.keys){
      if (checkBounds(i) > bounds(i).value) summedViolation += (checkBounds(i) - bounds(i).value)
    }
    c.check(summedViolation == violation.value,
      Some(s"summedViolation ($summedViolation) == violation.value (${violation.value})"))
  }
}
