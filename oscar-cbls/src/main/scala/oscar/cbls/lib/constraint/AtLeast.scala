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
 *            Yoann Guyot
 ******************************************************************************/

package oscar.cbls.lib.constraint

import oscar.cbls._
import oscar.cbls.core.computation.{IntValue, InvariantHelper, Value}
import oscar.cbls.core.constraint.Constraint
import oscar.cbls.core.propagation.Checker
import oscar.cbls.lib.invariant.logic.{DenseCount, IntITE}
import oscar.cbls.lib.invariant.minmax.Max2
import oscar.cbls.lib.invariant.numeric.Sum

import scala.collection.immutable.SortedMap

/**
 * Implement the AtLeast constraint on IntVars.
 * There is a set of minbounds, defined in the parameter bound as pair (value,minbound).
 * The variables should be such that there is at least ''minbound'' of them which have the value ''value''.
 *
 * @param variables the variable whose values are constrained
 * @param bounds map(value,minbound) specifying the minimal number of occurrence of ''value'' among the variables.
 *                We use a map to ensure that there is no two bounds on the same value.
 * @author renaud.delandtsheer@cetic.be
 */
case class AtLeast(variables: Iterable[IntValue], bounds: SortedMap[Int, IntValue]) extends Constraint {

  registerConstrainedVariables(variables)
  registerConstrainedVariables(bounds.values)

  private val countInvariant = DenseCount.makeDenseCount(variables.toArray)
  private val offset:Int = countInvariant.offset
  private val valueCount = countInvariant.counts //v => #occurrence of v+offset in variables

  private val noViolation:IntValue = 0L

  private val Violation =
    Sum(bounds.toList.map((value_bound) => Max2(noViolation,value_bound._2 - valueCount(value_bound._1+offset))))

  private val violationByVal:Array[IntValue]=Array.tabulate(valueCount.length)(value => {
    if(bounds.contains(value + offset))
      IntITE(valueCount(value + offset) - bounds(value + offset), Violation, noViolation)
    else Violation
    })

  //the violation of each input variable
  private val Violations:SortedMap[IntValue,IntValue] = {
    def accumulate(acc:SortedMap[IntValue,IntValue], variable:IntValue, violation:IntValue):SortedMap[IntValue,IntValue] =
      acc + (acc.get(variable) match{
        case Some(oldViolation) => (variable,(violation + oldViolation).setName(violation.name))
        case None => (variable,violation)})

    val violationForArray = variables.foldLeft(SortedMap.empty[IntValue,IntValue])(
      (acc, intvar) => accumulate(acc, intvar, violationByVal.element(intvar + offset).setName("Violation_AtLeast_" + intvar.name)))

    bounds.foldLeft(violationForArray)(
      (acc,boundAndVariable) => {
        val viol = Max2(noViolation,boundAndVariable._2 - valueCount(boundAndVariable._1+offset)).setName("Violation_AtLeast_" + boundAndVariable._2.name)
        accumulate(acc, boundAndVariable._2, viol)
      })
  }
  /**
   * the violation is the sum for all bounds of the number of missing variables to reach the bound
   */
  override def violation = Violation

  /**
   * The violation of a variable is zero if the value of the variable is the one of a bound that is not reached,
   * otherwise, it is equal to the global violation degree.
   */
  override def violation(v: Value) = Violations(v.asInstanceOf[IntValue])

  override def checkInternals(c: Checker): Unit = {
    val (minMin,maxMax) = InvariantHelper.getMinMaxBoundsShort(variables)
    var MyValueCount: SortedMap[Long,Long] = SortedMap.empty
    for(v <- variables){
      val oldCount = MyValueCount.getOrElse(v.value,0L)
      MyValueCount += ((v.value,oldCount + 1L))
    }

    for (v <- minMin to maxMax) {
      if (MyValueCount.isDefinedAt(v)){
        c.check(valueCount(v+offset).newValue == MyValueCount(v),
          Some(s"ValueCount($v+offset).newValue (${valueCount(v).newValue}) == MyValueCount($v) (${MyValueCount(v)})"))
      }else{
        c.check(valueCount(v+offset).newValue == 0L,
          Some(s"ValueCount($v+offset).newValue (${valueCount(v).newValue}) == 0L"))
      }
    }

    var MyViol: Long = 0L
    for (v <- bounds.keys) {
      MyViol += 0L.max(bounds(v).value - MyValueCount.getOrElse(v + offset,0L))
    }
    c.check(Violation.value == MyViol,
      Some(s"Violation.value (${Violation.value}) == MyViol ($MyViol)"))

    for (v <- variables) {
      if (bounds.contains(v.valueInt) && (MyValueCount(v.value + offset) <= bounds(v.valueInt).value)) {
        c.check(violation(v).value == 0L,
            Some(s"violation(${v.name}).value (${violation(v).value}) == 0L"))
      } else {
        c.check(violation(v).value == Violation.value,
            Some(s"violation(${v.name}).value (${violation(v).value}) == Violation.value (${Violation.value})"))
      }
    }
  }
}
