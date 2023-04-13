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
import oscar.cbls.core.computation.{CBLSIntVar, ChangingIntValue, Domain, IntValue, Invariant, InvariantHelper, ShortIntNotificationTarget, Value}
import oscar.cbls.core.constraint.Constraint
import oscar.cbls.core.propagation.Checker

import scala.collection.immutable.SortedMap

/**
 * Implement the AllDiff constraint on IntVars: all variables must have a different value.
 * in ase the same variable occurs several times (in case you submit a list of CBLSIntVar) these two occurrences
 * will be supposed to have different values as expected by the semantics of this constraint
 * @param variables the variable whose values should all be different.
 * @author renaud.delandtsheer@cetic.be
 */
case class AllDiff(variables: Iterable[IntValue])
  extends Invariant with Constraint with ShortIntNotificationTarget{
  //TODO: use bulking here

  val n = variables.size

  registerStaticAndDynamicDependencyAllNoID(variables)
  registerConstrainedVariables(variables)
  finishInitialization()
  val (minValueOfVars,maxValueOfVars) = InvariantHelper.getMinMaxBounds(variables)
  require(minValueOfVars >= Int.MinValue && maxValueOfVars <= Int.MaxValue,
    s"All diff constraints support only integer values. Got min bounds : $minValueOfVars maxBounds : $maxValueOfVars")

  //le degré global de violation est la somme des tailles -1L des ensembles de var ayant meme value
  // et on ne prend que les ensembles de cardinalite > 1L
  private val violationVariable: CBLSIntVar = new CBLSIntVar(model, 0L, Domain(0L ,n), "ViolationsOfAllDiff")
  violationVariable.setDefiningInvariant(this)

  private val N0: Int = maxValueOfVars.toInt

  private val offset: Int = -minValueOfVars.toInt

  private val N = N0 + offset
  private val range = 0 to N

  private val valueMinusOffsetToNbOccurrence: Array[CBLSIntVar] = Array.tabulate[CBLSIntVar](N + 1)((i: Int) => {
    val tmp = new CBLSIntVar(model,0L, Domain(0 ,n), "alldiff_count_of_value_" + (i - offset))
    tmp.setDefiningInvariant(this)
    tmp
  })

  for (v <- variables) {
    valueMinusOffsetToNbOccurrence(v.valueInt + offset) :+= 1L
  }

  for (i <- range) {
    val tmp = valueMinusOffsetToNbOccurrence(i).newValue - 1L
    if (tmp > 0L) violationVariable :+= tmp
  }

  /**the degree of violation of a variable is the number of other variables that have the same value as it. */
  private val Violations: SortedMap[IntValue, IntValue] = {
    def accumulate(acc:SortedMap[IntValue,IntValue], variable:IntValue, violation:IntValue):SortedMap[IntValue,IntValue] =
      acc + (acc.get(variable) match{
        case Some(oldViolation) => (variable,violation + oldViolation)
        case None => (variable,violation)})

    variables.foldLeft(
      SortedMap.empty[IntValue, IntValue])(
        (acc, intvar) => {
          val newvar = valueMinusOffsetToNbOccurrence.element(intvar + offset) - 1L
          accumulate(acc , intvar, newvar)
        })
  }

  @inline
  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Int, NewVal: Int): Unit = {
    valueMinusOffsetToNbOccurrence(OldVal + offset) :-= 1L
    valueMinusOffsetToNbOccurrence(NewVal + offset) :+= 1L

    val deltaOldVal = if (valueMinusOffsetToNbOccurrence(OldVal + offset).newValue == 0L) 0L else -1L
    val deltaNewVal = if (valueMinusOffsetToNbOccurrence(NewVal + offset).newValue == 1L) 0L else 1L
    violationVariable :+= (deltaNewVal + deltaOldVal)
  }

  /**
   * The degree of violation of this constraint is the number of variables that should be changed
   * to ensure that the constraint is not violated.
   * @return an IntVar that can be incorporated in an invariant.
   */
  override def violation = violationVariable

  /**
   * The degree of violation of a variable is the number of other variables that have the same value
   * @return an IntVar that can be incorporated in an invariant.
   */
  override def violation(v: Value): IntValue = {
    val tmp: IntValue = Violations.getOrElse(v.asInstanceOf[IntValue], null)
    assert(tmp != null)
    tmp
  }

  override def checkInternals(c: Checker): Unit = {
    val myValueCount: Array[Long] = (for (i <- 0 to N) yield 0L).toArray
    for (v <- variables) myValueCount(v.valueInt + offset) += 1L
    for (v <- range) {
      c.check(valueMinusOffsetToNbOccurrence(v).newValue == myValueCount(v),
        Some(s"valueMinusOffsetToNbOccurrence($v).newValue (${valueMinusOffsetToNbOccurrence(v).newValue}) == myValueCount($v) (${myValueCount(v)}"))
    }

    for (v <- variables) {
      c.check(violation(v).value == myValueCount(v.valueInt + offset) - 1L,
        Some(s"violation(${v.name}).value (${violation(v).value}) != myValueCount(${v.name}.value + offset) - 1L (${myValueCount(v.valueInt + offset) - 1L})"))
    }

    var MyViol: Long = 0L
    for (v <- range) MyViol += (0L.max(myValueCount(v) - 1L))
    c.check(MyViol == violationVariable.value,
      Some(s"MyViol ($MyViol) == violationVariable.value (${violationVariable.value})"))
  }
}
