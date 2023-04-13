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
  *         by Gaël Thouvenin
  ******************************************************************************/
package oscar.cbls.lib.constraint

import oscar.cbls.core.computation.{IntValue, Value}
import oscar.cbls.core.constraint.Constraint
import oscar.cbls.core.propagation.Checker
import oscar.cbls.lib.invariant.numeric.Sum2

import scala.collection.immutable.SortedMap

/**Implements the Exactly constraint on IntVar.
  * There is a set of bounds, defined in the parameter bound as pair (value,bound).
  * The variables should be such that there is at  exactly ''bound'' of them which have the value ''value''.
  *
  * @param variables the variables that should be bounded
  * @param bounds map(value,bound) the bounds on the variables. We use a map to ensure that there is no two bounds on the same value.
  * @author gael.thouvenin@student.umons.ac.be
  */
case class Exactly(variables:Iterable[IntValue], bounds:SortedMap[Int, IntValue]) extends Constraint {

  registerConstrainedVariables(variables)
  registerConstrainedVariables(bounds.values)

  private val least = AtLeast(variables, bounds)
  private val most = AtMost(variables, bounds)

  private val violationsByVal =
    Map[IntValue, IntValue]() ++
      (for(v <- variables) yield v -> Sum2(least.violation(v), most.violation(v)))

  /** returns the violation associated with variable v in this constraint
    * all variables that are declared as constraint should have an associated violation degree. */
  override def violation(v: Value): IntValue = violationsByVal(v.asInstanceOf[IntValue])

  /** returns the degree of violation of the constraint */
  override val violation: IntValue =  Sum2(least.violation, most.violation)

  override def checkInternals(c: Checker): Unit = { least.checkInternals(c); most.checkInternals(c)}
}


