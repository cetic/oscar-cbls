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
/**
  * @author Gustav Björdal
  *
  **/
package oscar.cbls.lib.constraint

import oscar.cbls.core.computation.{CBLSIntVar, ChangingIntValue, IntNotificationTarget, IntValue, Invariant, Value}
import oscar.cbls.core.constraint.Constraint
import oscar.cbls.core.propagation.Checker
import oscar.cbls.lib.invariant.logic.{IntElement, IntElementNoVar}
import oscar.cbls.lib.invariant.minmax.ArgMin
import oscar.cbls.lib.invariant.numeric.{Dist, Step}
import oscar.cbls.lib.invariant.set.TakeAny

/**
  * Implementation of the table constraint. For each row, the violation of the row is the
  * number of elements that are different from the corresponding variable in variables.
  * The violation of the constraint is the minimum violation of any row.
  * 
  * @param variables the variables.
  * @param table an array of the rows of the table.
  * @author gustav.bjordal@it.uu.se
  */

case class Table(variables: Array[IntValue], table:Array[Array[Long]])
  extends Invariant with Constraint with IntNotificationTarget{

  registerStaticAndDynamicDependencyArrayIndex(variables)
  registerConstrainedVariables(variables)

  finishInitialization()

  val rowViolation:Array[CBLSIntVar] = Array.tabulate(table.length)(i => {
    val tmp = CBLSIntVar(this.model,
                         table(i).zip(variables).foldLeft(0L)((acc, p) => acc + (if (p._1 == p._2.value) 0L else 1L)),
                         //table(i).zip(variables).foldLeft(0L)((acc, p) => acc + Math.abs(p._1 - p._2.value)),
                         0 to table.length)
    tmp.setDefiningInvariant(this)
    tmp
  }
  )

  val minViolatingRows = ArgMin(rowViolation.asInstanceOf[Array[IntValue]])

  val aMinViolatingRow = TakeAny(minViolatingRows,0)

  val variableViolation:Array[IntValue] = Array.tabulate(variables.length)( i =>
    Step(Dist(variables(i), IntElementNoVar(aMinViolatingRow, table.map(_(i)))), 0L,1L,0L)
  )
  /** returns the violation associated with variable v in this constraint
    * all variables that are declared as constraint should have an associated violation degree.
    * notice that you cannot create any new invariant or variable in this method
    * because they can only be created before the model is closed.
    * */
  override def violation(v: Value): IntValue = {
    val variablesIndex = variables.indexOf(v.asInstanceOf[IntValue])
    if(variablesIndex >= 0L){
      variableViolation(variablesIndex)
    }else{
      0L
    }
  }
  val minViolation = IntElement(aMinViolatingRow,rowViolation.asInstanceOf[Array[IntValue]])//MinArray(rowViolation.asInstanceOf[Array[IntValue]])
  /** returns the degree of violation of the constraint
    * notice that you cannot create any new invariant or variable in this method
    * because they can only be created before the model is closed.
    *
    * @return
    */
  override def violation: IntValue = minViolation

  override def checkInternals(c: Checker): Unit = {
    c.check(minViolation.value == rowViolation.map(_.value).min, Some("Min violation is not min"))
    c.check(rowViolation(aMinViolatingRow.valueInt).value == minViolation.value,Some("Min row is wrong"))
    for(i <- variables.indices){
      c.check(variableViolation(i).value == ( if(variables(i).value == table(aMinViolatingRow.valueInt)(i)) 0L else 1L), Some("Violation is not correct"))
    }
  }

  @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, OldVal: Long, NewVal: Long): Unit = {
   assert(OldVal != NewVal)
   for(r <- table.indices) {
     val row = table(r)
     //rowViolation(r) :+= Math.abs(NewVal-row(index))-Math.abs(OldVal-row(index))

     if(row(index) == OldVal){
       rowViolation(r) :+= 1L
     }else if(row(index) == NewVal){
       rowViolation(r) :-= 1L
     }
   }
  }
}
