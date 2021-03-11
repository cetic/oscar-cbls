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
  * @author Jean-Noël Monette
  */
package oscar.flatzinc.cbls

import oscar.cbls.core.computation.{CBLSIntVar, ChangingIntValue}
import oscar.cbls.core.objective.{IntVarObjective, Objective => CBLSObjective}
import oscar.cbls.lib.invariant.numeric.{Minus, Prod2, Sum2}
import oscar.flatzinc.Log
import oscar.flatzinc.model.Objective


class FZCBLSObjective(cblsmodel:FZCBLSModel, val bound:Option[CBLSIntVar] = None, log:Log){
  private val opt = cblsmodel.fzModel.search.obj
  val objectiveVar = cblsmodel.fzModel.search.variable.map(cblsmodel.getIntValue(_)).getOrElse(null)
  val violation = cblsmodel.c.violation
 /* val objectiveBound = CBLSIntVarDom(cblsmodel.c.fzModel, opt match {
    case Objective.SATISFY => 0
    case Objective.MINIMIZE => cblsmodel.fzModel.search.variable.get.max+1
    case Objective.MAXIMIZE => cblsmodel.fzModel.search.variable.get.min-1
  }, cblsmodel.fzModel.search.variable.get.domain, "ObjectiveBound")
*/
  val violationWeight = CBLSIntVar(cblsmodel.c.model, 1, 0 to (if(violation.max!=0)Int.MaxValue else 1) , "violation_weight")
  //TODO: The division does not seem right... why max and not min?
  val objectiveWeight = CBLSIntVar(cblsmodel.c.model, 1, 0 to (if(objectiveVar!=null && objectiveVar.max > 0) Int.MaxValue else 1) , "objective_weight")
  private val objective2 = opt match {
        case Objective.SATISFY => violation
        case Objective.MAXIMIZE => Minus(Prod2(violation, violationWeight), Prod2(objectiveVar, objectiveWeight))
        case Objective.MINIMIZE => Sum2(Prod2(violation, violationWeight), Prod2(objectiveVar, objectiveWeight))
      }
  val objective: CBLSObjective = new IntVarObjective(objective2.asInstanceOf[ChangingIntValue])
  def apply() = objective

  def getObjectiveValue(): Int = {
   opt match {
        case Objective.SATISFY => 0
        case Objective.MAXIMIZE => -objectiveVar.value
        case Objective.MINIMIZE => objectiveVar.value
      }
  }
  def increaseViolationWeight(minViolationSinceBest: Int): Unit ={
    if (objectiveWeight.value > 1) {
      correctWeights(objectiveWeight.value / 2,violationWeight.value)
    } else {
      correctWeights(objectiveWeight.value,Math.min(Int.MaxValue/3,violationWeight.value) + Math.min(Int.MaxValue/3,Math.max(10, Math.abs(minViolationSinceBest / 2))))
    }
  }
  def increaseObjectiveWeight(minObjectiveSinceBest: Int): Unit ={
    if (violationWeight.value > 1) {
      correctWeights(objectiveWeight.value,violationWeight.value / 2)
    } else {
      correctWeights(Math.min(Int.MaxValue/3,objectiveWeight.value) + Math.min(Int.MaxValue/3,Math.max(10, Math.abs(minObjectiveSinceBest / 2))),violationWeight.value)
    }
  }
  def correctWeights(newObjW: Int,newVioW: Int): Unit ={
    val minWeight = math.min(newObjW, newVioW)
    objectiveWeight := math.min(newObjW/minWeight,objectiveWeight.max)
    violationWeight := math.min(newVioW/minWeight,violationWeight.max)
    //violationWeight := 1000 + RandomGenerator.nextInt(10)
    log("Changed Violation Weight to "+violationWeight.value+(if(violationWeight.value==violationWeight.max)"(max)"else ""))
    log("    And Objective Weight to "+objectiveWeight.value+(if(objective.value==objectiveWeight.max)"(max)"else ""))

  }
}
