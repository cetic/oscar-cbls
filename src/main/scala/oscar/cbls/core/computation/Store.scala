// OscaR is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 2.1 of the License, or
// (at your option) any later version.
//
// OscaR is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License  for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with OscaR.
// If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html

package oscar.cbls.core.computation

import oscar.cbls.core.propagation._

/** Concrete implementation of a PropagationStructure
  *
  * It allows to create/restore Solution based on decision variables.
  *
  * The propagation structure has 4 levels of debugs.
  *   - 0: No debug at all
  *   - 1: The method checkInternals of PropagationElement is called after the element has been
  *     propagated if the propagation is total
  *   - 2: The method checkInternals of PropagationElement is called after the element has been
  *     propagated in a total or partial propagation
  *   - 3: Partial propagation is disabled (every propagation is a total propagation) and the method
  *     checkInternals of PropagationElement is called after the element has been propagated
  *
  * @param debugLevel
  *   the level of debug
  */
class Store(debugLevel: Int = 0) extends PropagationStructure(debugLevel) {

  private var idToVariable: Array[Variable]        = Array.empty
  private var idToDecisionVariable: List[Variable] = List.empty
  private var lastSolutionNb: Int                  = -1

  private def nextSolutionNb: Int = {
    lastSolutionNb += 1
    lastSolutionNb
  }

  /** Saves the current value of the input [[Variable]] registered in the Store.
    *
    * NOTE : The input variables are not defined by any [[Invariant]]
    *
    * @return
    *   The Solution representing this Store
    */
  def save: Solution = {
    require(closed, "Model must be closed before saving a new solution")
    Solution(idToDecisionVariable.map(id2Decision => id2Decision.save()), this, nextSolutionNb)
  }

  /** Triggers the propagation on all the model. */
  def performTotalPropagation(): Unit = {
    super.propagate()
  }

  /** Trigger the propagation up to the targeted element.
    *
    * Only the needed propagation elements will be updated.
    *
    * @param target
    *   The variable whose new value is needed
    */
  def performPartialPropagation(target: Variable): Unit = {
    super.propagate(Some(target))
  }

  /** Closes the model.
    *
    * This method must be called after initiating ALL the required [[Invariant]] and [[Variable]].
    * It will setup and fix the propagation structure, allowing the propagation of modification in
    * the model. (without it, no propagation structure, no propagation, no optimization)
    */
  def close(): Unit = {
    super.setupPropagationStructure()
    idToVariable = Array.fill(getPropagationElements.size)(null)
    getPropagationElements.foreach {
      case v: Variable =>
        idToVariable(v.id) = v
        if (v.isADecisionVariable)
          idToDecisionVariable :+= v
      case _ =>
    }
  }

  override def toString: String = "Store(vars:{" + idToVariable.mkString(";") + "})"
}
