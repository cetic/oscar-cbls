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

  private var variables: List[Variable]         = List.empty
  private var decisionVariables: List[Variable] = List.empty
  private var lastSolutionNb: Int               = -1

  private def nextSolutionNb: Int = {
    lastSolutionNb += 1
    lastSolutionNb
  }

  /** Checks if `close()` method has been called. */
  def isClosed: Boolean = closed

  /** Extracts the current value of the decision [[Variable]] registered in the Store.
    *
    * The decision variables are not defined by any [[Invariant]]. Those Variables combined with a
    * full propagation of the Store can restore the current value of each Variables.
    *
    * @return
    *   The Solution representing this Store
    */
  def extractSolution: Solution = {
    require(closed, "Model must be closed before saving a new solution")
    Solution(decisionVariables.map(id2Decision => id2Decision.save()), this, nextSolutionNb)
  }

  /** Closes the model.
    *
    * This method must be called after initiating ALL the required [[Invariant]] and [[Variable]].
    * It will setup and fix the propagation structure, allowing the propagation of modification in
    * the model. (without it, no propagation structure, no propagation, no optimization)
    */
  def close(): Unit = {
    setupPropagationStructure()
    getPropagationElements.foreach {
      case v: Variable =>
        variables = v :: variables
        if (v.isADecisionVariable)
          decisionVariables = v :: decisionVariables
      case _ =>
    }
  }

  override def toString: String = "Store(vars:{" + variables.mkString(";") + "})"
}
