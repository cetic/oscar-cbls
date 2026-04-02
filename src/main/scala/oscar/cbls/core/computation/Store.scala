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

import scala.collection.immutable.HashMap

/** Concrete implementation of a PropagationStructure.
  *
  * It allows to create/restore Solution based on decision variables.
  *
  * The propagation structure has 4 levels of debugs.
  *   - 0: No debug at all.
  *   - 1: The method checkInternals of PropagationElement is called after the element has been
  *     propagated if the propagation is total.
  *   - 2: The method checkInternals of PropagationElement is called after the element has been
  *     propagated in a total or partial propagation.
  *   - 3: Partial propagation is disabled (every propagation is a total propagation) and the method
  *     checkInternals of PropagationElement is called after the element has been propagated.
  *
  * @param debugLevel
  *   the level of debug
  */
class Store(debugLevel: Int = 0) extends PropagationStructure(debugLevel) {

  private var myVariables: Map[Int, Variable]         = HashMap.empty
  private var myDecisionVariables: Map[Int, Variable] = HashMap.empty

  /** the defined variables. only call this after the close
    * @return
    */
  def variables: Map[Int, Variable] = myVariables

  /** The defined decision variables; ie the ones that are not controlled by any invariant. only
    * call this after the close. It returns a map that maps unique ID to variables. Unique ID are
    * unique and deterministic.
    * @return
    *   varID --> Variable
    */
  def decisionVariables: Map[Int, Variable] = myDecisionVariables

  /** Checks if `close()` method has been called. */
  def isClosed: Boolean = closed

  /** Extracts the current value of the decision [[Variable]] and some specified [[Variable]]
    * registered in the Store .
    *
    * The decision variables are not defined by any [[Invariant]]. Those Variables combined with a
    * full propagation of the Store can restore the current value of each [[Variable]]. Only the
    * decisions variables are restored, the other one are computed through propagation.
    *
    * @param additionalVariablesToSave
    *   The list of additional variables that need to be saved.
    * @return
    *   The Solution representing this Store.
    */
  def extractSolution(additionalVariablesToSave: List[Variable] = List.empty): Solution = {
    require(closed, "Model must be closed before saving a new solution.")
    Solution(
      myDecisionVariables.values.map(variable => variable.save()),
      additionalVariablesToSave.map(_.save()),
      this
    )
  }

  /** Creates a new GlobalCheckpoint of the model, containing a checkpoint of each decision
    * variable. To roll back to this checkpoint it's preferable to use a
    * [[oscar.cbls.lib.neighborhoods.combinator.RollBackToGlobalCheckpointMove]]..
    *
    * Those checkpoints should contain all necessary data to roll back to the previous __state__.
    * Here a state is defined by the value and all internal data used to maintain that value. For
    * instance, the checkpoint of a [[oscar.cbls.core.computation.seq.SeqVariable]] holds its
    * checkpointStack, to notify updates...
    *
    * @return
    *   A new GlobalCheckpoint.
    */
  def createCheckpoint(): GlobalCheckpoint = {
    require(closed, "Model must be closed before setting a new checkpoint.")
    GlobalCheckpoint(myDecisionVariables.values.map(_.createGlobalCheckpoint()), this)
  }

  /** Closes the model.
    *
    * This method must be called after initiating ALL the required [[Invariant]] and [[Variable]].
    * It will set up and fix the propagation structure, allowing the propagation of modification in
    * the model. (without it, no propagation structure, no propagation, no optimization)
    */
  def close(): Unit = {
    setupPropagationStructure()
    getPropagationElements.foreach {
      case v: Variable =>
        myVariables += v.id -> v
        if (v.isADecisionVariable)
          myDecisionVariables += v.id -> v
      case _ =>
    }
  }

  override def toString: String = "Store(vars:{" + variables.mkString(";") + "})"

  // The method here below are devoted to distributed computation.

}
