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

import oscar.cbls.algo.sequence.IntSequence
import oscar.cbls.core.computation.integer.{IntSavedValue, IntVariable}
import oscar.cbls.core.computation.seq.{SeqSavedValue, SeqVariable}
import oscar.cbls.core.computation.set.{SetSavedValue, SetVariable}

import scala.collection.immutable.HashMap

/** The state of the model at a given time.
  *
  * It contains the value of the decisions variable as well as some additional specified variable.
  * This solution can be restored using the restoreSolution method.
  *
  * @param savedValues
  *   The values of the saved decision variable.
  * @param additionalSavedVariables
  *   The list of additional variables that need to be saved.
  * @param model
  *   The Store holding those variables.
  * @param solutionNb
  *   The identification of this solution.
  */
case class Solution(
  savedValues: Iterable[SavedValue],
  additionalSavedVariables: Iterable[SavedValue],
  model: Store,
  solutionNb: Int
) {

  private lazy val variableToValue: HashMap[Variable, SavedValue] =
    HashMap.from((savedValues ++ additionalSavedVariables).map(sv => sv.variable -> sv))

  /** Returns the saved value of the given [[IntVariable]].
    *
    * @param variable
    *   The requested variable.
    * @return
    *   The saved value of the requested variable.
    */
  def valueOfVariable(variable: IntVariable): Option[Long] = {
    variableToValue.get(variable) match {
      case None                            => None
      case Some(savedValue: IntSavedValue) => Some(savedValue.savedValue)
      case _ =>
        require(
          requirement = false,
          "Variable of type IntVariable can only be saved as a IntSavedValue."
        )
        None
    }
  }

  /** Returns the saved value of the given [[SetVariable]].
    *
    * @param variable
    *   The requested variable.
    * @return
    *   The saved value of the requested variable.
    */
  def valueOfVariable(variable: SetVariable): Option[Set[Int]] = {
    variableToValue.get(variable) match {
      case None                            => None
      case Some(savedValue: SetSavedValue) => Some(savedValue.savedValue)
      case _ =>
        require(
          requirement = false,
          "Variable of type SetVariable can only be saved as a SetSavedValue."
        )
        None
    }
  }

  /** Returns the saved value of the given [[SeqVariable]].
    *
    * @param variable
    *   The requested variable.
    * @return
    *   The saved value of the requested variable.
    */
  def valueOfVariable(variable: SeqVariable): Option[IntSequence] = {
    variableToValue.get(variable) match {
      case None                            => None
      case Some(savedValue: SeqSavedValue) => Some(savedValue.savedValue)
      case _ =>
        require(
          requirement = false,
          "Variable of type SeqVariable can only be saved as a SeqSavedValue."
        )
        None
    }
  }

  /** Restores the model in its previous saved state by restoring each decision variable. */
  def restoreSolution(): Unit = {
    savedValues.foreach(sv => sv.restoreValue())
    model.propagate()
  }

  /** Displays the solution as a human-readable string */
  override def toString: String = {
    "Solution(\n\t" + savedValues.map(_.toString()).mkString(",\n\t") + "\n)"
  }
}
