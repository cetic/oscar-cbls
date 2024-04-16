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

/** The state of the model at a given time.
  *
  * This solution can be restored using the restoreSolution method.
  *
  * @param savedValues
  *   The values of the saved decision variable
  * @param model
  *   The Store holding those variables
  * @param solutionNb
  *   The identification of this solution.
  */
case class Solution(savedValues: Iterable[SavedValue], model: Store, solutionNb: Int) {

  /** Restores the model it's previous saved state by restoring each decision variable. */
  def restoreSolution(): Unit = {
    savedValues.foreach(sv => sv.restoreValue())
    model.performTotalPropagation()
  }

  /** Displays the solution as a human-readable string */
  override def toString: String = {
    "Solution(\n\t" + savedValues.map(_.toString()).mkString(",\n\t") + "\n)"
  }
}
