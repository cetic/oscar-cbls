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

package oscar.cbls.visual.cartesian

import oscar.cbls.core.computation.{Solution, Variable}
import scalafx.scene.shape.Shape

/** This abstract class contains necessary method to handle individual shapes that'll end-up on a
  * cartesian plan.
  *
  * It works as layers that will be added on a white pane. Each layer is managed individually.
  *
  * @param additionalVariableForExtraction
  *   The additional [[oscar.cbls.core.computation.Variable]] needed to update the data displayed on
  *   the layer.
  */
abstract class CartesianLayer(additionalVariableForExtraction: List[Variable] = List.empty)
    extends Iterable[Shape] {
  // The list of shapes that will be displayed on the Stage.
  private[cartesian] var listOfShapes: List[Shape]

  /** Returns a list variable that should be extract from the solution for display purposes. */
  def variablesForSolutionExtraction(): List[Variable] = additionalVariableForExtraction

  /** Redraws some graphical data if needed.
    *
    * @param solution
    *   The current solution of the problem. It should contain all needed value.
    */
  def redraw(solution: Solution): Unit

  /** Initializes the CartesianLayer if needed. */
  def initLayer(): Unit

  override def iterator: Iterator[Shape] = {
    listOfShapes.iterator
  }
}
