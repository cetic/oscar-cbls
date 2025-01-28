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

package oscar.cbls.visual

import oscar.cbls.core.computation.{Solution, Variable}
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.stage.Stage

/** Extends Stage by adding some OscaR related methods. */
abstract class OscaRStage extends Stage {

  /** Returns a list variable that should be extracted from the solution for display purposes. */
  def variablesForSolutionExtraction(): List[Variable]

  /** Initializes the stage if needed. Default implementation does nothing. */
  def init(): Unit

  /** Redraws some part of the Stage if needed.
    *
    * @param solution
    *   The current solution of the problem. It must contain all values needed to update the Stage.
    */
  def redraw(solution: Solution): Unit

}

/** Extends PrimaryStage by adding some OscaR related methods. */
abstract class OscaRPrimaryStage extends PrimaryStage {

  /** Returns a list variable that should be extract from the solution for display purposes. */
  def variablesForSolutionExtraction(): List[Variable]

  /** Initializes the stage if needed. Default implementation does nothing. */
  def init(): Unit

  /** Redraws some part of the Stage if needed.
    *
    * @param solution
    *   The current solution of the problem. It must contain all values needed to update the Stage.
    */
  protected[visual] def redraw(solution: Solution): Unit

}
