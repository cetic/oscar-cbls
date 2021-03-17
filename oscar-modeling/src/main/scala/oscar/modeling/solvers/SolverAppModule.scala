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

package oscar.modeling.solvers

import org.rogach.scallop.Subcommand
import oscar.modeling.models.ModelDeclaration

/**
  * A Module for SolverApp. Represents a solver or a solving method.
  */
trait SolverAppModule {

  def name: String = subcommand.printedName

  /**
    * A Scallop subcommand containing all the needed information to start the solver, if needed
    */
  val subcommand: Subcommand

  /**
    * Solve the model
    */
  def solve[RetVal](): List[RetVal]

  /**
    * Called after the module has been selected, but before the (user-defined) constructor of the app
    */
  def onSelect(): Unit = {}
}
