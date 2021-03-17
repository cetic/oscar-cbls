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

trait Solve[RetVal] {
  def onSolution: () => RetVal
  def onSolution(o: => RetVal) = onSolutionF(() => o)
  def onSolutionF(o: () => RetVal): Unit
}

/**
  * Either proxy the calls to onSolution if to is of class Solve[RetVal], or store them locally
  * @tparam RetVal
  */
abstract class SolveHolder[RetVal] extends Solve[RetVal] {
  protected class MinimalSolve extends Solve[RetVal] {
    private var _onSolution: () => RetVal = null
    override def onSolution: () => RetVal = _onSolution
    override def onSolutionF(o: () => RetVal): Unit = _onSolution = o
  }
  private[this] val rto = solveRedirectTo match {
    case solve: Solve[RetVal] => solve
    case _ => new MinimalSolve
  }

  def onSolution: () => RetVal = rto.onSolution
  def onSolutionF(o: () => RetVal) = rto.onSolutionF(o)
  protected val solveRedirectTo: Any
}