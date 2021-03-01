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

package oscar.cp.constraints

import oscar.algo.search.Objective
import oscar.cp._
import oscar.cp.core._
import oscar.cp.core.variables.CPVar

/**
 * @author Pierre Schaus  pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class CPObjective(val st: CPStore, val objs: CPObjectiveUnit*) extends Constraint(st, "objective constraint") with Objective {

  override def associatedVars(): Iterable[CPVar] = objs.map(_.objVar)

  /** Returns the map of each objective variable to its corresponding objective object */
  def map = objs.map(o => o.objVar -> o).toMap

  def this(s: CPStore, o: Array[CPObjectiveUnit]) = this(s, o: _*)
  
  /** Tightens all the objective unit. The current value of an objective unit becomes 
   *  its best so far value. Returns an error if one objective variable is not bounded. */
  def tighten() = objs.foreach(_.tighten())

  /** Returns true if each objective has reached its best bound */
  def isOptimum(): Boolean = objs.forall(_.isOptimum())

  /** Returns true if the current state of the objective variables is consistent with the model */
  def isOK(): Boolean = !isInconsistent(propagate())

  /** Returns the corresponding objective object */
  def apply(objVar: CPIntVar) = map(objVar)

  /** All objective are set in the NoTighten mode*/
  def diversify(): Unit = {
    for (o <- objs) {
      o.tightenMode = TightenType.NoTighten
    }
  }

  /** The objective unit corresponding to objVar is set to the StrongTighten mode. All others 
   *  objective objects are set in the WeakTighten mode. */
  def intensify(sol: CPSol, objVar: CPIntVar): Unit = {
    for (o <- objs) {
      o.best = sol(o.objVar)
      o.tightenMode = if (o.objVar == objVar) TightenType.StrongTighten else TightenType.WeakTighten
    }
  }

  /** All the objective objects are set in the StrongTighten mode. */
  def intensify(sol: CPSol): Unit = {
    for (o <- objs) {
      o.best = sol(o.objVar)
      o.tightenMode = TightenType.StrongTighten
    }
  }

  /** Ensures that the domain of each objective objects only contains better values (according to 
   *  its tighten mode) than its best so far value. */
  override def propagate(): Unit = {
    objs.foreach(_.ensureBest())
  }
  
  override def setup(l: CPPropagStrength): Unit = propagate()

  override def toString = objs.mkString(" , ")
}
