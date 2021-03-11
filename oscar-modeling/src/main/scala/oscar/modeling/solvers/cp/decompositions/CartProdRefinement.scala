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

package oscar.modeling.solvers.cp.decompositions
import scala.math.Ordering.Double.TotalOrdering

import oscar.modeling.constraints.Constraint
import oscar.modeling.misc.CartesianProduct
import oscar.modeling.models.cp.MemoCPModel
import oscar.modeling.solvers.cp.Branchings.BranchingInstantiator
import oscar.modeling.solvers.cp.distributed.{SubProblem, SubProblemCartesianProductLog}
import oscar.modeling.vars.IntVar

/**
  * A decomposition strategy based on refinement and cartesian product size
  * @param allVars important variables that will be taken into account to compute the cartesian product log
  * @param search search to be used
  */
class CartProdRefinement(allVars: Iterable[IntVar], search: BranchingInstantiator) extends RefinementStrategy[Double](search) {
  override def generate(memoCPModel: MemoCPModel, constraints: List[Constraint], path: List[Int]): Double = CartesianProduct.computeLog(allVars)
  override def extendSubProblem(subproblem: SubProblem, cartProd: Double): SubProblem = subproblem addData(SubProblemCartesianProductLog, cartProd)
}
