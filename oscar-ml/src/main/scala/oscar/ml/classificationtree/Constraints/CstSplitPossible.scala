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

package oscar.ml.classificationtree.Constraints

import oscar.cp.core.{CPPropagStrength, Constraint}
import oscar.cp.core.variables.{CPIntVar, CPVar}

import scala.collection.Iterable
/**
 * Code of the paper "Learning Optimal Decision Tree Using CP", H. Verhaeghe, S. Nijssen, C-G Quimpert, G. Pesant, P. Schaus
 * @author helene.verhaeghe27@gmail.com
 */
class CstSplitPossible(val decision: CPIntVar, val countP: CPIntVar, val countM: CPIntVar, val countSum: CPIntVar, threshold: Int) extends Constraint(decision.store, "SplitPossible") {

  override def associatedVars(): Iterable[CPVar] = Array(decision, countP, countM, countSum)

  val threshold2 = 2 * threshold

  /**
    *
    * @param l
    * @return CPOutcome state
    */
  override def setup(l: CPPropagStrength): Unit = {
    countSum.callPropagateWhenBoundsChange(this)
    decision.callPropagateWhenBoundsChange(this)
    propagate()
  }

  /**
    *
    * @return CPOutcome state
    */
  override def propagate(): Unit = {

    if (countP.max == 0 || countM.max == 0) { // If partition, no need to make decisions
      decision.assign(0)
      this.deactivate()
    } else if (countSum.max < threshold2) { // If not enough to partition into, takes no decisions
      decision.assign(0)
      this.deactivate()
    } else if (decision.min > 0) { // If decision 0 already removed, should have enough to partition
      countSum.updateMin(threshold2)
      this.deactivate()
    }

  }


}
