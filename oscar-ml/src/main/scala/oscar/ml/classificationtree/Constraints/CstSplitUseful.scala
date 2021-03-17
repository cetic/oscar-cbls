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

import oscar.cp.constraints.Gr
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.{CPPropagStrength, Constraint}
import oscar.cp.minimum

import scala.collection.Iterable
/**
 * Code of the paper "Learning Optimal Decision Tree Using CP", H. Verhaeghe, S. Nijssen, C-G Quimpert, G. Pesant, P. Schaus
 * @author helene.verhaeghe27@gmail.com
 */
class CstSplitUseful(val decision: CPIntVar, val miniSum: CPIntVar, val countP: CPIntVar, val countM: CPIntVar, val errorUB:Int) extends Constraint(decision.store, "SplitUseful") {

  //  println( "sup " + Sup)
  override def associatedVars(): Iterable[CPVar] = Array(decision, miniSum, miniSumUB)

  //idempotent = true

  val miniSumUB: CPIntVar = CPIntVar(0 to errorUB, "minUB[" + miniSum.name + "]")(s)

    s.add(minimum(Array(countP, countM), miniSumUB))


  // ub > minisum
  class GrBis(x: CPIntVar, y: CPIntVar) extends Gr(x, y) {

    override def setup(l: CPPropagStrength): Unit = {
      if (!y.isBound && !x.isBound) {
        y.callPropagateWhenBoundsChange(this)
        x.callPropagateWhenBoundsChange(this)
        propagate()
      } else {
        x.updateMin(y.getMin + 1)
        y.updateMax(x.getMax - 1)
      }
    }

    override def propagate(): Unit = {
      x.updateMin(y.getMin + 1)
      y.updateMax(x.getMax - 1)
      if (x.getMin > y.getMax)
        this.deactivate()
    }
  }

  val cst = new GrBis(miniSumUB, miniSum)

  /**
    *
    * @param l
    * @return CPOutcome state
    */
  override def setup(l: CPPropagStrength): Unit = {
      decision.callPropagateWhenBoundsChange(this)
      propagate()

  }

  /**
    *
    * @return CPOutcome state
    */
  override def propagate(): Unit = {

    if (decision.min > 0) {
      s.add(cst)
      this.deactivate()
    } else if (decision.max == 0) {
      this.deactivate()
    }

  }


}