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

import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.{CPPropagStrength, Constraint}

import scala.collection.Iterable
/**
 * Code of the paper "Learning Optimal Decision Tree Using CP", H. Verhaeghe, S. Nijssen, C-G Quimpert, G. Pesant, P. Schaus
 * @author helene.verhaeghe27@gmail.com
 */
class CstDummy(val decisionParent: CPIntVar, val decisionChildLeft: CPIntVar, val decisionChildRight: CPIntVar, val sumChildLeft: CPIntVar, val sumChildRight: CPIntVar) extends Constraint(decisionParent.store, "NodeLink2") {

  override def associatedVars(): Iterable[CPVar] = Array(decisionParent, decisionChildLeft, decisionChildRight, sumChildLeft, sumChildRight)

  /**
    *
    * @param l
    * @return CPOutcome state
    */
  override def setup(l: CPPropagStrength): Unit = {
    decisionParent.callPropagateWhenBoundsChange(this)
    propagate()
  }

  /**
    *
    * @return CPOutcome state
    */
  override def propagate(): Unit = {
    if (decisionParent.max == 0) {
      decisionChildLeft.assign(0)
      decisionChildRight.assign(0)
      this.deactivate()
    } else if (decisionParent.min > 0) {
      sumChildRight.removeValue(0)
      sumChildLeft.removeValue(0)
      this.deactivate()
    }
  }


}

class CstDummyEnd(val decisionParent: CPIntVar, val sumChildLeft: CPIntVar, val sumChildRight: CPIntVar) extends Constraint(decisionParent.store, "NodeLink2") {

  override def associatedVars(): Iterable[CPVar] = Array(decisionParent, sumChildLeft, sumChildRight)

  /**
    *
    * @param l
    * @return CPOutcome state
    */
  override def setup(l: CPPropagStrength): Unit = {
    decisionParent.callPropagateWhenBoundsChange(this)
    propagate()
  }

  /**
    *
    * @return CPOutcome state
    */
  override def propagate(): Unit = {
    if (decisionParent.min > 0) {
      sumChildRight.removeValue(0)
      sumChildLeft.removeValue(0)
      this.deactivate()
    }
  }


}
