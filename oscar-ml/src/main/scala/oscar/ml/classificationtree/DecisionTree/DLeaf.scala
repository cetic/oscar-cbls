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

package oscar.ml.classificationtree.DecisionTree

import oscar.ml.classificationtree.DataManipulation.Data
import oscar.cp.core.variables.CPIntVar
import oscar.cp.{CPStore, minimum}

/**
 * Code of the paper "Learning Optimal Decision Tree Using CP", H. Verhaeghe, S. Nijssen, C-G Quimpert, G. Pesant, P. Schaus
 * @author helene.verhaeghe27@gmail.com
 */
class DLeaf(
             solver: CPStore,
             db: Data,
             id: Int
           ) extends DTree(0, solver, db, id) {


  val auxiliaryVariables: Array[CPIntVar] = Array(countM, countP, countSum, miniSum)

  /**
    * Apply Functions
    */
  def applyToAllTree(fct: DTree => Unit): Unit = fct(this)

  def applyToAllNode(fct: DTreeDecision => Unit): Unit = {}

  def applyToNode(fct: DNode => Unit): Unit = {}

  def applyToEndNode(fct: DEndNode => Unit): Unit = {}

  def applyToLeaf(fct: DLeaf => Unit): Unit = fct(this)

  /**
    * Constraints
    */
  // miniSum = miniSum_left + miniSum_right if decision node
  // miniSum = min(countP,countM) if leaf
  def _addCstMiniSum: Unit =
    solver.add(minimum(Array(countP, countM), miniSum))

  /**
    * Applying constraints
    */
  // right-most leaf countSum > 0 in any case
  def cstLeftMostCountSum: Unit =
    this._addNodeNonEmpty

  /**
    * Print
    */

  def toStringTree(): String =
    toStringTreeLeaf()



  /**
    * TODO Rm?
    */
  def fillBFSAllTree[T](fct: DTree => T, array: Array[T], spot: Int): Unit =
    array(spot) = fct(this)

  def fillBFSAllNode[T](fct: DTreeDecision => T, array: Array[T], spot: Int): Unit = {}

}

