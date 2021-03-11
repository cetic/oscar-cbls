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


import oscar.ml.classificationtree.Constraints.CstDummyEnd
import oscar.ml.classificationtree.DataManipulation.Data
import oscar.cp.core.CPStore
import oscar.cp.core.variables.CPIntVar

/**
 * Code of the paper "Learning Optimal Decision Tree Using CP", H. Verhaeghe, S. Nijssen, C-G Quimpert, G. Pesant, P. Schaus
 * @author helene.verhaeghe27@gmail.com
 */
class DEndNode(
                solver: CPStore,
                leftChild: DLeaf,
                rightChild: DLeaf,
                db: Data,
                id: Int
              )
  extends DTreeDecision(1, solver, db, id, leftChild, rightChild) {

  /**
    * Tree Structure
    */
  lazy val subProblems : Seq[DTreeDecision] = Seq()

  def getFirstChild: Option[DTreeDecision] = None

  val belowDecisionDFS: Array[CPIntVar] = Array(decision)
  val belowProblemDFS: Array[DTreeDecision] = Array(this)

  /**
    * Apply Functions
    */
  def applyToAllNode(fct: DTreeDecision => Unit): Unit = fct(this)

  def applyToNode(fct: DNode => Unit): Unit = {}

  def applyToEndNode(fct: DEndNode => Unit): Unit = fct(this)

  def applyToLeaf(fct: DLeaf => Unit): Unit = {
    leftChild.applyToLeaf(fct)
    rightChild.applyToLeaf(fct)
  }

  /**
    * Constraints
    */
  // Dummy
  def _addCstDummy: Unit =
    solver.add(new CstDummyEnd(decision, leftChild.countSum, rightChild.countSum))

  /**
    * TODO Rm?
    */
  def fillBFSAllNode[T](fct: DTreeDecision => T, array: Array[T], spot: Int): Unit =
    array(spot) = fct(this)
}
