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

import oscar.ml.classificationtree.Constraints.CoverSizeSR
import oscar.ml.classificationtree.DataManipulation.Data
import oscar.cp.core.variables.CPIntVar
import oscar.cp.{sum, _}

import scala.reflect.ClassManifest

/**
 * Code of the paper "Learning Optimal Decision Tree Using CP", H. Verhaeghe, S. Nijssen, C-G Quimpert, G. Pesant, P. Schaus
 * @author helene.verhaeghe27@gmail.com
 */
object DTree {
  /*
   * Build a given decision tree with BSF numbering of the nodes
   */
  def apply(depth: Int, solver: CPStore, db: Data, id: Int = 0): DTreeDecision = {
    if (depth > 1) {
      new DNode(solver, DTree(depth - 1, solver, db, 2 * id + 1), DTree(depth - 1, solver, db, 2 * id + 2), db, id)
    } else {
      new DEndNode(solver, new DLeaf(solver, db, 2 * id + 1), new DLeaf(solver, db, 2 * id + 2), db, id)
    }
  }
}

abstract class DTree(
                      val depth: Int,
                      val solver: CPStore,
                      val db: Data,
                      val id: Int
                    ) {

  /**
    * Tree Structure
    */
  var parent: Option[DTreeDecision] = None // DTreeDecision?
  var isLeft = false


  /**
    * Counting, minimisation & cover
    */
  val countP: CPIntVar = CPIntVar(0 to db.nbTransP, "c+[" + id + "]")(solver)
  val countM: CPIntVar = CPIntVar(0 to db.nbTransM, "c-[" + id + "]")(solver)
  val countSum: CPIntVar = CPIntVar(0 to db.nbTrans, "cS[" + id + "]")(solver)
  val miniSum: CPIntVar = CPIntVar(0 to db.errorUB, "min[" + id + "]")(solver)
  var decisionTake: Array[CPIntVar] = Array()
  var decisionReject: Array[CPIntVar] = Array()
  var coverP: CoverSizeSR = null
  var coverM: CoverSizeSR = null

  val auxiliaryVariables:Array[CPIntVar]
  /**
    * Apply Functions
    */
  def applyToAllTree(fct: DTree => Unit): Unit

  def applyToAllNode(fct: DTreeDecision => Unit): Unit

  def applyToNode(fct: DNode => Unit): Unit

  def applyToEndNode(fct: DEndNode => Unit): Unit

  def applyToLeaf(fct: DLeaf => Unit): Unit

  def leafString = {
    if (countM.min == countP.min)
      2+":"+miniSum.min
    else if (countM.min > countP.min)
      0 + ":"+miniSum.min
    else
      1+ ":"+miniSum.min
  }

  /**
    * Structure
    */
  def _addDecision(decision: CPIntVar, isLeft: Boolean) =
    if (isLeft)
      decisionTake +:= decision
    else
      decisionReject +:= decision

  def propagateDecision(decision: CPIntVar, isLeft: Boolean) =
    applyToAllTree(_._addDecision(decision, isLeft))


  /**
    * Constraints
    */
  // countP + countM == countSum
  def _addCstCountSum: Unit = {
    solver.add(sum(Array(countP, countM), countSum))
  }

  // miniSum = miniSum_left + miniSum_right if decision node
  // miniSum = min(countP,countM) if leaf
  def _addCstMiniSum: Unit

  // countSum > 0
  def _addNodeNonEmpty: Unit =
    solver.remove(countSum, 0)

  // [1;threshold] not in dom(countSum)
  def _addCstNodeThreshold(threshold: Int): Unit =
    for (value <- 1 until threshold)
      solver.remove(countSum, value)

  // gcc(decisions of path) except decision 0
  def _addAllDifferentExcept0Decision: Unit =
    solver.add(gcc(decisionTake ++ decisionReject, 1 until db.nbItem, 0, 1))

  // gcc(decisions of path)
  def _addAllDifferentDecision: Unit =
    solver.add(allDifferent(decisionTake ++ decisionReject))

  // coversize(decisionTake,decisionReject)
  def _addCoverSize: Unit = {
    if (isLeft) {
      solver.add(sum(Array(countM,parent.get.rightChild.countM),parent.get.countM))
      solver.add(sum(Array(countP,parent.get.rightChild.countP),parent.get.countP))
    } else {
      coverP = new CoverSizeSR(decisionTake, decisionReject, countP, db.nbItem, db.nbTransP, db.dataP,db.dataPI)
      coverM = new CoverSizeSR(decisionTake, decisionReject, countM, db.nbItem, db.nbTransM, db.dataM,db.dataMI)
      solver.add(coverP)
      solver.add(coverM)
    }
  }


  /**
    * Applying constraints
    */
  def cstCountSum: Unit =
    applyToAllTree(_._addCstCountSum)

  def cstSumMini: Unit =
    applyToAllTree(_._addCstMiniSum)

  def cstLeftMostCountSum: Unit

  def cstNoEmptyLeaves: Unit =
    applyToAllTree(_._addNodeNonEmpty)

  def cstLeafThreshold(threshold: Int): Unit =
    applyToAllTree(_._addCstNodeThreshold(threshold))

  def cstRedundantSums: Unit = {
    applyToAllNode { n =>
      n._addCstRedundantCountSum
      n._addCstRedundantCountP
      n._addCstRedundantCountN
    }
  }

  def cstAllDiffExcept0Path: Unit =
    applyToLeaf(_._addAllDifferentExcept0Decision)

  def cstAllDiffPath: Unit =
    applyToLeaf(_._addAllDifferentDecision)

  def cstCoverSize: Unit =
    applyToAllTree(_._addCoverSize)

  def cstRemoveDecision(value: Int): Unit =
    applyToAllNode(_._addCstRemoveDecision(value))

  def cstSplitPossible(threshold: Int): Unit =
    applyToAllNode(_._addCstSplitPossible(threshold))

  def cstDummy: Unit =
    applyToAllNode(_._addCstDummy)

  def cstSplitUseful:Unit=
    applyToAllNode(_._addCstSplitUseful)


  /**
    * Ordering
    */

  def computeValueOrdering: Unit = {
    val valueOrder = Array.tabulate(db.nbItem-1) {
      j =>
        val i = j + 1
        val (takeP, dropP) = coverP.countPartition(i)
        val (takeM, dropM) = coverM.countPartition(i)
        (i, Math.min(takeP, takeM) + Math.min(dropP, dropM), Math.abs((takeP + takeM) - (dropP + dropM)))
    }.sortBy(i => (i._2, i._3))
    this.addValueOrdering(valueOrder.map(_._1))
  }

  private def entropyPart(nb: Int, total: Int) = {
    if (nb > 0) {
      val ratio = 1.0 * nb / total
      -ratio * Math.log(ratio)
    } else {
      0.0
    }
  }

  private def entropy(positif: Int, negatif: Int) = {
    val sum = positif + negatif
    entropyPart(positif, sum) + entropyPart(negatif, sum)
  }

  def cstRemoveUseless(threshold:Int):Unit = {
    var nb = 0
    for (i <- 1 until db.nbItem){
      val (takeP, dropP) = coverP.countPartition(i)
      val (takeM, dropM) = coverM.countPartition(i)
      val sumTake = takeP + takeM
      val sumDrop = dropP + dropM
      if (sumTake == 0 || sumDrop == 0) {
        nb += 1
        this.cstRemoveDecision(i)
      } else if (sumTake < threshold || sumDrop < threshold) {
        nb += 1
        this.cstRemoveDecision(i)
      }
    }
  }

  def computeValueOrderingEntropy: Unit = {
    val valueOrder = Array.tabulate(db.nbItem-1) {
      j =>
        val i = j+1
        val (takeP, dropP) = coverP.countPartition(i)
        val (takeM, dropM) = coverM.countPartition(i)
        val sumTake = takeP + takeM
        val sumDrop = dropP + dropM
        val sumtotal = sumTake + sumDrop
        (i, entropy(takeP + dropP, takeM + dropM) - (1.0 * sumTake / sumtotal) * entropy(takeP, takeM) - (1.0 * sumDrop / sumtotal) * entropy(dropP, dropM))
    }
    this.addValueOrdering(valueOrder.sortBy(i => -i._2).map(_._1))
  }

  def computeValueOrderingLexico: Unit = {
    val valueOrder = Array.tabulate(db.nbItem-1)(i => i+1)
    this.addValueOrdering(valueOrder)
  }

  def addValueOrdering(array: Array[Int]): Unit = {
    applyToAllNode { n =>
      n.valueOrdering = array
    }
  }


  /**
    * BFS
    */
  def getBFSAllTree[T](fct: DTree => T)(implicit m: ClassManifest[T]): Array[T] = {
    val k = 2 * Math.pow(2, depth).toInt - 1
    val res = new Array[T](k)
    fillBFSAllTree(fct, res, 0)
    res
  }

  def fillBFSAllTree[T](fct: DTree => T, array: Array[T], spot: Int): Unit

  def getBFSAllNode[T](fct: DTreeDecision => T)(implicit m: ClassManifest[T]): Array[T] = {
    val k = Math.pow(2, depth).toInt - 1
    val res = new Array[T](k)
    fillBFSAllNode(fct, res, 0)
    res
  }

  def fillBFSAllNode[T](fct: DTreeDecision => T, array: Array[T], spot: Int): Unit

  def getDecisionBFS: Array[CPIntVar] =
    getBFSAllNode(_.decision)

  def getCountsBFS: (Array[CPIntVar], Array[CPIntVar]) =
    (getBFSAllTree(_.countP), getBFSAllTree(_.countM))

  def getCountSumBFS: Array[CPIntVar] =
    getBFSAllTree(_.countSum)

  def getMinimums: Array[CPIntVar] =
    getBFSAllTree(_.miniSum)


  /**
    * Print
    */

  def toStringTree() : String

  def toStringTreeLeaf() :String = {
    if (countM.max == countP.max)
      "c(2)"
    else if (countM.max > countP.max)
      "c(0)"
    else
      "c(1)"
  }


}





