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


import oscar.ml.classificationtree.Constraints.{CstSplitPossible, CstSplitUseful}
import oscar.ml.classificationtree.DataManipulation.Data
import oscar.algo.Inconsistency
import oscar.cp._
import oscar.cp.core.variables.CPIntVar

import scala.collection.mutable

/**
 * Code of the paper "Learning Optimal Decision Tree Using CP", H. Verhaeghe, S. Nijssen, C-G Quimpert, G. Pesant, P. Schaus
 * @author helene.verhaeghe27@gmail.com
 */
abstract class DTreeDecision(
                              depth: Int,
                              solver: CPStore,
                              db: Data,
                              id: Int,
                              val leftChild: DTree,
                              val rightChild: DTree
                            )
  extends DTree(depth, solver, db, id) /*with DecomposableProblem[(String, String), Array[Int]]*/ {

  /**
    * Tree Structure
    */
  leftChild.parent = Some(this)
  rightChild.parent = Some(this)
  leftChild.isLeft = true

  lazy val nextProblem: Option[DTreeDecision] = if (isLeft && parent.isDefined) Some(parent.get.rightChild.asInstanceOf[DTreeDecision]) else None
  //  val seqSubProblem: Seq[DecomposableProblem[(String, String), Array[Int]]]

  def getParent: Option[DTreeDecision] = parent

  //  def getSubProblems: Seq[DecomposableProblem[(String, String), Array[Int]]] = seqSubProblem // TODO rm
  def getNext: Option[DTreeDecision] = nextProblem

  // Decision feature
  val decision: CPIntVar = CPIntVar(0 until db.nbItem, "d[" + id + "]")(solver)
  leftChild.propagateDecision(decision, true)
  rightChild.propagateDecision(decision, false)

  val belowDecisionDFS: Array[CPIntVar]
  lazy val nbBelowDecision: Int = belowDecisionDFS.length
  val belowProblemDFS: Array[DTreeDecision]

  // Optimisation features
  lazy val target: CPIntVar = miniSum
  val isMinimisation: Boolean = true


  val auxiliaryVariables: Array[CPIntVar] = Array(countM, countP, countSum, miniSum) ++ leftChild.auxiliaryVariables ++ rightChild.auxiliaryVariables


  private def assignMultiple(x: Array[CPIntVar], v: Array[Int]): Unit = {
    if (solver.isFailed)
      throw Inconsistency

    try {
      var i = 0
      while (i < x.length) {
        x(i).assign(v(i))
        i += 1
      }
      solver.propagate()
    }
    catch {
      case i: Inconsistency => {
        solver.fail()
        throw i
      }
    }
  }

  def getNewStorage: mutable.Map[(String,String), (Int, Array[Int])] =
    new scala.collection.mutable.HashMap[(String,String), (Int, Array[Int])]
  def getNewStorageANDOR: mutable.Map[(String,String), (String,Int)] =
    new scala.collection.mutable.HashMap[(String,String), (String,Int)]

  /**
    * Apply Functions
    */
  def applyToAllTree(fct: DTree => Unit): Unit = {
    fct(this)
    leftChild.applyToAllTree(fct)
    rightChild.applyToAllTree(fct)
  }

  /**
    * Constraints
    */
  // miniSum = miniSum_left + miniSum_right if decision node
  // miniSum = min(countP,countM) if leaf
  def _addCstMiniSum: Unit =
    solver.add(sum(Array(leftChild.miniSum, rightChild.miniSum), miniSum))

  // countSum = countSum_left + countSum_right
  def _addCstRedundantCountSum: Unit =
    solver.add(sum(Array(leftChild.countSum, rightChild.countSum), countSum))

  // countSum = countSum_left + countSum_right
  def _addCstRedundantCountP: Unit =
    solver.add(sum(Array(leftChild.countP, rightChild.countP), countP))

  // countSum = countSum_left + countSum_right
  def _addCstRedundantCountN: Unit =
    solver.add(sum(Array(leftChild.countM, rightChild.countM), countM))

  // dom(decision) \ value
  def _addCstRemoveDecision(value: Int): Unit =
    solver.remove(decision, value)

  // SplitPossible
  def _addCstSplitPossible(threshold: Int): Unit =
    solver.add(new CstSplitPossible(decision, countP, countM, countSum, threshold))

  // Dummy
  def _addCstDummy: Unit

  // SplitUseful
  def _addCstSplitUseful: Unit =
    solver.add(new CstSplitUseful(decision, miniSum, countP, countM, db.errorUB))

  /**
    * Applying constraints
    */
  // right-most leaf countSum > 0 in any case
  def cstLeftMostCountSum: Unit =
    rightChild.cstLeftMostCountSum


  def assignTrivialSolution: Unit =
    applyToAllNode(n => {
      //      val k = "ERROR " + n.decision + " " + n.decision.min
      if (!n.decision.isBound) {
        //        try {
        solver.assign(n.decision, n.decision.min)
        //        } catch {
        //          case e: Inconsistency =>
        //            println("ERROR " + n.decision + " " + n.decision.min + " : "+ k)
        //            throw Inconsistency
        //        }
      }
    }
    )

  def assignTrivialSolutionToNext: Unit =
    if (nextProblem.isDefined)
      nextProblem.get.assignTrivialSolution


  def isBound: Boolean = decision.isBound


  /**
    * Ordering
    */
  var valueOrdering: Array[Int] = Array.tabulate(db.nbItem)(i => i)

  def getNextAssignation(assignBefore: () => Unit, assignAfter: () => Unit, removeBefore: () => Unit, removeAfter: () => Unit): Seq[Alternative] = {
    if (decision.hasValue(0)) {
      branch {
        removeBefore()
        solver.remove(decision, 0)
        removeAfter()
      } {
        assignBefore()
        solver.assign(decision, 0)
        assignAfter()
      }
    } else {
      branchAll(valueOrdering.filter(decision.hasValue(_))) {
        value =>
          assignBefore()
          solver.assign(decision, value)
          assignAfter()

      }
    }
  }

  /**
    * Storage & caching
    */
  def getHash: (String, String) =
    (decisionTake.map(_.min).sorted.mkString("-"), decisionReject.map(_.min).sorted.mkString("-"))

  def getHashUnique: (String, String) =
    (decisionTake.map(_.min).mkString("-"), decisionReject.map(_.min).mkString("-"))

  def getStored: (Array[Int]) =
    belowDecisionDFS.map(_.min)

  def getsizestored(stored: Array[Int]): Int =
    stored.length

  def storedToString(stored: Array[Int]): String = {
    stored.mkString("[", ",", "]")
    //    solver.pushState()
    //    this.assignStored(stored)
    //    val tree = this.toStringTree()
    //    solver.pop()
    //
    //    tree
  }

  def storedToTreeString(stored: Array[Int]): String = {
    solver.pushState()
    this.assignStored(stored)
    val tree = this.toStringTree()
    solver.pop()

    tree
  }


  def assignStored(stored: Array[Int]): Unit = {
    assignMultiple(belowDecisionDFS, stored)
  }


  def printstored(stored: Array[Int]) =
    stored.mkString("-")


//  private def addToStorage(storage: mutable.Map[(String, String), (Int, Array[Int])]): Unit = {
//    val hash = getHash
//    val newObj = target.value
//    if (!storage.contains(hash) || storage(hash)._1 > newObj) {
//      customOptimisation.updateBound(newObj)
//      storage += ((hash, (newObj, getStored)))
//    }
//
//  }
//
//  private def addToStorageNoCache(storage: mutable.Map[(String, String), (Int, Array[Int])]): Unit = {
//    val hash = getHashUnique
//    val newObj = target.max
//    if (!storage.contains(hash) || storage(hash)._1 > newObj) {
//      customOptimisation.updateBound(newObj)
//      storage += ((hash, (newObj, getStored)))
//    }
//
//  }
//
//  private def addToStorageNoMin(storage: mutable.Map[(String, String), (Int, Array[Int])]): Unit = {
//    val hash = getHash
//    val newObj = target.max
//    if (!storage.contains(hash) || storage(hash)._1 > newObj) {
//      storage += ((hash, (newObj, getStored)))
//    }
//  }
//
//
//  def cacheSolution(storage: mutable.Map[(String, String), (Int, Array[Int])]): Unit = {
//    addToStorage(storage)
//    if (getNext.isEmpty && parent.isDefined)
//      parent.get.cacheSolution(storage)
//  }
//
//  def cacheSolutionNoMin(storage: mutable.Map[(String, String), (Int, Array[Int])]): Unit = {
//    addToStorageNoMin(storage)
//    if (getNext.isEmpty && parent.isDefined)
//      parent.get.cacheSolutionNoMin(storage)
//  }
//
//
//  def storeSolution(storage: mutable.Map[(String, String), (Int, Array[Int])]): Unit = {
//    if (getNext.isDefined || parent.isEmpty)
//      addToStorage(storage)
//    if (parent.isDefined)
//      parent.get.storeSolution(storage)
//  }
//
//  def storeSolutionNoCache(storage: mutable.Map[(String, String), (Int, Array[Int])]): Unit = {
//    if (getNext.isDefined || parent.isEmpty)
//      addToStorageNoCache(storage)
//    if (parent.isDefined)
//      parent.get.storeSolutionNoCache(storage)
//  }

  /**
    * Print
    */

  def toStringTree(): String = {
    if (decision.max == 0) {
      toStringTreeLeaf()
    } else {
      (decision.max - 1) + "(" + leftChild.toStringTree() + "," + rightChild.toStringTree() + ")"
    }
  }


  /**
    * TODO Rm?
    */
  def fillBFSAllTree[T](fct: DTree => T, array: Array[T], spot: Int): Unit = {
    array(spot) = fct(this)
    leftChild.fillBFSAllTree(fct, array, spot * 2 + 1)
    rightChild.fillBFSAllTree(fct, array, spot * 2 + 2)
  }

  def printINFONODE = {
    println(id + " " + decision + " " + countP + "+" + countM + "=" + countSum + " " + miniSum )
  }
}