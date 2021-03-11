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

package oscar.algo.paretofront

import java.util

import oscar.util.RandomGenerator

import scala.Iterable

/**
  * @author Renaud Hartert ren.hartert@gmail.com
  * @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
  */
class QT[U: Numeric](private val nDim: Int, private val cmp: (Int, Int) => Boolean) extends ParetoFront[U, QTNode[U]] with Iterable[QTNode[U]] {
  
  val nSuccessors = 1 << nDim
  val Successors = 0 until nSuccessors
  val NonDomSuccessors = 1 until nSuccessors - 1
  val bestSuccessor = Successors.max
  val worstSuccessor = Successors.min
  
  /** Successor functions */
  
  def opposite(kSucc: Int): Int = kSucc ^ bestSuccessor

  def asStrong(kSucc: Int): IndexedSeq[Int] = {
    (kSucc to NonDomSuccessors.max).filter(s => (s - (s ^ kSucc)) == kSucc)
  }

  def stronger(kSucc: Int): IndexedSeq[Int] = {
    (kSucc + 1 to NonDomSuccessors.max).filter(s => (s - (s ^ kSucc)) == kSucc)
  }

  def asWeak(kSucc: Int): IndexedSeq[Int] = {
    val dim = opposite(kSucc)
    (NonDomSuccessors.min to kSucc).filter(s => ((s ^ dim) - s) == dim)
  }

  def weaker(kSucc: Int): IndexedSeq[Int] = {
    val dim = opposite(kSucc)
    (NonDomSuccessors.min until kSucc).filter(s => ((s ^ dim) - s) == dim)
  }

  /** QuadTree functions */
  var root: Option[QTNode[U]] = None

  def process(cand: QTNode[U], subR: QTNode[U]): Unit = {

    val kSucc = subR.successorship(cand.objectives)

    // Discard the candidate node
    if (kSucc == worstSuccessor) {}

    // Replace the root by the dominated node
    else if (kSucc == bestSuccessor) {
      priorityQueue.enqueue(cand)
      replace(cand, subR)
      for (son <- NonDomSuccessors if cand.successors(son).isDefined) {
        clean(cand, cand.successors(son).get)
      }
    } // Dominance + Clean
    else {
      // Check dominance
      for (son <- stronger(kSucc) if subR.successors(son).isDefined) {
        val dom = checkDominance(cand, subR.successors(son).get)
        if (dom) return
      }
      // Remove dominated node
      val weak = weaker(kSucc)
      for (son <- weak if subR.successors(son).isDefined) {
        clean(cand, subR.successors(son).get)
      }

      if (subR.successors(kSucc).isDefined) {
        process(cand, subR.successors(kSucc).get)
      } else {
        insert0NoCheck(cand, subR)
      }
    }
  }

  /** Insert the candidate node without checking for dominance */
  def insertNoCheck(cand: QTNode[U]): Unit = {
    if (root.isDefined) insert0NoCheck(cand, root.get)
    else root = Some(cand)
  }

  private def insert0NoCheck(cand: QTNode[U], root: QTNode[U]): Unit = {
    val kSucc = root.successorship(cand.objectives)
    // Recursive traversal
    if (root.successors(kSucc).isDefined) {
      insert0NoCheck(cand, root.successors(kSucc).get)
    } // Insertion
    else {
      cand.father = Some(root)
      cand.kSucc = kSucc
      root.successors(kSucc) = Some(cand)
      priorityQueue.enqueue(cand)
    }
  }

  /** Check if the candidate node is dominated by some node in the tree rooted at root */

  def checkDominance(cand: QTNode[U], root: QTNode[U]): Boolean = {
    val kSucc = root.successorship(cand.objectives)
    // Is the candidate node dominated by the root ?
    if (kSucc == worstSuccessor) true
    // Is the candidate node dominated by a subtree ?
    else {
      val sons = asStrong(kSucc)
      for (son <- sons if root.successors(son).isDefined) {
        if (checkDominance(cand, root.successors(son).get)) return true
      }
      false // Not dominated by any node in the tree rooted at root
    }
  }

  /** Replace the root */

  def replace(cand: QTNode[U], root: QTNode[U]): Unit = {
    // Transplant
    if (root == this.root.get) {
      this.root = Some(cand)
    } else {
      val father = root.father.get
      father.successors(root.kSucc) = Some(cand)
      cand.father = Some(father)
      cand.kSucc = root.kSucc
    }
    // Reinsert sons
    for (son <- NonDomSuccessors if root.successors(son).isDefined) {
      reinsertIn(root.successors(son).get, this.root.get)
    }
  }

  /** Reinsert without dominance check all the subtree rooted at root in inNode */

  def reinsertIn(root: QTNode[U], inNode: QTNode[U]): Unit = {
    root.detach
    for (son <- NonDomSuccessors if root.successors(son).isDefined) {
      reinsertIn(root.successors(son).get, inNode)
    }
    insert0NoCheck(root, inNode)
  }

  /** Remove nodes that are dominated by the candidate node */

  def clean(cand: QTNode[U], root: QTNode[U]): Unit = {
    val kSucc = root.successorship(cand.objectives)
    // Is the root dominated by the candidate node ?
    if (kSucc == bestSuccessor) {
      val newRoot = deleteAndRepair(root)
      if (newRoot.isDefined) {
        clean(cand, newRoot.get)
      }
    } // Is the candidate node dominated by a subtree ?
    else {
      val sons = asWeak(kSucc)
      for (son <- sons if root.successors(son).isDefined) {
        clean(cand, root.successors(son).get)
      }
    }
  }

  def deleteAndRepair(root: QTNode[U]): Option[QTNode[U]] = {

    // Search first son
    var son = 1
    while (!root.successors(son).isDefined && son < bestSuccessor) {
      son += 1
    }
    
    // First son replace its father
    if (son < bestSuccessor) {
      val newRoot = root.successors(son).get
      newRoot.detach() // prevents newRoot to be reinserted into newRoot
      
      // Reinsert
      if (root == this.root.get) {
        this.root = Some(newRoot)
      } 
      else {
        val father = root.father.get
        father.successors(root.kSucc) = Some(newRoot)
        newRoot.father = Some(father)
        newRoot.kSucc = root.kSucc
      }
      // Reinsert sons
      for (son <- NonDomSuccessors if root.successors(son).isDefined) {
        reinsertIn(root.successors(son).get, newRoot)
      }
      
      return Some(newRoot)
      
    } else {
      root.detach()
      
      return None
    }
  }
  
  override def foreach[T](f: QTNode[U] => T): Unit = {
    def forEach0(root: QTNode[U]): Unit = {
      f(root)
      for (son <- NonDomSuccessors if root.successors(son).isDefined) {
        forEach0(root.successors(son).get)
      }
    }
    if (root.isDefined) forEach0(root.get)
  }

  override def iterator: Iterator[QTNode[U]] = new Iterator[QTNode[U]] {
    val todo = new util.Stack[QTNode[U]]
    if (root.isDefined)
      todo.push(root.get)

    override def hasNext: Boolean = !todo.empty()

    override def next(): QTNode[U] = {
      val nroot = todo.pop()
      for (son <- NonDomSuccessors if nroot.successors(son).isDefined) {
        todo.push(nroot.successors(son).get)
      }
      nroot
    }
  }

  def getEvaluations: Set[Array[Double]] = {
    val qtNodeSet = this.toSet
    qtNodeSet.map((e: QTNode[U]) => e.objectives)
  }

  def insert(cand: QTNode[U]): Unit = {
    if (root.isDefined) process(cand, root.get)
    else {
      root = Some(cand)
      priorityQueue.enqueue(cand)
    }
  }
  
  def randomElement: QTNode[U] = {
    val qtNodeList = this.toList
    val randIndex = RandomGenerator.nextInt(qtNodeList.length)
    qtNodeList(randIndex)
  }
  
  def score[T1 <: ParetoElement[U]](candidate: T1): Int = {
    var acc = 0
    for (elem <- this.toSeq) {
      val domi = candidate.dominance(elem)
      if (domi > 0) acc += 1
      else if (domi < 0) acc -= 1
    }
    acc
  }
  
  def contains[T1 <: ParetoElement[U]](elem: T1): Boolean = {
    for (element <- this.toSeq) {
      if (elem.objectives == element.objectives) return true
    }
    false
  }
}

object QT {
  def apply[U: Numeric](nDim: Int, comp: (Int, Int) => Boolean) = new QT(nDim, comp)
}

object QTMin {
  def apply[U: Numeric](nDim: Int) = new QT(nDim, (a, b) => a < b)
}

object QTMinInt {
  def apply(nDim: Int) = new QT[Int](nDim, (a, b) => a < b)
}

object QTMinDouble {
  def apply(nDim: Int) = new QT[Double](nDim, (a, b) => a < b)
}

object QTMax {
  def apply[U: Numeric](nDim: Int) = new QT(nDim, (a, b) => a > b)
}

object QTMaxInt {
  def apply(nDim: Int) = new QT[Int](nDim, (a, b) => a > b)
}

object QTMaxDouble {
  def apply(nDim: Int) = new QT[Double](nDim, (a, b) => a > b)
}
