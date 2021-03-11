package oscar.cp.examples


import oscar.cp._


import java.awt.Color

import oscar.algo.Inconsistency
import oscar.algo.search.VisualSearchTree
import oscar.cp.{CPIntVar, CPModel, Weak, add, allDifferent, branch, noAlternative, onSolution, post, search, start}
import oscar.util.selectMin
import oscar.util.tree.Tree
import oscar.visual.{VisualFrame, VisualGrid}

/**
 * n-queens model: place n-queens on a chess-board such that they don't attack each other.
 * this program search for all the solutions
 * Using Non Deterministic Search
 *
 * @author Pierre Schaus pschaus@gmail.com
 */
object QueensVisu extends CPModel with App {

  val nQueens = 8 // Number of queens
  val Queens = 0 until nQueens

  // Variables
  val queens = Array.fill(nQueens)(CPIntVar(Queens))


  val tree = new Tree()
  // -----------------------------------------------
  val f = new VisualFrame("n-Queens", 1, 2)
  val w1 = f.createFrame("Tree")
  val vt = new VisualSearchTree(tree)
  w1.add(vt)
  w1.pack()

  val w2 = f.createFrame("Queens")
  val vg = new VisualGrid(nQueens, nQueens)
  w2.add(vg)
  w2.pack()

  def updateVisu(doms: Seq[Set[Int]], i: Int, v: Int, assign: Boolean): Unit = {
    for (q <- Queens; qv <- Queens) {
      val col = if (doms(q).contains(qv)) Color.green else Color.red
      vg(qv)(q).innerCol = col
    }
    vg(v)(i).innerCol = if (assign) new Color(0, 102, 0) /* dark green */ else new Color(153, 0, 0) /* dark red */
  }
  // -------------------------------------------------
  var currNode = 0

  onSolution { tree.addSuccess(currNode) }

  add(allDifferent(queens),Weak )
  add(allDifferent(for (i <- Queens) yield queens(i) + i), Weak )
  add(allDifferent(for (i <- Queens) yield queens(i) - i), Weak )

  search {
    selectMin(Queens)(i => !queens(i).isBound)(i => i) match {
      case None => noAlternative
      case Some(i) => {
        val x = queens(i)
        val parent = currNode
        val v = x.min
        branch {
          var failed = false
          try {
            post(x === v)
          } catch {
            case _: Throwable => {
              failed = true
            }
          }
          currNode += 1
          val doms = queens.map(_.toSet)
          tree.createBranch(parent, currNode, currNode.toString, "x"+i+"="+v) {
            updateVisu(doms,i,v,true)
          }
          if (failed) throw Inconsistency

        } {
          var failed = false
          try {
            post(x !== v)
          } catch {
            case _: Throwable => {
              failed = true
            }
          }
          currNode += 1
          val doms = queens.map(_.toSet)
          tree.createBranch(parent, currNode, currNode.toString, "x"+i+"!="+v) {
            updateVisu(doms,i,v,false)
          }
          if (failed) throw Inconsistency
        }
      }
    }
  }

  val stats = start(nSols = 3)
  vt.update()
  println(tree.toTikz())

  //print some statistics
  println(stats)


}
