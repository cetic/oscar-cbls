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
package oscar.visual.tree

import java.awt.Color

import oscar.util.tree.Node
import oscar.util.tree.PositionedNode
import javax.swing.SwingUtilities

import oscar.visual.shapes.VisualLabelledRoundRectangle
import oscar.visual.VisualDrawing
import oscar.visual.VisualFrame

class VisualLabelledTree[T](var tree: PositionedNode[T]) extends VisualDrawing(false, false) {

  private def baseOffset: Double = tree.getMaxStringWidth(this) + tree.minOffset

  var levelHeights: Array[Int] = Array.fill(tree.getMaxDepth + 1)(1)
  var maxLinesPerLevel: Array[Int] = tree.getMaxLinesPerLevel

  computeLevelHeights()

  var rectSet: Set[VisualLabelledRoundRectangle] = Set[VisualLabelledRoundRectangle]()
  var branchSet: Set[VisualLabelledBranch] = Set[VisualLabelledBranch]()
  computeRectangles()
  
  def this(tree: Node[T]) = {
    this(Node.design(tree))
  }

  def update(t: PositionedNode[T]): Unit = {
    SwingUtilities.invokeLater(() => {
      tree = t
      clear()
      rectSet = Set[VisualLabelledRoundRectangle]()
      branchSet = Set[VisualLabelledBranch]()
      computeLevelHeights()
      computeRectangles()
      revalidate()
      repaint()
    })
  }

  def computeLevelHeights(): Unit = {
    levelHeights = Array.fill(tree.getMaxDepth + 1)(1)
    maxLinesPerLevel = tree.getMaxLinesPerLevel

    levelHeights(0) = 0
    for (level <- 1 until levelHeights.length) {
      levelHeights(level) = levelHeights(level - 1) + (3 + maxLinesPerLevel(level - 1)) * this.getFontMetrics(this.getFont).getHeight
    }
  }
  
  def computeRectangles(): Unit = {
    def rectAux(node: PositionedNode[T], accOffset: Double, level: Int): Unit = {
      val newNode = new VisualLabelledRoundRectangle(this, accOffset + node.pos - node.getMaxStringWidth(this) / 2, levelHeights(level), node.label.toString, 10)
      newNode.innerCol = node.col
      newNode.onClick(node.action())
      newNode.onClick {
        newNode.innerCol = Color.gray
      }
      rectSet += newNode
      for (i <- node.sons.indices) {
        branchSet += new VisualLabelledBranch(this,
            accOffset + node.pos - node.getMaxStringWidth(this) / 2 + newNode.width / 2,
            levelHeights(level) + newNode.height,
            accOffset + node.pos + node.sons(i).pos - node.sons(i).getMaxStringWidth(this) / 2 + newNode.getWidth(node.sons(i).label.toString) / 2,
            levelHeights(level + 1) , node.edgeLabels(i).toString)
        rectAux(node.sons(i), accOffset + node.pos, level + 1)
      }
    }
    rectAux(tree, baseOffset, 0)
  }
  
  def replaceTree(newTree: PositionedNode[T]): Unit = {
    //this.removeAllShapes
    this.tree = newTree
    update()
  }
  
  def update(): Unit = {
    //this.removeAll()
    rectSet = Set[VisualLabelledRoundRectangle]()
    branchSet = Set[VisualLabelledBranch]()
    computeRectangles()
    repaint()
  }
}

object VisualLabelledTree {
  	
  def main(args : Array[String]): Unit = {
    val f = VisualFrame("toto", 1, 1)
    val inf = f.createFrame("Drawing")

    val C = Node("C",action = () => {println("hello")})
    val D = Node("D")
    val E = Node("E")
    val H = Node("Hello world\nI am a node")
    val I = Node("I")
    val J = Node("J")
    val B = Node("B", List(C, D, E), List("Son 1", "Son 2", "Son 3"))
    val F = Node("FFF")
    val G = Node("G\nL", List(H, I, J), List("Son 1", "Son 2", "Son 3"))
    val A = Node("A\nThe root!\nI like to be a root\nBeing a root is my purpose in life", List(B, F, G), List("Son 1", "Son 2", "Son 3"))
    println(A)
    val positionedA = Node.design(A, 145)

    val visualTree = new VisualLabelledTree(positionedA)

    inf.add(visualTree)
    f.pack()

    visualTree.repaint()

    //Thread.sleep(100000)
  }
}
