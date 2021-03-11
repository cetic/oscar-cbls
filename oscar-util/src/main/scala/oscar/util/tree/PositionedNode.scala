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
package oscar.util.tree

import java.awt.Color
import javax.swing.JPanel
import scala.math.Ordering.Double.TotalOrdering

class PositionedNode[T](val label: T, var pos: Double, val sons: List[PositionedNode[T]], val edgeLabels: List[T], val col: Color= Color.white, val action: () => Unit = () => {}) {
  private var maxLinesPerLevel = Array[Int]()

  def moveTree(x: Double) = new PositionedNode(label, this.pos + x, sons, edgeLabels,col,action)
  
  def minOffset: Double = {
    def minOffsetAux(curNode: PositionedNode[T], acc: Double): List[Double] = {
      var myIntList = List(curNode.pos + acc)
      for (son <- curNode.sons) {
        myIntList :::= minOffsetAux(son, acc + curNode.pos)
      }
      myIntList
    }
    Math.abs(minOffsetAux(this, 0).min)
  }

  def getMaxDepth: Int = {
    getMaxDepthAux(this, 0)
  }

  private def getMaxDepthAux(curNode: PositionedNode[T], acc: Int): Int = {
    if (curNode.sons.isEmpty) {
      acc
    }
    else {
      curNode.sons.map(son => getMaxDepthAux(son, acc + 1)).max
    }
  }

  def getMaxLinesPerLevel: Array[Int] = {
    println(getMaxDepth)
    maxLinesPerLevel = Array.fill(getMaxDepth + 1)(1)
    println(maxLinesPerLevel.mkString(" "))
    computeMaxLinesPerLevelAux(this, 0)
    maxLinesPerLevel
  }

  private def computeMaxLinesPerLevelAux(curNode: PositionedNode[T], level: Int): Unit = {
    maxLinesPerLevel(level) = math.max(curNode.label.toString.split("\n").length, maxLinesPerLevel(level))
    for (son <- curNode.sons) {
      computeMaxLinesPerLevelAux(son, level + 1)
    }
  }

  def getMaxStringWidth(d: JPanel): Int = {
    label.toString.split("\n").map(l => d.getFontMetrics(d.getFont).stringWidth(l)).max
  }

}

object PositionedNode {
  def apply[T](label: T, pos: Double, sons: List[PositionedNode[T]], edgeLabels: List[T], col: Color=Color.white,action: () => Unit = () => {}) = new PositionedNode(label, pos, sons, edgeLabels, col, action)
}
