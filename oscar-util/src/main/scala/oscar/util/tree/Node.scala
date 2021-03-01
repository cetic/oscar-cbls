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

class Node[T](val label: T, val sons: List[Node[T]], val edgeLabels: List[T], val col: Color= Color.white, val action: () => Unit = ()=>{}) {
	override def toString = {
	  label.toString + (sons match {
	    case Nil => ""
	    case e => e.mkString("(",",",")")
	  })
	}
}

object Node {
  def apply[T](label: T, sons: List[Node[T]], edgeLabels: List[T], col: Color, action: () => Unit) = new Node(label, sons, edgeLabels,col,action)
  def apply[T](label: T, sons: List[Node[T]], edgeLabels: List[T], col: Color) = new Node(label, sons, edgeLabels,col,() => {})
  def apply[T](label: T, sons: List[Node[T]], edgeLabels: List[T]) = new Node(label, sons, edgeLabels,Color.WHITE,() => {})
  def apply[T](label: T, col: Color, action: () => Unit) = new Node(label, List[Node[T]](), List[T](),col,action)
  def apply[T](label: T, action: () => Unit) = new Node(label, List[Node[T]](), List[T](),Color.WHITE,action)
  
  def apply[T](label: T, col: Color) = new Node(label, List[Node[T]](), List[T](),col,() => {})
  def apply[T](label: T) = new Node(label, List[Node[T]](), List[T](),Color.white,() => {})
  
  def design[T](tree: Node[T], minDist: Double = 2): PositionedNode[T] = {
    val formerMinDist = Extent.minDist
    Extent.minDist = minDist
    def designAux(node: Node[T]): (PositionedNode[T], Extent) = {
      val (trees: List[PositionedNode[T]], extents: List[Extent]) = node.sons.map(subTree => designAux(subTree)).unzip
      val positions = Extent.fitList(extents)
      val pTrees = trees.zip(positions).map(e => e._1.moveTree(e._2))
      val pExtents = extents.zip(positions).map(e => e._1.moveExtent(e._2))
      val resultExtent = Extent ((0.0, 0.0) :: Extent.mergeList(pExtents).extentList)
      val resultTree = PositionedNode(node.label, 0.0, pTrees, node.edgeLabels, node.col, node.action)
      (resultTree, resultExtent)
    }
    val treeToRet = designAux(tree)._1
    Extent.minDist = formerMinDist
    treeToRet
  }
}
