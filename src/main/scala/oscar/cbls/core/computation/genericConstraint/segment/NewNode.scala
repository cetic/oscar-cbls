// OscaR is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 2.1 of the License, or
// (at your option) any later version.
//
// OscaR is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License  for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with OscaR.
// If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html

package oscar.cbls.core.computation.genericConstraint.segment

/** This represents a node that was not in the initial sequence when pre-computation was performed.
  *
  * @param node
  *   The node added in the global sequence.
  */
case class NewNode(node: Int) extends Segment {

  override def splitAtNode(
    beforeSplitNode: Int,
    splitNode: Int,
    leftLength: Int,
    rightLength: Int
  ): (Option[Segment], Option[Segment]) = {
    require(
      beforeSplitNode == node || splitNode == node,
      s"Splitting $this between $beforeSplitNode and $splitNode is impossible"
    )
    if (beforeSplitNode == node) (Some(this), None)
    else (None, Some(this))
  }

  override def flip(): Segment = this

  override def length(): Int = 1

  override def startNode(): Int = node

  override def endNode(): Int = node

  override def toString: String = s"NewNode (node: $node)"
}
