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

package oscar.cbls.core.computation.globalConstraint.segment

/** This represents a subsequence starting at `startNode` and ending at `endNode`. This subsequence
  * was not present in the global sequence when the precomputation was performed. However, the
  * flipped subsequence obtained by flipping it was present in the global sequence was performed.
  *
  * @param startNode
  *   The first node of the subsequence knowing that the sequence is flipped (it was the last node
  *   of the subsequence when the precomputation was performed).
  * @param endNode
  *   The last node of the subsequence knowing that the sequence if flipped (it was the first node
  *   of the subsequence when the precomputation was performed).
  * @param length
  *   The length of the subsequence.
  */
case class FlippedPreComputedSubSequence(startNode: Int, endNode: Int, length: Int)
    extends Segment {

  override def splitAtNode(
    beforeSplitNode: Int,
    splitNode: Int,
    leftLength: Int,
    rightLength: Int
  ): (Option[Segment], Option[Segment]) = {

    if (splitNode == startNode) // No left subsegment
      (None, Some(this))
    else if (beforeSplitNode == endNode) // No right subsegment
      (Some(this), None)
    else {
      (
        Some(FlippedPreComputedSubSequence(startNode, beforeSplitNode, leftLength)),
        Some(FlippedPreComputedSubSequence(splitNode, endNode, rightLength))
      )
    }
  }

  override def flip(): Segment = PrecomputedSubSequence(endNode, startNode, length)

  override def toString: String =
    s"FlippedPreComputedSubSequence (StartNode: $startNode EndNode: $endNode Length: $length)"
}
