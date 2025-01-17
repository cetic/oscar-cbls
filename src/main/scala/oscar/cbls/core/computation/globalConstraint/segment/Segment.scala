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

/** Abstract class that models a segment of a [[oscar.cbls.core.computation.seq.SeqVariable]].
  *
  * The segments are used to symbol on which parts of the sequence precomputation can be used. For
  * example, if we perform precomputation on the sequence `0 -> 1 -> 2 -> 3 -> 4 -> 5`, we have only
  * one segment. After a move on the sequence we have `0 -> 1 -> 4 -> 5-> 2 -> 3`. We have three
  * segment on which we can reuse precomputation:
  *   - `0 -> 1`
  *   - `4 -> 5`
  *   - `2 -> 3`
  */
abstract class Segment {

  /** Splits this Segment into two Segment: one from the start included to the split node excluded
    * and another one from the split node included to the end excluded.
    *
    * If `splitNode == startNode()` or if `splitNode == endNode()`, there will be only one Segment
    * returned, the segment itself.
    *
    * @param beforeSplitNode
    *   The node right before the split node.
    * @param splitNode
    *   The node used to perform the split.
    * @param leftLength
    *   The length of the left subsegment.
    * @param rightLength
    *   The length of the right subsegment.
    * @return
    *   The left part of the split Segment (if it exists) and the right part (if it exists),
    *   including the split node.
    */
  def splitAtNode(
    beforeSplitNode: Int,
    splitNode: Int,
    leftLength: Int,
    rightLength: Int
  ): (Option[Segment], Option[Segment])

  /** Flips this Segment. */
  def flip(): Segment

  /** Returns the length of this Segment. */
  def length(): Int

  /** Returns the first node of this Segment. */
  def startNode(): Int

  /** Returns the last node of this Segment. */
  def endNode(): Int
}
