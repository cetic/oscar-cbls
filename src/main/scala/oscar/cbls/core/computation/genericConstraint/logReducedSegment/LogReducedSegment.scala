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

package oscar.cbls.core.computation.genericConstraint.logReducedSegment

/** Abstract class for log reduced segments.
  *
  * A LogReducedSegment is a [[oscar.cbls.core.computation.genericConstraint.segment.Segment]] that
  * has been split into several steps of length power of two. Each LogReducedSegment contains some
  * precomputed value corresponding to each step composing it. Those value are used to compute
  * global constraint over a route.
  *
  * @tparam T
  *   The precomputed value. Each concrete GlobalConstraint should define its own.
  */
sealed abstract class LogReducedSegment[T] {}

/** A [[oscar.cbls.core.computation.genericConstraint.segment.PrecomputedSubSequence]] that has been
  * log reduced.
  *
  * @param startNode
  *   The start node of the PreComputedSubSequence.
  * @param endNode
  *   The end node of the PreComputedSubSequence.
  * @param steps
  *   An ordered list of precomputed value corresponding to the PreComputedSubSequence.
  * @tparam T
  *   The precomputed value. Each concrete GlobalConstraint should define its own.
  */
case class LogReducedPreComputedSegment[T](startNode: Int, endNode: Int, steps: List[T])
    extends LogReducedSegment[T] {
  override def toString: String = {
    s"LogReducedSubSequence(startNode: $startNode endNode: $endNode steps: {${steps.mkString(",")}})"
  }
}

/** A [[oscar.cbls.core.computation.genericConstraint.segment.FlippedPreComputedSubSequence]] that
  * has been log reduced.
  *
  * @param startNode
  *   The start node of the FlippedPreComputedSubSequence.
  * @param endNode
  *   The end node of the FlippedPreComputedSubSequence.
  * @param steps
  *   An ordered list of precomputed value corresponding to the FlippedPreComputedSubSequence.
  * @tparam T
  *   The precomputed value. Each concrete GlobalConstraint should define its own.
  */
case class LogReducedFlippedPreComputedSegment[T](startNode: Int, endNode: Int, steps: List[T])
    extends LogReducedSegment[T] {

  override def toString: String = {
    s"LogReducedFlippedSubSequence(startNode: $startNode endNode: $endNode steps: {${steps.mkString(",")}})"
  }
}

/** A [[oscar.cbls.core.computation.genericConstraint.segment.NewNode]] that has been log reduced.
  *
  * @param node
  *   The new node.
  * @param value
  *   The new node PreComputed value.
  * @tparam T
  *   The precomputed value. Each concrete GlobalConstraint should define its own.
  */
case class LogReducedNewNode[T](node: Int, value: T) extends LogReducedSegment[T] {
  override def toString: String = {
    s"LogReducedNewNode(node: $node value: $value)"
  }
}
