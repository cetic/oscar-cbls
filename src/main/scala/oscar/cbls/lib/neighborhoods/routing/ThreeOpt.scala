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

package oscar.cbls.lib.neighborhoods.routing

import oscar.cbls.algo.sequence.IntSequenceExplorer
import oscar.cbls.core.search.SimpleNeighborhood
import oscar.cbls.core.search.loop.LoopBehavior
import oscar.cbls.modeling.routing.VRP

/** Factory for variants of the 3-opt neighborhood. */
object ThreeOpt {

  /** Return a classical 3-opt neighborhood. This implementation first iterates on insertion points
    * and then searches for segment to move somewhere else in the routes.
    *
    * @param vrp
    *   The routing problem to solve.
    * @param insertionPoints
    *   Returns a list of ''nodes'' after which it is relevant to move a segment.
    * @param startOfMovedSegment
    *   For an insertion point, returns a set of nodes than can define the start of a segment to
    *   move after the insertion point.
    * @param maxLengthOfMovedSegment
    *   The maximum length of the moved segment.
    * @param name
    *   The name of the neighborhood.
    * @param tryFlip
    *   If the search has to try to flip segments.
    * @param selectInsertPointBehavior
    *   How to iterate over the insertion points.
    * @param selectMovedSegmentBehavior
    *   How to iterate over the moved segments.
    * @param selectFlipBehavior
    *   How to select if we flip the moved segment or not. If `tryFlip` is `true`, the neighborhood
    *   can select the best move between flipping the segment or not or simply choose the first
    *   possibility improving the objective function.
    * @param skipOnePointMove
    *   If `true`, the moved segments will include more thant one point.
    * @param breakSymmetry
    *   Breaks symmetry in 3-opt when moving a segment within the same vehicle without flipping it.
    *   For example, in the route `0 -> 1 -> 2 -> 3 -> 4 -> 5`, moving `2 -> 3` after `5` gives the
    *   same result as moving `4 -> 5` after `1`. The symmetry is broken by only moving the segment
    *   after a bigger position in the vehicle route. In the example only moving `2 -> 3` after `5`
    *   is tested.
    * @param hotRestart
    *   Whether to use a [[oscar.cbls.algo.search.HotRestart]] mechanism.
    */
  def insertionPointFirst(
    vrp: VRP,
    insertionPoints: () => Iterable[Int],
    startOfMovedSegment: Int => Iterable[Int],
    maxLengthOfMovedSegment: Int,
    name: String = "3-opt neighborhood insertion point first",
    tryFlip: Boolean = true,
    selectInsertPointBehavior: LoopBehavior = LoopBehavior.first(),
    selectMovedSegmentBehavior: LoopBehavior = LoopBehavior.first(),
    selectFlipBehavior: LoopBehavior = LoopBehavior.best(),
    skipOnePointMove: Boolean = false,
    breakSymmetry: Boolean = true,
    hotRestart: Boolean = true
  ): ThreeOptInsertionPointFirst = {
    new ThreeOptInsertionPointFirst(
      vrp,
      insertionPoints,
      startOfMovedSegment,
      maxLengthOfMovedSegment,
      name,
      tryFlip,
      selectInsertPointBehavior,
      selectMovedSegmentBehavior,
      selectFlipBehavior,
      skipOnePointMove,
      breakSymmetry,
      hotRestart
    )
  }

  /** Return a classical 3-opt neighborhood. This implementation first iterates on segments to move
    * and then search for an insertion point somewhere else in the routes
    *
    * @param vrp
    *   The routing problem to solve.
    * @param startOfMovedSegment
    *   Returns a set of ''nodes'' than can define the start of a segment to move.
    * @param insertionPoints
    *   Given the node starting the moved segment, returns a set of ''nodes'' after which it is
    *   relevant to move the moved segment.
    * @param maxLengthOfMovedSegment
    *   The maximum length of the moved segment.
    * @param name
    *   The name of the neighborhood.
    * @param tryFlip
    *   If the search has to try to flip segments.
    * @param selectMovedSegmentBehavior
    *   How to iterate over the insertion points.
    * @param selectInsertPointBehavior
    *   How to iterate over the insertion points.
    * @param selectFlipBehavior
    *   How to select if we flip the moved segment or not.
    * @param skipOnePointMove
    *   If `true`, the moved segments will include more thant one point.If `tryFlip` is `true`, the
    *   neighborhood can select the best move between flipping the segment or not or simply choose
    *   the first possibility improving the objective function.
    * @param breakSymmetry
    *   Breaks symmetry in 3-opt when moving a segment within the same vehicle without flipping it.
    *   For example, in the route `0 -> 1 -> 2 -> 3 -> 4 -> 5`, moving `2 -> 3` after `5` gives the
    *   same result as moving `4 -> 5` after `1`. The symmetry is broken by only moving the segment
    *   after a bigger position in the vehicle route. In the example only moving `2 -> 3` after `5`
    *   is tested.
    * @param hotRestart
    *   Whether to use a [[oscar.cbls.algo.search.HotRestart]] mechanism.
    */
  def movedSegmentFirst(
    vrp: VRP,
    startOfMovedSegment: () => Iterable[Int],
    insertionPoints: Int => Iterable[Int],
    maxLengthOfMovedSegment: Int,
    name: String = "3-opt neighborhood moved segment first",
    tryFlip: Boolean = true,
    selectMovedSegmentBehavior: LoopBehavior = LoopBehavior.first(),
    selectInsertPointBehavior: LoopBehavior = LoopBehavior.first(),
    selectFlipBehavior: LoopBehavior = LoopBehavior.best(),
    skipOnePointMove: Boolean = false,
    breakSymmetry: Boolean = true,
    hotRestart: Boolean = true
  ): ThreeOptMovedSegmentFirst = new ThreeOptMovedSegmentFirst(
    vrp,
    startOfMovedSegment,
    insertionPoints,
    maxLengthOfMovedSegment,
    name,
    tryFlip,
    selectMovedSegmentBehavior,
    selectInsertPointBehavior,
    selectFlipBehavior,
    skipOnePointMove,
    breakSymmetry,
    hotRestart
  )
}

/** Abstract class containing useful methods to implement variants of the 3-opt neighborhoods.
  *
  * @param vrp
  *   The routing problem to solve.
  * @param maxLengthOfMovedSegment
  *   The maximum length of the moved segment.
  * @param name
  *   The name of the neighborhood.
  * @param skipOnePointMove
  *   If `true`, the moved segments will include more thant one point.
  */
abstract class ThreeOpt(
  vrp: VRP,
  maxLengthOfMovedSegment: Int,
  name: String,
  skipOnePointMove: Boolean
) extends SimpleNeighborhood[ThreeOptMove](name) {

  require(maxLengthOfMovedSegment > 0, "The moved segment must contain at least 1 node.")

  override def doMove(move: ThreeOptMove): Unit = move.commit()

  /** Returns all the nodes' explorers from `startExp` to the end of the vehicle route. Returns also
    * the vehicle of the segment.
    */
  protected def findPotentialEndOfSegment(
    startExp: IntSequenceExplorer,
    maxPos: Int
  ): List[IntSequenceExplorer] = {
    var toReturn: List[IntSequenceExplorer] = if (skipOnePointMove) List() else List(startExp)
    var currentExp                          = startExp.next
    while (
      currentExp.position < maxPos
      && currentExp.position < startExp.position + maxLengthOfMovedSegment
      && currentExp.value >= vrp.v
    ) {
      toReturn = currentExp :: toReturn
      currentExp = currentExp.next
    }
    toReturn
  }
}
