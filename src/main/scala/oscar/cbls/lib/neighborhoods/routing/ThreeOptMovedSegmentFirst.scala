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

import oscar.cbls.algo.search.HotRestart
import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.computation.objective.Exploration
import oscar.cbls.core.search.NoMoveFound
import oscar.cbls.core.search.loop.LoopBehavior
import oscar.cbls.modeling.routing.{CachedVehicleSearcher, VRP}

/** Classical 3-opt neighborhood. This implementation first iterates on segments to move and then
  * search for an insertion point somewhere else in the routes
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
  *   How to select if we flip the moved segment or not. If `tryFlip` is `true`, the neighborhood
  *   can select the best move between flipping the segment or not or simply choose the first
  *   possibility improving the objective function.
  * @param skipOnePointMove
  *   If `true`, the moved segments will include more thant one point.
  * @param breakSymmetry
  *   Breaks symmetry in 3-opt when moving a segment within the same vehicle without flipping it.
  *   For example, in the route `0 -> 1 -> 2 -> 3 -> 4 -> 5`, moving `2 -> 3` after `5` gives the
  *   same result as moving `4 -> 5` after `1`. The symmetry is broken by only moving the segment
  *   after a bigger position in the vehicle route. In the example only moving `2 -> 3` after `5` is
  *   tested.
  * @param hotRestart
  *   Whether to use a [[oscar.cbls.algo.search.HotRestart]] mechanism.
  */
class ThreeOptMovedSegmentFirst(
  vrp: VRP,
  startOfMovedSegment: () => Iterable[Int],
  insertionPoints: Int => Iterable[Int],
  maxLengthOfMovedSegment: Int,
  name: String,
  tryFlip: Boolean,
  selectMovedSegmentBehavior: LoopBehavior,
  selectInsertPointBehavior: LoopBehavior,
  selectFlipBehavior: LoopBehavior,
  skipOnePointMove: Boolean,
  breakSymmetry: Boolean,
  hotRestart: Boolean
) extends ThreeOpt(vrp, maxLengthOfMovedSegment, name, skipOnePointMove) {

  private[this] var pivot: Int = 0
  reset()

  override protected def exploreNeighborhood(exploration: Exploration[ThreeOptMove]): Unit = {
    val seqValue: IntSequence = vrp.routes.defineCurrentValueAsCheckpoint()

    // Creates a vehicle searcher at the checkpoint. Only needed if we break symmetries
    val vehicleSearcher: Option[CachedVehicleSearcher] =
      if (breakSymmetry) Some(CachedVehicleSearcher(seqValue, vrp.v))
      else None

    val startNodes: Iterable[Int] = startOfMovedSegment()
    val iterationScheme: Iterable[Int] =
      if (hotRestart) HotRestart(startNodes, pivot) else startNodes

    val (startSegmentIterator, stopStartSegment) =
      selectMovedSegmentBehavior.toIterator(iterationScheme)

    for (startSeg: Int <- startSegmentIterator) {
      val startSegExp: Option[IntSequenceExplorer] = seqValue.explorerAtAnyOccurrence(startSeg)
      assert(startSegExp.nonEmpty, "Try to move a segment not starting in the sequence.")

      // Finds the vehicle of the segment. Only needed if we break symmetries.
      val vehicleOfSegment =
        if (breakSymmetry)
          vehicleSearcher.get.vehicleReachingPosition(startSegExp.get.position, seqValue)
        else -1

      val (insertPointsIterator, stopInsertPoints) =
        selectInsertPointBehavior.toIterator(insertionPoints(startSeg))

      for (insertPoint <- insertPointsIterator) {

        val insertPointExp: Option[IntSequenceExplorer] =
          seqValue.explorerAtAnyOccurrence(insertPoint)
        assert(insertPointExp.nonEmpty, "Insert Point not in the sequence.")

        // Find the vehicle of the insertion point. Only needed if we break symmetry
        val vehicleOfInsertPoint: Int =
          if (breakSymmetry)
            vehicleSearcher.get.vehicleReachingPosition(insertPointExp.get.position, seqValue)
          else -1

        //  What are the possible end of the moved segment.
        // The ends are always in the same vehicle that the start
        val potentialEndOfMovedSegment: List[IntSequenceExplorer] =
          findPotentialEndOfSegment(startSegExp.get, seqValue.size)

        val (endSegmentIterator, stopEndSegment) =
          selectMovedSegmentBehavior.toIterator(potentialEndOfMovedSegment)

        for (endSegExp: IntSequenceExplorer <- endSegmentIterator) {

          if (
            insertPointExp.get.position < startSegExp.get.position
            || endSegExp.position < insertPointExp.get.position
          ) { // If the moved segment does not contain the insertion point.

            val (flipIterator, stopFlip) =
              selectFlipBehavior.toIterator(if (tryFlip) List(true, false) else List(false))

            for (flip: Boolean <- flipIterator) {

              if (
                !(breakSymmetry
                  && vehicleOfInsertPoint == vehicleOfSegment
                  && insertPointExp.get.position < startSegExp.get.position
                  && !flip)
              ) { // Skips this if we break symmetry, we move the segment on the
                // left in the same vehicle without flipping it.

                vrp.routes.move(startSegExp.get, endSegExp, insertPointExp.get, flip)
                searchProfiler().foreach(x => x.neighborSelected())

                exploration.checkNeighborWP(objValue =>
                  ThreeOptMove(
                    vrp.routes,
                    startSegExp.get,
                    endSegExp,
                    insertPointExp.get,
                    flip,
                    objValue,
                    name
                  )
                ) // Checks if the move is improving

                vrp.routes.rollbackToTopCheckpoint(Some(seqValue)) // Cancels the move

                if (exploration.toReturn != NoMoveFound) {
                  stopFlip()
                  stopEndSegment()
                  stopInsertPoints()
                  stopStartSegment()
                }
              }
            }
          }
        }
      }
    }

    vrp.routes.releaseTopCheckpoint()
    if (startSegmentIterator.hasUnboundedNext) pivot = startSegmentIterator.unboundedNext()
    else reset() // We tried all the values. The exploration stops and we can reset the pivot.
  }

  override def reset(): Unit = {
    val startNodes = startOfMovedSegment()
    pivot = if (startNodes.nonEmpty) startNodes.head else 0
  }
}