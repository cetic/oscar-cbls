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
import oscar.cbls.core.search.{NoMoveFound, SimpleNeighborhood}
import oscar.cbls.core.search.loop.LoopBehavior
import oscar.cbls.modeling.routing.VRP

/** Companion object of the [[TwoOpt]] class. */
object TwoOpt {

  /** Creates a 2-opt neighborhood, which contains solutions where a subsequence of the original
    * route is flipped (i.e., reversed).
    *
    * @param vrp
    *   The routing problem to solve.
    * @param relevantStartOfSegment
    *   Returns a set of candidate nodes for the first endpoint of a subsequence flip.
    * @param maxSegmentLength
    *   The maximum length of a flipped segment.
    * @param name
    *   The name of the neighborhood.
    * @param selectFlippedSegmentBehavior
    *   How to iterate over the start of segments.
    * @param hotRestart
    *   Whether to use a [[oscar.cbls.algo.search.HotRestart]] mechanism.
    */
  def apply(
    vrp: VRP,
    relevantStartOfSegment: () => Iterable[Int],
    maxSegmentLength: Option[Int] = None,
    name: String = "2-opt",
    selectFlippedSegmentBehavior: LoopBehavior = LoopBehavior.first(),
    hotRestart: Boolean = true
  ): TwoOpt = {

    val maxLength = maxSegmentLength match {
      case None => vrp.n
      case Some(v) =>
        require(v > 0, "The maximum length of the moved segment must > 0")
        v
    }

    val relevantEndOfSegment: IntSequenceExplorer => List[IntSequenceExplorer] =
      (startExp: IntSequenceExplorer) => {
        var toReturn: List[IntSequenceExplorer] = List()
        var currentExp: IntSequenceExplorer     = startExp.next
        while (
          currentExp.position < vrp.routes.pendingValue.size
          && currentExp.position < startExp.position + maxLength
          && currentExp.value >= vrp.v
        ) {
          toReturn = currentExp :: toReturn
          currentExp = currentExp.next
        }
        toReturn
      }

    new TwoOpt(
      vrp,
      relevantStartOfSegment,
      relevantEndOfSegment,
      name,
      selectFlippedSegmentBehavior,
      selectFlippedSegmentBehavior,
      hotRestart
    )
  }

}

/** Classical 2-opt neighborhood, which contains solutions where a subsequence of the original route
  * is flipped (i.e., reversed).
  *
  * Complexity is `O(m * n)`, where `m` is the size of `relevantStartOfSegment` and `n` is the size
  * of `relevantEndOfSegment`.
  *
  * @param vrp
  *   The routing problem to solve.
  * @param relevantStartOfSegment
  *   Returns a set of candidate nodes for the first endpoint of a subsequence flip.
  * @param relevantEndOfSegment
  *   Returns a set of candidate nodes for the second endpoint of a subsequence flip depending on
  *   the first one.
  * @param name
  *   The name of the neighborhood.
  * @param selectStartOfFlippedSegmentBehavior
  *   How to iterate over the start of segments.
  * @param hotRestart
  *   Whether to use a [[oscar.cbls.algo.search.HotRestart]] mechanism.
  */
class TwoOpt(
  vrp: VRP,
  relevantStartOfSegment: () => Iterable[Int],
  relevantEndOfSegment: IntSequenceExplorer => Iterable[IntSequenceExplorer],
  name: String,
  selectStartOfFlippedSegmentBehavior: LoopBehavior,
  selectEndOfFlippedSegmentBehavior: LoopBehavior,
  hotRestart: Boolean
) extends SimpleNeighborhood[TwoOptMove](name) {

  var pivot: Int = 0
  reset()

  override protected def exploreNeighborhood(exploration: Exploration[TwoOptMove]): Unit = {
    val seqValue: IntSequence = vrp.routes.defineCurrentValueAsCheckpoint()

    val startSegments: Iterable[Int] = relevantStartOfSegment()
    val iterationSchemeOnZone: Iterable[Int] =
      if (hotRestart) HotRestart(startSegments, pivot) else startSegments

    val (startSegmentsIterator, stopStartSegment) =
      selectStartOfFlippedSegmentBehavior.toIterator(iterationSchemeOnZone)

    for (startSeg: Int <- startSegmentsIterator) {
      val startSegExp: Option[IntSequenceExplorer] = seqValue.explorerAtAnyOccurrence(startSeg)
      assert(startSegExp.nonEmpty, "Trying to move segment not in the sequence")

      val potentialEnd = relevantEndOfSegment(startSegExp.get)

      val (endSegmentIterator, stopEndSegment) =
        selectEndOfFlippedSegmentBehavior.toIterator(potentialEnd)

      for (endSegExp <- endSegmentIterator) {
        vrp.routes.flip(startSegExp.get, endSegExp)
        searchProfiler().foreach(x => x.neighborSelected())

        exploration.checkNeighborWP(objValue =>
          new TwoOptMove(vrp.routes, startSegExp.get, endSegExp, objValue, name)
        ) // Checks if the move is improving

        vrp.routes.rollbackToTopCheckpoint() // Cancels the move

        if (exploration.toReturn != NoMoveFound) {
          stopEndSegment()
          stopStartSegment()
        }
      }
    }

    vrp.routes.releaseTopCheckpoint()
    if (startSegmentsIterator.hasUnboundedNext) pivot = startSegmentsIterator.unboundedNext()
    else reset()

  }

  override def doMove(move: TwoOptMove): Unit = move.commit()

  override def reset(): Unit = {
    val startSegment = relevantStartOfSegment()
    pivot = if (startSegment.nonEmpty) startSegment.head else 0
  }
}
