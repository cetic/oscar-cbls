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

package oscar.cbls.lib.neighborhoods.combinator

import oscar.cbls.algo.heap.{BinaryHeap, BinaryHeapWithMove}
import oscar.cbls.core.computation.objective.Objective
import oscar.cbls.core.search.profiling.{CombinatorProfiler, SelectionProfiler}
import oscar.cbls.core.search.{
  MoveFound,
  Neighborhood,
  NeighborhoodCombinator,
  NoMoveFound,
  SearchResult
}

/** At each invocation, this combinator explores one of the neighborhoods in `subNeighborhood` (and
  * try another one if it is exhausted). <br>
  *
  * Neighborhoods are selected based on the abstract method `bestKey`.<br>
  *
  * A tabu is added: in case a neighborhood is exhausted, it is not explored for a number of
  * exploration of this combinator.The tabu can be overridden if all `Neighborhoods` are tabu or
  * exhausted. If all `Neighborhoods` are exhausted, it means that none of them would return a Move,
  * therefore the search would be finished. <br>
  *
  * The `refreshRate` parameter forces the combinator to try all neighborhoods every `refreshRate`
  * invocation. It is useful because some neighborhood can perform poorly at the beginning of search
  * and much better later on, and we do not want the combinator to just "stick to its first
  * impression".
  *
  * @param subNeighborhoods
  *   The Neighborhood that this combinator handles. Those can be
  *   [[oscar.cbls.core.search.SimpleNeighborhood]] or other NeighborhoodCombinator.
  * @param tabuLength
  *   The number of iterations a neighborhood has to wait before being explored again.
  * @param overrideTabuOnFullExhaust
  *   If all the neighborhoods had been exhausted, this combinator tries the older tabu
  *   neighborhoods. This parameter must be lesser than `tabuLength` and is used to determine which
  *   tabu neighborhood can ben explored again.
  * @param refreshRate
  *   Each time the number of iteration is a multiple of `refreshRate`, the profiled statistics are
  *   reset.
  * @param neighborhoodCombinatorName
  *   The name of the neighborhood combinator.
  */
abstract class BestNeighborhoodFirst(
  override val subNeighborhoods: List[Neighborhood],
  tabuLength: Int,
  overrideTabuOnFullExhaust: Int,
  refreshRate: Int,
  neighborhoodCombinatorName: String
) extends NeighborhoodCombinator(neighborhoodCombinatorName, subNeighborhoods) {

  require(
    overrideTabuOnFullExhaust < tabuLength,
    s"overrideTabuOnFullExhaust ($overrideTabuOnFullExhaust) must be < tabuLength ($tabuLength)"
  )

  protected var _selectionProfilerOpt: Option[SelectionProfiler] = {
    subNeighborhoods.foreach(_.profileSearch())
    Some(new SelectionProfiler(this, subNeighborhoods))
  }

  private var iterationNum: Int                      = 0
  private val neighborhoodArray: Array[Neighborhood] = subNeighborhoods.toArray

  /** The tabu value of a neighborhood is the next iteration from which it can be explored again. */
  private val tabu: Array[Int] = Array.fill(neighborhoodArray.length)(0)

  /** Heap containing the tabu neighborhoods ordered by their tabu value. */
  private val tabuNeighborhoods: BinaryHeap[Int] = BinaryHeap(tabu(_), tabu.length)

  /** Heap containing the explorable neighborhoods ordered by `bestKey`. */
  private val neighborhoodHeap: BinaryHeapWithMove[Int] =
    BinaryHeapWithMove((neighId: Int) => bestKey(neighId), neighborhoodArray.length)

  neighborhoodArray.indices.foreach((i: Int) => neighborhoodHeap.insert(i))

  /** Method used to determine which neighborhood is the best. */
  protected def bestKey(neighborhoodId: Int): Long

  override def searchProfiler(): Option[CombinatorProfiler] = _selectionProfilerOpt

  override def profileSearch(): Unit = {
    searchProfiler() match {
      case None =>
        subNeighborhoods.foreach(_.profileSearch())
        _selectionProfilerOpt = Some(new SelectionProfiler(this, subNeighborhoods))
      case _ => ;
    }
  }

  /** Makes the given neighborhood tabu. */
  private def makeTabu(neighborhoodId: Int): Unit = {
    neighborhoodHeap.removeElement(neighborhoodId)
    tabu(neighborhoodId) = iterationNum + tabuLength
    tabuNeighborhoods.insert(neighborhoodId)
  }

  /** Remove older neighborhood from the tabu list. */
  private def updateTabu(): Unit = {
    iterationNum += 1
    while (tabuNeighborhoods.nonEmpty && tabu(tabuNeighborhoods.getFirst.get) <= iterationNum) {
      // If the current iteration number is bigger than the tabu value, the neighborhood can be explored again
      val newNonTabu: Int = tabuNeighborhoods.popFirst().get
      neighborhoodArray(newNonTabu).reset()
      neighborhoodHeap.insert(newNonTabu)
    }
  }

  override protected[this] def exploreCombinator(objective: Objective): SearchResult = {
    if (
      _selectionProfilerOpt.nonEmpty
      && (iterationNum > 0)
      && (iterationNum % refreshRate == 0)
      && neighborhoodArray.indices.exists(_selectionProfilerOpt.get.nbFoundOfNeighborhood(_) != 0)
    ) {
      _selectionProfilerOpt.get.resetSelectionNeighborhoodStatistics()
      for (neighId <- neighborhoodArray.indices) {
        // We reset the key only for neighborhood that will no longer be tabu after this iteration.
        if (tabu(neighId) <= iterationNum) neighborhoodHeap.notifyChange(neighId)
      }
    }
    updateTabu()
    while (neighborhoodHeap.nonEmpty) {
      val bestId           = neighborhoodHeap.getFirst.get
      val bestNeighborhood = neighborhoodArray(bestId)
      val bestProfiler     = bestNeighborhood.searchProfiler().get
      val bestResult       = bestNeighborhood.getMove(objective)
      _selectionProfilerOpt.get.aggregateSubProfilerData(bestProfiler)
      bestResult match {
        case NoMoveFound =>
          // The current best neighborhood is exhausted. We make it tabu to try another one.
          makeTabu(bestId)
        case mf: MoveFound =>
          // The neighborhood found a move. It's key can be updated (e.g., if depending on the profiling).
          // So we update its key in the heap.
          neighborhoodHeap.notifyChange(bestId)
          return mf
      }
    }

    // All the neighborhood are exhausted. But we can retry the older tabu neighborhood.
    if (
      tabuNeighborhoods.nonEmpty && tabu(
        tabuNeighborhoods.getFirst.get
      ) <= iterationNum + overrideTabuOnFullExhaust
    ) {
      val newNonTabu: Int = tabuNeighborhoods.popFirst().get
      neighborhoodArray(newNonTabu).reset()
      neighborhoodHeap.insert(newNonTabu)
      iterationNum -= 1
      exploreCombinator(objective)
    } else NoMoveFound
  }
}
