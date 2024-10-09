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

import oscar.cbls.algo.search.{HotRestart, IdenticalAggregator}
import oscar.cbls.algo.sequence.IntSequenceExplorer
import oscar.cbls.core.computation.objective.Exploration
import oscar.cbls.core.search.loop.LoopBehavior
import oscar.cbls.core.search.{NoMoveFound, SimpleNeighborhood}
import oscar.cbls.modeling.routing.VRP

/** Companion object of the [[RemovePointNeighborhood]] class. */
object RemovePointNeighborhood {

  /** Creates a RemovePointNeighborhood that removes nodes from a given sequence.
    *
    * @param vrp
    *   The routing problem to solve
    * @param relevantNodesToRemove
    *   The nodes in the sequence that can be removed
    * @param name
    *   The name of the neighborhood
    * @param selectNodesToRemoveBehavior
    *   How to iterate over the nodes that can be removed
    * @param nodesToRemoveSymmetryClass
    *   An optional function that takes nodes as input and returns their symmetry class. If
    *   provided, only one of the nodes in each class will be considered for removal. Note that
    *   `Int.MinValue` is considered different to itself
    * @param hotRestart
    *   Whether to use a [[oscar.cbls.algo.search.HotRestart]] mechanism an assert
    */
  def apply(
    vrp: VRP,
    relevantNodesToRemove: () => Iterable[Int],
    name: String = "RemovePointNeighborhood",
    selectNodesToRemoveBehavior: LoopBehavior = LoopBehavior.first(),
    nodesToRemoveSymmetryClass: Option[Int => Int] = None,
    hotRestart: Boolean = true
  ): RemovePointNeighborhood = new RemovePointNeighborhood(
    vrp,
    relevantNodesToRemove,
    name,
    selectNodesToRemoveBehavior,
    nodesToRemoveSymmetryClass,
    hotRestart
  )
}

/** Neighborhood that removes nodes from a sequence. For a routing problem, nodes that can be
  * removed must be route nodes, not vehicles nodes. The size of this neighborhood is in `O(r)`,
  * where `r` is the number of nodes that can be removed.
  *
  * @param vrp
  *   The routing problem to solve
  * @param relevantNodesToRemove
  *   The nodes in the sequence that can be removed
  * @param name
  *   The name of the neighborhood
  * @param selectNodesToRemoveBehavior
  *   How to iterate over the nodes that can be removed
  * @param nodesToRemoveSymmetryClass
  *   An optional function that takes nodes as input and returns their symmetry class. If provided,
  *   only one of the nodes in each class will be considered for removal. Note that `Int.MinValue`
  *   is considered different to itself
  * @param hotRestart
  *   Whether to use a [[oscar.cbls.algo.search.HotRestart]] mechanism
  */
class RemovePointNeighborhood(
  vrp: VRP,
  relevantNodesToRemove: () => Iterable[Int],
  name: String,
  selectNodesToRemoveBehavior: LoopBehavior,
  nodesToRemoveSymmetryClass: Option[Int => Int],
  hotRestart: Boolean
) extends SimpleNeighborhood[RemovePointMove](name) {

  // Used for hot restart
  private[this] var pivot: Int = 0
  reset()

  override protected def exploreNeighborhood(exploration: Exploration[RemovePointMove]): Unit = {
    val seqValue = vrp.routes.defineCurrentValueAsCheckpoint()

    // Activates hot restart if needed
    val nodes                          = relevantNodesToRemove()
    val iterationScheme: Iterable[Int] = if (hotRestart) HotRestart(nodes, pivot) else nodes

    // Removes symmetry from nodes to remove
    val iterationSchemeSymmetryFree: Iterable[Int] = nodesToRemoveSymmetryClass match {
      case None    => iterationScheme
      case Some(s) => IdenticalAggregator.removeIdenticalClassesLazily(iterationScheme, s)
    }

    // How to iterate over the nodes to remove
    val (nodeToRemoveIterator, stopNodesToRemove) =
      selectNodesToRemoveBehavior.toIterator(iterationSchemeSymmetryFree)

    for (nodeToRemove <- nodeToRemoveIterator) {
      assert(nodeToRemove >= vrp.v, "Trying to remove a vehicle.")

      val toRemoveExplorer: Option[IntSequenceExplorer] =
        seqValue.explorerAtAnyOccurrence(nodeToRemove)

      assert(toRemoveExplorer.nonEmpty, "Trying to remove an unrouted node.")
      vrp.routes.remove(toRemoveExplorer.get)
      searchProfiler().foreach(x => x.neighborSelected())

      exploration.checkNeighborWP(objValue =>
        new RemovePointMove(vrp.routes, toRemoveExplorer.get, objValue, name)
      ) // Checks if the move is improving

      vrp.routes.rollbackToTopCheckpoint(Some(seqValue)) // Cancels the move

      if (exploration.toReturn != NoMoveFound) stopNodesToRemove()

    }

    vrp.routes.releaseTopCheckpoint()
    if (nodeToRemoveIterator.hasUnboundedNext) pivot = nodeToRemoveIterator.unboundedNext()
    else reset() // We tried all the values. The exploration stops and we can reset the pivot.
  }

  override def doMove(move: RemovePointMove): Unit = move.commit()

  override def reset(): Unit = {
    // relevantNodesToRemove() is potentially an unsorted collection of value. The pivot is
    // initialized with the first value from relevantNodesToRemove() if it is non empty
    val nodesToRemove = relevantNodesToRemove()
    if (nodesToRemove.nonEmpty) pivot = nodesToRemove.head
    else pivot = 0
  }
}
