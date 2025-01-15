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
import oscar.cbls.core.computation.objective.Exploration
import oscar.cbls.core.search.loop.LoopBehavior
import oscar.cbls.core.search.{NoMoveFound, SimpleNeighborhood}
import oscar.cbls.modeling.routing.VRP

/** Companion object of the [[OnePointMoveNeighborhood]] class. */
object OnePointMoveNeighborhood {

  /** Creates a OnePointMoveNeighborhood, which moves a value from a sequence to another place in
    * this sequence.
    *
    * @param vrp
    *   The routing problem to resolve with a search. defining the search space.
    * @param nodesToMove
    *   The nodes this neighborhood can move.
    * @param relevantDestinationNodes
    *   A function which returns, for each node to move, a list of relevant destinations '''nodes'''
    *   after which move the node.
    * @param name
    *   The name of the neighborhood.
    * @param selectNodeToMoveBehavior
    *   How to iterate over the nodes to move.
    * @param selectDestinationBehavior
    *   How to iterate over the destination points.
    * @param hotRestart
    *   When you have symmetries among points to insert and hotRestart, this option will try to have
    *   the hotRestart starting at a different symmetry class than the last one.
    */
  def apply(
    vrp: VRP,
    nodesToMove: () => Iterable[Int],
    relevantDestinationNodes: Int => Iterable[Int],
    name: String = "OnePointMoveNeighborhood",
    selectNodeToMoveBehavior: LoopBehavior = LoopBehavior.first(),
    selectDestinationBehavior: LoopBehavior = LoopBehavior.first(),
    hotRestart: Boolean = true
  ) = new OnePointMoveNeighborhood(
    vrp,
    nodesToMove,
    relevantDestinationNodes,
    name,
    selectNodeToMoveBehavior,
    selectDestinationBehavior,
    hotRestart
  )
}

/** Neighborhood which moves a value from a sequence to another place in this sequence. The search
  * complexity is `O(nk)` where `n` is the number of values in the sequences and `k` the number of
  * relevant destination for each value.
  *
  * '''Note:''' depots cannot be moved.
  *
  * @param vrp
  *   The routing problem to resolve with a search. defining the search space.
  * @param nodesToMove
  *   The nodes this neighborhood can move.
  * @param relevantDestinationNodes
  *   A function which returns, for each node to move, a list of relevant destinations '''nodes'''
  *   after which move the node.
  * @param name
  *   The name of the neighborhood.
  * @param selectNodeToMoveBehavior
  *   How to iterate over the nodes to move.
  * @param selectDestinationBehavior
  *   How to iterate over the destination points.
  * @param hotRestart
  *   When you have symmetries among points to insert and hotRestart, this option will try to have
  *   the hotRestart starting at a different symmetry class than the last one.
  */
class OnePointMoveNeighborhood(
  vrp: VRP,
  nodesToMove: () => Iterable[Int],
  relevantDestinationNodes: Int => Iterable[Int],
  name: String,
  selectNodeToMoveBehavior: LoopBehavior,
  selectDestinationBehavior: LoopBehavior,
  hotRestart: Boolean
) extends SimpleNeighborhood[OnePointMoveMove](name) {

  // Used for hot restart
  private[this] var pivot: Int = 0
  reset()

  override protected def exploreNeighborhood(exploration: Exploration[OnePointMoveMove]): Unit = {
    val seqValue = vrp.routes.defineCurrentValueAsCheckpoint()

    // Activate hot restart if needed
    val iterationScheme =
      if (hotRestart) HotRestart(nodesToMove(), pivot)
      else nodesToMove()

    // How to iterate over the value of the sequence
    val (nodesToMoveIterator, stopValue) = selectNodeToMoveBehavior.toIterator(iterationScheme)

    for (nodeToMove <- nodesToMoveIterator) {

      if (!vrp.isDepot(nodeToMove)) { // Depots cannot be moved at all
        seqValue.explorerAtAnyOccurrence(nodeToMove) match {
          case None => throw new Error("Try to move a node which is not routed")
          case Some(valueToMoveExplorer) =>
            // How to iterate over the destination points
            val (destinationIterator, stopDestination) =
              selectDestinationBehavior.toIterator(relevantDestinationNodes(nodeToMove))

            for (afterPoint <- destinationIterator) {
              val afterPointExplorer = seqValue.explorerAtAnyOccurrence(afterPoint)

              afterPointExplorer match {
                case None => throw new Error("Try to move a value outside the sequence")
                case Some(destinationExplorer) =>
                  vrp.routes.move(
                    valueToMoveExplorer,
                    valueToMoveExplorer,
                    destinationExplorer,
                    flip = false
                  ) // Commit the move.
                  searchProfiler().foreach(x => x.neighborSelected())

                  exploration.checkNeighborWP(objValue =>
                    OnePointMoveMove(
                      vrp.routes,
                      valueToMoveExplorer,
                      destinationExplorer,
                      objValue,
                      name
                    )
                  ) // Check if the move is improving

                  vrp.routes.rollbackToTopCheckpoint(Some(seqValue)) // Cancels the move

                  if (exploration.toReturn != NoMoveFound) {
                    stopDestination()
                    stopValue()
                  }
              }
            }
        }
      }

    }
    vrp.routes.releaseTopCheckpoint()
    if (nodesToMoveIterator.hasUnboundedNext) pivot = nodesToMoveIterator.unboundedNext()
    else reset() // We tried all the values. The exploration stops and we can reset the pivot
  }

  override def doMove(move: OnePointMoveMove): Unit = move.commit()

  override def reset(): Unit = {
    // nodesToMove() is potentially an unsorted collection of value. The pivot is
    // initialized with the first value from nodesToMove() if it is non empty
    val values: Iterable[Int] = nodesToMove()
    pivot = if (values.nonEmpty) values.head else 0
  }
}
