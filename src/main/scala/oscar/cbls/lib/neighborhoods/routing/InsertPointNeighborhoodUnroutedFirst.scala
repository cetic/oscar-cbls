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
import oscar.cbls.core.computation.objective.Exploration
import oscar.cbls.core.search.loop.LoopBehavior
import oscar.cbls.core.search.{NoMoveFound, SimpleNeighborhood}
import oscar.cbls.model.routing.VRP

/** Companion object of the [[InsertPointNeighborhoodUnroutedFirst]] class */
object InsertPointNeighborhoodUnroutedFirst {

  /** Creates a InsertPointNeighborhoodUnroutedFirst which inserts a new value in a sequence. It
    * first selects the new value to insert and then where to insert it.
    *
    * @param vrp
    *   The routing problem to resolve with a search.
    * @param nodesToInsert
    *   The nodes this neighborhood will try to insert.
    * @param relevantInsertAfterNodes
    *   A function that, for each new value, returns a list of sequenced '''nodes''' such that it is
    *   relevant to insert the new value after.
    * @param name
    *   The name of the neighborhood.
    * @param selectNodeBehavior
    *   How to iterate over the new variables.
    * @param selectInsertionAfterPointBehavior
    *   How to iterate over the sequenced values to find an insertion point.
    * @param nodesSymmetryClass
    *   A function that input the unrouted and returns a symmetry class; only one of the unrouted
    *   nodes in each class will be considered for insert. `Int.MinValue` is considered different to
    *   itself if you set to None this will not be used at all.
    * @param hotRestart
    *   Set the use of a [[oscar.cbls.algo.search.HotRestart]] mechanism.
    */
  def apply(
    vrp: VRP,
    nodesToInsert: () => Iterable[Int],
    relevantInsertAfterNodes: Int => Iterable[Int],
    name: String = "InsertPointNeighborhoodUnroutedFirst",
    selectNodeBehavior: LoopBehavior = LoopBehavior.first(),
    selectInsertionAfterPointBehavior: LoopBehavior = LoopBehavior.first(),
    nodesSymmetryClass: Option[Int => Int] = None,
    hotRestart: Boolean = true
  ): InsertPointNeighborhoodUnroutedFirst = new InsertPointNeighborhoodUnroutedFirst(
    vrp,
    nodesToInsert,
    relevantInsertAfterNodes,
    name,
    selectNodeBehavior,
    selectInsertionAfterPointBehavior,
    nodesSymmetryClass,
    hotRestart
  )
}

/** Neighborhood which inserts a new value in a sequence. It first selects the new value to insert
  * and then where to insert it. The size of this neighborhood is `O(v * n)` where `v` is the number
  * of possible values to insert and `n` the size of the sequence. It can be cut down to `O(v * k)`
  * by using the relevant neighbors, and specifying `k` neighbors for each new value.
  *
  * @param vrp
  *   The routing problem to resolve with a search.
  * @param nodesToInsert
  *   The nodes this neighborhood will try to insert.
  * @param relevantInsertAfterNode
  *   A function that, for each new value, returns a list of sequenced '''nodes''' such that it is
  *   relevant to insert the new value after.
  * @param name
  *   The name of the neighborhood.
  * @param selectNodeBehavior
  *   How to iterate over the new variables.
  * @param selectInsertionAfterPointBehavior
  *   How to iterate over the sequenced value to find an insertion point.
  * @param nodesSymmetryClass
  *   A function that input the unrouted nodes and returns a symmetry class; only one of the
  *   unrouted nodes in each class will be considered for insert. `Int.MinValue` is considered
  *   different to itself if you set to None this will not be used at all.
  * @param hotRestart
  *   Set the use of a [[oscar.cbls.algo.search.HotRestart]] mechanism.
  */
class InsertPointNeighborhoodUnroutedFirst(
  vrp: VRP,
  nodesToInsert: () => Iterable[Int],
  relevantInsertAfterNode: Int => Iterable[Int],
  name: String,
  selectNodeBehavior: LoopBehavior,
  selectInsertionAfterPointBehavior: LoopBehavior,
  nodesSymmetryClass: Option[Int => Int],
  hotRestart: Boolean
) extends SimpleNeighborhood[InsertPointMove](name) {

  // Used for hot restart
  private[this] var pivot: Int = 0
  reset()

  override protected def exploreNeighborhood(exploration: Exploration[InsertPointMove]): Unit = {
    val seqValue = vrp.routes.defineCurrentValueAsCheckpoint()

    // Activates hot restart if needed
    val iterationScheme =
      if (hotRestart) HotRestart(nodesToInsert(), pivot)
      else nodesToInsert()

    // Removes symmetries from considered values
    val iterationSchemeSymmetryFree = nodesSymmetryClass match {
      case None    => iterationScheme
      case Some(s) => IdenticalAggregator.removeIdenticalClassesLazily(iterationScheme, s)
    }

    // How to iterate over selected values
    val (valueToInsertIterator, stopValue) =
      selectNodeBehavior.toIterator(iterationSchemeSymmetryFree)

    for (nodeToInsert <- valueToInsertIterator) {
      assert(
        vrp.isUnrouted(nodeToInsert),
        "The search zone should be restricted to unrouted nodes when inserting"
      )

      val (insertPointIterator, stopInsertPoint) =
        selectInsertionAfterPointBehavior.toIterator(relevantInsertAfterNode(nodeToInsert))

      for (insertPoint <- insertPointIterator) {
        val insertPointExplorer = seqValue.explorerAtAnyOccurrence(insertPoint)

        insertPointExplorer match {
          case None =>
            throw new Error("Trying to do an insertion after a value not in the sequence.")
          case Some(insertAfterExplorer) =>
            vrp.routes.insertAfterPosition(nodeToInsert, insertAfterExplorer) // Commits the move
            searchProfiler().foreach(x => x.neighborSelected())

            exploration.checkNeighborWP(objValue =>
              new InsertPointMove(
                vrp.routes,
                nodeToInsert,
                insertAfterExplorer,
                false,
                objValue,
                name
              )
            ) // Checks if the move is improving

            vrp.routes.rollbackToTopCheckpoint(Some(seqValue)) // Cancels the move

            if (exploration.toReturn != NoMoveFound) {
              stopValue()
              stopInsertPoint()
            }
        }
      }
    }

    vrp.routes.releaseTopCheckpoint()
    if (valueToInsertIterator.hasUnboundedNext) pivot = valueToInsertIterator.unboundedNext()
    else reset() // We tried all the values. The exploration stops and we can reset the pivot

  }

  override def doMove(move: InsertPointMove): Unit = move.commit()

  override def reset(): Unit = {
    // nodesToInsert() is potentially an unsorted collection of value. The pivot is
    // initialized with the first value from nodesToInsert() if it is non empty
    val toInsert = nodesToInsert()
    pivot = if (toInsert.nonEmpty) toInsert.head else 0
  }
}
