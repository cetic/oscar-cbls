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

/** Companion object of the [[InsertPointNeighborhoodInsertionPointFirst]] class. */
object InsertPointNeighborhoodInsertionPointFirst {

  /** Creates a InsertPointNeighborhoodInsertionPointFirst, which inserts a new value in a sequence.
    * It first selects the insertion point and then the value to insert
    *
    * @param vrp
    *   The routing problem to resolve with a search.
    * @param insertionAfterNodes
    *   The sequenced '''nodes''' after which we can insert new values.
    * @param relevantNodesToInsert
    *   The unrouted node to insert given the insertion point.
    * @param name
    *   The name of the neighborhood.
    * @param selectInsertionPointBehavior
    *   How to iterate over the sequenced values to find an insertion point.
    * @param selectInsertedNodeBehavior
    *   How to iterate over the unrouted nodes.
    * @param insertedNodesSymmetryClass
    *   A function that input the unrouted nodes and returns a symmetry class; only one of the
    *   inserted values in each class will be considered for insert. `Int.MinValue` is considered
    *   different to itself if you set to None this will not be used at all.
    * @param hotRestart
    *   Set the use of a [[oscar.cbls.algo.search.HotRestart]] mechanism.
    */
  def apply(
    vrp: VRP,
    insertionAfterNodes: () => Iterable[Int],
    relevantNodesToInsert: Int => Iterable[Int],
    name: String = "InsertPointNeighborhoodInsertionPointFirst",
    selectInsertionPointBehavior: LoopBehavior = LoopBehavior.first(),
    selectInsertedNodeBehavior: LoopBehavior = LoopBehavior.first(),
    insertedNodesSymmetryClass: Option[Int => Int] = None,
    hotRestart: Boolean = true
  ) = new InsertPointNeighborhoodInsertionPointFirst(
    vrp,
    insertionAfterNodes,
    relevantNodesToInsert,
    name,
    selectInsertionPointBehavior,
    selectInsertedNodeBehavior,
    insertedNodesSymmetryClass,
    hotRestart
  )
}

/** Neighborhood which inserts a new value in a sequence. It first selects the insertion point and
  * then the value to insert. The size of this neighborhood is `O(v * n)` where `v` is the number of
  * possible values to insert and `n` the size of the sequence. It can be cut down to `O(v * k)` by
  * using the relevant neighbors, and specifying `k` neighbors for each new value.
  *
  * @param vrp
  *   The routing problem to resolve with a search.
  * @param insertionAfterNode
  *   The sequenced '''nodes''' after which we can insert new values.
  * @param relevantNodesToInsert
  *   A function that, given a chosen insertion point, returns a list of relevant new values to
  *   insert after.
  * @param name
  *   The name of the neighborhood
  * @param selectInsertionPointBehavior
  *   How to iterate over the sequenced values to find an insertion point.
  * @param selectInsertedNodeBehavior
  *   How to iterate over the new variables.
  * @param insertedNodesSymmetryClass
  *   A function that input the inserted values and returns a symmetry class; only one of the
  *   inserted values in each class will be considered for insert. `Int.MinValue` is considered
  *   different to itself if you set to None this will not be used at all.
  * @param hotRestart
  *   Set the use of a [[oscar.cbls.algo.search.HotRestart]] mechanism.
  */
class InsertPointNeighborhoodInsertionPointFirst(
  vrp: VRP,
  insertionAfterNode: () => Iterable[Int],
  relevantNodesToInsert: Int => Iterable[Int],
  name: String,
  selectInsertionPointBehavior: LoopBehavior,
  selectInsertedNodeBehavior: LoopBehavior,
  insertedNodesSymmetryClass: Option[Int => Int],
  hotRestart: Boolean
) extends SimpleNeighborhood[InsertPointMove](name) {

  // Used for hot restart
  private[this] var pivot: Int = 0
  reset()

  override protected def exploreNeighborhood(exploration: Exploration[InsertPointMove]): Unit = {
    val seqValue = vrp.routes.defineCurrentValueAsCheckpoint()

    // Activates hot restart if needed
    val iterationScheme =
      if (hotRestart) HotRestart(insertionAfterNode(), pivot)
      else insertionAfterNode()

    // How to iterate over insertion points
    val (insertPointsIterator, stopInsertPoints) =
      selectInsertionPointBehavior.toIterator(iterationScheme)

    for (pointWhereToInsertAfter <- insertPointsIterator) {

      val insertPointExplorer = seqValue.explorerAtAnyOccurrence(pointWhereToInsertAfter)

      insertPointExplorer match {
        case None => throw new Error("Trying to do an insertion after a value not in the sequence.")
        case Some(insertAfterExplorer) =>
          // Removes symmetries from considered values
          val valuesSymmetryFree = insertedNodesSymmetryClass match {
            case None => relevantNodesToInsert(pointWhereToInsertAfter)
            case Some(s) =>
              IdenticalAggregator.removeIdenticalClassesLazily(
                relevantNodesToInsert(pointWhereToInsertAfter),
                s
              )
          }

          val (insertValuesIterator, stopInsertValue) =
            selectInsertedNodeBehavior.toIterator(valuesSymmetryFree)

          for (insertNode <- insertValuesIterator) {
            assert(
              vrp.isUnrouted(insertNode),
              "The search zone should be restricted to unrouted nodes when inserting"
            )

            vrp.routes.insertAfterPosition(insertNode, insertAfterExplorer) // Commits the move
            searchProfiler().foreach(x => x.neighborSelected())

            exploration.checkNeighborWP(objValue =>
              InsertPointMove(vrp.routes, insertNode, insertAfterExplorer, objValue, name)
            ) // Checks if the move is improving

            vrp.routes.rollbackToTopCheckpoint(Some(seqValue)) // Cancels the move

            if (exploration.toReturn != NoMoveFound) {
              stopInsertValue()
              stopInsertPoints()
            }
          }
      }
    }

    vrp.routes.releaseTopCheckpoint()
    if (insertPointsIterator.hasUnboundedNext) pivot = insertPointsIterator.unboundedNext()
    else reset() // We tried all the values. The exploration stops and we can reset the pivot
  }

  override def doMove(move: InsertPointMove): Unit = move.commit()

  override def reset(): Unit = {
    // insertionAfterPoints() is potentially an unsorted collection of value. The pivot is
    // initialized with the first value from insertionAfterPoints() if it is non empty
    val insertPoints = insertionAfterNode()
    pivot = if (insertPoints.nonEmpty) insertPoints.head else 0
  }
}
