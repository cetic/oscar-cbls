package oscar.cbls.modeling.search

import oscar.cbls.core.search.loop.LoopBehavior
import oscar.cbls.lib.neighborhoods.routing.{
  InsertPointNeighborhoodUnroutedFirst,
  OnePointMoveNeighborhood
}
import oscar.cbls.modeling.Model

/** This trait collects methods used to define search neighborhoods for a search procedure over a
  * model set for a vehicle routing problem.
  */
trait Routing {

  /** Constructs a [[oscar.cbls.lib.neighborhoods.routing.InsertPointNeighborhoodUnroutedFirst]]
    * neighborhood over a model with a defined VRP.
    *
    * @param nodesToInsert
    *   The nodes this neighborhood will try to insert.
    * @param relevantInsertAfterNodes
    *   A function that, given a value to insert, returns a list of candidate sequenced nodes such
    *   that insertion after each node will be evaluated.
    * @param name
    *   The name of the neighborhood.
    * @param selectNodeBehavior
    *   How to iterate over the new variables to insert.
    * @param selectInsertionAfterPointBehavior
    *   How to iterate over the sequenced values to find an insertion point.
    * @param nodesSymmetryClass
    *   A function that returns the symmetry class of an unrouted node, denoted by an integer. Only
    *   one node in each class will be considered for insertion. Note that nodes in class
    *   `Int.MinValue` are always considered. If set to None, this feature is not used
    * @param hotRestart
    *   Whether this neighborhood uses of a [[oscar.cbls.algo.search.HotRestart]] mechanism.
    * @param model
    *   Underlying model, which must be set as a VRP.
    * @return
    *   An instance of an
    *   [[oscar.cbls.lib.neighborhoods.routing.InsertPointNeighborhoodUnroutedFirst]].
    */
  def insertPointUnroutedFirst(
    nodesToInsert: () => Iterable[Int],
    relevantInsertAfterNodes: Int => Iterable[Int],
    name: String = "InsertPointNeighborhoodUnroutedFirst",
    selectNodeBehavior: LoopBehavior = LoopBehavior.first(),
    selectInsertionAfterPointBehavior: LoopBehavior = LoopBehavior.first(),
    nodesSymmetryClass: Option[Int => Int] = None,
    hotRestart: Boolean = true
  )(implicit model: Model): InsertPointNeighborhoodUnroutedFirst = {
    require(model.vrp.isDefined, "VRP not defined for this model")
    InsertPointNeighborhoodUnroutedFirst(
      model.vrp.get,
      nodesToInsert,
      relevantInsertAfterNodes,
      name,
      selectNodeBehavior,
      selectInsertionAfterPointBehavior,
      nodesSymmetryClass,
      hotRestart
    )
  }

  /** Constructs a [[oscar.cbls.lib.neighborhoods.routing.OnePointMoveNeighborhood]] over a model
    * with a defined VRP.
    *
    * @param nodesToMove
    *   The nodes this neighborhood can move.
    * @param relevantDestinationNodes
    *   A function which returns, for each node to move, a list of relevant destinations nodes after
    *   which to move the node.
    * @param name
    *   The name of this neighborhood.
    * @param selectNodeToMoveBehavior
    *   How to iterate over the nodes to move.
    * @param selectDestinationBehavior
    *   How to iterate over the destination points.
    * @param hotRestart
    *   Whether this neighborhood uses of a [[oscar.cbls.algo.search.HotRestart]] mechanism.
    * @param model
    *   Underlying model, which must be set as a VRP.
    * @return
    *   An instance of a [[oscar.cbls.lib.neighborhoods.routing.OnePointMoveNeighborhood]].
    */
  def onePointMove(
    nodesToMove: () => Iterable[Int],
    relevantDestinationNodes: Int => Iterable[Int],
    name: String = "OnePointMoveNeighborhood",
    selectNodeToMoveBehavior: LoopBehavior = LoopBehavior.first(),
    selectDestinationBehavior: LoopBehavior = LoopBehavior.first(),
    hotRestart: Boolean = true
  )(implicit model: Model): OnePointMoveNeighborhood = {
    require(model.vrp.isDefined, "VRP not defined for this model")
    OnePointMoveNeighborhood(
      model.vrp.get,
      nodesToMove,
      relevantDestinationNodes,
      name,
      selectNodeToMoveBehavior,
      selectDestinationBehavior,
      hotRestart
    )
  }
}
