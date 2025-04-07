package oscar.cbls.modeling.search

import oscar.cbls.core.search.loop.LoopBehavior
import oscar.cbls.lib.neighborhoods.routing._
import oscar.cbls.modeling.routing.VRS

/** This trait collects methods used to define search neighborhoods for a search procedure over a
  * vehicle routing structure.
  */
trait Routing {

  /** Constructs an [[oscar.cbls.lib.neighborhoods.routing.InsertPointUnroutedFirst]] neighborhood
    * over a vehicle routing structure.
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
    * @param vrs
    *   The vehicle routing structure on which this neighborhood is defined.
    * @return
    *   An instance of an [[oscar.cbls.lib.neighborhoods.routing.InsertPointUnroutedFirst]].
    */
  def insertPointUnroutedFirst(
    nodesToInsert: () => Iterable[Int],
    relevantInsertAfterNodes: Int => Iterable[Int],
    name: String = "InsertPtUnroutedFirst",
    selectNodeBehavior: LoopBehavior = LoopBehavior.first(),
    selectInsertionAfterPointBehavior: LoopBehavior = LoopBehavior.first(),
    nodesSymmetryClass: Option[Int => Int] = None,
    hotRestart: Boolean = true
  )(implicit vrs: VRS): InsertPointUnroutedFirst = {
    InsertPointUnroutedFirst(
      vrs,
      nodesToInsert,
      relevantInsertAfterNodes,
      name,
      selectNodeBehavior,
      selectInsertionAfterPointBehavior,
      nodesSymmetryClass,
      hotRestart
    )
  }

  /** Constructs an [[oscar.cbls.lib.neighborhoods.routing.InsertPointInsertionPointFirst]]
    * neighborhood over a vehicle routing structure.
    *
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
    * @param vrs
    *   The vehicle routing structure on which the neighborhood operates.
    * @return
    *   An instance of an [[oscar.cbls.lib.neighborhoods.routing.InsertPointInsertionPointFirst]].
    */
  def insertPointInsertionPointFirst(
    insertionAfterNodes: () => Iterable[Int],
    relevantNodesToInsert: Int => Iterable[Int],
    name: String = "InsertPtFirst",
    selectInsertionPointBehavior: LoopBehavior = LoopBehavior.first(),
    selectInsertedNodeBehavior: LoopBehavior = LoopBehavior.first(),
    insertedNodesSymmetryClass: Option[Int => Int] = None,
    hotRestart: Boolean = true
  )(implicit vrs: VRS): InsertPointInsertionPointFirst = {
    InsertPointInsertionPointFirst(
      vrs,
      insertionAfterNodes,
      relevantNodesToInsert,
      name,
      selectInsertionPointBehavior,
      selectInsertedNodeBehavior,
      insertedNodesSymmetryClass,
      hotRestart
    )
  }

  /** Constructs a [[oscar.cbls.lib.neighborhoods.routing.OnePointMove]] neighborhood over a vehicle
    * routing structure.
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
    * @param vrs
    *   The vehicle routing structure on which this array is computed.
    * @return
    *   An instance of a [[oscar.cbls.lib.neighborhoods.routing.OnePointMove]].
    */
  def onePointMove(
    nodesToMove: () => Iterable[Int],
    relevantDestinationNodes: Int => Iterable[Int],
    name: String = "1PtMove",
    selectNodeToMoveBehavior: LoopBehavior = LoopBehavior.first(),
    selectDestinationBehavior: LoopBehavior = LoopBehavior.first(),
    hotRestart: Boolean = true
  )(implicit vrs: VRS): OnePointMove = {
    OnePointMove(
      vrs,
      nodesToMove,
      relevantDestinationNodes,
      name,
      selectNodeToMoveBehavior,
      selectDestinationBehavior,
      hotRestart
    )
  }

  /** Constructs a [[oscar.cbls.lib.neighborhoods.routing.RemovePoint]] neighborhood over a vehicle
    * routing structure.
    *
    * @param relevantNodesToRemove
    *   The nodes in the sequence that can be removed.
    * @param name
    *   The name of the neighborhood.
    * @param selectNodesToRemoveBehavior
    *   How to iterate over the nodes that can be removed.
    * @param nodesToRemoveSymmetryClass
    *   An optional function that takes nodes as input and returns their symmetry class. If
    *   provided, only one of the nodes in each class will be considered for removal. Note that
    *   `Int.MinValue` is considered different to itself.
    * @param hotRestart
    *   Whether to use a [[oscar.cbls.algo.search.HotRestart]] mechanism.
    * @param vrs
    *   The vehicle routing structure on which the neighborhood operates.
    * @return
    *   An instance of a [[oscar.cbls.lib.neighborhoods.routing.RemovePoint]].
    */
  def removePoint(
    relevantNodesToRemove: () => Iterable[Int],
    name: String = "RemovePt",
    selectNodesToRemoveBehavior: LoopBehavior = LoopBehavior.first(),
    nodesToRemoveSymmetryClass: Option[Int => Int] = None,
    hotRestart: Boolean = true
  )(implicit vrs: VRS): RemovePoint = {
    RemovePoint(
      vrs,
      relevantNodesToRemove,
      name,
      selectNodesToRemoveBehavior,
      nodesToRemoveSymmetryClass,
      hotRestart
    )
  }

  /** Constructs a [[oscar.cbls.lib.neighborhoods.routing.TwoOpt]] neighborhood over a vehicle
    * routing structure.
    *
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
    * @param vrs
    *   The vehicle routing structure on which the neighborhood operates.
    * @return
    *   An instance of a [[oscar.cbls.lib.neighborhoods.routing.TwoOpt]].
    */
  def twoOpt(
    relevantStartOfSegment: () => Iterable[Int],
    maxSegmentLength: Option[Int] = None,
    name: String = "2-Opt",
    selectFlippedSegmentBehavior: LoopBehavior = LoopBehavior.first(),
    hotRestart: Boolean = true
  )(implicit vrs: VRS): TwoOpt = {
    TwoOpt(
      vrs,
      relevantStartOfSegment,
      maxSegmentLength,
      name,
      selectFlippedSegmentBehavior,
      hotRestart
    )
  }

  /** Constructs a [[oscar.cbls.lib.neighborhoods.routing.ThreeOptInsertionPointFirst]] neighborhood
    * over a vehicle routing structure.
    *
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
    * @param vrs
    *   The vehicle routing structure on which the neighborhood operates.
    * @return
    *   An instance of a [[oscar.cbls.lib.neighborhoods.routing.ThreeOptInsertionPointFirst]].
    */
  def threeOptInsertionPointFirst(
    insertionPoints: () => Iterable[Int],
    startOfMovedSegment: Int => Iterable[Int],
    maxLengthOfMovedSegment: Int,
    name: String = "3-Opt (insert pt first)",
    tryFlip: Boolean = true,
    selectInsertPointBehavior: LoopBehavior = LoopBehavior.first(),
    selectMovedSegmentBehavior: LoopBehavior = LoopBehavior.first(),
    selectFlipBehavior: LoopBehavior = LoopBehavior.best(),
    skipOnePointMove: Boolean = false,
    breakSymmetry: Boolean = true,
    hotRestart: Boolean = true
  )(implicit vrs: VRS): ThreeOptInsertionPointFirst = {
    ThreeOpt.insertionPointFirst(
      vrs,
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

  /** Constructs a [[oscar.cbls.lib.neighborhoods.routing.ThreeOptMovedSegmentFirst]] neighborhood
    * over a vehicle routing structure.
    *
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
    * @param vrs
    *   The vehicle routing structure on which the neighborhood operates.
    * @return
    *   An instance of a [[oscar.cbls.lib.neighborhoods.routing.ThreeOptMovedSegmentFirst]].
    */
  def threeOptMovedSegmentFirst(
    startOfMovedSegment: () => Iterable[Int],
    insertionPoints: Int => Iterable[Int],
    maxLengthOfMovedSegment: Int,
    name: String = "3-Opt (moved segment first)",
    tryFlip: Boolean = true,
    selectMovedSegmentBehavior: LoopBehavior = LoopBehavior.first(),
    selectInsertPointBehavior: LoopBehavior = LoopBehavior.first(),
    selectFlipBehavior: LoopBehavior = LoopBehavior.best(),
    skipOnePointMove: Boolean = false,
    breakSymmetry: Boolean = true,
    hotRestart: Boolean = true
  )(implicit vrs: VRS): ThreeOptMovedSegmentFirst = {
    ThreeOpt.movedSegmentFirst(
      vrs,
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

  /** Creates a [[oscar.cbls.lib.neighborhoods.routing.RandomizeRoutes]] neighborhood that generates
    * random routes.
    *
    * @param numNodeToInsert
    *   How many nodes (vehicles excluded) has to be routed in the generated routes.
    * @param numTriesWithoutImprovement.
    *   How many tries to find an improving random solution.
    * @param rng
    *   The random number generator used to generate a random valid route.
    * @param vrs
    *   The vehicle routing structure on which the neighborhood operates.
    * @return
    *   An instance of a [[oscar.cbls.lib.neighborhoods.routing.RandomizeRoutes]] that generates
    *   random routes.
    */
  def randomizeRoutes(
    numNodeToInsert: () => Int,
    numTriesWithoutImprovement: Int = 1,
    rng: scala.util.Random = scala.util.Random
  )(implicit vrs: VRS): RandomizeRoutes =
    RandomizeRoutes.randomize(vrs, numNodeToInsert, numTriesWithoutImprovement, rng)

  /** Creates a [[oscar.cbls.lib.neighborhoods.routing.RandomizeRoutes]] neighborhood that shuffles
    * routed nodes.
    *
    * @param numTriesWithoutImprovement.
    *   How many tries to find an improving random solution.
    * @param rng
    *   The random number generator used to shuffle the routed nodes.
    * @param vrs
    *   The vehicle routing structure on which the neighborhood operates.
    * @return
    *   An instance of a [[oscar.cbls.lib.neighborhoods.routing.RandomizeRoutes]] that shuffles
    *   routed nodes.
    */
  def shuffleRoutes(
    numTriesWithoutImprovement: Int = 1,
    rng: scala.util.Random = scala.util.Random
  )(implicit vrs: VRS): RandomizeRoutes =
    RandomizeRoutes.shuffle(vrs, numTriesWithoutImprovement, rng)

  /** Creates a [[oscar.cbls.lib.neighborhoods.routing.RandomizeRoutes]] neighborhood that randomly
    * removes routed nodes.
    *
    * @param numNodesToRemove
    *   How many nodes has to be removed.
    * @param numTriesWithoutImprovement.
    *   How many tries to find an improving random solution.
    * @param rng
    *   The random number generator used to select nodes to remove.
    * @param vrs
    *   The vehicle routing structure on which the neighborhood operates.
    * @return
    *   An instance of a [[oscar.cbls.lib.neighborhoods.routing.RandomizeRoutes]] that randomly
    *   removes routed nodes.
    */
  def removeRandomRoutedNode(
    numNodesToRemove: () => Int,
    numTriesWithoutImprovement: Int = 1,
    rng: scala.util.Random = scala.util.Random
  )(implicit vrs: VRS): RandomizeRoutes =
    RandomizeRoutes.removeNodes(vrs, numNodesToRemove, numTriesWithoutImprovement, rng)

}
