package oscar.cbls.api

import oscar.cbls.core.search.loop.LoopBehavior
import oscar.cbls.lib.invariant.routing.RouteFlatMap
import oscar.cbls.lib.invariant.routing.timeWindows.TimeWindow
import oscar.cbls.lib.neighborhoods.routing._
import oscar.cbls.modeling.{Invariants => Inv, Neighborhoods => Nrs}

/** This object exposes the API for invariants and neighborhoods related to routing applications.
  */
object Routing {

  // *** Invariants *** //

  /** This method returns a derived variable expressing the total length of all routes over the
    * relevant vehicle routing structure, given the distance matrix associated to the nodes in the
    * graph.
    *
    * @param vrs
    *   The vehicle routing structure on which the total length is computed.
    * @param distanceMatrix
    *   Distance matrix, expressed as a function.
    * @return
    *   An [[IntVariable]] maintaining the value of the total route length.
    */
  def totalRouteLength(vrs: VRS, distanceMatrix: Int => Int => Long): IntVariable =
    Inv.routing.totalRouteLength(distanceMatrix)(vrs)

  /** This method returns a derived variable expressing the total length of all routes over the
    * relevant vehicle routing structure, given the distance matrix associated to the nodes in the
    * graph.
    *
    * @param vrs
    *   The vehicle routing structure on which the total length is computed.
    * @param distanceMatrix
    *   Distance matrix, expressed as an array of arrays of integer distances.
    * @return
    *   An [[IntVariable]] maintaining the value of the total route length.
    */
  def totalRouteLength(vrs: VRS, distanceMatrix: Array[Array[Long]]): IntVariable =
    Inv.routing.totalRouteLength(distanceMatrix)(vrs)

  /** This method returns a derived variable expressing the total length of all routes over the
    * relevant vehicle routing structure, given the distance matrix associated to the nodes in the
    * graph.
    *
    * @param vrs
    *   The vehicle routing structure on which the total length is computed.
    * @param distanceMatrix
    *   Distance matrix, expressed as a function.
    * @param matrixIsSymmetrical
    *   Flag expressing whether the distance matrix is symmetrical.
    * @return
    *   An [[IntVariable]] maintaining the value of the total route length.
    */
  def totalRouteLength(
    vrs: VRS,
    distanceMatrix: Int => Int => Long,
    matrixIsSymmetrical: Boolean
  ): IntVariable = Inv.routing.totalRouteLength(distanceMatrix, matrixIsSymmetrical)(vrs)

  /** This method returns a derived variable expressing the total length of all routes over the
    * relevant vehicle routing structure, given the distance matrix associated to the nodes in the
    * graph.
    *
    * @param vrs
    *   The vehicle routing structure on which the total length is computed.
    * @param distanceMatrix
    *   Distance matrix, expressed as an array of arrays of integer distances.
    * @param matrixIsSymmetrical
    *   Flag expressing whether the distance matrix is symmetrical.
    * @return
    *   An [[IntVariable]] maintaining the value of the total route length.
    */
  def totalRouteLength(
    vrs: VRS,
    distanceMatrix: Array[Array[Long]],
    matrixIsSymmetrical: Boolean
  ): IntVariable = Inv.routing.totalRouteLength(distanceMatrix, matrixIsSymmetrical)(vrs)

  /** This method returns an array of derived variables expressing the length of the route of each
    * vehicle over the relevant vehicle routing structure, given the distance matrix associated to
    * the nodes in the graph.
    *
    * @param vrs
    *   The vehicle routing structure on which the total lengths are computed.
    * @param distanceMatrix
    *   Distance matrix expressed as a function.
    * @return
    *   An array of [[IntVariable]] maintaining the values of the routes of each vehicle.
    */
  def routeLength(vrs: VRS, distanceMatrix: Int => Int => Long): Array[IntVariable] =
    Inv.routing.routeLength(distanceMatrix)(vrs)

  /** This method returns an array of derived variables expressing the length of the route of each
    * vehicle over the relevant vehicle routing structure, given the distance matrix associated to
    * the nodes in the graph.
    *
    * @param vrs
    *   The vehicle routing structure on which the total lengths are computed.
    * @param distanceMatrix
    *   Distance matrix, expressed as an array of arrays of integer distances.
    * @param matrixIsTriangular
    *   Flag expressing whether the distance matrix is triangular (see
    *   [[oscar.cbls.lib.invariant.routing.RouteLength]]).
    * @return
    *   An array of [[IntVariable]] maintaining the values of the routes of each vehicle.
    */
  def routeLength(
    vrs: VRS,
    distanceMatrix: Array[Array[Long]],
    matrixIsTriangular: Boolean = false
  ): Array[IntVariable] = Inv.routing.routeLength(distanceMatrix, matrixIsTriangular)(vrs)

  /** This method returns an array of derived variables expressing the number of nodes in the route
    * of each vehicle over the relevant vehicle routing structure.
    *
    * This method is based on
    * [[oscar.cbls.lib.invariant.routing.abstractGenericConstraint.GlobalConstraintCore]].
    *
    * @param vrs
    *   The vehicle routing structure on which this array is computed.
    * @return
    *   An array of [[IntVariable]] maintaining the number of nodes in the routes of each vehicle.
    */
  def nbNodes(vrs: VRS): Array[IntVariable] = Inv.routing.nbNodes(vrs)

  /** This method returns a RouteFlatMap invariant.
    *
    * @param vrs
    *   The vehicle routing structure on which this invariant is computed.
    * @param fun
    *   The function defining the mapping.
    * @return
    *   A RouteFlatMap invariant from which we can access to the fields:
    *   - `output`: the resulting [[SetVariable]] maintaining the result of the mapping.
    *   - `numDuplicates`: the resulting [[IntVariable]] maintaining the sum of the mapped values
    *     with an occurrence > 1.
    */
  def routeFlatMap(vrs: VRS, fun: (Int, Int) => Set[Int]): RouteFlatMap =
    Inv.routing.routeFlatMap(fun)(vrs)

  /** Returns an array of [[IntVariable]], one per vehicle, stating if there is a capacity violation
    * at some point in the vehicle's route. 0 means no violation, 1 means that there is a violation.
    *
    * There is a violation of the capacity if :
    *   - At some point the content of the vehicle is negative.
    *   - At some point the content of the vehicle exceed the maximal capacity of the vehicle.
    *
    * @param vrs
    *   the vehicle routing structure on which this array is computed.
    * @param vehiclesCapacity
    *   The maximal capacity of each vehicle.
    * @param contentVariationAtNode
    *   The variation of the content reaching a specific node or leaving from depot.
    * @param contentVariationBackAtDepot
    *   The (optional) variation of the content getting back to depot.
    * @param withLogReduction
    *   If true the log reduction algorithm will be activated.
    * @param withExtremesPC
    *   If true classical pre-computation will be applied for each pair of node starting at
    *   vehicle's depot and ending in the vehicle's route. And also for each pair of node starting
    *   at the end of the route and ending in the vehicle's route. (Useless without using log
    *   reduction as well)
    * @param name
    *   The (optional) name of the Invariant.
    */
  def capacityConstraint(
    vrs: VRS,
    vehiclesCapacity: Array[Long],
    contentVariationAtNode: Array[Long],
    contentVariationBackAtDepot: Option[Array[Long]] = None,
    withLogReduction: Boolean = false,
    withExtremesPC: Boolean = false,
    name: String = ""
  ): Array[IntVariable] = Inv.routing.capacityConstraint(
    vehiclesCapacity,
    contentVariationAtNode,
    contentVariationBackAtDepot,
    withLogReduction,
    withExtremesPC,
    name
  )(vrs)

  /** Returns an array of [[IntVariable]], one per vehicle, stating if there is a time windows
    * constraint violation at some point of the vehicles' routes. `0` means no violation, `1` means
    * that there is a violation.<br>
    *
    * There is a violation if the vehicle visits a node after its last permit arrival time.
    *
    * @param vrs
    *   The object that represents the vehicle routing structure.
    * @param timeFunction
    *   Function that gives the travel time between two nodes.
    * @param singleNodeTimeWindows
    *   For each node, associates a [[oscar.cbls.lib.invariant.routing.timeWindows.TimeWindow]].
    * @param withLogReduction
    *   If true the log reduction algorithm will be activated.
    * @param withExtremesPC
    *   If true classical pre-computation will be applied for each pair of node starting at
    *   vehicle's depot and ending in the vehicle's route. And also for each pair of node starting
    *   at the end of the route and ending in the vehicle's route. (Useless without using log
    *   reduction as well)
    * @param name
    *   The (optional) name of the Invariant.
    */
  def timeWindowsConstraint(
    vrs: VRS,
    timeFunction: Int => Int => Long,
    singleNodeTimeWindows: Array[TimeWindow],
    withLogReduction: Boolean = false,
    withExtremesPC: Boolean = false,
    name: String = "Time Windows Constraint"
  ): Array[IntVariable] = Inv.routing.timeWindowsConstraint(
    timeFunction,
    singleNodeTimeWindows,
    withLogReduction,
    withExtremesPC,
    name
  )(vrs)

  /** Returns an array of [[IntVariable]], one per vehicle, stating if there is a time windows
    * constraint violation at some point of the vehicles' routes. `0` means no violation, `1` means
    * that there is a violation.<br>
    *
    * There is a violation if the vehicle visits a node after its last permit arrival time.
    *
    * @param vrs
    *   The object that represents the vehicle routing structure.
    * @param timeMatrix
    *   Matrix that gives the travel time between two nodes.
    * @param singleNodeTimeWindows
    *   For each node, associates a [[oscar.cbls.lib.invariant.routing.timeWindows.TimeWindow]].
    * @param withLogReduction
    *   If true the log reduction algorithm will be activated.
    * @param withExtremesPC
    *   If true classical pre-computation will be applied for each pair of node starting at
    *   vehicle's depot and ending in the vehicle's route. And also for each pair of node starting
    *   at the end of the route and ending in the vehicle's route. (Useless without using log
    *   reduction as well)
    * @param name
    *   The (optional) name of the Invariant.
    */
  def timeWindowsConstraintFromMatrix(
    vrs: VRS,
    timeMatrix: Array[Array[Long]],
    singleNodeTimeWindows: Array[TimeWindow],
    withLogReduction: Boolean = false,
    withExtremesPC: Boolean = false,
    name: String = "Time Windows Constraint"
  ): Array[IntVariable] = Inv.routing.timeWindowsConstraintFromMatrix(
    timeMatrix,
    singleNodeTimeWindows,
    withLogReduction,
    withExtremesPC,
    name
  )(vrs)

  /** This method returns a derived variable expressing the number of violated precedences of all
    * routes over the relevant vehicle routing structure.<br>
    *
    * Given a list of pairs `(L, R)`, the precedence constraint is violated if:
    *   - `L` and `R` are routed but `L` is positioned after `R` ;
    *   - Or, `L` and `R` are not on the same vehicle.
    *   - Or, only one of these value is routed.
    *
    * Otherwise, the constraint is respected.
    *
    * @param vrs
    *   The object that represents the Vehicle Routing Problem.
    * @param precedences
    *   An array of pairs describing the precedences.
    * @param name
    *   The (optional) name of the Invariant.
    */
  def nodePrecedence(vrs: VRS, precedences: Array[(Int, Int)], name: String = ""): IntVariable =
    Inv.routing.nodePrecedence(precedences, name)(vrs)

  // *** Search ***// TODO

  /** Constructs an [[oscar.cbls.lib.neighborhoods.routing.InsertPointUnroutedFirst]] neighborhood
    * over a vehicle routing structure.
    *
    * @param vrs
    *   The vehicle routing structure on which this neighborhood is defined.
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
    * @return
    *   An instance of an [[oscar.cbls.lib.neighborhoods.routing.InsertPointUnroutedFirst]].
    */
  def insertPointUnroutedFirst(
    vrs: VRS,
    nodesToInsert: () => Iterable[Int],
    relevantInsertAfterNodes: Int => Iterable[Int],
    name: String = "InsertPtUnroutedFirst",
    selectNodeBehavior: LoopBehavior = LoopBehavior.first(),
    selectInsertionAfterPointBehavior: LoopBehavior = LoopBehavior.first(),
    nodesSymmetryClass: Option[Int => Int] = None,
    hotRestart: Boolean = true
  ): InsertPointUnroutedFirst = Nrs.routing.insertPointUnroutedFirst(
    nodesToInsert,
    relevantInsertAfterNodes,
    name,
    selectNodeBehavior,
    selectInsertionAfterPointBehavior,
    nodesSymmetryClass,
    hotRestart
  )(vrs)

  /** Constructs an [[oscar.cbls.lib.neighborhoods.routing.InsertPointInsertionPointFirst]]
    * neighborhood over a vehicle routing structure.
    *
    * @param vrs
    *   * The vehicle routing structure on which the neighborhood operates.
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
    * @return
    *   An instance of an [[oscar.cbls.lib.neighborhoods.routing.InsertPointInsertionPointFirst]].
    */
  def insertPointInsertionPointFirst(
    vrs: VRS,
    insertionAfterNodes: () => Iterable[Int],
    relevantNodesToInsert: Int => Iterable[Int],
    name: String = "InsertPtFirst",
    selectInsertionPointBehavior: LoopBehavior = LoopBehavior.first(),
    selectInsertedNodeBehavior: LoopBehavior = LoopBehavior.first(),
    insertedNodesSymmetryClass: Option[Int => Int] = None,
    hotRestart: Boolean = true
  ): InsertPointInsertionPointFirst = Nrs.routing.insertPointInsertionPointFirst(
    insertionAfterNodes,
    relevantNodesToInsert,
    name,
    selectInsertionPointBehavior,
    selectInsertedNodeBehavior,
    insertedNodesSymmetryClass,
    hotRestart
  )(vrs)

  /** Constructs a [[oscar.cbls.lib.neighborhoods.routing.OnePointMove]] neighborhood over a vehicle
    * routing structure.
    *
    * @param vrs
    *   The vehicle routing structure on which this array is computed.
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
    * @return
    *   An instance of a [[oscar.cbls.lib.neighborhoods.routing.OnePointMove]].
    */
  def onePointMove(
    vrs: VRS,
    nodesToMove: () => Iterable[Int],
    relevantDestinationNodes: Int => Iterable[Int],
    name: String = "1PtMove",
    selectNodeToMoveBehavior: LoopBehavior = LoopBehavior.first(),
    selectDestinationBehavior: LoopBehavior = LoopBehavior.first(),
    hotRestart: Boolean = true
  ): OnePointMove = Nrs.routing.onePointMove(
    nodesToMove,
    relevantDestinationNodes,
    name,
    selectNodeToMoveBehavior,
    selectDestinationBehavior,
    hotRestart
  )(vrs)

  /** Constructs a [[oscar.cbls.lib.neighborhoods.routing.RemovePoint]] neighborhood over a vehicle
    * routing structure.
    *
    * @param vrs
    *   The vehicle routing structure on which the neighborhood operates.
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
    * @return
    *   An instance of a [[oscar.cbls.lib.neighborhoods.routing.RemovePoint]].
    */
  def removePoint(
    vrs: VRS,
    relevantNodesToRemove: () => Iterable[Int],
    name: String = "RemovePt",
    selectNodesToRemoveBehavior: LoopBehavior = LoopBehavior.first(),
    nodesToRemoveSymmetryClass: Option[Int => Int] = None,
    hotRestart: Boolean = true
  ): RemovePoint = Nrs.routing.removePoint(
    relevantNodesToRemove,
    name,
    selectNodesToRemoveBehavior,
    nodesToRemoveSymmetryClass,
    hotRestart
  )(vrs)

  /** Constructs a [[oscar.cbls.lib.neighborhoods.routing.TwoOpt]] neighborhood over a vehicle
    * routing structure.
    *
    * @param vrs
    *   The vehicle routing structure on which the neighborhood operates.
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
    * @return
    *   An instance of a [[oscar.cbls.lib.neighborhoods.routing.TwoOpt]].
    */
  def twoOpt(
    vrs: VRS,
    relevantStartOfSegment: () => Iterable[Int],
    maxSegmentLength: Option[Int] = None,
    name: String = "2-Opt",
    selectFlippedSegmentBehavior: LoopBehavior = LoopBehavior.first(),
    hotRestart: Boolean = true
  ): TwoOpt = Nrs.routing.twoOpt(
    relevantStartOfSegment,
    maxSegmentLength,
    name,
    selectFlippedSegmentBehavior,
    hotRestart
  )(vrs)

  /** Constructs a [[oscar.cbls.lib.neighborhoods.routing.ThreeOptInsertionPointFirst]] neighborhood
    * over a vehicle routing structure.
    *
    * @param vrs
    *   The vehicle routing structure on which the neighborhood operates.
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
    * @return
    *   An instance of a [[oscar.cbls.lib.neighborhoods.routing.ThreeOptInsertionPointFirst]].
    */
  def threeOptInsertionPointFirst(
    vrs: VRS,
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
  ): ThreeOptInsertionPointFirst = Nrs.routing.threeOptInsertionPointFirst(
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
  )(vrs)

  /** Constructs a [[oscar.cbls.lib.neighborhoods.routing.ThreeOptMovedSegmentFirst]] neighborhood
    * over a vehicle routing structure.
    *
    * @param vrs
    *   The vehicle routing structure on which the neighborhood operates.
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
    * @return
    *   An instance of a [[oscar.cbls.lib.neighborhoods.routing.ThreeOptMovedSegmentFirst]].
    */
  def threeOptMovedSegmentFirst(
    vrs: VRS,
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
  ): ThreeOptMovedSegmentFirst = Nrs.routing.threeOptMovedSegmentFirst(
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
  )(vrs)

  /** Creates a [[oscar.cbls.lib.neighborhoods.routing.RandomizeRoutes]] neighborhood that generates
    * random routes.
    *
    * @param vrs
    *   The vehicle routing structure on which the neighborhood operates.
    * @param numNodeToInsert
    *   How many nodes (vehicles excluded) has to be routed in the generated routes.
    * @param numTriesWithoutImprovement.
    *   How many tries to find an improving random solution.
    * @param rng
    *   The random number generator used to generate a random valid route.
    * @return
    *   An instance of a [[oscar.cbls.lib.neighborhoods.routing.RandomizeRoutes]] that generates
    *   random routes.
    */
  def randomizeRoutes(
    vrs: VRS,
    numNodeToInsert: () => Int,
    numTriesWithoutImprovement: Int = 1,
    rng: scala.util.Random = scala.util.Random
  ): RandomizeRoutes =
    Nrs.routing.randomizeRoutes(numNodeToInsert, numTriesWithoutImprovement, rng)(vrs)

  /** Creates a [[oscar.cbls.lib.neighborhoods.routing.RandomizeRoutes]] neighborhood that shuffles
    * routed nodes.
    *
    * @param vrs
    *   The vehicle routing structure on which the neighborhood operates.
    * @param numTriesWithoutImprovement.
    *   How many tries to find an improving random solution.
    * @param rng
    *   The random number generator used to shuffle the routed nodes.
    * @return
    *   An instance of a [[oscar.cbls.lib.neighborhoods.routing.RandomizeRoutes]] that shuffles
    *   routed nodes.
    */
  def shuffleRoutes(
    vrs: VRS,
    numTriesWithoutImprovement: Int = 1,
    rng: scala.util.Random = scala.util.Random
  ): RandomizeRoutes = Nrs.routing.shuffleRoutes(numTriesWithoutImprovement, rng)(vrs)

  /** Creates a [[oscar.cbls.lib.neighborhoods.routing.RandomizeRoutes]] neighborhood that randomly
    * removes routed nodes.
    *
    * @param vrs
    *   The vehicle routing structure on which the neighborhood operates.
    * @param numNodesToRemove
    *   How many nodes has to be removed.
    * @param numTriesWithoutImprovement.
    *   How many tries to find an improving random solution.
    * @param rng
    *   The random number generator used to select nodes to remove.
    * @return
    *   An instance of a [[oscar.cbls.lib.neighborhoods.routing.RandomizeRoutes]] that randomly
    *   removes routed nodes.
    */
  def removeRandomRoutedNode(
    vrs: VRS,
    numNodesToRemove: () => Int,
    numTriesWithoutImprovement: Int = 1,
    rng: scala.util.Random = scala.util.Random
  ): RandomizeRoutes =
    Nrs.routing.removeRandomRoutedNode(numNodesToRemove, numTriesWithoutImprovement, rng)(vrs)

}
