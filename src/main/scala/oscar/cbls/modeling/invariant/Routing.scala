package oscar.cbls.modeling.invariant

import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.lib.invariant.routing._
import oscar.cbls.lib.invariant.routing.abstractGenericConstraint.GlobalConstraintCore
import oscar.cbls.lib.invariant.routing.capacityConstraint.GlobalCapacityConstraint
import oscar.cbls.modeling.routing.VRS

/** This trait collects invariant methods normally used to express derived quantities related to a
  * routing problem context, such as total route length.
  */
trait Routing {

  /** This method returns a derived variable expressing the total length of all routes over the
    * relevant vehicle routing structure, given the distance matrix associated to the nodes in the
    * graph.
    *
    * @param distanceMatrix
    *   Distance matrix, expressed as a function.
    * @param vrs
    *   The vehicle routing structure on which the total length is computed.
    * @return
    *   An [[IntVariable]] maintaining the value of the total route length.
    */
  def totalRouteLength(distanceMatrix: Int => Int => Long)(implicit vrs: VRS): IntVariable = {
    val trl = TotalRouteLength(vrs, distanceMatrix)
    trl.routeLength
  }

  /** This method returns a derived variable expressing the total length of all routes over the
    * relevant vehicle routing structure, given the distance matrix associated to the nodes in the
    * graph.
    *
    * @param distanceMatrix
    *   Distance matrix, expressed as an array of arrays of integer distances.
    * @param vrs
    *   The vehicle routing structure on which the total length is computed.
    * @return
    *   An [[IntVariable]] maintaining the value of the total route length.
    */
  def totalRouteLength(distanceMatrix: Array[Array[Long]])(implicit vrs: VRS): IntVariable = {
    val trl = TotalRouteLength(vrs, distanceMatrix)
    trl.routeLength
  }

  /** This method returns a derived variable expressing the total length of all routes over the
    * relevant vehicle routing structure, given the distance matrix associated to the nodes in the
    * graph.
    *
    * @param distanceMatrix
    *   Distance matrix, expressed as a function.
    * @param matrixIsSymmetrical
    *   Flag expressing whether the distance matrix is symmetrical.
    * @param vrs
    *   The vehicle routing structure on which the total length is computed.
    * @return
    *   An [[IntVariable]] maintaining the value of the total route length.
    */
  def totalRouteLength(distanceMatrix: Int => Int => Long, matrixIsSymmetrical: Boolean)(implicit
    vrs: VRS
  ): IntVariable = {
    val trl = TotalRouteLength(vrs, distanceMatrix, matrixIsSymmetrical)
    trl.routeLength
  }

  /** This method returns a derived variable expressing the total length of all routes over the
    * relevant vehicle routing structure, given the distance matrix associated to the nodes in the
    * graph.
    *
    * @param distanceMatrix
    *   Distance matrix, expressed as an array of arrays of integer distances.
    * @param matrixIsSymmetrical
    *   Flag expressing whether the distance matrix is symmetrical.
    * @param vrs
    *   The vehicle routing structure on which the total length is computed.
    * @return
    *   An [[IntVariable]] maintaining the value of the total route length.
    */
  def totalRouteLength(distanceMatrix: Array[Array[Long]], matrixIsSymmetrical: Boolean)(implicit
    vrs: VRS
  ): IntVariable = {
    val trl = TotalRouteLength(vrs, distanceMatrix, matrixIsSymmetrical)
    trl.routeLength
  }

  /** This method returns an array of derived variables expressing the length of the route of each
    * vehicle over the relevant vehicle routing structure, given the distance matrix associated to
    * the nodes in the graph.
    *
    * @param distanceMatrix
    *   Distance matrix expressed as a function.
    * @param vrs
    *   The vehicle routing structure on which the total lengths are computed.
    * @return
    *   An array of [[IntVariable]] maintaining the values of the routes of each vehicle.
    */
  def routeLength(distanceMatrix: Int => Int => Long)(implicit vrs: VRS): Array[IntVariable] = {
    val rl = RouteLength(vrs, distanceMatrix)
    rl()
  }

  /** This method returns an array of derived variables expressing the length of the route of each
    * vehicle over the relevant vehicle routing structure, given the distance matrix associated to
    * the nodes in the graph.
    *
    * @param distanceMatrix
    *   Distance matrix, expressed as an array of arrays of integer distances.
    * @param matrixIsTriangular
    *   Flag expressing whether the distance matrix is triangular (see
    *   [[oscar.cbls.lib.invariant.routing.RouteLength]]).
    * @param vrs
    *   The vehicle routing structure on which the total lengths are computed.
    * @return
    *   An array of [[IntVariable]] maintaining the values of the routes of each vehicle.
    */
  def routeLength(distanceMatrix: Array[Array[Long]], matrixIsTriangular: Boolean = false)(implicit
    vrs: VRS
  ): Array[IntVariable] = {
    val rl = RouteLength(vrs, distanceMatrix, matrixIsTriangular)
    rl()
  }

  /** This method returns an array of derived variables expressing the number of nodes in the route
    * of each vehicle over the relevant vehicle routing structure.
    *
    * This method is based on [[GlobalConstraintCore]].
    *
    * @param vrs
    *   The vehicle routing structure on which this array is computed.
    * @return
    *   An array of [[IntVariable]] maintaining the number of nodes in the routes of each vehicle.
    */
  def nbNodes(implicit vrs: VRS): Array[IntVariable] = {
    val inv = NbNodes(vrs)
    inv()
  }

  /** This method returns a RouteFlatMap invariant.
    *
    * @param fun
    *   The function defining the mapping.
    * @param vrs
    *   The vehicle routing structure on which this invariant is computed.
    * @return
    *   A RouteFlatMap invariant from which we can access to the fields:
    *   - `output`: the resulting [[SetVariable]] maintaining the result of the mapping.
    *   - `numDuplicates`: the resulting [[IntVariable]] maintaining the sum of the mapped values
    *     with an occurrence > 1.
    */
  def routeFlatMap(fun: (Int, Int) => Set[Int])(implicit vrs: VRS): RouteFlatMap = {
    RouteFlatMap(vrs, fun)
  }

  /** Returns an array of [[IntVariable]], one per vehicle, stating if there is a capacity violation
    * at some point in the vehicle's route. 0 means no violation, 1 means that there is a violation.
    *
    * There is a violation of the capacity if :
    *   - At some point the content of the vehicle is negative.
    *   - At some point the content of the vehicle exceed the maximal capacity of the vehicle.
    *
    * @param vehiclesCapacity
    *   The maximal capacity of each vehicle.
    * @param contentVariationAtNode
    *   The variation of the content reaching a specific node or leaving from depot.
    * @param contentVariationBackAtDepot
    *   The (optional) variation of the content getting back to depot.
   * @param withLogReduction
   *   If true the log reduction algorithm will be activated.
   * @param withExtremesPC
   *   If true classical pre-computation will be applied for each pair of node starting at vehicle's
   *   depot and ending in the vehicle's route. And also for each pair of node starting at the end of
   *   the route and ending in the vehicle's route. (Useless without using log reduction as well)
    * @param name
    *   The (optional) name of the Invariant.
    * @param vrs
    *   the vehicle routing structure on which this array is computed.
    */
  def capacityConstraint(
    vehiclesCapacity: Array[Long],
    contentVariationAtNode: Array[Long],
    contentVariationBackAtDepot: Option[Array[Long]] = None,
    withLogReduction: Boolean = false,
    withExtremesPC: Boolean = false,
    name: String = ""
  )(implicit vrs: VRS): Array[IntVariable] = {
    val capacityConstraint = GlobalCapacityConstraint(
      vrs,
      vehiclesCapacity,
      contentVariationAtNode,
      contentVariationBackAtDepot,
      withLogReduction,
      withExtremesPC,
      name
    )
    capacityConstraint()
  }
}
