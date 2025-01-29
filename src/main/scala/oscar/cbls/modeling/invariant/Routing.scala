package oscar.cbls.modeling.invariant

import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.lib.invariant.routing._
import oscar.cbls.lib.invariant.routing.capacityConstraint.GlobalCapacityConstraintWithLogReduction
import oscar.cbls.modeling.Model

/** This trait collects invariant methods normally used to express derived quantities related to a
  * routing problem context, such as total route length. The underlying model is generally required
  * to be set as a VRP.
  */
trait Routing {

  /** This method returns a derived variable expressing the total length of all routes over the
    * current VRP, given the distance matrix associated to the nodes in the graph.
    * @param distanceMatrix
    *   distance matrix, expressed as a function
    * @param model
    *   The underlying model of the problem, which must be set as a VRP.
    * @return
    *   An [[IntVariable]] maintaining the value of the total route length.
    */
  def totalRouteLength(distanceMatrix: Int => Int => Long)(implicit model: Model): IntVariable = {
    require(model.vrp.isDefined, "VRP not defined for this model")
    val trl = TotalRouteLength(model.vrp.get, distanceMatrix)
    trl.routeLength
  }

  /** This method returns a derived variable expressing the total length of all routes over the
    * current VRP, given the distance matrix associated to the nodes in the graph.
    * @param distanceMatrix
    *   Distance matrix, expressed as an array of arrays of integer distances.
    * @param model
    *   The underlying model of the problem, which must be set as a VRP.
    * @return
    *   An [[IntVariable]] maintaining the value of the total route length.
    */
  def totalRouteLength(distanceMatrix: Array[Array[Long]])(implicit model: Model): IntVariable = {
    require(model.vrp.isDefined, "VRP not defined for this model")
    val trl = TotalRouteLength(model.vrp.get, distanceMatrix)
    trl.routeLength
  }

  /** This method returns a derived variable expressing the total length of all routes over the
    * current VRP, given the distance matrix associated to the nodes in the graph.
    * @param distanceMatrix
    *   Distance matrix, expressed as a function.
    * @param model
    *   The underlying model of the problem, which must be set as a VRP.
    * @param matrixIsSymmetrical
    *   Flag expressing whether the distance matrix is symmetrical.
    * @return
    *   An [[IntVariable]] maintaining the value of the total route length.
    */
  def totalRouteLength(distanceMatrix: Int => Int => Long, matrixIsSymmetrical: Boolean)(implicit
    model: Model
  ): IntVariable = {
    require(model.vrp.isDefined, "VRP not defined for this model")
    val trl = TotalRouteLength(model.vrp.get, distanceMatrix, matrixIsSymmetrical)
    trl.routeLength
  }

  /** This method returns a derived variable expressing the total length of all routes over the
    * current VRP, given the distance matrix associated to the nodes in the graph.
    * @param distanceMatrix
    *   Distance matrix, expressed as an array of arrays of integer distances.
    * @param model
    *   The underlying model of the problem, which must be set as a VRP.
    * @param matrixIsSymmetrical
    *   Flag expressing whether the distance matrix is symmetrical.
    * @return
    *   An [[IntVariable]] maintaining the value of the total route length.
    */
  def totalRouteLength(distanceMatrix: Array[Array[Long]], matrixIsSymmetrical: Boolean)(implicit
    model: Model
  ): IntVariable = {
    require(model.vrp.isDefined, "VRP not defined for this model")
    val trl = TotalRouteLength(model.vrp.get, distanceMatrix, matrixIsSymmetrical)
    trl.routeLength
  }

  /** This method returns an array of derived variable expressing the length of the route of each
    * vehicle over the current VRP, given the distance matrix associated to the nodes in the graph.
    *
    * @param distanceMatrix
    *   Distance matrix expressed as a function.
    * @param model
    *   The underlying model of the problem, which must be set as a VRP.
    * @return
    *   An array of [[IntVariable]] maintaining the values of the routes of each vehicle.
    */
  def routeLength(distanceMatrix: Int => Int => Long)(implicit model: Model): Array[IntVariable] = {
    require(model.vrp.isDefined, "VRP not defined for this model")
    val rl = RouteLength(model.vrp.get, distanceMatrix)
    rl()
  }

  /** This method returns an array of derived variable expressing the length of the route of each
    * vehicle over the current VRP, given the distance matrix associated to the nodes in the graph.
    *
    * @param distanceMatrix
    *   Distance matrix, expressed as an array of arrays of integer distances.
    * @param matrixIsTriangular
    *   Flag expressing whether the distance matrix is triangular (see
    *   [[oscar.cbls.lib.invariant.routing.RouteLength]]).
    * @param model
    *   The underlying model of the problem, which must be set as a VRP.
    * @return
    *   An array of [[IntVariable]] maintaining the values of the routes of each vehicle.
    */
  def routeLength(distanceMatrix: Array[Array[Long]], matrixIsTriangular: Boolean = false)(implicit
    model: Model
  ): Array[IntVariable] = {
    require(model.vrp.isDefined, "VRP not defined for this model")
    val rl = RouteLength(model.vrp.get, distanceMatrix, matrixIsTriangular)
    rl()
  }

  /** This method returns an array of derived variable expressing the number of nodes int the route
    * of each vehicle over the current VRP.
    *
    * This method is based on
    * [[oscar.cbls.core.computation.genericConstraint.GlobalConstraintCore]].
    *
    * @param model
    *   The underlying model of the problem, which must be set as a VRP.
    * @return
    *   An array of [[IntVariable]] maintaining the number of nodes in the routes of each vehicle.
    */
  def nbNodes()(implicit model: Model): Array[IntVariable] = {
    require(model.vrp.isDefined, "VRP not defined for this model")
    val inv = NbNodes(model.vrp.get)
    inv()
  }

  /** This method returns an array of derived variable expressing the number of nodes int the route
    * of each vehicle over the current VRP.
    *
    * This method is based on
    * [[oscar.cbls.core.computation.genericConstraint.LogReducedGlobalConstraint]].
    *
    * @param model
    *   The underlying model of the problem, which must be set as a VRP.
    * @return
    *   An array of [[IntVariable]] maintaining the number of nodes in the routes of each vehicle.
    */
  def nbNodesLogReduced()(implicit model: Model): Array[IntVariable] = {
    require(model.vrp.isDefined, "VRP not defined for this model")
    val inv = NbNodesLogReduced(model.vrp.get)
    inv()
  }

  /** This method returns a RouteFlatMap invariant.
    *
    * @param fun
    *   The function defining the mapping.
    * @param model
    *   The underlying model of the problem, which must be set as a VRP.
    * @return
    *   A RouteFlatMap invariant from which we can access to the fields:
    *   - `output`: the resulting [[SetVariable]] maintaining the result of the mapping.
    *   - `numDuplicates`: the resulting [[IntVariable]] maintaining the sum of the mapped values
    *     with an occurrence > 1.
    */
  def routeFlatMap(fun: (Int, Int) => Set[Int])(implicit model: Model): RouteFlatMap = {
    require(model.vrp.isDefined, "VRP not defined for this model")
    RouteFlatMap(model.vrp.get, fun)
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
    * @param name
    *   The (optional) name of the Invariant.
    */
  def capacityConstraint(
    vehiclesCapacity: Array[Long],
    contentVariationAtNode: Array[Long],
    contentVariationBackAtDepot: Option[Array[Long]] = None,
    name: String = ""
  )(implicit model: Model): Array[IntVariable] = {
    require(model.vrp.isDefined, "VRP not defined for this model")
    val optName = if (name == "") None else Some(name)
    val capacityConstraint = GlobalCapacityConstraintWithLogReduction(
      model.vrp.get,
      vehiclesCapacity,
      contentVariationAtNode,
      contentVariationBackAtDepot,
      optName
    )
    capacityConstraint()
  }
}
