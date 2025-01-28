package oscar.cbls.modeling.invariant

import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.lib.invariant.routing.TotalRouteLength
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
}
