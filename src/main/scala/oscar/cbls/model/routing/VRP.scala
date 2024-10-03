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

package oscar.cbls.model.routing

import oscar.cbls.algo.search.KSmallest
import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer, RootIntSequenceExplorer}
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.seq.SeqVariable
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.lib.invariant.routing.RoutingConventionConstraint
import oscar.cbls.lib.invariant.seq.Content
import oscar.cbls.lib.invariant.set.Diff

import scala.collection.immutable.HashMap
import scala.collection.mutable

/** Companion object of the [[VRP]] class. */
object VRP {

  /** Builds a VRP
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which the
    *   [[oscar.cbls.core.computation.seq.SeqVariable]] modeling the routes is attached.
    * @param n
    *   The number of points (deposits and customers) in the problem.
    * @param v
    *   The number of vehicles in the problem.
    * @param maxPivotPerValuePercent
    *   The maximum number of [[oscar.cbls.algo.sequence.affineFunction.Pivot]] per 100 value in the
    *   IntSequence. When defining a new checkpoint, if this value is exceeded, a regularization is
    *   done.
    * @param debug
    *   If the debug mode is activated or not.
    * @return
    *   An instance of the VRP with `n` points (depots and customers) and `v` vehicles.
    */
  def apply(
    model: Store,
    n: Int,
    v: Int,
    maxPivotPerValuePercent: Int = 4,
    debug: Boolean = false
  ): VRP =
    new VRP(model, n, v, maxPivotPerValuePercent, debug)
}

/** Models a VRP with `n` points (depots and customers) and `v` vehicles.
  *
  * Vehicles are supposed to leave from their depot, and come back to it. In the model, each vehicle
  * has its own depot, even if it is the same physical point.
  *
  * In our routing convention, the nodes are numbered from 0 to n - 1 and vehicles are numbered from
  * 0 to v - 1 (with v <= n). In the sequence, the vehicle nodes are ordered from the smallest
  * vehicle id to the biggest vehicle id. Each node can appear only once in the sequence. The nodes
  * between two vehicle nodes represent the route of the vehicle associated to the first node.
  *
  * For example, Sequence IntSequence(0,4,3,1,2,6) with v = 2 and n = 7 means that
  *   - there are 7 nodes identified from 0 to 6;
  *   - there are 2 vehicles identified by 0 and 1;
  *   - Vehicle 0 visits node 4 and 3 in that order and come back to its depot;
  *   - Vehicle 1 visits node 2 and 6 and come back to its depot;
  *   - Node 5 is not routed
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which the
  *   [[oscar.cbls.core.computation.seq.SeqVariable]] modeling the routes is attached.
  * @param n
  *   The number of points (deposits and customers) in the problem.
  * @param v
  *   The number of vehicles in the problem.
  * @param maxPivotPerValuePercent
  *   The maximum number of [[oscar.cbls.algo.sequence.affineFunction.Pivot]] per 100 value in the
  *   IntSequence. When defining a new checkpoint, if this value is exceeded, a regularization is
  *   done.
  * @param debug
  *   If the debug mode is activated or not.
  */
class VRP(val model: Store, val n: Int, val v: Int, maxPivotPerValuePercent: Int, debug: Boolean) {

  require(v >= 1, "A VRP should have at least one vehicle")
  require(
    v <= n,
    s"The number of vehicle (v: $v) must be lesser or equal than the number of nodes (n: $n)."
  )

  val routes: SeqVariable = SeqVariable(
    model,
    List.from(0 until v),
    name = "Routes",
    maxPivotPerValuePercent = this.maxPivotPerValuePercent
  )

  /** The range of nodes (customers and deposits including) of the problem. */
  val nodes: Range = 0 until n

  /** The range vehicle of the problem. */
  val vehicles: Range = 0 until v

  /** Set which maintains all the routed nodes. */
  val routed: SetVariable = SetVariable(model, Set.empty[Int], name = Some("Routed nodes"))
  Content(model, routes, routed)

  /** Set which maintains all the unrouted nodes. */
  val unrouted: SetVariable = SetVariable(model, Set.empty[Int], name = Some("Unrouted nodes"))
  Diff(
    model,
    SetVariable(model, Set.from(nodes)),
    routed,
    unrouted,
    name = Some("Unrouted nodes computation invariant")
  )

  private[this] val routingConventionConstraint: Option[RoutingConventionConstraint] =
    if (debug) Some(RoutingConventionConstraint(model, this)) else None

  /** Returns if a given node is a depot or not. */
  def isDepot(node: Int): Boolean = node < v

  /** Returns the `k` first values of `values(node)` satisfying `filter(node)`. */
  def kFirst(k: Int, values: Int => Iterable[Int], filter: Int => Int => Boolean = _ => _ => true)(
    node: Int
  ): Iterable[Int] = {
    if (k >= n - 1) return values(node).filter(filter(node))

    KSmallest.getKFirst(k, values(node), filter(node))
  }

  /** Returns if the input node is routed or not. */
  def isRouted(node: Int): Boolean = routed.pendingValue.contains(node)

  /** Returns if the input node is unrouted or not. */
  def isUnrouted(node: Int): Boolean = unrouted.pendingValue.contains(node)

  /** Assigns the input nodes to the routes and checks if the new route contains all the vehicles
    * and respects the routing convention constraints in debug mode. '''NOTE:''' This function is
    * intended to be used only for testing.
    */
  def setCircuit(nodes: Iterable[Int]): Unit = {
    routes := IntSequence(nodes)
    for (vehicle <- 0 until v) {
      require(
        routes.pendingValue.contains(vehicle),
        "The current route do not contain all the expected vehicle"
      )
    }
    if (debug) routingConventionConstraint.get.checkVehicleOrder(routes.pendingValue)
  }

  /** Returns all the unrouted nodes as an iterable. */
  def unroutedNodes: Iterable[Int] = unrouted.pendingValue

  /** Given an explorer, returns the next node in the route taking routing conventions into account.
    *
    * This means that if the next node is a vehicle id or if it's the end of the sequence, the next
    * node should be the id of the previous vehicle
    *
    * @param exp
    *   The given explorer
    * @return
    *   The id of the next node
    */
  def nextNodeInRouting(exp: IntSequenceExplorer): Int = {
    exp.next match {
      case _: RootIntSequenceExplorer => v - 1
      case exp: IntSequenceExplorer =>
        if (exp.value < v)
          exp.value - 1 max 0
        else
          exp.value
    }
  }

  /** Returns the node that stands after [node] in the route
    *
    * '''Note:''' If you will use this method very often, you should maybe use the
    * [[nextNodeOfAllNodes]] method instead.
    *
    * @param node
    *   The node to get the successor.
    * @return
    *   - The successor of the input node if it is routed
    *   - `n` if the input node is unrouted
    *   - `None` if the input node is the last of the sequence
    */
  def nextNodeOf(node: Int): Option[Int] = {
    val routesExplorer = routes.pendingValue.explorerAtAnyOccurrence(node)
    routesExplorer match {
      case Some(explorer) =>
        val nextNode = explorer.next
        nextNode match {
          case _: RootIntSequenceExplorer => None
          case _                          => Some(nextNode.value)
        }
      case None => Some(n)
    }
  }

  /** Returns the route of the input vehicle, not including the return to the depot.
    *
    * @param vehicle
    *   The vehicle for which the route is required
    * @return
    *   The list of node that are in the vehicle route
    */
  def routeOfVehicle(vehicle: Int): List[Int] = {
    require(
      vehicle < v,
      s"Asking route for a node (node $vehicle) that is not a vehicle (number of vehicle = $v)"
    )
    var currentVehicleExplorer = routes.pendingValue.explorerAtAnyOccurrence(vehicle).get.next
    var toReturn: List[Int]    = List(vehicle)
    while (
      currentVehicleExplorer match {
        case _: RootIntSequenceExplorer => false
        case explorer: IntSequenceExplorer if explorer.value >= v =>
          true
        case _ => false
      }
    ) {
      toReturn = currentVehicleExplorer.value :: toReturn
      currentVehicleExplorer = currentVehicleExplorer.next
    }

    toReturn.reverse
  }

  /** Returns an HashMap which maps each vehicle to its route. */
  def mapVehicleToRoute: HashMap[Int, List[Int]] =
    HashMap.from(vehicles.map((vehicle: Int) => vehicle -> routeOfVehicle(vehicle)))

  /** For each node, returns its predecessor in the route. If the node is not routed or is a depot,
    * its predecessor is `n`.
    */
  def previousNodeOfAllNodes: Array[Int] = {
    val prevNodeOfNodes: Array[Int] = Array.fill(n)(n)
    var prev: Int                   = n

    for (node: Int <- routes.pendingValue) {
      if (node >= v) prevNodeOfNodes(node) = prev
      prev = node
    }
    prevNodeOfNodes
  }

  /** Returns an array that, for each node, contains its successor in the route. If the node is
    * unrouted or if it is a vehicle, its successor is `n`. If the node is the last of its route,
    * its successor is the vehicle of its route.
    */
  def nextNodeOfAllNodes: Array[Int] = {
    val it: Iterator[IntSequenceExplorer] = routes.pendingValue.iterator
    val nextNodeOfNodes: Array[Int]       = Array.fill(n)(n)
    var prev: Int                         = it.next().value

    while (it.hasNext) {
      val node: Int = it.next().value
      if (node < v) nextNodeOfNodes(prev) = node - 1
      else nextNodeOfNodes(prev) = node
      prev = node
    }
    nextNodeOfNodes(prev) = v - 1
    nextNodeOfNodes
  }

  /** Returns an array that for each node, contains its position in the routes' sequence. The array
    * contains `n` if the node is unrouted
    */
  def routesPositionOfAllNodes: Array[Int] = {
    val routesPos: Array[Int] = Array.fill(n)(n)
    var i: Int                = 0
    for (node <- routes.pendingValue) {
      routesPos(node) = i
      i += 1
    }
    routesPos
  }

  /** Returns all the moving vehicles in routes. */
  def movingVehicles: Iterable[Int] = {
    var toReturn: List[Int] = List.empty
    for (vehicle <- v - 1 to 0 by -1) {
      val explorer = routes.pendingValue.explorerAtAnyOccurrence(vehicle).get
      if (explorer.next.value >= v) toReturn = vehicle :: toReturn
    }
    toReturn
  }

  /** Returns a string representing the current route of the input vehicle. */
  def stringOfVehicle(vehicle: Int): Option[String] = {
    val currentRoute: List[Int] = routeOfVehicle(vehicle)
    if (currentRoute.length == 1) None
    else Some(s"${currentRoute.mkString(" -> ")} -> $vehicle")
  }

  override def toString: String = {
    var vehiclesStr: String           = ""
    val notMoving: mutable.Queue[Int] = mutable.Queue.empty

    for (vehicle <- 0 until v) {
      val currentRoute: List[Int] = routeOfVehicle(vehicle)
      if (currentRoute.length == 1) notMoving += vehicle
      else {
        vehiclesStr += s"vehicle $vehicle (nbNodes: ${currentRoute.size - 1}): " +
          s"${currentRoute.mkString(" -> ")} -> $vehicle\n"
      }
    }

    s"""
       |VRP n: $n v: $v
       |${unrouted.value().size} unrouted nodes: $unrouted
       |${notMoving.size} not used vehicle: {${notMoving.mkString(", ")}}
       |$vehiclesStr
       |""".stripMargin
  }
}
