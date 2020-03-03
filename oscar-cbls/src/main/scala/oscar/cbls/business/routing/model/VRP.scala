package oscar.cbls.business.routing.model

/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/

import oscar.cbls._
import oscar.cbls.algo.search.KSmallest
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing._
import oscar.cbls.lib.invariant.seq.Content
import oscar.cbls.lib.invariant.set.Diff

import scala.collection.immutable.{List, SortedSet}

/**
 * The class constructor models a VRP problem with N points (deposits and customers)
 * and V vehicles.
 *
 * Vehicles are supposed to leave from their depot, and come back to it.
 * they all have a different depot (but yo ucan put them at the same place if you want)
 *
 * Info: after instantiation, each customer point is unrouted, and each vehicle loop on his deposit.
  *
  * @param n the number of points (deposits and customers) in the problem.
 * @param v the number of vehicles.
 * @param m the model.
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 */
class VRP(val m: Store, val n: Int, val v: Int, maxPivotPerValuePercent:Long = 4L) {

  val routes = new CBLSSeqVar(m, IntSequence(0L until v), n-1L, "routes", maxPivotPerValuePercent=maxPivotPerValuePercent)

  /**
   * the range of nodes (customers and deposits including) of the problem.
   */
  val nodes = 0L until n

  /**
   * the range vehicle of the problem.
   */
  val vehicles = 0L until v

  //TODO: renaud: enlever çà!
  val vehicleOfNode = vehicleOfNodes(routes.createClone(),v)

  val routed = Content(routes.createClone(50L)).setName("routed nodes")
  val unrouted = Diff(CBLSSetConst(SortedSet(nodes:_*)),routed).setName("unrouted nodes")

  /**
   * Returns if a given point is a depot.
    *
    * @param n the point queried.
   * @return true if the point is a depot, else false.
   */
  def isADepot(n:Long): Boolean = { n < v }

  def kFirst(k: Long, values:(Long) => Iterable[Long], filter: Long => Long => Boolean = _ => _ => true)(node: Long): Iterable[Long] = {
    if (k >= n - 1L) return values(node).filter(filter(node))

    KSmallest.kFirst(k: Long, values(node), filter(node))
  }

  /**
   * Returns if a given point is still routed.
    *
    * @param n the point queried.
   * @return true if the point is still routed, else false.
   */
  def isRouted(n:Long): Boolean = {routes.value.contains(n)}

  /**
    * Returns if a given point is still routed.
    *
    * @param n the point queried.
    * @return true if the point is not routed, else false.
    */
  def isUnrouted(n:Long): Boolean = {!routes.value.contains(n)}

  /**
   * This function is intended to be used for testing only.
   * setCircuit(List(1L,2L,3L,4L)) produces the following route :
   * 1L -> 2L -> 3L -> 4L (-> 1L)
   */
  def setCircuit(nodes: Iterable[Long]): Unit = {
    routes := IntSequence(nodes)
    for(v <- 0L until v) require(routes.value.contains(v))
  }

  def unroutedNodes:Iterable[Long] = nodes.filterNot(isRouted)

  def onSameVehicle()(node1:Long,node2:Long): Boolean = {
    vehicleOfNode(node1) == vehicleOfNode(node2) && isRouted(node1)
  }

  def onVehicle(vehicle:Long)(node:Long): Boolean={
    vehicleOfNode(node).value == vehicle
  }

  /**
    * Return the next node of the given node
    *   or n if the node isn't routed
    *   or None if the node is last of his route
    *
    * NOTE: if you'll use this method very often,
    *       you should maybe use the getNextNodeOfAllNodes method instead
    * @param node The node we want to get the next
    * @return the next node of the given node or None
    */
  def nextNodeOf(node: Long): Option[Long]={
    val routeExplorer = routes.value.explorerAtAnyOccurrence(node)
    if(routeExplorer.isDefined) {
      val nextNode = routeExplorer.get.next
      if(nextNode.isDefined && nextNode.get.value >= v)
        return Some(nextNode.get.value)
      else
        return None
    }
    return Some(n)
  }

  /**
    * the route of the vehicle, starting at the vehicle node, and not including the last vehicle node
    *
    * @param vehicle
    * @return
    */
  def getRouteOfVehicle(vehicle:Long):List[Long] = {
    require(vehicle < v, "asking route of vehicle:" + vehicle + " with v:" + v)
    var currentVExplorer = routes.value.explorerAtAnyOccurrence(vehicle).head.next
    var acc:List[Long] = List(vehicle)
    while (currentVExplorer match{
      case Some(x) if x.value >= v =>
        acc = x.value :: acc
        currentVExplorer = x.next
        true
      case _ => false}) {}
    acc.reverse
  }
  /*
  def getPrevNodeOfAllNodes: Array[Long] = {
    val it = routes.value.iterator
    val prevNodeOfNodes = Array.fill(n)(n)
    var prev = n
    while(it.hasNext){
      val node = it.next()
      if(prevNodeOfNodes
    }
  }*/

  /**
    * Compute the previous node of all nodes in the routes.
    * If the node isn't routed or is a depot, his previous is n.
    * @return
    */
  def getGlobalPrevNodeOfAllNodes: Array[Long] = {
    val it = routes.value.iterator
    val prevNodeOfNodes = Array.fill[Long](n)(n)
    var prev = n
    while(it.hasNext){
      val node = it.next()
      if(node >= v)
        prevNodeOfNodes(node) = prev
      prev = node
    }
    prevNodeOfNodes
  }

  /**
    * Compute the next node of all nodes in the routes.
    * If the node isn't routed his next is n.
    * If the node is the last of his route, his next node is the vehicle of his route
    * @return
    */
  def getGlobalNextNodeOfAllNodes: Array[Long] = {
    val it = routes.value.iterator
    val nextNodeOfNodes = Array.fill[Long](n)(n)
    var prev = it.next()
    while(it.hasNext){
      val node = it.next()
      if(node < v)
        nextNodeOfNodes(prev) = node-1L
      else
        nextNodeOfNodes(prev) = node
      prev = node
    }
    nextNodeOfNodes(prev) = v-1L
    nextNodeOfNodes
  }

  def getRoutePositionOfAllNode:Array[Int] = {
    def buildRoutePositionOfAllNode(it: Iterator[Long],currentPosition: Int, nodeToPosition: List[Int]): Array[Int] = {
      if(!it.hasNext)
        nodeToPosition.toArray
      else{
        val node = it.next()
        if(node < v)
          buildRoutePositionOfAllNode(it,0L,nodeToPosition ++ List(0))
        else
          buildRoutePositionOfAllNode(it,currentPosition+1L,nodeToPosition ++ List(currentPosition))

      }
    }
    val it = routes.value.iterator
    buildRoutePositionOfAllNode(it,0,List.empty)
  }

  def getGlobalRoutePositionOfAllNode:Array[Int] = {
    val it = routes.value.iterator
    val globalRoutePosition = Array.fill(n)(n)
    var inc = 0
    while(it.hasNext) {
      globalRoutePosition(it.next()) = inc
      inc += 1
    }
    globalRoutePosition
  }

  /**
    * Redefine the toString method.
    * @return the VRP problem as a String.
    */
  override def toString: String = {
    var toReturn = ""
    var notMoving:List[Long] = List.empty

    for (vehicle <- 0L until v) {
      val routeOfV = getRouteOfVehicle(vehicle)
      if(routeOfV.length == 1L){
        notMoving  = vehicle :: notMoving
      }else{
        toReturn +=  "vehicle " + vehicle + ": " +  routeOfV.mkString("->") + "->" + vehicle + "\n"
      }
    }
    val u = unroutedNodes
    "Vehicle routing n:" + n + " v:" + v + "\n" +
    "" + u.size + " unrouted nodes:{" + u.toList.mkString(",") + "}\n" +
    "" + notMoving.size + " not used vehicles:{" + notMoving.reverse.mkString(",") + "}\n" +
      toReturn
  }

  def stringOfVehicle(vehicle:Long):Option[String] = {
    val routeOfV = getRouteOfVehicle(vehicle)
    if(routeOfV.length == 1L){
      None
    }else{
      Some("" + routeOfV.mkString("->") + "->" + vehicle)
    }
  }
}
