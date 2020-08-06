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
package oscar.cbls.business.routing.invariants.capa

import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.core.computation.{CBLSIntVar, ChangingIntValue, ChangingSeqValue, Domain, DomainRange, IntNotificationTarget, IntValue}
import oscar.cbls.core.propagation.Checker

/**
 * Created by  Jannou Brohée on 3/10/16.
 */

object ForwardCumulativeIntegerDimensionOnVehicle {
  /**
   * creates a GenericCumulativeIntegerDimensionOnVehicle Invariant
   * @param routes The sequence representing the route associated at each vehicle
   * @param n The maximum number of nodes
   * @param v The number of vehicles
   * @param op A function which returns the capacity change between two nodes : (fromNode,toNode,contentAtFromNode)=> contentAtToNode
   * @param contentAtStart Array of lenght = v where initValue(car) = content at start pos of vehicle #car
   * @param defaultForUnroutedNodes is the content of a node that is not routed
   * @param minContent Min content of a node (used for creating the output variable, but not considered as a constraint)
   * @param maxContent Max content of a node (used for creating the output variable, but not considered as a constraint)
   * @param contentName the name of this content, for debug purpose. it is atributed to all variales created by this invariant
   * @return (contentAtNode,contentAtEnd,lastPointOfVehicle) content at node, of vehice nodes (0..v-1) is the content on vehicle start. content on vehicle end is in contentAtEnd.
   */
  def apply(routes:ChangingSeqValue,
            n:Int,
            v:Int,
            op:(Int,Int,Long)=>Long,
            contentAtStart:Array[IntValue],
            defaultForUnroutedNodes:Long,
            minContent:Long = 0L,
            maxContent:Long = Long.MaxValue,
            contentName:String = "content",fullDebug:Boolean = false):(Array[CBLSIntVar],Array[CBLSIntVar],Array[CBLSIntVar],ForwardCumulativeIntegerDimensionOnVehicle) ={
    val contentAtNode = Array.tabulate(n)((node: Int) => CBLSIntVar(routes.model, 0L, Domain.coupleToDomain(minContent,maxContent).union(defaultForUnroutedNodes), contentName + " at node "+node))
    val contentAtEnd = Array.tabulate(v)((vehicle: Int) => CBLSIntVar(routes.model, 0L, Domain.coupleToDomain(minContent,maxContent), contentName + " at end of route " + vehicle))
    val lastPointOfVehicle = Array.tabulate(v)((vehicle: Int) => CBLSIntVar(routes.model, 0L, DomainRange(0L,n), s"last point of vehicle $vehicle"))

    val invariant = new ForwardCumulativeIntegerDimensionOnVehicle(routes,n,v,op,contentAtStart,contentAtNode,contentAtEnd,lastPointOfVehicle,defaultForUnroutedNodes,contentName,fullDebug)
    (contentAtNode,contentAtEnd,lastPointOfVehicle,invariant)
  }
}

/**
 * ia generic invariant for representing a dimension on a vehicle, that is an integer value that travels with the vehicle and changes at each poit according to a function "op"
 * @param routes The sequence representing the route associated at each vehicle
 * @param n The maximum number of nodes
 * @param v The number of vehicles
 * @param op A function which returns the capacity change between two nodes : (fromNode,toNode,contentAtFromNode)=> contentAtToNode
 * @param contentAtStart Array of lenght = v where initValue(car) = content at start pos of vehicle #car
 * @param contentAtNode output: the content of the vehicle at each node (content at node 0 to v-1 is equal to contentAtStart)
 * @param contentAtEnd output: the content at the end of the route of each vehicle (that is whent hey come back to their departure point)
 * @param lastPointOfVehicle output: the last point of the vehicle before coming back to its departure point
 * @param defaultVehicleContentForUnroutedNodes is the content of a node that is not routed
 * @param contentName the name of this content, for debug purpose. it is atributed to all variales created by this invariant
 * @param fullDebug to activate some internal debug stuff (do not use)
 */
class ForwardCumulativeIntegerDimensionOnVehicle(routes:ChangingSeqValue,
                                                 n:Int,
                                                 v:Int,
                                                 op:(Int,Int,Long)=>Long,
                                                 contentAtStart:Array[IntValue],
                                                 val contentAtNode:Array[CBLSIntVar],
                                                 val contentAtEnd:Array[CBLSIntVar],
                                                 val lastPointOfVehicle:Array[CBLSIntVar],
                                                 defaultVehicleContentForUnroutedNodes:Long,
                                                 val contentName:String = "content",
                                                 fullDebug:Boolean = false)
  extends AbstractForwardCumulativeDimensionOnVehicle(routes,n,v,fullDebug) with IntNotificationTarget{

  registerStaticAndDynamicDependency(routes)
  registerStaticAndDynamicDependencyArrayIndex(contentAtStart)
  finishInitialization()
  for(i <- contentAtNode) i.setDefiningInvariant(this)
  for(i <- contentAtEnd) i.setDefiningInvariant(this)
  for(i <- lastPointOfVehicle) i.setDefiningInvariant(this)

  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Long, NewVal: Long): Unit ={
    toUpdateZonesAndVehicleStartAfter match {
      case None => ;
      case Some((toUpdateZones,vehicleLocation)) =>
        toUpdateZonesAndVehicleStartAfter = Some((toUpdateZones.insert(id, smartPrepend(0,0,toUpdateZones.getOrElse(id, List.empty[(Int,Int)]))),vehicleLocation))
    }
    scheduleForPropagation()
  }

  /**
   * @param vehicle
   * @return true if changed, false otherwise
   */
  override def setVehicleContentAtStart(vehicle : Int) : Boolean = {
    val newValue = contentAtStart(vehicle).value
    val oldValue = contentAtNode(vehicle).newValue
    if(oldValue == newValue){
      false
    }else{
      contentAtNode(vehicle) := newValue
      true
    }
  }

  /**
   * @param prevNode
   * @param node
   * @return true if changed, false otherwise
   */
  override def setVehicleContentAtNode(prevNode : Int, node : Int) : Boolean = {
    val oldValue = contentAtNode(node).newValue
    val newValue = op(prevNode,node,contentAtNode(prevNode).newValue)  //(fromNode,toNode,contentAtFromNode)=> contentAtToNode
    if(oldValue != newValue) {
      contentAtNode(node) := newValue
      true
    }else{
      false
    }
  }

  override def setVehicleContentAtEnd(vehicle : Int, lastNode : Int): Unit ={
    lastPointOfVehicle(vehicle) := lastNode
    contentAtEnd(vehicle) := op(lastNode,vehicle,contentAtNode(lastNode).newValue)
  }

  override def setNodesUnrouted(unroutedNodes : Iterable[Int]): Unit ={
    for(node <- unroutedNodes)
      contentAtNode(node) := defaultVehicleContentForUnroutedNodes
  }

  override def toString : String = {
    s"""ForwardCumulativeIntegerDimensionOnVehicle(routes:${routes.name} n:$n v:$v contentName:$contentName){
       |${(0 until v).toList.map((vehicle:Int) =>
    {
      val header = s"\tvehicle$vehicle contentAtStart:${contentAtStart(vehicle).value}\n"
      var explorerOpt = routes.value.explorerAtAnyOccurrence(vehicle).get.next
      var acc:String = ""

      while(explorerOpt match{
        case None => //at end of last vehicle
          val vehicle = v-1
          acc += s"\t\tendOfRoute of vehicle$vehicle contentAtEnd:${contentAtEnd(vehicle).value}\n"
          false
        case Some(explorer) if explorer.value < v =>
          //reached another vehicle
          val vehicle = explorer.value-1
          acc += s"\t\tendOfRoute of vehicle$vehicle contentAtEnd:${contentAtEnd(vehicle).value}\n"
          false
        case Some(explorer) if explorer.value >= v =>
          val node = explorer.value
          acc += "\t\tnode:" + node + "\t" + " content:" + contentAtNode(node).value + "\n"
          explorerOpt = explorer.next
          true
      }){}
      header+acc}).mkString("")}
       |""".stripMargin
  }

  override def checkInternals(c : Checker) : Unit = {
    check(c,routes.value)
  }

  def check(c : Checker,s:IntSequence): Unit ={
    val (nodeToContent,vehicleToContentAtEnd,vehicleLocation) =
      AbstractVehicleCapacity.computeNodeToContentAndVehicleContentAtEndAndVehicleStartPositionsFromScratch(n,v,op,v => contentAtStart(v).value,s,defaultVehicleContentForUnroutedNodes)
    val currentVehicleLocation = this.toUpdateZonesAndVehicleStartAfter.get._2

    for(vehicle <- 0 until v){
      c.check(vehicleLocation.startPosOfVehicle(vehicle) == s.positionOfAnyOccurrence(vehicle).get,
        Some(s"Found start of vehicle($vehicle):=${vehicleLocation.startPosOfVehicle(vehicle)} should be :=${s.positionOfAnyOccurrence(vehicle)} seq :${s.mkString(",")}"))
      c.check(currentVehicleLocation.startPosOfVehicle(vehicle) == vehicleLocation.startPosOfVehicle(vehicle),Some("x"))
    }

    for(node <- 0 until n){
      c.check(nodeToContent(node) == contentAtNode(node).newValue,
        Some(s"Vehicle content at node($node) at pos : ${s.positionsOfValue(node)} := ${contentAtNode(node).newValue} should be :=${nodeToContent(node)} routes:${s.mkString(",")} contentAtStart:${contentAtStart.mkString(",")}"))
    }

    for(vehicle <- 0 until v){
      c.check(contentAtEnd(vehicle).newValue == vehicleToContentAtEnd(vehicle),
        Some(s"Error on vehicle content at end vehicle:$vehicle contentAtEnd(vehicle).newValue:${contentAtEnd(vehicle).newValue} should be:${vehicleToContentAtEnd(vehicle)}"))
    }
  }
}
