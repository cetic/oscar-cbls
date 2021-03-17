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
package oscar.cbls.business.routing.modeling

import oscar.cbls.business.routing.invariants.capa.{ForwardCumulativeConstraintOnVehicle, ForwardCumulativeIntegerDimensionOnVehicle, ForwardCumulativeIntegerIntegerDimensionOnVehicle}
import oscar.cbls.core.computation.{CBLSIntVar, ChangingSeqValue, IntValue}

trait CapacityInvariants {

  /**
   * the violation maintained by this invariant is the sum over all routed nodes of the overshoot
   * strictly above cMax and the undershoot strictly below 0L of the content of the vehicles
   * @param routes
   * @param n The maximum number of nodes
   * @param v The number of vehicles
   * @param op A function which describes the capacity between two nodes : (startingNode,destinationNode,capacityAtStartingNode)=> capacityAtDestinationNode
   * @param cMax the maximal capacity of all vehicles (it is shared among all vehicles, so if you do not like it, you can use contentAtVehicleStart to make up for this)
   * @param contentAtVehicleStart the content of the vehicle at its start point
   * @param violation the violation that will be controlled by the invariant
   * @param maxCheckpointLevel the maximal level of checkpoints that this should support.
   *                           it consumes O(n) memory per level, so do not overdrive uselessly
   */
  def forwardCumulativeConstraintOnVehicle(routes:ChangingSeqValue,
                                           n:Int,
                                           v:Int,
                                           op :(Int,Int,Long)=>Long,
                                           cMax:Long,
                                           contentAtVehicleStart:Array[Long],
                                           violation:CBLSIntVar,
                                           maxCheckpointLevel:Int,
                                           capacityName:String = "capacity",
                                           fullDebug:Boolean = false):ForwardCumulativeConstraintOnVehicle =
    new ForwardCumulativeConstraintOnVehicle(routes:ChangingSeqValue,
      n,
      v,
      op,
      cMax,
      contentAtVehicleStart,
      violation,
      maxCheckpointLevel,
      capacityName,
      fullDebug)

  /**
   * ia generic invariant for representing a dimension on a vehicle, that is an integer value that travels with the vehicle and changes at each poit according to a function "op"
   * @param routes The sequence representing the route associated at each vehicle
   * @param n The maximum number of nodes
   * @param v The number of vehicles
   * @param op A function which returns the capacity change between two nodes : (fromNode,toNode,contentAtFromNode)=> contentAtToNode
   * @param contentAtStart Array of lenght = v where initValue(car) = content at start pos of vehicle #car
   * @param contentAtNode output: the content of the vehicle at each node (content at node 0 to v-1L is equal to contentAtStart)
   * @param contentAtEnd output: the content at the end of the route of each vehicle (that is whent hey come back to their departure point)
   * @param lastPointOfVehicle output: the last point of the vehicle before coming back to its departure point
   * @param defaultVehicleContentForUnroutedNodes is the content of a node that is not routed
   * @param contentName the name of this content, for debug purpose. it is atributed to all variales created by this invariant
   */
  def forwardCumulativeIntegerDimensionOnVehicle(routes:ChangingSeqValue,
                                                 n:Int,
                                                 v:Int,
                                                 op:(Int,Int,Long)=>Long,
                                                 contentAtStart:Array[IntValue],
                                                 contentAtNode:Array[CBLSIntVar],
                                                 contentAtEnd:Array[CBLSIntVar],
                                                 lastPointOfVehicle:Array[CBLSIntVar],
                                                 defaultVehicleContentForUnroutedNodes:Long,
                                                 contentName:String = "content")
  = new ForwardCumulativeIntegerDimensionOnVehicle(
    routes,
    n,
    v,
    op,
    contentAtStart,
    contentAtNode,
    contentAtEnd,
    lastPointOfVehicle,
    defaultVehicleContentForUnroutedNodes,
    contentName)
  val ForwardCumulativeIntegerDimensionOnVehicle = oscar.cbls.business.routing.invariants.capa.ForwardCumulativeIntegerDimensionOnVehicle

  /**
    * ia generic invariant for representing a dimension on a vehicle, that is an integer value that travels with the vehicle and changes at each poit according to a function "op"
    * @param routes The sequence representing the route associated at each vehicle
    * @param n The maximum number of nodes
    * @param v The number of vehicles
    * @param op A function which returns the capacity change between two nodes : (fromNode,toNode,content1AtFromNode,content2AtFromNode)=> (content1AtToNode,content2AtToNode)
    * @param content1AtStart Array of length = v where initValue(car) = content at start pos of vehicle #car
    * @param content2AtStart Array of length = v where initValue(car) = content at start pos of vehicle #car
    * @param default1ForUnroutedNodes is the content1 of a node that is not routed
    * @param default2ForUnroutedNodes is the content2 of a node that is not routed
    * @param minContent Min content of a node (used for creating the output variable, but not considered as a constraint)
    * @param maxContent Max content of a node (used for creating the output variable, but not considered as a constraint)
    * @param contentName the name of this content, for debug purpose. it is atributed to all variales created by this invariant
    */
  def forwardCumulativeIntegerIntegerDimensionOnVehicle(routes:ChangingSeqValue,
                                                        n:Int,
                                                        v:Int,
                                                        op:(Int,Int,Long,Long)=>(Long,Long),
                                                        content1AtStart:Array[IntValue],
                                                        content2AtStart:Array[IntValue],
                                                        default1ForUnroutedNodes:Long,
                                                        default2ForUnroutedNodes:Long,
                                                        minContent:Long = 0L,
                                                        maxContent:Long = Long.MaxValue,
                                                        contentName:String = "content")
  = ForwardCumulativeIntegerIntegerDimensionOnVehicle(
    routes,
    n,
    v,
    op,
    content1AtStart,
    content2AtStart,
    default1ForUnroutedNodes,
    default2ForUnroutedNodes,
    minContent,
    maxContent,
    contentName)
}
