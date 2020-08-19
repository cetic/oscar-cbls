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

object ForwardCumulativeIntegerIntegerDimensionOnVehicleWithFirstAndContentAtFirst {
  /**
    * creates a GenericCumulativeIntegerDimensionOnVehicle Invariant
    * @param routes The sequence representing the route associated at each vehicle
    * @param n The maximum number of nodes
    * @param v The number of vehicles
    * @param op A function which returns the capacity change between two nodes : (fromNode,toNode,content1AtFromNode,content2AtFromNode)=> (content1AtToNode,content2AtToNode)
    * @param content1AtStart Array of lenght = v where initValue(car) = content at start pos of vehicle #car
    * @param content2AtStart Array of lenght = v where initValue(car) = content at start pos of vehicle #car
    * @param default1ForUnroutedNodes is the content1 of a node that is not routed
    * @param default2ForUnroutedNodes is the content2 of a node that is not routed
    * @param minContent Min content of a node (used for creating the output variable, but not considered as a constraint)
    * @param maxContent Max content of a node (used for creating the output variable, but not considered as a constraint)
    * @param contentName the name of this content, for debug purpose. it is atributed to all variales created by this invairant
    * @return  (content1AtNode,content2AtNode,content1AtEnd,content2AtEnd,lastPointOfVehicle) content at end is the content when back at the vehicle start
    */
  def apply(routes:ChangingSeqValue,
            n:Int,
            v:Int,
            op:(Int,Int,Long,Long)=>(Long,Long),
            content1AtStart:Array[IntValue],
            content2AtStart:Array[IntValue],
            default1ForUnroutedNodes:Long,
            default2ForUnroutedNodes:Long,
            minContent:Long = 0L,
            maxContent:Long = Long.MaxValue,
            contentName:String = "content"): ForwardCumulativeIntegerIntegerDimensionOnVehicle ={


    val content1FirstNode = Array.tabulate(v)((vehicle: Int) => CBLSIntVar(routes.model, 0L, Domain.coupleToDomain(minContent,maxContent), contentName + "1 at first node after start of of vehicle " + vehicle))
    val content2AtFirstNode = Array.tabulate(v)((vehicle: Int) => CBLSIntVar(routes.model, 0L, Domain.coupleToDomain(minContent,maxContent), contentName + "2 at first node after start of vehicle " + vehicle))
    val firstPointOfVehicle = Array.tabulate(v)((vehicle: Int) => CBLSIntVar(routes.model, 0L, DomainRange(0L,n), "next(" + vehicle + ")"))

    val content1AtNode = Array.tabulate(n)((node: Int) => CBLSIntVar(routes.model, 0L, Domain.coupleToDomain(minContent,maxContent).union(default1ForUnroutedNodes), contentName + "1 at node "+node))
    val content2AtNode = Array.tabulate(n)((node: Int) => CBLSIntVar(routes.model, 0L, Domain.coupleToDomain(minContent,maxContent).union(default2ForUnroutedNodes), contentName + "2 at node "+node))

    val content1AtEnd = Array.tabulate(v)((vehicle: Int) => CBLSIntVar(routes.model, 0L, Domain.coupleToDomain(minContent,maxContent), contentName + "1 at end of route " + vehicle))
    val content2AtEnd = Array.tabulate(v)((vehicle: Int) => CBLSIntVar(routes.model, 0L, Domain.coupleToDomain(minContent,maxContent), contentName + "2 at end of route " + vehicle))

    val lastPointOfVehicle = Array.tabulate(v)((vehicle: Int) => CBLSIntVar(routes.model, 0L, DomainRange(0L,n), "last point of vehicle" + vehicle))

    new ForwardCumulativeIntegerIntegerDimensionOnVehicleWithFirstAndContentAtFirst(routes,n,v,op,
      content1AtStart,content2AtStart,
      content1FirstNode,content2AtFirstNode,firstPointOfVehicle,
      content1AtNode,content2AtNode,
      content1AtEnd,content2AtEnd,
      lastPointOfVehicle,
      default1ForUnroutedNodes,default2ForUnroutedNodes,
      contentName)
  }
}

class ForwardCumulativeIntegerIntegerDimensionOnVehicleWithFirstAndContentAtFirst(routes:ChangingSeqValue,
                                                                                  n:Int,
                                                                                  v:Int,
                                                                                  op:(Int,Int,Long,Long)=>(Long,Long),
                                                                                  override val content1AtStart:Array[IntValue],
                                                                                  override val content2AtStart:Array[IntValue],

                                                                                  val content1AtFirstNode:Array[CBLSIntVar],
                                                                                  val content2AtFirstNode:Array[CBLSIntVar],
                                                                                  val firstPointOfVehicle:Array[CBLSIntVar],

                                                                                  override val content1AtNode:Array[CBLSIntVar],
                                                                                  override val content2AtNode:Array[CBLSIntVar],

                                                                                  override val content1AtEnd:Array[CBLSIntVar],
                                                                                  override val content2AtEnd:Array[CBLSIntVar],
                                                                                  override val lastPointOfVehicle:Array[CBLSIntVar],
                                                                                  defaultVehicleContent1ForUnroutedNodes:Long,
                                                                                  defaultVehicleContent2ForUnroutedNodes:Long,
                                                                                  contentName:String = "content")
  extends ForwardCumulativeIntegerIntegerDimensionOnVehicle(routes:ChangingSeqValue,
    n:Int,
    v:Int,
    op:(Int,Int,Long,Long)=>(Long,Long),
    content1AtStart:Array[IntValue],
    content2AtStart:Array[IntValue],
    content1AtNode:Array[CBLSIntVar],
    content2AtNode:Array[CBLSIntVar],
    content1AtEnd:Array[CBLSIntVar],
    content2AtEnd:Array[CBLSIntVar],
    lastPointOfVehicle:Array[CBLSIntVar],
    defaultVehicleContent1ForUnroutedNodes:Long,
    defaultVehicleContent2ForUnroutedNodes:Long,
    contentName:String){

  for(i <- content1AtFirstNode) i.setDefiningInvariant(this)
  for(i <- content2AtFirstNode) i.setDefiningInvariant(this)
  for(i <- firstPointOfVehicle) i.setDefiningInvariant(this)

  override def setVehicleContentAtNode(prevNode : Int, node : Int): Boolean = {
    val toReturn = super.setVehicleContentAtNode(prevNode, node)
    if (prevNode < v) {
      //updating the first node of vehicle prevNode

      content1AtFirstNode(prevNode) := content1AtNode(node).newValue
      content2AtFirstNode(prevNode) := content2AtNode(node).newValue
      firstPointOfVehicle(prevNode) := node
    }
    toReturn
  }
}

object ForwardCumulativeIntegerIntegerDimensionOnVehicle {
  /**
    * creates a GenericCumulativeIntegerDimensionOnVehicle Invariant
    * @param routes The sequence representing the route associated at each vehicle
    * @param n The maximum number of nodes
    * @param v The number of vehicles
    * @param op A function which returns the capacity change between two nodes : (fromNode,toNode,content1AtFromNode,content2AtFromNode)=> (content1AtToNode,content2AtToNode)
    * @param content1AtStart Array of lenght = v where initValue(car) = content at start pos of vehicle #car
    * @param content2AtStart Array of lenght = v where initValue(car) = content at start pos of vehicle #car
    * @param default1ForUnroutedNodes is the content1 of a node that is not routed
    * @param default2ForUnroutedNodes is the content2 of a node that is not routed
    * @param minContent Min content of a node (used for creating the output variable, but not considered as a constraint)
    * @param maxContent Max content of a node (used for creating the output variable, but not considered as a constraint)
    * @param contentName the name of this content, for debug purpose. it is atributed to all variales created by this invairant
    * @return  (content1AtNode,content2AtNode,content1AtEnd,content2AtEnd,lastPointOfVehicle) content at end is the content when back at the vehicle start
    */
  def apply(routes:ChangingSeqValue,
            n:Int,
            v:Int,
            op:(Int,Int,Long,Long)=>(Long,Long),
            content1AtStart:Array[IntValue],
            content2AtStart:Array[IntValue],
            default1ForUnroutedNodes:Long,
            default2ForUnroutedNodes:Long,
            minContent:Long = 0L,
            maxContent:Long = Long.MaxValue,
            contentName:String = "content"): ForwardCumulativeIntegerIntegerDimensionOnVehicle ={

    val content1AtNode = Array.tabulate(n)((node: Int) => CBLSIntVar(routes.model, 0L, Domain.coupleToDomain(minContent,maxContent).union(default1ForUnroutedNodes), contentName + "1 at node "+node))
    val content2AtNode = Array.tabulate(n)((node: Int) => CBLSIntVar(routes.model, 0L, Domain.coupleToDomain(minContent,maxContent).union(default2ForUnroutedNodes), contentName + "2 at node "+node))

    val content1AtEnd = Array.tabulate(v)((vehicle: Int) => CBLSIntVar(routes.model, 0L, Domain.coupleToDomain(minContent,maxContent), contentName + "1 at end of route " + vehicle))
    val content2AtEnd = Array.tabulate(v)((vehicle: Int) => CBLSIntVar(routes.model, 0L, Domain.coupleToDomain(minContent,maxContent), contentName + "2 at end of route " + vehicle))

    val lastPointOfVehicle = Array.tabulate(v)((vehicle: Int) => CBLSIntVar(routes.model, 0L, DomainRange(0L,n), "last point of vehicle" + vehicle))

    new ForwardCumulativeIntegerIntegerDimensionOnVehicle(routes,n,v,op,
      content1AtStart,content2AtStart,
      content1AtNode,content2AtNode,
      content1AtEnd,content2AtEnd,
      lastPointOfVehicle,
      default1ForUnroutedNodes,default2ForUnroutedNodes,
      contentName)
  }
}

class ForwardCumulativeIntegerIntegerDimensionOnVehicle(routes:ChangingSeqValue,
                                                        n:Int,
                                                        v:Int,
                                                        op:(Int,Int,Long,Long)=>(Long,Long),
                                                        val content1AtStart:Array[IntValue],
                                                        val content2AtStart:Array[IntValue],
                                                        val content1AtNode:Array[CBLSIntVar],
                                                        val content2AtNode:Array[CBLSIntVar],
                                                        val content1AtEnd:Array[CBLSIntVar],
                                                        val content2AtEnd:Array[CBLSIntVar],
                                                        val lastPointOfVehicle:Array[CBLSIntVar],
                                                        defaultVehicleContent1ForUnroutedNodes:Long,
                                                        defaultVehicleContent2ForUnroutedNodes:Long,
                                                        contentName:String = "content")

  extends AbstractForwardCumulativeDimensionOnVehicle(routes,n,v) with IntNotificationTarget{

  registerStaticAndDynamicDependency(routes)
  registerStaticAndDynamicDependencyArrayIndex(content1AtStart)
  registerStaticAndDynamicDependencyArrayIndex(content2AtStart)
  finishInitialization()
  for(i <- content1AtNode) i.setDefiningInvariant(this)
  for(i <- content2AtNode) i.setDefiningInvariant(this)
  for(i <- content1AtEnd) i.setDefiningInvariant(this)
  for(i <- content2AtEnd) i.setDefiningInvariant(this)
  for(i <- lastPointOfVehicle) i.setDefiningInvariant(this)

  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Long, NewVal: Long): Unit = {
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
    val newValue1 = content1AtStart(vehicle).value
    val newValue2 = content2AtStart(vehicle).value

    val oldValue1 = content1AtNode(vehicle).newValue
    val oldValue2 = content2AtNode(vehicle).newValue

    if(oldValue1 == newValue1 && oldValue2 == newValue2){
      false
    }else{
      content1AtNode(vehicle) := newValue1
      content2AtNode(vehicle) := newValue2
      true
    }
  }

  /**
    * @param prevNode
    * @param node
    * @return true if changed, false otherwise
    */
  override def setVehicleContentAtNode(prevNode : Int, node : Int) : Boolean = {
    val oldValue1 = content1AtNode(node).newValue
    val oldValue2 = content2AtNode(node).newValue
    //(fromNode,toNode,content1AtFromNode,content2AtFromNode)=> (content1AtToNode,content2AtToNode)
    val (newValue1,newValue2) = op(prevNode,node,content1AtNode(prevNode).newValue,content2AtNode(prevNode).newValue)
    if(oldValue1 != newValue1 || oldValue2 != newValue2) {
      content1AtNode(node) := newValue1
      content2AtNode(node) := newValue2
      true
    }else{
      false
    }
  }

  override def setVehicleContentAtEnd(vehicle : Int, lastNode : Int): Unit ={
    lastPointOfVehicle(vehicle) := lastNode
    //(fromNode,toNode,content1AtFromNode,content2AtFromNode)=> (content1AtToNode,content2AtToNode)
    val (newValue1,newValue2) = op(lastNode,vehicle,content1AtNode(lastNode).newValue,content2AtNode(lastNode).newValue)
    content1AtEnd(vehicle) := newValue1
    content2AtEnd(vehicle) := newValue2
  }

  override def setNodesUnrouted(unroutedNodes : Iterable[Int]): Unit ={
    for(node <- unroutedNodes) {
      content1AtNode(node) := defaultVehicleContent1ForUnroutedNodes
      content2AtNode(node) := defaultVehicleContent2ForUnroutedNodes
    }
  }

  override def toString : String = {
    s"""ForwardCumulativeIntegerIntegerDimensionOnVehicle(routes:${routes.name} n:$n v:$v contentName: $contentName){
       |${(0 until v).toList.map((vehicle:Int) =>
    {
      val header = s"""   vehicle$vehicle contentAtStart:${(content1AtStart,content2AtStart)}
"""
      var explorerOpt = routes.value.explorerAtAnyOccurrence(vehicle).get.next
      var acc:String = ""

      while(explorerOpt match{
        case None => //at end of last vehicle
          val vehicle = v-1
          acc += s"""endOfRoute of vehicle$vehicle contentAtEnd:${(content1AtEnd(vehicle).value,content2AtEnd(vehicle).value)}
"""
          false
        case Some(explorer) if explorer.value < v =>
          //reached another vehicle
          val vehicle = explorer.value-1
          acc += s"""endOfRoute of vehicle$vehicle contentAtEnd:${(content1AtEnd(vehicle).value,content2AtEnd(vehicle).value)}
"""
          false
        case Some(explorer) if explorer.value >= v =>
          val node = explorer.value
          acc += s"""    node:$node    content:${(content1AtNode(node).value,content2AtNode(node).value)}
"""
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

    //(fromNode,toNode,content1AtFromNode,content2AtFromNode)=> (content1AtToNode,content2AtToNode)
    def op2(fromNode:Int,toNode:Int,content:(Long,Long)) = op(fromNode,toNode,content._1,content._2)

    val (nodeToContent,vehicleToContentAtEnd,vehicleLocation) =
      AbstractVehicleCapacity.computeNodeToContentAndVehicleContentAtEndAndVehicleStartPositionsFromScratch(
        n,
        v,
        op2,
        v => (content1AtStart(v).value,content2AtStart(v).value),
        s,
        (defaultVehicleContent1ForUnroutedNodes,defaultVehicleContent2ForUnroutedNodes))

    val currentVehicleLocation = this.toUpdateZonesAndVehicleStartAfter.get._2

    for(vehicle <- 0 until v){
      c.check(vehicleLocation.startPosOfVehicle(vehicle) == s.positionOfAnyOccurrence(vehicle).get,
        Some(s"Found start of vehicle($vehicle):=${vehicleLocation.startPosOfVehicle(vehicle)} should be :=${s.positionOfAnyOccurrence(vehicle)} seq :${s.mkString(",")}"))
      c.check(currentVehicleLocation.startPosOfVehicle(vehicle) == vehicleLocation.startPosOfVehicle(vehicle),Some("x"))
    }

    for(node <- 0 until n){
      c.check(nodeToContent(node) == (content1AtNode(node).newValue,content2AtNode(node).newValue),
        Some(s"Vehicle content at node($node) at pos : ${s.positionsOfValue(node)} := ${(content1AtNode(node).newValue,content2AtNode(node).newValue)} should be :=${nodeToContent(node)} routes:${s.mkString(",")}"))
    }

    for(vehicle <- 0 until v){
      c.check((content1AtEnd(vehicle).newValue,content2AtEnd(vehicle).newValue) == vehicleToContentAtEnd(vehicle),
        Some(s"Error on vehicle content at end vehicle:$vehicle contentAtEnd(vehicle).newValue:${(content1AtEnd(vehicle).newValue,content2AtEnd(vehicle).newValue)} should be:${vehicleToContentAtEnd(vehicle)}"))
    }
  }
}
