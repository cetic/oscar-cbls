package oscar.cbls.business.routing.invariants

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


import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.computation.ChangingSeqValue
import oscar.cbls._
import oscar.cbls.business.routing.invariants.group.{FlippedPreComputedSubSequence, GlobalConstraintCore, GlobalConstraintDefinition, LogReducedFlippedPreComputedSubSequence, LogReducedGlobalConstraint, LogReducedGlobalConstraintWithExtremes, LogReducedNewNode, LogReducedPreComputedSubSequence, LogReducedSegment, NewNode, PreComputedSubSequence, Segment}


object NbNodes{
  /**
    * this constraints maintains the number of node per vehicle.
    *
    * @param gc The GlobalConstraint linked to this constraints
    * @param v number of vehicle
    * @param nbNodesPerVehicle an array telling how many nodes are reached per vehicle
    */
  def apply(gc: GlobalConstraintCore, n: Int, v : Int, nbNodesPerVehicle : Array[CBLSIntVar]) =
    new NbNodes(gc, n, v , nbNodesPerVehicle)
}


/**
  * this constraints maintains the number of node per vehicle.
  *
  * @param gc The GlobalConstraint linked to this constraint
  * @param v number of vehicle
  * @param nbNodesPerVehicle an array telling how many nodes are reached per vehicle
  */
class NbNodes(gc: GlobalConstraintCore, n: Long, v : Int, nbNodesPerVehicle : Array[CBLSIntVar])
  extends GlobalConstraintDefinition(gc,v){

  type U = Long

  val preComputedVals: Array[Option[Long]] = Array.fill(n)(None)

  // Initialize the vehicles value, the precomputation value and link these invariant to the GlobalConstraintCore
  gc.register(this)
  vehiclesValueAtCheckpoint0 = Array.fill(v)(0)
  currentVehiclesValue = Array.fill(v)(0)
  for(outputVariable <- nbNodesPerVehicle)outputVariable.setDefiningInvariant(gc)

  /**
    * tis method is called by the framework when a pre-computation must be performed.
    * you are expected to assign a value of type T to each node of the vehicle "vehicle" through the method "setNodeValue"
    *
    * @param vehicle      the vehicle where pre-computation must be performed
    * @param routes       the sequence representing the route of all vehicle
    *                     BEWARE,other vehicles are also present in this sequence; you must only work on the given vehicle
    */
  override def performPreCompute(vehicle: Long, routes: IntSequence): Unit = {
    var nbNode = 0
    var continue = true
    var vExplorer = routes.explorerAtAnyOccurrence(vehicle)
    while(continue){
      vExplorer match {
        case None => continue = false
        case Some(elem) =>
          if (elem.value < v && elem.value != vehicle){
            continue = false
          } else {
            nbNode = nbNode + 1
            preComputedVals(elem.value) = Some(nbNode)
          }
          vExplorer = elem.next
      }
    }
  }

  /**
    * this method is called by the framework when the value of a vehicle must be computed.
    *
    * @param vehicle   the vehicle that we are focusing on
    * @param segments  the segments that constitute the route.
    *                  The route of the vehicle is equal to the concatenation of all given segments in the order thy appear in this list
    * @param routes    the sequence representing the route of all vehicle
    * @return the value associated with the vehicle
    */
  override def computeVehicleValue(vehicle: Long, segments: QList[Segment], routes: IntSequence): Unit = {
    val tmp = QList.qMap(segments, (s: Segment) =>
      s match {
        case PreComputedSubSequence (fstNode, lstNode, length) =>
          preComputedVals(lstNode).get - preComputedVals(fstNode).get + 1
        case FlippedPreComputedSubSequence(lstNode,fstNode,length) =>
          preComputedVals(lstNode).get - preComputedVals(fstNode).get + 1
        case NewNode(_) =>
          1
      }).sum
    //println("Vehicle : " + vehicle + "--" + segments.mkString(","))
    saveVehicleValue(vehicle,tmp)
  }



  /**
    * the framework calls this method to assign the value U to he output variable of your invariant.
    * It has been dissociated from the method above because the framework memorizes the output value of the vehicle,
    * and is able to restore old value without the need to re-compute them, so it only will call this assignVehicleValue method
    *
    * @param vehicle the vehicle number
    */
  override def assignVehicleValue(vehicle: Long): Unit = {
    nbNodesPerVehicle(vehicle) := currentVehiclesValue(vehicle)
  }


  def countVehicleNode(vehicle : Long,vExplorer : Option[IntSequenceExplorer]) : Long = {
    vExplorer match {
      case None => 0
      case Some(elem) =>
        if (elem.value < v) 0 else (1 + countVehicleNode(vehicle,elem.next))
    }
  }

  /**
    *
    * @param vehicle
    * @param routes
    * @return
    */
  override def computeVehicleValueFromScratch(vehicle: Long, routes: IntSequence, save: Boolean = true): Long = {
    val result = 1 + countVehicleNode(vehicle,routes.explorerAtAnyOccurrence(vehicle).get.next)
    if(save)saveVehicleValue(vehicle, result)
    result
  }
}

case class NodesOnSubsequence(nbNodes:Long,
                              level:Long,
                              firstNode:Long,
                              lastNode:Long){
  //  require(nbNodes == ((1L << (level-1L))  -1L))

  override def toString: String = {
    "NodesOnSubsequence(nbNodes:" + nbNodes + ",level:" + level + ",firstNode:" + firstNode + ",lastNode:" + lastNode + ")"
  }
}


@deprecated("This is for example only, do not use this version","")
class LogReducedNumberOfNodes(gc: GlobalConstraintCore, n:Long, v:Int, nbNodesPerRoute:Array[CBLSIntVar])
  extends LogReducedGlobalConstraint[NodesOnSubsequence](gc,n,v){

  type U = Long

  // Initialize the vehicles value, the precomputation value and link these invariant to the GlobalConstraintCore
  gc.register(this)
  vehiclesValueAtCheckpoint0 = Array.fill(v)(0)
  currentVehiclesValue = Array.fill(v)(0)
  for(outputVariable <- nbNodesPerRoute)outputVariable.setDefiningInvariant(gc)

  /**
    * this method delivers the value of the node
    *
    * @return the type T associated with the node "node"
    */
  override def nodeValue(node: Long): NodesOnSubsequence = {
    NodesOnSubsequence(1L, level = 0L,node,node)
  }


  /**
    * this one is similar to the nodeValue except that it only is applied on vehicle,
    * to represent the return to the vehicle start at teh end of its route
    *
    * @param vehicle
    * @return
    */
  override def endNodeValue(vehicle: Long): NodesOnSubsequence = {
    NodesOnSubsequence(0L, level = 0L, vehicle, vehicle) //since it is the number of nodes, return is not counted as a node
  }

  /**
    * this method is for composing steps into bigger steps.
    *
    * @param firstStep  the type T associated with stepping over a sequence of nodes (which can be minial two)
    * @param secondStep the type T associated with stepping over a sequence of nodes (which can be minial two)
    * @return the type T associated with the first step followed by the second step
    */
  override def composeSteps(firstStep: NodesOnSubsequence, secondStep: NodesOnSubsequence): NodesOnSubsequence = {
    require(firstStep.level == secondStep.level)
    NodesOnSubsequence(firstStep.nbNodes + secondStep.nbNodes, firstStep.level + 1L,firstStep.firstNode,secondStep.lastNode)
  }

  /**
    * this method is called by the framework when the value of a vehicle must be computed.
    *
    * @param vehicle  the vehicle that we are focusing on
    * @param segments the segments that constitute the route.
    *                 The route of the vehicle is equal to the concatenation of all given segments in the order thy appear in this list
    * @return the value associated with the vehicle. this value should only be computed based on the provided segments
    */
  override def computeVehicleValueComposed(vehicle: Long, segments: QList[LogReducedSegment[NodesOnSubsequence]]): Unit = {
    saveVehicleValue(vehicle, segments.qMap({
      case s@LogReducedPreComputedSubSequence(startNode, endNode, steps)=>
        //require(steps.isEmpty || steps.head.firstNode == startNode)
        //require(steps.isEmpty || steps.last.lastNode == endNode)
        QList.qFold[NodesOnSubsequence,Long](steps,(a,b) => a + b.nbNodes,0L)
      case s@LogReducedFlippedPreComputedSubSequence(startNode, endNode, steps) =>
        //require(steps.isEmpty || steps.last.lastNode == startNode, steps)
        //require(steps.isEmpty || steps.head.firstNode == endNode)
        QList.qFold[NodesOnSubsequence,Long](steps,(a,b) => a + b.nbNodes,0L)
      case s@LogReducedNewNode(_, _) =>
        1L
    }).sum)
  }

  /**
    * the framework calls this method to assign the value U to he output variable of your invariant.
    * It has been dissociated from the method above because the framework memorizes the output value of the vehicle,
    * and is able to restore old value without the need to re-compute them, so it only will call this assignVehicleValue method
    *
    * @param vehicle the vehicle number
    */
  override def assignVehicleValue(vehicle: Long): Unit = {
    nbNodesPerRoute(vehicle) := currentVehiclesValue(vehicle)
  }

  /**
    *
    * @param vehicle
    * @param routes
    * @return
    */
  override def computeVehicleValueFromScratch(vehicle: Long, routes: IntSequence, save: Boolean = true): Long = {
    val result = if(vehicle == v-1L){
      routes.size - routes.positionOfAnyOccurrence(vehicle).get
    }else{
      routes.positionOfAnyOccurrence(vehicle+1L).get - routes.positionOfAnyOccurrence(vehicle).get
    }
    if(save) saveVehicleValue(vehicle, result)
    result
  }
}

@deprecated("This is for example only, do not use this version","")
class LogReducedNumberOfNodesWithExtremes(gc: GlobalConstraintCore, n: Long, v:Int, nbNodesPerRoute:Array[CBLSIntVar])
  extends LogReducedGlobalConstraintWithExtremes[NodesOnSubsequence](gc,n,v){

  type U = Long

  // Initialize the vehicles value, the precomputation value and link these invariant to the GlobalConstraintCore
  gc.register(this)
  vehiclesValueAtCheckpoint0 = Array.fill(v)(0)
  currentVehiclesValue = Array.fill(v)(0)
  for(outputVariable <- nbNodesPerRoute)outputVariable.setDefiningInvariant(gc)

  /**
    * this method delivers the value of the node
    *
    * @return the type T associated with the node "node"
    */
  override def nodeValue(node: Long): NodesOnSubsequence = {
    NodesOnSubsequence(1L, level = 0L,node,node)
  }


  /**
    * this one is similar to the nodeValue except that it only is applied on vehicle,
    * to represent the return to the vehicle start at teh end of its route
    *
    * @param vehicle
    * @return
    */
  override def endNodeValue(vehicle: Long): NodesOnSubsequence = {
    NodesOnSubsequence(0L, level = 0L, vehicle, vehicle) //since it is the number of nodes, return is not counted as a node
  }

  /**
    * this method is for composing steps into bigger steps.
    *
    * @param firstStep  the type T associated with stepping over a sequence of nodes (which can be minial two)
    * @param secondStep the type T associated with stepping over a sequence of nodes (which can be minial two)
    * @return the type T associated with the first step followed by the second step
    */
  override def composeSteps(firstStep: NodesOnSubsequence, secondStep: NodesOnSubsequence): NodesOnSubsequence = {
    NodesOnSubsequence(firstStep.nbNodes + secondStep.nbNodes, firstStep.level + 1L,firstStep.firstNode,secondStep.lastNode)
  }


  /**
    * this method is called by the framework when the value of a vehicle must be computed.
    *
    * @param vehicle  the vehicle that we are focusing on
    * @param segments the segments that constitute the route.
    *                 The route of the vehicle is equal to the concatenation of all given segments in the order thy appear in this list
    * @return the value associated with the vehicle. this value should only be computed based on the provided segments
    */
  override def computeVehicleValueComposed(vehicle: Long, segments: QList[LogReducedSegment[NodesOnSubsequence]]): Unit = {
    saveVehicleValue(vehicle, segments.qMap({
      case s@LogReducedPreComputedSubSequence(startNode, endNode, steps)=>
        //require(steps.isEmpty || steps.head.firstNode == startNode)
        //require(steps.isEmpty || steps.last.lastNode == endNode)
        QList.qFold[NodesOnSubsequence,Long](steps,(a,b) => a + b.nbNodes,0L)
      case s@LogReducedFlippedPreComputedSubSequence(startNode, endNode, steps) =>
        //require(steps.isEmpty || steps.last.lastNode == startNode, steps)
        //require(steps.isEmpty || steps.head.firstNode == endNode)
        QList.qFold[NodesOnSubsequence,Long](steps,(a,b) => a + b.nbNodes,0L)
      case s@LogReducedNewNode(_, _) =>
        1L
    }).sum)
  }

  /**
    * the framework calls this method to assign the value U to he output variable of your invariant.
    * It has been dissociated from the method above because the framework memorizes the output value of the vehicle,
    * and is able to restore old value without the need to re-compute them, so it only will call this assignVehicleValue method
    *
    * @param vehicle the vehicle number
    */
  override def assignVehicleValue(vehicle: Long): Unit = {
    nbNodesPerRoute(vehicle) := currentVehiclesValue(vehicle)
  }

  /**
    *
    * @param vehicle
    * @param routes
    * @return
    */
  override def computeVehicleValueFromScratch(vehicle: Long, routes: IntSequence, save: Boolean = true): Long = {
    val result = if(vehicle == v-1L){
      routes.size - routes.positionOfAnyOccurrence(vehicle).get
    }else{
      routes.positionOfAnyOccurrence(vehicle+1L).get - routes.positionOfAnyOccurrence(vehicle).get
    }
    if(save) saveVehicleValue(vehicle,result)
    result
  }
}

