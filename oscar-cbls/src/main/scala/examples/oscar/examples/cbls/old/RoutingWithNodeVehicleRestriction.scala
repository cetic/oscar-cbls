package oscar.cbls.test.routing

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
/*
import oscar.cbls._
import oscar.cbls.business.routing._
import oscar.cbls.business.routing.invariants.NodeVehicleRestrictions
import oscar.examples.cbls.routing.RoutingMatrixGenerator

class VRPWithNodeVehicleRestriction(n:Int,v:Int,symmetricDistance:Array[Array[Long]],m:Store, maxPivot:Int, nodeVehicleRestriction:Iterable[(Long,Long)])
  extends VRP(m,n,v,maxPivot){
  
  private val cloneOfRoute = routes.createClone()
  val routingDistance = routeLength(routes,n,v,false,symmetricDistance,true,false,false)

  //this is useless, but it makes some fun.
  val positionOf48 = positionsOf(cloneOfRoute, 48)

  val size = length(cloneOfRoute)

  //initializes to something simple
  setCircuit(nodes)

  val violationOfRestriction = nodeVehicleRestrictions(routes,v, nodeVehicleRestriction)

  val totalViolationOnRestriction = sum(violationOfRestriction)

  //val obj = new CascadingObjective(totalViolationOnRestriction, totalDistance)
  val obj = Objective(totalViolationOnRestriction*10000 + routingDistance(0))

  override def toString : String = super.toString +
    positionOf48 + "\n" +
    "objective: " + obj.value  + "\n" +
    "violationOfRestriction:[" + violationOfRestriction.toList.map(_.value).mkString(",") + "]"  + "\n" +
    size + "\n" +
    "number of restrictions:" + nodeVehicleRestriction.size

  val nodesThatShouldBeMovedToOtherVehicle =  NodeVehicleRestrictions.violatedNodes(vehicleOfNode,v,nodeVehicleRestriction)

  val closestNeighboursForward = Array.tabulate(n)(DistanceHelper.lazyClosestPredecessorsOfNode(symmetricDistance, (_) => nodes)(_))
}

object RoutingWithNodeVehicleRestriction extends App{

  val n = 1000
  val v = 10
  val nbRestrictions = 3000

  val maxPivotPerValuePercent = 4

  println("VRP(n:" + n + " v:" + v + ")")

  val symmetricDistanceMatrix = RoutingMatrixGenerator(n)._1
  val restrictions = RoutingMatrixGenerator.generateRestrictions(n,v,nbRestrictions)

  //  println("restrictions:" + restrictions)
  val model = new Store(checker = Some(new ErrorChecker())) //

  val myVRP = new VRPWithNodeVehicleRestriction(n,v,symmetricDistanceMatrix,model,maxPivotPerValuePercent,restrictions)
  val nodes = myVRP.nodes

  model.close()

  println(myVRP)

  val onePtMove = profile(onePointMove(() => nodes, ()=>myVRP.kFirst(40,myVRP.closestNeighboursForward(_)), myVRP))

  val onePtMoveSolvingRestrictions = profile(
    (onePointMove(myVRP.nodesThatShouldBeMovedToOtherVehicle, ()=>myVRP.kFirst(20,myVRP.closestNeighboursForward(_)), myVRP)
      exhaust onePointMove(myVRP.nodesThatShouldBeMovedToOtherVehicle, ()=>myVRP.kFirst(100,myVRP.closestNeighboursForward(_)), myVRP))
      guard(() => myVRP.totalViolationOnRestriction.value >0)
      name "MoveForRestr")

  val customTwoOpt = profile(twoOpt(() => nodes, ()=>myVRP.kFirst(40,myVRP.closestNeighboursForward(_)), myVRP))

  def customThreeOpt(k:Int, breakSym:Boolean) = profile(threeOpt(() => nodes, ()=>myVRP.kFirst(k,myVRP.closestNeighboursForward(_)), myVRP,breakSymmetry = breakSym, neighborhoodName = "ThreeOpt(k=" + k + ")"))

  //This neighborhood is useless given the other one, and for this kind of routing problem, yet we leave it for doc purpose
  val segExchange = segmentExchange(myVRP,
    ()=>myVRP.kFirst(40,myVRP.closestNeighboursForward(_)), //must be routed
    vehicles=() => myVRP.vehicles)

  val search = bestSlopeFirst(List(onePtMove,customTwoOpt, customThreeOpt(10,true),onePtMoveSolvingRestrictions)) exhaust customThreeOpt(20,true)

  search.doAllMoves(obj=myVRP.obj)

  model.propagate()

  println
  println(myVRP)
  println
  println(search.profilingStatistics)
}
*/