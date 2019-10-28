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
import oscar.cbls.core.search.Best
import oscar.cbls.business.routing._
import oscar.examples.cbls.routing.RoutingMatrixGenerator


class MySimpleRoutingWithCumulatives(n:Int,v:Int,symmetricDistance:Array[Array[Int]],m:Store, maxPivot:Int, deltaAtNode:Array[Int], maxCapa:Int)
  extends VRP(m,n,v,maxPivot){

  val routingDistance = constantRoutingDistance(routes,n,v,false,symmetricDistance,true,false,false)

  val penaltyForUnrouted  = 10000

  val maxNodes = (length(routes) le (n-3)).violation

  val violation = new CBLSIntVar(routes.model, 0, 0 to Int.MaxValue, "violation of capacity test")
  val violation2 = new CBLSIntVar(routes.model, 0, 0 to Int.MaxValue, "violation of capacity2 test")

  val contentConstraint = forwardCumulativeConstraintOnVehicle(
    routes,
    n,
    v,
    {case (fromNode,toNode,content) => content + deltaAtNode(toNode)},
    maxCapa,
    Array.tabulate(v)(deltaAtNode),
    violation,
    6)


  val contentConstraint2 = forwardCumulativeConstraintOnVehicle(
    routes,
    n,
    v,
    {case (fromNode,toNode,content) => content + deltaAtNode(fromNode)},
    maxCapa-2,
    Array.tabulate(v)(deltaAtNode),
    violation2,
    6,"content-2")


  val contentAtStart = Array.tabulate(v)(vehicle => CBLSIntVar(m,0,0 to 10,"start content of vehicle " + vehicle))
    val cumulative2 = ForwardCumulativeIntegerDimensionOnVehicle(routes,n,v,{case (fromNode,toNode,fromContent) => ((fromNode*toNode)/2)+toNode/(fromNode+1)+(2*fromContent)+1},contentAtStart,-1)

  val obj = new CascadingObjective(
    contentConstraint.violation,
    new CascadingObjective(maxNodes,
      Objective(cumulative2._3(1) + cumulative2._2(1) + routingDistance(0) + (penaltyForUnrouted*(n - length(routes))))))

  val closestNeighboursForward = Array.tabulate(n)(DistanceHelper.lazyClosestPredecessorsOfNode(symmetricDistance, (_) => nodes))

  def size = routes.value.size

  val (next,prev) = routeSuccessorAndPredecessors(routes,v,n)()

  val movingVehiclesInv = movingVehicles(routes,v)

  override def toString : String = super.toString +
    "objective: " + obj.detailedString(false) + "\n" +
    "next: [" + next.map(_.value).mkString(",") + "]" + "\n" +
    "prev: [" + prev.map(_.value).mkString(",") + "]" + "\n" +
    "content: [" + contentConstraint.contentAtNodes.mkString(",") + "]" + "\n" +
    "routed:" + this.routed.value + "\n" +
    "unRouted:" + this.unrouted.value + "\n" +
    contentConstraint + "\n" + contentConstraint2 + "\n"
}

object TestCumulatives extends App{

  val n = 30
  val v = 5
  val delta = Array(0,1,1,2,2,3,-3,4,-4,0,0,1,-1,2,-2,3,-3,4,-4,0,0,1,-1,2,-2,3,-3,4,-4,0)
  val maxPivotPerValuePercent = 4
  val maxcapa = 4
  println("VRP(n:" + n + " v:" + v + ")")

  val (symmetricDistanceMatrix,pointsList) = RoutingMatrixGenerator(n)
  //  println("restrictions:" + restrictions)
  val model = new Store(checker = Some(new ErrorChecker()))

  val myVRP = new RoutingWithCapacityMax(n,v,symmetricDistanceMatrix,model,maxPivotPerValuePercent,delta,maxcapa)

  model.close()

  def routeUnroutedPoint(k:Int) =  insertPointUnroutedFirst(myVRP.unrouted,()=>myVRP.kFirst(k,myVRP.closestNeighboursForward, (_) => myVRP.isRouted), myVRP,neighborhoodName = "InsertUF",selectNodeBehavior = Best(),selectInsertionPointBehavior = Best())

  //TODO: using post-filters on k-nearest is probably crap
  val routeUnroutedPoint2 =  profile(insertPointRoutedFirst(myVRP.routed,()=>myVRP.kFirst(10,myVRP.closestNeighboursForward, (_) => x => !myVRP.isRouted(x)),myVRP,neighborhoodName = "InsertRF")  guard(() => myVRP.size < n/2))

  def onePtMove(k:Int) = profile(onePointMove(
    myVRP.routed,
    () => myVRP.kFirst(k,myVRP.closestNeighboursForward, (_) => myVRP.isRouted),
    myVRP,
    selectPointToMoveBehavior = Best(),
    selectDestinationBehavior = Best()))

  val customTwoOpt = profile(twoOpt(myVRP.routed, ()=>myVRP.kFirst(40,myVRP.closestNeighboursForward, (_) => myVRP.isRouted), myVRP))

  def customThreeOpt(k:Int, breakSym:Boolean) = profile(threeOpt(myVRP.routed, ()=>myVRP.kFirst(k,myVRP.closestNeighboursForward, (_) => myVRP.isRouted), myVRP,breakSymmetry = breakSym, neighborhoodName = "ThreeOpt(k=" + k + ")"))

  val vlsn1pt = profile(mu[OnePointMoveMove](
    onePointMove(myVRP.routed, () => myVRP.kFirst(10,myVRP.closestNeighboursForward, (_) => myVRP.isRouted),myVRP),
    l => Some(onePointMove(() => List(l.head.newPredecessor).filter(_ >= v), () => myVRP.kFirst(10,myVRP.closestNeighboursForward, (_) => myVRP.isRouted),myVRP, hotRestart = false)),
    intermediaryStops = true,
    maxDepth = 3))


  val vlsnInsert = mu[InsertPointMove](
  routeUnroutedPoint(3),
  l => if (myVRP.unroutedNodes.isEmpty) None else Some(routeUnroutedPoint(5)),
  intermediaryStops = false,
  maxDepth = 2)

  val remove = removePoint(() => myVRP.routed.value.filter(_>=v), myVRP,selectNodeBehavior = Best())
  def segExchange(k:Int) = segmentExchange(myVRP,()=>myVRP.kFirst(k,myVRP.closestNeighboursForward, (_) => myVRP.isRouted),() => myVRP.vehicles)

  val swapInOut = profile((remove andThen routeUnroutedPoint(10)) name ("SWAPInsert"))
  val doubleInsert = profile((routeUnroutedPoint(10) andThen routeUnroutedPoint(10)) name ("doubleInsert"))
  val doubleRemove = profile(( removePoint(() => myVRP.routed.value.filter(_>=v), myVRP,selectNodeBehavior = Best())) andThen  removePoint(() => myVRP.routed.value.filter(_>=v), myVRP,selectNodeBehavior = Best()) name ("doubleRemove"))

  val search = roundRobin(List(onePtMove(100),
    doubleInsert,
    doubleRemove,
    swapInOut,
    vlsnInsert,
    vlsn1pt,
    customThreeOpt(5,false),
    customTwoOpt,
    segExchange(10))) onExhaustRestartAfter (doubleRemove acceptAll(),5,myVRP.obj)

  //search.verbose = 1
  search.verboseWithExtraInfo(3, ()=> "" + myVRP)

  print("Doing all moves ...")


  search.doAllMoves(obj = myVRP.obj)
  model.propagate()
  println(search.profilingStatistics)

  println(myVRP)
}
*/