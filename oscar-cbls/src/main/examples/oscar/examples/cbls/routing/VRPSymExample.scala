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
package oscar.examples.cbls.routing

import oscar.cbls._
import oscar.cbls.business.routing._
import oscar.cbls.business.routing.model.helpers.DistanceHelper
import oscar.cbls.core.computation.Store
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.{Best, First}

class SimpleVRPSymModelWithUnroutedPoints(n:Int,v:Int,symmetricDistance:Array[Array[Long]],m:Store, maxPivot:Int)
  extends VRP(m,n,v,maxPivot){

  val penaltyForUnrouted  = 10000

  m.registerForPartialPropagation(unrouted)
  m.registerForPartialPropagation(routed)

  val totalDistance = routeLength(routes, n, v ,false, symmetricDistance, true)(0)

  val obj = Objective(totalDistance + (penaltyForUnrouted*(n - length(routes))))

  override def toString : String = super.toString +
    s"objective: ${obj.value}\n"

  val closestNeighboursForward = Array.tabulate(n)(DistanceHelper.lazyClosestPredecessorsOfNode(symmetricDistance, (_) => nodes)(_))

  def size = routes.value.size
  def nbRouted = size
}

object VRPSymExample extends App {
  val n = 1000
  val v = 10
  val verbose = 1
  new SimpleVRPSymSolver(n,v,4,verbose)
}

class SimpleVRPSymSolver(n:Int,v:Int,maxPivotPerValuePercent:Int, verbose:Int){
  val routingMatrix = RoutingMatrixGenerator(n,side=1000)
  val symmetricDistanceMatrix = routingMatrix._1
  val pointsPositions = routingMatrix._2

  val model = new Store()

  val myVRP = new SimpleVRPSymModelWithUnroutedPoints(n,v,symmetricDistanceMatrix,model,maxPivotPerValuePercent)
  val nodes = myVRP.nodes

  model.close()

  val routeUnroutedPoint =  profile(insertPointUnroutedFirst(myVRP.unrouted,
    ()=>myVRP.kFirst(10,myVRP.closestNeighboursForward(_),(_) => myVRP.isRouted),
    myVRP,
    neighborhoodName = "InsertUF",
    hotRestart = false,
    selectNodeBehavior = First(),
    selectInsertionPointBehavior = Best()))
  
  //using post-filters on k-nearest is probably a bit slower than possible for large problems.
  //that's why we prefer to block this neighborhood when many nodes are already routed (so few are unrouted, so the filter filters many nodes away)
  val routeUnroutedPoint2 =  profile(insertPointRoutedFirst(
    myVRP.routed,
    ()=>myVRP.kFirst(10,myVRP.closestNeighboursForward(_),(_) => x => !myVRP.isRouted(x)),  //should be the backward ones but this is a symmetric distance so we do not care
    myVRP,
    neighborhoodName = "InsertRF")
    guard(() => myVRP.nbRouted < n/2))

  def onePtMove(k:Int) = profile(onePointMove(
    myVRP.routed,
    () => myVRP.kFirst(k,myVRP.closestNeighboursForward(_),(_) => myVRP.isRouted),
    myVRP,
    selectDestinationBehavior = Best()))

  val customTwoOpt = profile(twoOpt(myVRP.routed, ()=>myVRP.kFirst(20,myVRP.closestNeighboursForward(_),(_) => myVRP.isRouted), myVRP))

  def customThreeOpt(k:Int, breakSym:Boolean) =
    profile(threeOpt(myVRP.routed, ()=>myVRP.kFirst(k,myVRP.closestNeighboursForward(_),(_) => myVRP.isRouted), myVRP,breakSymmetry = breakSym, neighborhoodName = "ThreeOpt(k=" + k + ")"))

  val vlsn1pt = mu[OnePointMoveMove](
    onePointMove(myVRP.routed, () => myVRP.kFirst(5,myVRP.closestNeighboursForward(_),(_) => myVRP.isRouted),myVRP),
    l => Some(onePointMove(() => List(l.head.newPredecessor).filter(_ >= v), () => myVRP.kFirst(3,myVRP.closestNeighboursForward(_),(_) => myVRP.isRouted),myVRP, hotRestart = false)),
    intermediaryStops = true,
    maxDepth = 6)

  def segExchange(k:Int) = segmentExchange(myVRP,()=>myVRP.kFirst(k,myVRP.closestNeighboursForward(_),(_) => myVRP.isRouted),() => myVRP.vehicles)

  val search =
    (bestSlopeFirst(
      List(
        routeUnroutedPoint,
        routeUnroutedPoint2,
        vlsn1pt,
        onePtMove(10),
        customTwoOpt,
        customThreeOpt(10,true),
        segExchange(10)))
      exhaust
      bestSlopeFirst(
        List(
          customThreeOpt(30,true),
          vlsn1pt)))

  search.verbose = verbose

  search.doAllMoves(obj=myVRP.obj)

  print(myVRP)
}