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
import oscar.examples.cbls.routing.RoutingMatrixGenerator

class MySimpleRoutingWithUnroutedPoints(n:Int,v:Int,symmetricDistance:Array[Array[Int]],m:Store, maxPivot:Int)
  extends VRP(m,n,v,maxPivot){

  val routingDistance = constantRoutingDistance(routes,n,v,false,symmetricDistance,true,false,false)

  val penaltyForUnrouted  = 10000
  
  m.registerForPartialPropagation(unrouted)
  m.registerForPartialPropagation(routed)
  
  val obj = Objective(routingDistance(0) + (penaltyForUnrouted*(n - length(routes))))

  override def toString : String = super.toString +
    "objective: " + obj.value + "\n"

  val closestNeighboursForward = Array.tabulate(n)(DistanceHelper.lazyClosestPredecessorsOfNode(symmetricDistance, (_) => nodes))

  def size = routes.value.size
}

object TSProutePoints extends App {
  val n = 10000
  val v = 100
  val verbose = 1
  new TSPRoutePointsS(n,v,4,verbose)
}

class TSPRoutePointsS(n:Int,v:Int,maxPivotPerValuePercent:Int, verbose:Int){

  val symmetricDistanceMatrix = RoutingMatrixGenerator(n)._1

  //  println("restrictions:" + restrictions)
  val model = new Store() //checker = Some(new ErrorChecker()))

  val myVRP = new MySimpleRoutingWithUnroutedPoints(n,v,symmetricDistanceMatrix,model,maxPivotPerValuePercent)
  val nodes = myVRP.nodes

  model.close()

  val routeUnroutdPoint =  profile(insertPointUnroutedFirst(myVRP.unrouted,()=>myVRP.kFirst(10,myVRP.closestNeighboursForward,(_) => myVRP.isRouted), myVRP,neighborhoodName = "InsertUF"))

  //TODO: using post-filters on k-nearest is probably a bit slower than possible in this context
  val routeUnroutdPoint2 =  profile(insertPointRoutedFirst(myVRP.routed,()=>myVRP.kFirst(10,myVRP.closestNeighboursForward,(_) => x => !myVRP.isRouted(x)),myVRP,neighborhoodName = "InsertRF")  guard(() => myVRP.size < n/2))

  def onePtMove(k:Int) = profile(onePointMove(myVRP.routed, () => myVRP.kFirst(k,myVRP.closestNeighboursForward,(_) => myVRP.isRouted), myVRP))

  val customTwoOpt = profile(twoOpt(myVRP.routed, ()=>myVRP.kFirst(20,myVRP.closestNeighboursForward,(_) => myVRP.isRouted), myVRP))

  def customThreeOpt(k:Int, breakSym:Boolean) = profile(threeOpt(myVRP.routed, ()=>myVRP.kFirst(k,myVRP.closestNeighboursForward,(_) => myVRP.isRouted), myVRP,breakSymmetry = breakSym, neighborhoodName = "ThreeOpt(k=" + k + ")"))

  val vlsn1pt = mu[OnePointMoveMove](
  onePointMove(myVRP.routed, () => myVRP.kFirst(5,myVRP.closestNeighboursForward,(_) => myVRP.isRouted),myVRP),
  l => Some(onePointMove(() => List(l.head.newPredecessor).filter(_ >= v), () => myVRP.kFirst(3,myVRP.closestNeighboursForward,(_) => myVRP.isRouted),myVRP)),
  intermediaryStops = true,
  maxDepth = 7)

  val search = bestSlopeFirst(List(routeUnroutdPoint2, routeUnroutdPoint, vlsn1pt, onePtMove(10),customTwoOpt, customThreeOpt(10,true))) exhaust customThreeOpt(20,true) exhaust vlsn1pt
  //val search = (BestSlopeFirst(List(routeUnroutdPoint2, routeUnroutdPoint, vlsn1pt)))


  search.verbose = verbose
  //search.verboseWithExtraInfo(1, ()=> "" + myVRP)

  search.doAllMoves(obj=myVRP.obj)
}
*/