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

class MySimpleRoutingWithUnroutedPointsAndNext(n:Int,v:Int,symmetricDistance:Array[Array[Int]],m:Store, maxPivot:Int, pointsList:Array[(Int,Int)] = null)
  extends VRP(m,n,v,maxPivot)
{
  val routingDistance = constantRoutingDistance(routes,n,v,false,symmetricDistance,true,false,false)

  val penaltyForUnrouted  = 10000

  val obj = Objective(routingDistance(0) + (penaltyForUnrouted*(n - length(routes))))


  val closestNeighboursForward = Array.tabulate(n)(DistanceHelper.lazyClosestPredecessorsOfNode(symmetricDistance, (_) => nodes))

  def size = routes.value.size

  //TODO: how about using NextAndPRev trait? or use a clone of route?
  val (next,prev) = routeSuccessorAndPredecessors(routes,v,n)()

  override def toString : String = super.toString +
    "objective: " + obj.value + "\n" +
    "next:" + next.map(_.value).mkString(",") + "\n" +
    "prev:" + prev.map(_.value).mkString(",") + "\n"
}

object TestNext extends App{

  val n = 200
  val v = 10

  val maxPivotPerValuePercent = 4

  println("VRP(n:" + n + " v:" + v + ")")

  val (symmetricDistanceMatrix,pointsList) = RoutingMatrixGenerator(n)

  val model = new Store(checker = Some(new ErrorChecker()))

  val myVRP = new MySimpleRoutingWithUnroutedPointsAndNext(n,v,symmetricDistanceMatrix,model,maxPivotPerValuePercent,pointsList)

  model.close()

  def routeUnroutedPoint =  profile(insertPointUnroutedFirst(myVRP.unrouted,()=>myVRP.kFirst(10,myVRP.closestNeighboursForward,(_) => myVRP.isRouted), myVRP,neighborhoodName = "InsertUF"))

  //TODO: using post-filters on k-nearest is probably crap
  val routeUnroutedPoint2 =  profile(insertPointRoutedFirst(myVRP.routed,()=>myVRP.kFirst(10,myVRP.closestNeighboursForward,(_) => x => !myVRP.isRouted(x)),myVRP,neighborhoodName = "InsertRF")  guard(() => myVRP.size < n/2))

  def onePtMove(k:Int) = profile(onePointMove(myVRP.routed, () => myVRP.kFirst(k,myVRP.closestNeighboursForward,(_) => myVRP.isRouted), myVRP))

  val customTwoOpt = profile(twoOpt(myVRP.routed, ()=>myVRP.kFirst(40,myVRP.closestNeighboursForward,(_) => myVRP.isRouted), myVRP))

  def customThreeOpt(k:Int, breakSym:Boolean) = profile(threeOpt(myVRP.routed, ()=>myVRP.kFirst(k,myVRP.closestNeighboursForward,(_) => myVRP.isRouted), myVRP,breakSymmetry = breakSym, neighborhoodName = "ThreeOpt(k=" + k + ")"))

  val vlsn1pt = profile(mu[OnePointMoveMove](
    onePointMove(myVRP.routed, () => myVRP.kFirst(5,myVRP.closestNeighboursForward,(_) => myVRP.isRouted),myVRP),
    l => Some(onePointMove(() => List(l.head.newPredecessor).filter(_ >= v), () => myVRP.kFirst(3,myVRP.closestNeighboursForward,(_) => myVRP.isRouted),myVRP, hotRestart = false)),
    intermediaryStops = true,
    maxDepth = 6))

  val search = bestSlopeFirst(List(routeUnroutedPoint2,
                                   routeUnroutedPoint,
                                   onePtMove(10),
                                   customTwoOpt,
                                   customThreeOpt(10,true),
                                   vlsn1pt,
                                   insertPointUnroutedFirst(myVRP.unrouted,
                                                            ()=>myVRP.kFirst(10,
                                                                             myVRP.closestNeighboursForward,
                                                                             (_) => myVRP.isRouted),
                                                                             myVRP,neighborhoodName = "InsertUF") andThen
                                   routeUnroutedPoint)) exhaust customThreeOpt(20,true)
  // afterMove(/*myVRP.drawRoutes()*/)

  search.verbose = 1

  print("Doing all moves ...")


  search.doAllMoves(obj = myVRP.obj)
  model.propagate()
  println(search.profilingStatistics)
}
*/