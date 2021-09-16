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

class MySimpleRouting(n:Int,v:Int,symmetricDistance:Array[Array[Long]],m:Store, maxPivot:Int)
  extends VRP(m,n,v,maxPivot) {

  //initializes to something simple; vehicle v-1 does all nodes (but other vehicles)
  //initialization must be done ASAP, to ensure that invariants will initialize straight based on this value
  setCircuit(nodes)

  val totalDistance = routeLength(routes, n, v ,false, symmetricDistance, true)(0)
  
  val obj = Objective(totalDistance)

  override def toString : String = super.toString + "objective: " + obj.value + "\n"

  val closestNeighboursForward = Array.tabulate(n)(DistanceHelper.lazyClosestPredecessorsOfNode(symmetricDistance, (_) => nodes)(_))
}

object TSPsym extends App{

  val n = 100
  val v = 1 //the script is really made for a single vehicle, as it initializes on a singel vehicle solution so 1 here should not be changed.

  val maxPivotPerValuePercent = 4

  println("VRP(n:" + n + " v:" + v + ")")

  val symmetricDistanceMatrix = RoutingMatrixGenerator(n)._1

  //  println("restrictions:" + restrictions)
  val model = new Store(checker = Some(new ErrorChecker()))

  val myVRP = new MySimpleRouting(n,v,symmetricDistanceMatrix,model,maxPivotPerValuePercent)
  val nodes = myVRP.nodes

  model.close()

  println(myVRP)

  val onePtMove = profile(onePointMove(() => nodes, ()=>myVRP.kFirst(40,myVRP.closestNeighboursForward(_)), myVRP))

  val customTwoOpt = profile(twoOpt(() => nodes, ()=>myVRP.kFirst(40,myVRP.closestNeighboursForward(_)), myVRP))

  def customThreeOpt(k:Int, breakSym:Boolean) = profile(threeOpt(() => nodes, ()=>myVRP.kFirst(k,myVRP.closestNeighboursForward(_)), myVRP,breakSymmetry = breakSym, neighborhoodName = "ThreeOpt(k=" + k + ")"))

  val search = bestSlopeFirst(List(onePtMove,customTwoOpt, customThreeOpt(10,true))) exhaust customThreeOpt(20,true)

  search.verbose = 1

  search.doAllMoves(obj=myVRP.obj)

  model.propagate()

  println
  println(myVRP)
  println
  println(search.profilingStatistics)
}
*/