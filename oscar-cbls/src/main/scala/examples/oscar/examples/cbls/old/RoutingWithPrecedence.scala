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
import oscar.cbls.core.search.Best
import oscar.examples.cbls.routing.RoutingMatrixGenerator

import scala.collection.immutable.SortedSet

class MySimpleRoutingP(n:Int,v:Int,symmetricDistance:Array[Array[Int]],m:Store, maxPivot:Int, precedences:List[(Int,Int)])
  extends VRP(m,n,v,maxPivot){

  //initializes to something simple; vehicle v-1 does all nodes (but other vehicles)
  // Fabian : Don't really understand the utility of the setCircuit.
  // Obviously, when set like this, the routes violate strong constraints => the objectif value is MaxInt => no way to optimize)
  setCircuit(nodes)

  val routingDistance = constantRoutingDistance(routes,n,v,false,symmetricDistance,true,false,false)

  val precedenceInvar = precedence(routes,precedences)

  val nodesStartingAPrecedence:SortedSet[Int] = precedenceInvar.nodesStartingAPrecedence
  val nodesEndingAPrecedenceStartedAt:(Int => Iterable[Int]) = precedenceInvar.nodesEndingAPrecedenceStartedAt

  val obj = new CascadingObjective(new IntVarObjective(precedenceInvar), routingDistance(0))
  //val obj = Objective(totalDistance)

  override def toString : String = super.toString +
    "precedences: " + precedences + "\n" +
    "objective: " + obj  + "\n"

  val nearestForward:Array[Iterable[Int]] = Array.tabulate(n)(DistanceHelper.lazyClosestPredecessorsOfNode(symmetricDistance, (_) => nodes))
}

object RoutingWithPrecedence extends App{

  val n = 100
  val v = 1
  val nbPRecedences = (n - v) / 2

  val maxPivotPerValuePercent = 4

  println("VRPPrecedence(n:" + n + " v:" + v + ")")

  val symmetricDistanceMatrix = RoutingMatrixGenerator(n)._1

  val precedences = RoutingMatrixGenerator.generateChainsPrecedence(n,v,nbPRecedences)._2

  //  println("restrictions:" + restrictions)
  val model = new Store(checker = Some(new ErrorChecker())) //)) //checker = Some() //new ErrorChecker()))

  val myVRP = new MySimpleRoutingP(n,v,symmetricDistanceMatrix,model,maxPivotPerValuePercent,precedences)
  val nodes = myVRP.nodes

  model.close()

  println(myVRP)

  def onePtMove = profile(onePointMove(() => nodes, ()=>myVRP.kFirst(10,myVRP.nearestForward), myVRP))

  val twoPointMove = (onePointMove(() => nodes, ()=>myVRP.kFirst(40,myVRP.nearestForward), myVRP,"firstPointMove") andThen onePointMove(() => nodes, ()=>myVRP.kFirst(40,myVRP.nearestForward), myVRP,"secondPointMove")) name("TwoPointMove")
  val twoPointMoveSmart = profile(
    (onePointMove(
      () => myVRP.nodesStartingAPrecedence,
      ()=>myVRP.kFirst(40,myVRP.nearestForward),
      myVRP,
      "firstPointMove",
      selectPointToMoveBehavior= Best(),
      selectDestinationBehavior= Best())
      dynAndThen ((o:OnePointMoveMove) => {
      onePointMove(
        () => myVRP.nodesEndingAPrecedenceStartedAt(o.movedPoint),
        ()=>myVRP.kFirst(40,myVRP.nearestForward),
        myVRP,
        "secondPointMove",
        selectPointToMoveBehavior= Best(),
        selectDestinationBehavior=Best())})) name "SmartTwoPtMove" )

  val customTwoOpt = profile(twoOpt(() => nodes, ()=>myVRP.kFirst(40,myVRP.nearestForward), myVRP))

  def customThreeOpt(k:Int, breakSym:Boolean) = profile(threeOpt(() => nodes, ()=>myVRP.kFirst(k,myVRP.nearestForward), myVRP,breakSymmetry = breakSym, neighborhoodName = "ThreeOpt(k=" + k + ")"))

  //,,(onePtMove andThen onePtMove) name ("twoPointMove")
  val search = (
    bestSlopeFirst(List(customThreeOpt(10,true),onePtMove,customTwoOpt,twoPointMoveSmart),refresh = 10)
      exhaust bestSlopeFirst(List(customThreeOpt(20,true))))

  //val search = threeOpt(20,true)
  //search.verboseWithExtraInfo(2, ()=> "" + myVRP)
  search.verbose = 1

  println("obj : " + myVRP.obj.value)

  search.doAllMoves(obj=myVRP.obj)

  println("final propagation: ")
  model.propagate()
  println("done.")

  println(model.getPropagationElements.toList)
  model.propagate()

  println
  println(myVRP)
  println
  println(search.profilingStatistics)
}
*/