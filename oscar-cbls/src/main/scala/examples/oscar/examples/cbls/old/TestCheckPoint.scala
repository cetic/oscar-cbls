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

/**
  * Created by f.germeau on 07/09/2016.
  */
class MyVRP(n:Int,v:Int,model:Store,symmetricDistanceMatrix:Array[Array[Long]]) extends VRP(model,n,v){

  val routingDistance = routeLength(routes,n,v,false,symmetricDistanceMatrix,true,false,false)

  val penaltyForUnrouted = 10000

  val obj = Objective(routingDistance(0) + (penaltyForUnrouted*(n - length(routes))))

  val closestNeighborsForward = Array.tabulate(n)(DistanceHelper.lazyClosestPredecessorsOfNode(symmetricDistanceMatrix, (_) => nodes)(_))
}

object TestCheckPoint extends App{

  val n = 500
  val v = 10
  val maxMovesBeforeRollBack = 100

  println("VRP(n:" + n + " v:" + v + ")")

  val (symmetricDistanceMatrix,pointsList) = RoutingMatrixGenerator(n)

  val model = Store(noCycle = false, checker = Some(new ErrorChecker))

  val myVRP = new MyVRP(n,v,model,symmetricDistanceMatrix)

  model.close()

  val insertPoint = profile(insertPointUnroutedFirst(
    unroutedNodesToInsert = () => myVRP.unroutedNodes,
    relevantPredecessor = () => myVRP.kFirst(n/10,myVRP.closestNeighborsForward(_),(_) => myVRP.isRouted),
    vrp = myVRP
  ))

  val customOnePointMove = profile(onePointMove(
    nodesToMove = myVRP.routed,
    relevantNewPredecessors = () => myVRP.kFirst(n/10, myVRP.closestNeighborsForward(_),(_) => myVRP.isRouted),
    vrp = myVRP
  ))

  val customTwoOpt = profile(twoOpt(
    segmentStartValues = myVRP.routed,
    relevantNewSuccessors = () => myVRP.kFirst(n/10, myVRP.closestNeighborsForward(_),(_) => myVRP.isRouted),
    vrp = myVRP
  ))

  val customThreeOpt = profile(threeOpt(
    potentialInsertionPoints = myVRP.routed,
    relevantNeighbors = () => myVRP.kFirst(20, myVRP.closestNeighborsForward(_),(_) => myVRP.isRouted),
    vrp = myVRP
  ))

  val doubleInsert = profile(dynAndThen(
    insertPointUnroutedFirst(
      unroutedNodesToInsert = () => myVRP.unroutedNodes,
      relevantPredecessor = () => myVRP.kFirst(n/10,myVRP.closestNeighborsForward(_),(_) => myVRP.isRouted),
      vrp= myVRP
    ), (moveResult:InsertPointMove)=>
      insertPointUnroutedFirst(
        unroutedNodesToInsert = () => myVRP.unroutedNodes,
        relevantPredecessor = () => myVRP.kFirst(n/10,myVRP.closestNeighborsForward(_),(_) => myVRP.isRouted),
        vrp= myVRP
      )
  ))

  //For  test purpose, we create an additional checkpoint here.
  var checkPoint = myVRP.routes.defineCurrentValueAsCheckpoint(true)
  var movesBeforeRollBack = 1
  var moves = 0

  val search = profile(bestSlopeFirst(List(insertPoint,customOnePointMove,customTwoOpt,customThreeOpt,doubleInsert))).afterMove({
    if(moves == movesBeforeRollBack && movesBeforeRollBack < maxMovesBeforeRollBack) {
      myVRP.routes.rollbackToTopCheckpoint(checkPoint)
      movesBeforeRollBack += 1
      moves = 0
    }
    moves += 1
  })

  //TODO : Understand why it crashes when setting verbose to 1
  search.verbose = 2
  //search.verboseWithExtraInfo(1, ()=> "" + myVRP)

  print("Doing all moves ...")


  search.doAllMoves(obj = myVRP.obj)
  model.propagate()
  println(search.profilingStatistics)
}
*/