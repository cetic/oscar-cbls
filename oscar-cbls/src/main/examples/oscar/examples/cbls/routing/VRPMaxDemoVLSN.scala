package oscar.examples.cbls.routing

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

import oscar.cbls._
import oscar.cbls.business.routing._
import oscar.cbls.lib.search.neighborhoods.vlsn.{CycleFinderAlgoType, VLSN}
import oscar.cbls.core.search._
import oscar.cbls.util.StopWatch

import scala.collection.immutable.{SortedMap, SortedSet}

//50.404631, 4.452595
//50.415162, 4.440849


object VRPMaxDemoVLSN  extends App {

  println("usage: VRPMaxDemo n v")
  val n:Int=args(0).toInt
  val v = args(1).toInt
  println(s"VRPMaxDemoVLSN(n:$n, v:$v)")

  require(v < n)
  val displayDelay = if (n >= 1000) 1500 else 500 //ms
  val verbose = 1
  val maxPivotPerValuePercent = 5 //VLSN generates a lot of additional pivots
  val mapSide = 1000

  new VRPMaxDemoVLSN(n,v,maxPivotPerValuePercent,verbose,displayDelay, mapSide)
}

class VRPMaxDemoVLSN (n:Int, v:Int, maxPivotPerValuePercent:Int, verbose:Int, displayDelay:Int, mapSide:Int) extends StopWatch{

  val (symmetricDistanceMatrix1,nodesPositions) = RoutingMatrixGenerator.apply(n,1000)

  val symmetricDistanceMatrix = Array.tabulate(n)({a =>
    Array.tabulate[Long](n)({b =>
      symmetricDistanceMatrix1(a min b)(a max b)
    })})

  val maxWorkloadPerVehicle = 2500
  val serviceTimePerNode = 100
  val vehicles = 0 until v

  //val maxWorkloadPerVehicle = 4000
  //val serviceTimePerNode = 100

  startWatch()
  val model = new Store() //checker = Some(new ErrorChecker()))

  val myVRP = new VRP(model,n,v)
  val routeLengthPerVehicle = routeLength(myVRP.routes,n,v,perVehicle = true,symmetricDistanceMatrix,true)
  val totalRouteLength = sum(routeLengthPerVehicle)
  val nodesPerVehicle = nodesOfVehicle(myVRP.routes,v)

  val totalServiceTimePerVehicle = nodesPerVehicle.map(cardinality(_)*serviceTimePerNode)

  val c = new ConstraintSystem(model)

  val vehicletoWorkload = Array.tabulate(v)(
    vehicle => totalServiceTimePerVehicle(vehicle) + routeLengthPerVehicle(vehicle)
  )

  val vehicleToWorkloadConsraint = Array.tabulate(v)(
    vehicle => {
      val constr = vehicletoWorkload(vehicle) le maxWorkloadPerVehicle
      c.add(constr)
      constr
    }
  )

  c.close()

  val penaltyForUnrouted  = 10000

  val objPerVehicle = Array.tabulate[Objective](v)(vehicle =>
    new CascadingObjective(
      Objective(vehicleToWorkloadConsraint(vehicle).violation),
      Objective(routeLengthPerVehicle(vehicle)))
  )

  val unroutedPenaltyObj = Objective(penaltyForUnrouted*(n - length(myVRP.routes)))

  val obj = new CascadingObjective(
    c,
    Objective(totalRouteLength + unroutedPenaltyObj.objective)
  )

  model.close()


  def result: String =
    myVRP.toString +
      vehicles.map(vehicle => "workload_vehicle_" + vehicle + ": " + (100*vehicletoWorkload(vehicle).value.toDouble / maxWorkloadPerVehicle).ceil.toInt + " %" ).mkString("\n") + "\n" +obj

  val relevantPredecessorsOfNodes = (node:Int) => myVRP.nodes
  val closestRelevantNeighborsByDistance = Array.tabulate(n)((node:Int) => DistanceHelper.lazyClosestPredecessorsOfNode(symmetricDistanceMatrix,relevantPredecessorsOfNodes)(node))

  val routedPostFilter = (node:Int) => (neighbor:Int) => myVRP.isRouted(neighbor)
  val unRoutedPostFilter = (node:Int) => (neighbor:Int) => !myVRP.isRouted(neighbor)

  def vlsn(l:Int = Int.MaxValue) = {

    val lClosestNeighborsByDistance: Array[SortedSet[Int]] = Array.tabulate(n)(node =>
      SortedSet.empty[Int] ++ myVRP.kFirst(l, (node:Int) => closestRelevantNeighborsByDistance(node))(node))

    //VLSN neighborhood
    val nodeToAllVehicles = SortedMap.empty[Int, Iterable[Int]] ++ (v until n).map(node => (node:Int, vehicles))

    def routeUnroutedPointVLSN(targetVehicle: Int):(Int => Neighborhood) = {
      if(vehicletoWorkload(targetVehicle).value + serviceTimePerNode > maxWorkloadPerVehicle){
        (_ => NoMoveNeighborhood)
      }else {
        val nodesOfTargetVehicle = myVRP.getRouteOfVehicle(targetVehicle)

        unroutedNodeToInsert:Int => {
          val lNearestNodesOfTargetVehicle = nodesOfTargetVehicle.filter(x => lClosestNeighborsByDistance(unroutedNodeToInsert) contains x)
          insertPointUnroutedFirst(
            () => List(unroutedNodeToInsert),
            () => _ => lNearestNodesOfTargetVehicle,
            myVRP,
            hotRestart = false,
            selectInsertionPointBehavior = Best(),
            positionIndependentMoves = true //compulsory because we are in VLSN
          )
        }
      }
    }

    //targetVehicleNodeToMoveNeighborhood:Int => Int => Neighborhood,
    def movePointVLSN(targetVehicle: Int):(Int => Neighborhood) = {
      if(vehicletoWorkload(targetVehicle).value + serviceTimePerNode > maxWorkloadPerVehicle){
        (_ => NoMoveNeighborhood)
      }else {
        val nodesOfTargetVehicle = myVRP.getRouteOfVehicle(targetVehicle)

        nodeToMove:Int => {
          val lNearestNodesOfTargetVehicle = nodesOfTargetVehicle.filter(x => lClosestNeighborsByDistance(nodeToMove) contains x)
          onePointMove(
            () => List(nodeToMove),
            () => _ => lNearestNodesOfTargetVehicle,
            myVRP,
            selectDestinationBehavior = Best(),
            hotRestart = false,
            positionIndependentMoves = true  //compulsory because we are in VLSN
          )
        }
      }
    }

    def removePointVLSN(node: Int) =
      removePoint(
        () => List(node),
        myVRP,
        positionIndependentMoves = true,
        hotRestart = false)

    //for re-optimization
    def threeOptOnVehicle(vehicle:Int) = {
      val nodesOfTargetVehicle = myVRP.getRouteOfVehicle(vehicle)
      //insertions points are position where we perform the insert,
      // basically the segment will start in plae of the insertion point and the insertion point will be moved upward
      val nodesOfTargetVehicleButVehicle = nodesOfTargetVehicle.filter(_ != vehicle)

      threeOpt(() => nodesOfTargetVehicle,
        () => _ => nodesOfTargetVehicleButVehicle,
        myVRP,
        breakSymmetry = false).afterMove(graphical.drawRoutes())
    }

    def removeAndReInsertVLSN(pointToRemove: Int): (() => Unit) = {
      val checkpointBeforeRemove = myVRP.routes.defineCurrentValueAsCheckpoint(true)
      require(pointToRemove >= v, "cannot remove vehicle point: " + v)

      myVRP.routes.value.positionOfAnyOccurrence(pointToRemove) match {
        case None => throw new Error("cannot remove non routed point:" + pointToRemove)
        case Some(positionOfPointToRemove) =>
          myVRP.routes.remove(positionOfPointToRemove)
      }

      def restoreAndRelease: (() => Unit) = () => {
        myVRP.routes.rollbackToTopCheckpoint(checkpointBeforeRemove)
        myVRP.routes.releaseTopCheckpoint()
      }

      restoreAndRelease
    }

    new VLSN(
      v,
      () => SortedMap.empty[Int, SortedSet[Int]] ++
        vehicles.map((vehicle: Int) =>
          (vehicle:Int, SortedSet.empty[Int] ++ myVRP.getRouteOfVehicle(vehicle).filter(_ >= v))),
      () => SortedSet.empty[Int] ++ myVRP.unroutedNodes,
      nodeToRelevantVehicles = () => nodeToAllVehicles,

      targetVehicleNodeToInsertNeighborhood = routeUnroutedPointVLSN,
      targetVehicleNodeToMoveNeighborhood = movePointVLSN,
      removePointVLSN,

      removeNodeAndReInsert = removeAndReInsertVLSN,

      reOptimizeVehicle = Some(vehicle => Some(threeOptOnVehicle(vehicle))),

      objPerVehicle,
      unroutedPenaltyObj,
      obj,

      cycleFinderAlgoSelection = CycleFinderAlgoType.Mouthuy,

      //debugNeighborhoodExploration = true,
      name="VLSN(" + l + ")"
    )
  }

  val routeUnroutedPoint =  insertPointUnroutedFirst(myVRP.unrouted,
    ()=>myVRP.kFirst(10,closestRelevantNeighborsByDistance(_),routedPostFilter),
    myVRP,
    neighborhoodName = "InsertUF",
    hotRestart = false,
    selectNodeBehavior = First(),
    selectInsertionPointBehavior = Best())

  def onePtMove(k:Int) = onePointMove(
    myVRP.routed,
    () => myVRP.kFirst(k,closestRelevantNeighborsByDistance(_),routedPostFilter),
    myVRP,
    selectDestinationBehavior = Best())

  def customTwoOpt(k:Int=10) = twoOpt(myVRP.routed, ()=>myVRP.kFirst(k,closestRelevantNeighborsByDistance(_),routedPostFilter), myVRP)

  def customThreeOpt(k:Int, breakSym:Boolean) =
    threeOpt(myVRP.routed, ()=>myVRP.kFirst(k,closestRelevantNeighborsByDistance(_),routedPostFilter), myVRP,breakSymmetry = breakSym, neighborhoodName = "ThreeOpt(k=" + k + ")")

  def segExchange(k:Int) = segmentExchange(myVRP,()=>myVRP.kFirst(k,closestRelevantNeighborsByDistance(_),routedPostFilter), () => myVRP.vehicles)

  val graphical = display(myVRP,
    nodesPositions.map(np => (np._1.toDouble,np._2.toDouble)),
    sizeOfMap = Some(mapSide),
    refreshRate = displayDelay,
    title = "VRPMaxDemoVLSN(n=" + n + " v=" + v + ")")

  val search = (bestSlopeFirst(List(
    profile(routeUnroutedPoint),
    profile(onePtMove(10)),
    profile(customTwoOpt(20)),
    profile(customThreeOpt(20,true))
  )) exhaust (profile(vlsn(80)) maxMoves 1 exhaustBack bestSlopeFirst(List(segExchange(40),customTwoOpt(30))))) .afterMove(graphical.drawRoutes()).showObjectiveFunction(obj)

  search.verbose = 2
  //search.verboseWithExtraInfo(2, () => result)

  search.doAllMoves(obj = obj)

  println(result)

  println(search.profilingStatistics)

  graphical.drawRoutes(force = true)
}
