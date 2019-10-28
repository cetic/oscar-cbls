package oscar.examples.cbls.routing

import oscar.cbls._
import oscar.cbls.business.routing._
import oscar.cbls.business.routing.invariants.group.{GlobalConstraintCore, RouteLength}
import oscar.cbls.business.routing.invariants.timeWindow.{TimeWindowConstraint, TimeWindowConstraintWithLogReduction}
import oscar.cbls.core.computation.FullRange
import oscar.cbls.core.objective.CascadingObjective
import oscar.cbls.core.search.{Best, First}

object VRPWithOnlyTimeWindow extends App {

  def runConfiguration(ns: List[Long], vs: List[Long],
                       timeWindowConstraints: List[Int],
                       bests: List[Boolean], procedures: List[Int],
                       iterations: Int): Unit ={
    for(twc <- timeWindowConstraints){
      println("================================================")
      println(if(twc == 0) "Using old invariant ..." else if(twc == 1) "Using Global Constraint ..." else "Using Global Constraint With Log Reduction ...")
      for(best <- bests){
        println("------------------------------------------------")
        println(if(best) "\n\nStarting best mod !" else "Starting first mod !")
        for(procedure <- procedures){
          procedure match {
            case 1 => println("\n++++++ Starting insertPoint procedure ++++++")
            case 2 => println("\n++++++ Starting insertPoint exhaust onePtMove procedure ++++++")
            case 3 => println("\n++++++ Starting insertPoint exhaust threeOpt procedure ++++++")
            case 4 => println("\n++++++ Starting insertPoint exhaust mu[OnePtMove] procedure ++++++")
          }
          for(n <- ns){
            for(v <- vs){
              println("Run with " + v + " vehicles and " + n + " nodes.")
              val res = List.tabulate(iterations)(it =>
                procedure match {
                  case 1 => new VRPWithOnlyTimeWindow(twc, n, v, iteration = it).run1(best)
                  case 2 => new VRPWithOnlyTimeWindow(twc, n, v, iteration = it).run2(best)
                  case 3 => new VRPWithOnlyTimeWindow(twc, n, v, iteration = it).run3(best)
                  case 4 => new VRPWithOnlyTimeWindow(twc, n, v, iteration = it).run4(best)
                }).
                foldLeft(Array[Long](0,0,0,0,0,0,0,0,0,0,0,0))((acc,item) =>
                  Array(acc(0) + item._1,acc(1) + item._2,
                    acc(2) + item._3,acc(3) + item._4,
                    acc(4) + item._5,acc(5) + item._6,
                    acc(6) + item._7,acc(7) + item._8,
                    acc(8) + item._9,acc(9) + item._10)).toList.map(_/iterations)
              println("Average total duration : " + res(1) + " - Average quality : " + res.head +
                "\nAverage total time in Notify : " + res(2) + " - COUNT : " + res(3) +
                "\nAverage total time in PreComputation : " + res(4) + " - COUNT : " + res(5) +
                "\nAverage total time in VehicleValueComputation : " + res(6) + " - COUNT : " + res(7) +
                "\nAverage total time in Assignation : " + res(8) + " - COUNT : " + res(9) + "\n")
            }
          }
        }
      }
    }
  }

  // 0 == old constraint, 1 == New TimeWindow constraint, 2 == New TimeWindow constraint with log reduction
  val timeWindowConstraints = List(1)
  // Add true if you want to run with Best and/or false if you want to run with First
  val bests = List(false)
  // Add the procedures you want (see at the end of this files for more informations)
  val procedures = List(1,2)
  // The variations of n values
  val ns_1 = List(100L, 200L, 300L, 400L, 500L, 600L, 700L, 800L, 900L, 1000L)
  val ns_2 = List(1000L)
  // The variations of v values
  val vs_1 = List(10L)
  val vs_2 = List(10L, 20L, 30L, 40L, 50L, 60L, 70L, 80L, 90L, 100L)
  //val vs_2 = List(10)
  // The number of iterations of each configuration
  val iterations = 50
  runConfiguration(ns_1,vs_1,timeWindowConstraints,bests, procedures,iterations)
  println("\n\n\n\n\n\n\n#####################################################\n\n\n\n\n\n")
  runConfiguration(ns_2,vs_2,timeWindowConstraints,bests, procedures,iterations)
}

class VRPWithOnlyTimeWindow(version: Long, n: Long = 100, v: Long = 10, fullInfo: Boolean = false, iteration: Int = 0){
  val m = new Store(noCycle = false, propagateOnToString = false/*, checker = Some(new ErrorChecker())*/)
  val penaltyForUnrouted = 10000
  RoutingMatrixGenerator.random.setSeed(iteration)
  val symmetricDistance = RoutingMatrixGenerator.apply(n)._1
  val travelDurationMatrix = RoutingMatrixGenerator.generateLinearTravelTimeFunction(n,symmetricDistance)
  var (earliestArrivalTimes, latestLeavingTimes, taskDurations, maxWaitingDurations) = RoutingMatrixGenerator.generateFeasibleTimeWindows(n,v,travelDurationMatrix)
  latestLeavingTimes = latestLeavingTimes.take(v).map(x => Math.min(latestLeavingTimes.drop(v).max*2, Long.MaxValue/10*9)) ++ latestLeavingTimes.drop(v)

  val myVRP =  new VRP(m,n,v)

  var globalConstraintWithLogReduc: Option[TimeWindowConstraintWithLogReduction] = None
  var globalConstraint: Option[TimeWindowConstraint] = None
  var routeLengthInvariant: Option[RouteLength] = None
  var cascadingObjective: CascadingObjective = null
  // Distance


  // This class isn't meant to last but given the time left I don't want to modify it.
  // It uses an old representation of time windows with earliestArrivalTimes and latestLeavingTimes.
  val timeWindowExtension = timeWindows(Some(earliestArrivalTimes), None, None, Some(latestLeavingTimes), taskDurations, None)

  // Defintion of the objective function using naive constraint or global contraint
  val obj: CascadingObjective =
    if(version == 0){
      val totalRouteLength = routeLength(myVRP.routes,n,v,false,symmetricDistance,true,true,false)(0)
      // Naive constraint
      val oldTimeWindowInvariant = forwardCumulativeIntegerIntegerDimensionOnVehicle(
        myVRP.routes,n,v,
        (fromNode,toNode,arrivalTimeAtFromNode,leaveTimeAtFromNode)=> {
          val arrivalTimeAtToNode = leaveTimeAtFromNode + travelDurationMatrix.getTravelDuration(fromNode,0,toNode)
          val leaveTimeAtToNode =
            if(toNode < v) 0
            else Math.max(arrivalTimeAtToNode,earliestArrivalTimes(toNode)) + taskDurations(toNode)
          (arrivalTimeAtToNode,leaveTimeAtToNode)
        },
        Array.tabulate(v)(x => new CBLSIntConst(0)),
        Array.tabulate(v)(x => new CBLSIntConst(earliestArrivalTimes(x)+taskDurations(x))),
        0,
        0,
        contentName = "Time at node"
      )
      val constraints = new ConstraintSystem(myVRP.routes.model)
      val arrivalTimes = oldTimeWindowInvariant.content1AtNode
      val leaveTimes = oldTimeWindowInvariant.content2AtNode
      val arrivalTimesAtEnd = oldTimeWindowInvariant.content1AtEnd

      // Verification of violations
      for(i <- 0 until n){
        if(i < v && latestLeavingTimes(i) != Long.MaxValue) {
          constraints.post(arrivalTimesAtEnd(i).le(latestLeavingTimes(i)).nameConstraint("end of time for vehicle " + i))
        } else {
          if(latestLeavingTimes(i) != Long.MaxValue)
            constraints.post(leaveTimes(i).le(latestLeavingTimes(i)).nameConstraint("end of time window on node " + i))
          if(maxWaitingDurations(i) != Long.MaxValue)
            constraints.post(arrivalTimes(i).ge(earliestArrivalTimes(i)).nameConstraint("start of time window on node (with duration)" + i))
        }
      }

      cascadingObjective = new CascadingObjective(constraints,
        totalRouteLength + (penaltyForUnrouted*(n - length(myVRP.routes))))
      cascadingObjective
    }
    else if(version == 1){
      val gc = GlobalConstraintCore(myVRP.routes, v)
      val vehicleToRouteLength = Array.fill(v)(CBLSIntVar(m, 0, FullRange))
      routeLengthInvariant = Some(new RouteLength(gc,n,v,vehicleToRouteLength,(from,to) => symmetricDistance(from)(to)))
      // Global constraint
      val violations = Array.fill(v)(new CBLSIntVar(m, 0, Domain.coupleToDomain((0,1))))
      val timeMatrix = Array.tabulate(n)(from => Array.tabulate(n)(to => travelDurationMatrix.getTravelDuration(from, 0, to)))
      val smartTimeWindowInvariant =
        TimeWindowConstraint(gc, n, v,
          timeWindowExtension.earliestArrivalTimes,
          timeWindowExtension.latestLeavingTimes,
          timeWindowExtension.taskDurations,
          timeMatrix, violations)
      globalConstraint = Some(smartTimeWindowInvariant)
      cascadingObjective = new CascadingObjective(sum(violations),
        sum(vehicleToRouteLength) + (penaltyForUnrouted*(n - length(myVRP.routes))))
      cascadingObjective
    } else {
      val gc = GlobalConstraintCore(myVRP.routes, v)
      val vehicleToRouteLength = Array.fill(v)(CBLSIntVar(m, 0, FullRange))
      routeLengthInvariant = Some(new RouteLength(gc,n,v,vehicleToRouteLength,(from,to) => symmetricDistance(from)(to)))
      // Global constraint with log reduction
      val violations = Array.fill(v)(new CBLSIntVar(m, 0, Domain.coupleToDomain((0,1))))
      val timeMatrix = Array.tabulate(n)(from => Array.tabulate(n)(to => travelDurationMatrix.getTravelDuration(from, 0, to)))
      val smartTimeWindowInvariant =
        TimeWindowConstraintWithLogReduction(gc, n, v,
          timeWindowExtension.earliestArrivalTimes,
          timeWindowExtension.latestLeavingTimes,
          timeWindowExtension.taskDurations,
          timeMatrix, violations)
      globalConstraintWithLogReduc = Some(smartTimeWindowInvariant)
      cascadingObjective = new CascadingObjective(sum(violations),
        sum(vehicleToRouteLength) + (penaltyForUnrouted*(n - length(myVRP.routes))))
      cascadingObjective
    }
  m.close()

  // Building the relevant predecessors of each node based on time window
  val relevantPredecessorsOfNodes: Map[Long,Set[Long]] = TimeWindowHelper.relevantPredecessorsOfNodes(myVRP, timeWindowExtension, travelDurationMatrix)

  // A post filter that prevents insertion after unrouted nodes
  def postFilter(node:Long): (Long) => Boolean = {
    (neighbor: Long) => {
      myVRP.isRouted(neighbor)
    }
  }

  // Given the relevant predecessors we sort them by distance
  val closestRelevantPredecessorsByDistance = Array.tabulate(n)(DistanceHelper.lazyClosestPredecessorsOfNode(symmetricDistance,relevantPredecessorsOfNodes(_))(_))

  // InsertPoint neighborhood
  def insertPoint(best: Boolean) = insertPointUnroutedFirst(
    () => myVRP.unrouted.value,
    ()=> myVRP.kFirst(if (best) n else 20,closestRelevantPredecessorsByDistance(_), postFilter),
    myVRP,
    selectInsertionPointBehavior = if(best) Best() else First(),
    neighborhoodName = "InsertUF")

  // OnePointMove neighborhood
  def onePtMove(best: Boolean) = onePointMove(
    () => myVRP.routed.value,
    ()=> myVRP.kFirst(if (best) n else 20,closestRelevantPredecessorsByDistance(_),postFilter),
    myVRP,
    selectDestinationBehavior = if(best) Best() else First(),

    neighborhoodName = "OnePtMove")

  // ThreeOpt neighborhood
  def threeOptMove(best: Boolean) = threeOpt(
    myVRP.routed,
    ()=>myVRP.kFirst(if (best) n else 20,closestRelevantPredecessorsByDistance(_),postFilter),
    myVRP,
    neighborhoodName = "ThreeOpt(k=" + v*2 + ")",
    selectMovedSegmentBehavior = if(best) Best() else First(),
    selectInsertionPointBehavior = if(best) Best() else First(),
    selectFlipBehavior = if(best) Best() else First())

  def nextMoveGenerator(best: Boolean) = {
    (exploredMoves:List[OnePointMoveMove]) => {
      val moveNeighborhood = onePtMove(best)
      Some(moveNeighborhood)
    }
  }

  def muOnePtMove(depth: Int, best: Boolean) =
    mu[OnePointMoveMove](
      onePtMove(best),
      nextMoveGenerator(best),
      depth,
      true)

  // Simple InsertPoint procedure
  def run1(best: Boolean): (Long,Long,Long,Long,Long,Long,Long,Long,Long,Long) ={
    val search = insertPoint(best)
    //search.verbose = 1
    val start = System.nanoTime()
    search.doAllMoves(obj=obj)
    val end = System.nanoTime()
    val duration = ((end - start)/1000000).toInt
    /*if(globalConstraint.isDefined)
      (obj.value, duration,
        globalConstraint.get.notifyTime/1000000, globalConstraint.get.notifyCount,
        (globalConstraint.get.preComputeTime + routeLengthInvariant.get.preComputeTime) /1000000, globalConstraint.get.preComputeCount + routeLengthInvariant.get.preComputeCount,
        (globalConstraint.get.computeValueTime + routeLengthInvariant.get.computeValueTime)/1000000, globalConstraint.get.computeValueCount + routeLengthInvariant.get.computeValueCount,
        (globalConstraint.get.assignTime + routeLengthInvariant.get.assignTime)/1000000, globalConstraint.get.assignCount + routeLengthInvariant.get.assignCount
      )
    else if(globalConstraintWithLogReduc.isDefined)
      (obj.value, duration,
        globalConstraintWithLogReduc.get.notifyTime/1000000, globalConstraintWithLogReduc.get.notifyCount,
        (globalConstraint.get.preComputeTime + routeLengthInvariant.get.preComputeTime) /1000000, globalConstraint.get.preComputeCount + routeLengthInvariant.get.preComputeCount,
        (globalConstraint.get.computeValueTime + routeLengthInvariant.get.computeValueTime)/1000000, globalConstraint.get.computeValueCount + routeLengthInvariant.get.computeValueCount,
        (globalConstraint.get.assignTime + routeLengthInvariant.get.assignTime)/1000000, globalConstraint.get.assignCount + routeLengthInvariant.get.assignCount
      )
    else*/
      (obj.value, duration,0,0,0,0,0,0,0,0)
  }

  // Simple InsertPoint exhaust OnePoitnMove procedure
  def run2(best: Boolean): (Long,Long,Long,Long,Long,Long,Long,Long,Long,Long) ={
    val search = insertPoint(best) exhaust onePtMove(best)
    //search.verbose = 2
    val start = System.nanoTime()
    search.doAllMoves(obj=obj)
    val end = System.nanoTime()
    val duration = ((end - start)/1000000).toInt
    /*if(globalConstraint.isDefined)
      (obj.value, duration,
        globalConstraint.get.notifyTime/1000000, globalConstraint.get.notifyCount,
        (globalConstraint.get.preComputeTime + routeLengthInvariant.get.preComputeTime) /1000000, globalConstraint.get.preComputeCount + routeLengthInvariant.get.preComputeCount,
        (globalConstraint.get.computeValueTime + routeLengthInvariant.get.computeValueTime)/1000000, globalConstraint.get.computeValueCount + routeLengthInvariant.get.computeValueCount,
        (globalConstraint.get.assignTime + routeLengthInvariant.get.assignTime)/1000000, globalConstraint.get.assignCount + routeLengthInvariant.get.assignCount
      )
    else if(globalConstraintWithLogReduc.isDefined)
      (obj.value, duration,
        globalConstraintWithLogReduc.get.notifyTime/1000000, globalConstraintWithLogReduc.get.notifyCount,
        (globalConstraint.get.preComputeTime + routeLengthInvariant.get.preComputeTime) /1000000, globalConstraint.get.preComputeCount + routeLengthInvariant.get.preComputeCount,
        (globalConstraint.get.computeValueTime + routeLengthInvariant.get.computeValueTime)/1000000, globalConstraint.get.computeValueCount + routeLengthInvariant.get.computeValueCount,
        (globalConstraint.get.assignTime + routeLengthInvariant.get.assignTime)/1000000, globalConstraint.get.assignCount + routeLengthInvariant.get.assignCount
      )
    else*/
      (obj.value, duration,0,0,0,0,0,0,0,0)
  }

  // Simple InsertPoint exhaust ThreeOpt procedure
  def run3(best: Boolean): (Long,Long,Long,Long,Long,Long,Long,Long,Long,Long) ={
    val search = insertPoint(best) exhaust threeOptMove(best)
    //search.verbose = 2
    val start = System.nanoTime()
    search.doAllMoves(obj=obj)
    val end = System.nanoTime()
    val duration = ((end - start)/1000000).toInt
    /*/if(globalConstraint.isDefined)
      (obj.value, duration,
        globalConstraint.get.notifyTime/1000000, globalConstraint.get.notifyCount,
        (globalConstraint.get.preComputeTime + routeLengthInvariant.get.preComputeTime) /1000000, globalConstraint.get.preComputeCount + routeLengthInvariant.get.preComputeCount,
        (globalConstraint.get.computeValueTime + routeLengthInvariant.get.computeValueTime)/1000000, globalConstraint.get.computeValueCount + routeLengthInvariant.get.computeValueCount,
        (globalConstraint.get.assignTime + routeLengthInvariant.get.assignTime)/1000000, globalConstraint.get.assignCount + routeLengthInvariant.get.assignCount
      )
    else if(globalConstraintWithLogReduc.isDefined)
      (obj.value, duration,
        globalConstraintWithLogReduc.get.notifyTime/1000000, globalConstraintWithLogReduc.get.notifyCount,
        (globalConstraint.get.preComputeTime + routeLengthInvariant.get.preComputeTime) /1000000, globalConstraint.get.preComputeCount + routeLengthInvariant.get.preComputeCount,
        (globalConstraint.get.computeValueTime + routeLengthInvariant.get.computeValueTime)/1000000, globalConstraint.get.computeValueCount + routeLengthInvariant.get.computeValueCount,
        (globalConstraint.get.assignTime + routeLengthInvariant.get.assignTime)/1000000, globalConstraint.get.assignCount + routeLengthInvariant.get.assignCount
      )
    else*/
      (obj.value, duration,0,0,0,0,0,0,0,0)
  }

  def run4(best: Boolean): (Long,Long,Long,Long,Long,Long,Long,Long,Long,Long) ={
    val search = insertPoint(best) exhaust muOnePtMove(2,best)
    search.verbose = 0
    val start = System.nanoTime()
    search.doAllMoves(obj=obj)
    val end = System.nanoTime()
    val duration = ((end - start)/1000000).toInt
    /*if(globalConstraint.isDefined)
      (obj.value, duration,
        globalConstraint.get.notifyTime/1000000, globalConstraint.get.notifyCount,
        (globalConstraint.get.preComputeTime + routeLengthInvariant.get.preComputeTime) /1000000, globalConstraint.get.preComputeCount + routeLengthInvariant.get.preComputeCount,
        (globalConstraint.get.computeValueTime + routeLengthInvariant.get.computeValueTime)/1000000, globalConstraint.get.computeValueCount + routeLengthInvariant.get.computeValueCount,
        (globalConstraint.get.assignTime + routeLengthInvariant.get.assignTime)/1000000, globalConstraint.get.assignCount + routeLengthInvariant.get.assignCount
      )
    else if(globalConstraintWithLogReduc.isDefined)
      (obj.value, duration,
        globalConstraintWithLogReduc.get.notifyTime/1000000, globalConstraintWithLogReduc.get.notifyCount,
        (globalConstraint.get.preComputeTime + routeLengthInvariant.get.preComputeTime) /1000000, globalConstraint.get.preComputeCount + routeLengthInvariant.get.preComputeCount,
        (globalConstraint.get.computeValueTime + routeLengthInvariant.get.computeValueTime)/1000000, globalConstraint.get.computeValueCount + routeLengthInvariant.get.computeValueCount,
        (globalConstraint.get.assignTime + routeLengthInvariant.get.assignTime)/1000000, globalConstraint.get.assignCount + routeLengthInvariant.get.assignCount
      )
    else*/
      (obj.value, duration,0,0,0,0,0,0,0,0)
  }
}
