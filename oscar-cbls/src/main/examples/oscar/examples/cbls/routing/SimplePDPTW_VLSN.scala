package oscar.examples.cbls.routing

import oscar.cbls._
import oscar.cbls.business.routing._
import oscar.cbls.business.routing.invariants.global.{GlobalConstraintCore, RouteLength}
import oscar.cbls.business.routing.invariants.vehicleCapacity.GlobalVehicleCapacityConstraint
import oscar.cbls.core.search.{Best, Neighborhood, NoMoveNeighborhood}
import oscar.cbls.lib.search.neighborhoods.vlsn.{CycleFinderAlgoType, VLSN}

import scala.collection.immutable.{HashSet, SortedMap, SortedSet}

/**
 * Created by fg on 12/05/17.
 */

object SimplePDPTW_VLSN extends App{
  val m = new Store(noCycle = false)
  val v = 10
  val n = 500

  println("VLSN(PDPTW) v:" + v +" n:" + n)
  val penaltyForUnrouted = 10000
  val maxVehicleCapacity = 8
  val minVehicleCapacity = 4
  val symmetricDistance = RoutingMatrixGenerator.apply(n)._1
  val (listOfChains,precedences) = RoutingMatrixGenerator.generateChainsPrecedence(n,v,(n-v)/2)
  val contentsFlow = RoutingMatrixGenerator.generateContentFlow(n,listOfChains,maxVehicleCapacity)
  val vehiclesCapacity = RoutingMatrixGenerator.generateVehiclesSize(v,maxVehicleCapacity,minVehicleCapacity)

  val myVRP =  new VRP(m,n,v)
  val vehicles = 0L until v

  val k = 10
  val l = 20

  // GC
  val gc = GlobalConstraintCore(myVRP.routes, v)

  // Distance
  val vehiclesRouteLength = Array.tabulate(v)(vehicle => CBLSIntVar(m, name = "Route length of vehicle " + vehicle))
  val routeLengthInvariant = new RouteLength(gc,n,v,vehiclesRouteLength,(from: Long, to: Long) => symmetricDistance(from)(to))

  //Chains
  val precedenceRoute = myVRP.routes.createClone()

  //TODO: we need a faster precedence constraint!
  val precedenceInvariant = precedence(precedenceRoute,precedences)
  val vehicleOfNodesNow = vehicleOfNodes(precedenceRoute,v)
  val precedencesConstraints = new ConstraintSystem(m)
  for(start <- precedenceInvariant.nodesStartingAPrecedence)
    precedencesConstraints.add(vehicleOfNodesNow(start) === vehicleOfNodesNow(precedenceInvariant.nodesEndingAPrecedenceStartedAt(start).head))
  precedencesConstraints.add(0 === precedenceInvariant)
  val chainsExtension = chains(myVRP,listOfChains)

  // Vehicle content
  val violationOfContentOfVehicle = Array.tabulate(v)(vehicle =>
    CBLSIntVar(myVRP.routes.model, name = "Violation of capacity of vehicle " + vehicle))
  val capacityInvariant = GlobalVehicleCapacityConstraint(gc, n, v, vehiclesCapacity, contentsFlow, violationOfContentOfVehicle)

  //Objective function
  val unroutedPenalty = penaltyForUnrouted*(n - length(myVRP.routes))

  val obj = new CascadingObjective(precedencesConstraints,
    new CascadingObjective(sum(violationOfContentOfVehicle),
      sum(vehiclesRouteLength) + unroutedPenalty))

  val objPerVehicle = Array.tabulate[Objective](v)(vehicle =>
    new CascadingObjective(
      new CascadingObjective(precedencesConstraints,violationOfContentOfVehicle(vehicle)),
      Objective(vehiclesRouteLength(vehicle)))
  )
  val unroutedPenaltyOBj = Objective(unroutedPenalty)

  m.close()

  val relevantPredecessors = GlobalVehicleCapacityConstraint.relevantPredecessorsOfNodes(capacityInvariant)

  val closestRelevantPredecessorsByDistance = Array.tabulate(n)(DistanceHelper.lazyClosestPredecessorsOfNode(symmetricDistance,relevantPredecessors)(_))

  // MOVING

  val nextMoveGenerator = {
    (exploredMoves:List[OnePointMoveMove], t:Option[List[Long]]) => {
      val chainTail: List[Long] = t match {
        case None =>
          val movedNode = exploredMoves.head.movedPoint
          chainsExtension.nextNodesInChain(chainsExtension.firstNodeInChainOfNode(movedNode))
        case Some(tail: List[Long]) => tail
      }

      chainTail match {
        case Nil => None
        case head :: Nil => None
        case nextNodeToMove :: newTail =>
          val moveNeighborhood = onePointMove(() => Some(nextNodeToMove),
            () => ChainsHelper.computeRelevantNeighborsForInternalNodes(myVRP,chainsExtension), myVRP)
          Some(moveNeighborhood, Some(newTail))
      }
    }
  }

  val firstNodeOfChainMove = onePointMove(
    () => myVRP.routed.value.filter(chainsExtension.isHead),
    ()=> myVRP.kFirst(k,closestRelevantPredecessorsByDistance(_)), myVRP,neighborhoodName = "MoveHeadOfChain")

  def lastNodeOfChainMove(lastNode:Long) = onePointMove(
    () => List(lastNode),
    ()=> myVRP.kFirst(k,
      ChainsHelper.relevantNeighborsForLastNodeAfterHead(
        myVRP,
        chainsExtension,
        Some(HashSet() ++ relevantPredecessors(lastNode)))),
    myVRP,
    neighborhoodName = "MoveLastOfChain")

  val oneChainMove = {
    dynAndThen(firstNodeOfChainMove,
      (moveMove: OnePointMoveMove) => {
        mu[OnePointMoveMove, Option[List[Long]]](
          lastNodeOfChainMove(chainsExtension.lastNodeInChainOfNode(moveMove.movedPoint)),
          nextMoveGenerator,
          None,
          Long.MaxValue,
          false)
      }) name "OneChainMove"
  }

  def onePtMove(k:Long) = profile(onePointMove(myVRP.routed, () => myVRP.kFirst(k,closestRelevantPredecessorsByDistance(_)), myVRP))

  // INSERTING

  val nextInsertGenerator = {
    (exploredMoves:List[InsertPointMove], t:Option[List[Long]]) => {
      val chainTail: List[Long] = t match {
        case None =>
          val insertedNode = exploredMoves.head.insertedPoint
          chainsExtension.nextNodesInChain(chainsExtension.firstNodeInChainOfNode(insertedNode))
        case Some(tail: List[Long]) => tail
      }

      chainTail match {
        case Nil => None
        case head :: Nil => None
        case nextNodeToInsert :: newTail =>
          val insertNeighborhood = insertPointUnroutedFirst(() => Some(nextNodeToInsert),
            () => ChainsHelper.computeRelevantNeighborsForInternalNodes(myVRP,chainsExtension), myVRP)
          Some(insertNeighborhood, Some(newTail))
      }
    }
  }

  val firstNodeOfChainInsertion = insertPointUnroutedFirst(() => myVRP.unrouted.value.filter(chainsExtension.isHead),()=> {
    myVRP.kFirst(k,closestRelevantPredecessorsByDistance(_))
  }, myVRP,neighborhoodName = "InsertUF")

  def lastNodeOfChainInsertion(lastNode:Long) = insertPointUnroutedFirst(
    () => List(lastNode),
    ()=> myVRP.kFirst(
      k,
      ChainsHelper.relevantNeighborsForLastNodeAfterHead(
        myVRP,
        chainsExtension)),
    myVRP,
    neighborhoodName = "InsertUF")

  val oneChainInsert = {
    dynAndThen(firstNodeOfChainInsertion,
      (insertMove: InsertPointMove) => {
        mu[InsertPointMove,Option[List[Long]]](
          lastNodeOfChainInsertion(chainsExtension.lastNodeInChainOfNode(insertMove.insertedPoint)),
          nextInsertGenerator,
          None,
          Long.MaxValue,
          false)
      }) name "OneChainInsert"
  }

  // //////////////////////////////////////////////////////////////////////////////////////////////////
  val relevantPredecessorsOfNodes = (node:Long) => myVRP.nodes

  val lClosestNeighborsByDistance: Array[SortedSet[Long]] = Array.tabulate(n)(node =>
    SortedSet.empty[Long] ++ myVRP.kFirst(l, (node:Long) => closestRelevantPredecessorsByDistance(node))(node))

  def routeUnroutedChainVLSN(targetVehicle:Long)(firstNodeOfUnroutedChain:Long): Neighborhood = {
    val nextInsertGenerator = {
      (exploredMoves:List[InsertPointMove], t:Option[List[Long]]) => {
        val chainTail: List[Long] = t match {
          case None =>
            val insertedNode = exploredMoves.head.insertedPoint
            chainsExtension.nextNodesInChain(chainsExtension.firstNodeInChainOfNode(insertedNode))
          case Some(tail: List[Long]) => tail
        }

        chainTail match {
          case Nil => None
          case head :: Nil => None
          case nextNodeToInsert :: newTail =>
            val insertNeighborhood = insertPointUnroutedFirst(
              () => Some(nextNodeToInsert),
              () => ChainsHelper.computeRelevantNeighborsForInternalNodes(myVRP,chainsExtension),
              myVRP,
              selectInsertionPointBehavior = Best(),
              positionIndependentMoves = true, //compulsory because we are in VLSN
              neighborhoodName = "insertChainMiddle"
            )
            Some(insertNeighborhood, Some(newTail))
        }
      }
    }

    val nodesOfTargetVehicle = myVRP.getRouteOfVehicle(targetVehicle)

    val lNearestNodesOfTargetVehicle = nodesOfTargetVehicle.filter(x => lClosestNeighborsByDistance(firstNodeOfUnroutedChain) contains x)

    val firstNodeOfChainInsertion =
      insertPointUnroutedFirst(
        () => List(firstNodeOfUnroutedChain),
        () => _ => lNearestNodesOfTargetVehicle,
        myVRP,
        hotRestart = false,
        selectInsertionPointBehavior = Best(),
        positionIndependentMoves = true, //compulsory because we are in VLSN
        neighborhoodName = "insertChainHead"
      )

    def lastNodeOfChainInsertion(lastNode:Long) = insertPointUnroutedFirst(
      () => List(lastNode),
      ()=> myVRP.kFirst(
        l,
        ChainsHelper.relevantNeighborsForLastNodeAfterHead(
          myVRP,
          chainsExtension)),
      myVRP,
      selectInsertionPointBehavior = Best(),
      positionIndependentMoves = true, //compulsory because we are in VLSN
      neighborhoodName = "insertChainLast")

    dynAndThen(firstNodeOfChainInsertion,
      (insertMove: InsertPointMove) => {
        mu[InsertPointMove,Option[List[Long]]](
          lastNodeOfChainInsertion(chainsExtension.lastNodeInChainOfNode(insertMove.insertedPoint)),
          nextInsertGenerator,
          None,
          Long.MaxValue,
          false)
      }) name "insertChainVLSN"
  }

  def moveChainVLSN(targetVehicle: Long)(chainHeadToMove:Long): Neighborhood = {
    val nodesOfTargetVehicle = myVRP.getRouteOfVehicle(targetVehicle)
    val lNearestNodesOfTargetVehicle = nodesOfTargetVehicle.filter(x => lClosestNeighborsByDistance(chainHeadToMove) contains x)

    val nextMoveGenerator = {
      (exploredMoves:List[OnePointMoveMove], t:Option[List[Long]]) => {
        val chainTail: List[Long] = t match {
          case None =>
            val movedNode = exploredMoves.head.movedPoint
            chainsExtension.nextNodesInChain(chainsExtension.firstNodeInChainOfNode(movedNode))
          case Some(tail: List[Long]) => tail
        }

        chainTail match {
          case Nil => None
          case head :: Nil => None
          case nextNodeToMove :: newTail =>
            val moveNeighborhood = onePointMove(
              () => Some(nextNodeToMove),
              () => ChainsHelper.computeRelevantNeighborsForInternalNodes(myVRP,chainsExtension),
              myVRP,
              selectDestinationBehavior = Best(),
              positionIndependentMoves = true
            )
            Some(moveNeighborhood, Some(newTail))
        }
      }
    }

    val firstNodeOfChainMove =
      onePointMove(
        () => List(chainHeadToMove),
        () => _ => lNearestNodesOfTargetVehicle,
        myVRP,
        selectDestinationBehavior = Best(),
        hotRestart = false,
        positionIndependentMoves = true,  //compulsory because we are in VLSN
        neighborhoodName = "MoveChainHead"
      )

    def lastNodeOfChainMove(lastNode:Long) = onePointMove(
      () => List(lastNode),
      ()=> myVRP.kFirst(l,
        ChainsHelper.relevantNeighborsForLastNodeAfterHead(
          myVRP,
          chainsExtension,
          Some(HashSet() ++ relevantPredecessors(lastNode)))),
      myVRP,
      positionIndependentMoves = true,
      neighborhoodName = "MoveChainLast")

    dynAndThen(firstNodeOfChainMove,
      (moveMove: OnePointMoveMove) => {
        mu[OnePointMoveMove, Option[List[Long]]](
          lastNodeOfChainMove(chainsExtension.lastNodeInChainOfNode(moveMove.movedPoint)),
          nextMoveGenerator,
          None,
          Long.MaxValue,
          false)
      }) name "OneChainMove"
  }

  def removeNode(node:Long) = removePoint(
    () => List(node),
    myVRP,
    positionIndependentMoves = true,
    hotRestart = false)

  def removeChainVLSN(chainHead:Long):Neighborhood = {
    mu[RemovePointMove, List[Long]](
      removeNode(chainHead),
      //(List[(MoveType)], X) => Option[(Neighborhood, X)],
      (_,chainTail:List[Long]) => chainTail match{
        case Nil => None
        case h::t => Some((removeNode(h),t))
      },
      chainsExtension.chainOfNode(chainHead).tail,
      Long.MaxValue,
      false)
  }

  def removeAndReInsertVLSN(headOfChainToRemove: Long): (() => Unit) = {
    val checkpointBeforeRemove = myVRP.routes.defineCurrentValueAsCheckpoint(true)
    require(headOfChainToRemove >= v, "cannot remove vehicle point: " + headOfChainToRemove)

    val allNodesOfChain = chainsExtension.chainOfNode(headOfChainToRemove)
    for(nodeToRemove <- allNodesOfChain) {
      myVRP.routes.value.positionOfAnyOccurrence(nodeToRemove) match {
        case None => throw new Error("cannot remove non routed point:" + nodeToRemove)
        case Some(positionOfPointToRemove) =>
          myVRP.routes.remove(positionOfPointToRemove)
      }
    }

    def restoreAndRelease: (() => Unit) = () => {
      myVRP.routes.rollbackToTopCheckpoint(checkpointBeforeRemove)
      myVRP.routes.releaseTopCheckpoint()
    }

    restoreAndRelease
  }

  def vlsn(l:Int = Int.MaxValue) = {

    //VLSN neighborhood
    val nodeToAllVehicles = SortedMap.empty[Long, Iterable[Long]] ++ chainsExtension.heads.map(node => (node:Long, vehicles))

    //TODO: speedup this 3-opt; it eats most of the run time because Precedence is SSLLOOWWW

    //for re-optimization
    def threeOptOnVehicle(vehicle:Int) = {
      val nodesOfTargetVehicle = myVRP.getRouteOfVehicle(vehicle)
      //insertions points are position where we perform the insert,
      // basically the segment will start in place of the insertion point and the insertion point will be moved upward
      val nodesOfTargetVehicleButVehicle = nodesOfTargetVehicle.filter(_ != vehicle)

      threeOpt(() => nodesOfTargetVehicle,
        () => _ => nodesOfTargetVehicleButVehicle,
        myVRP,
        breakSymmetry = false) filter((t:ThreeOptMove) => if(t.flipSegment) t.segmentEndPosition - t.segmentStartPosition < 3 else math.abs(t.insertionPoint - t.segmentStartPosition) < 10)
    }

    new VLSN(
      v,
      () => SortedMap.empty[Long, SortedSet[Long]] ++
        vehicles.map((vehicle: Long) =>
          (vehicle:Long, SortedSet.empty[Long] ++ myVRP.getRouteOfVehicle(vehicle).filter(node => chainsExtension.isHead(node)))),
      () => SortedSet.empty[Long] ++ myVRP.unroutedNodes.filter(node => chainsExtension.isHead(node)),
      nodeToRelevantVehicles = () => nodeToAllVehicles,

      targetVehicleNodeToInsertNeighborhood = routeUnroutedChainVLSN,
      targetVehicleNodeToMoveNeighborhood = moveChainVLSN,
      removeChainVLSN,

      removeNodeAndReInsert = removeAndReInsertVLSN,

      reOptimizeVehicle = Some(vehicle => Some(threeOptOnVehicle(vehicle))),
      useDirectInsert = true,

      objPerVehicle,
      unroutedPenaltyOBj,
      obj,

      cycleFinderAlgoSelection = CycleFinderAlgoType.Mouthuy,

      name="VLSN(" + l + ")",
      reoptimizeAtStartUp = true
    )
  }
  // ///////////////////////////////////////////////////////////////////////////////////////////////////

  val vlsnNeighborhood = vlsn(l)
  val search = bestSlopeFirst(List(oneChainInsert,oneChainMove, onePtMove(20))) exhaustBack (vlsnNeighborhood maxMoves 1)

  search.verbose = 2
  vlsnNeighborhood.verbose = 3

  search.doAllMoves(obj=obj)

  println(myVRP)

}