package oscar.examples.cbls.routing

import oscar.cbls._
import oscar.cbls.algo.search.KSmallest
import oscar.cbls.business.routing._
import oscar.cbls.business.routing.invariants.global.{GlobalConstraintCore, RouteLength}
import oscar.cbls.business.routing.invariants.vehicleCapacity.GlobalVehicleCapacityConstraint
import oscar.cbls.core.ChangingIntValue
import oscar.cbls.core.search.{Best, Neighborhood, NoMoveNeighborhood}
import oscar.cbls.lib.search.neighborhoods.vlsn.CycleFinderAlgoType.CycleFinderAlgoType
import oscar.cbls.lib.search.neighborhoods.vlsn.{CycleFinderAlgoType, VLSN}

import scala.collection.immutable.{HashSet, SortedMap, SortedSet}

/**
 * Created by fg on 12/05/17.
 */

object DemoPDP_VLSN extends App{
  val m = new Store(noCycle = false)
  val v = 10
  val n = 500

  println("VLSN(PDPTW) v:" + v +" n:" + n)
  val penaltyForUnrouted = 10000
  val maxVehicleCapacity = 8
  val minVehicleCapacity = 6
  val symmetricDistance = RoutingMatrixGenerator.apply(n)._1
  val (listOfChains,precedences) = RoutingMatrixGenerator.generateChainsPrecedence(n,v,(n-v)/2)
  val contentsFlow = RoutingMatrixGenerator.generateContentFlow(n,listOfChains,minVehicleCapacity)
  val vehiclesCapacity = RoutingMatrixGenerator.generateVehiclesSize(v,maxVehicleCapacity,minVehicleCapacity)

  val myVRP =  new VRP(m,n,v)
  val vehicles = 0L until v

  val k = 30
  val l = 40
  val xNearestVehicles = 7

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

  val maxLengthConstraints = new ConstraintSystem(m)
  val maxLengthConstraintPerVehicle:Array[ChangingIntValue] = Array.fill(v)(null)
  for(vehicle <- 0 until v){
    val c = vehiclesRouteLength(vehicle) le 13000
    maxLengthConstraintPerVehicle(vehicle) = c.violation
    maxLengthConstraints.add(c)
  }

  m.registerForPartialPropagation(maxLengthConstraintPerVehicle:_*)


  //for a chain, we want the x nearest vehicles to the chain.
  //for each node we take the summe distance to each nodes of the chain to the vehicle
  val chainHeadToSummedDistanceToVehicles = listOfChains.map((chain:List[Long]) => (chain.head, vehicles.map(vehicle => (chain.map((node:Long) => symmetricDistance(node)(vehicle)).sum)).toArray))
  val chainHeadToxNearestVehicles = SortedMap.empty[Long,List[Long]] ++ chainHeadToSummedDistanceToVehicles.map({case (chainHead,vehicleToDistance) => (chainHead,KSmallest.getkSmallests(vehicles.toArray, xNearestVehicles, (v:Long) => vehicleToDistance(v)))})

  // Vehicle content
  val violationOfContentOfVehicle = Array.tabulate(v)(vehicle =>
    CBLSIntVar(myVRP.routes.model, name = "Violation of capacity of vehicle " + vehicle))
  val capacityInvariant = GlobalVehicleCapacityConstraint(gc, n, v, vehiclesCapacity, contentsFlow, violationOfContentOfVehicle)

  //Objective function
  val unroutedPenalty = penaltyForUnrouted*(n - length(myVRP.routes))

  val obj = new CascadingObjective(new CascadingObjective(maxLengthConstraints,precedencesConstraints),
    new CascadingObjective(sum(violationOfContentOfVehicle),
      sum(vehiclesRouteLength) + unroutedPenalty))

  val objPerVehicle = Array.tabulate[Objective](v)(vehicle =>
    new CascadingObjective(
      new CascadingObjective(new CascadingObjective(maxLengthConstraints,precedencesConstraints),violationOfContentOfVehicle(vehicle)),
      Objective(vehiclesRouteLength(vehicle)))
  )
  val unroutedPenaltyOBj = Objective(unroutedPenalty)

  m.close()

  val relevantPredecessorsTmp:Map[Long,Iterable[Long]] = GlobalVehicleCapacityConstraint.relevantPredecessorsOfNodes(capacityInvariant)

  val relevantPredecessors = SortedMap.empty[Long,SortedSet[Long]] ++ (relevantPredecessorsTmp.map({case (node,v) => (node,SortedSet.empty[Long] ++ v)}))

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
        chainsExtension
        ,Some(HashSet() ++ relevantPredecessors(lastNode))
      )),
    myVRP,
    neighborhoodName = "MoveLastOfChain")

  val oneChainMove = {
    dynAndThen(firstNodeOfChainMove,
      (moveMove: OnePointMoveMove) => {
        if(maxLengthConstraints.Violation.value != 0){
          NoMoveNeighborhood
        }else{
          mu[OnePointMoveMove, Option[List[Long]]](
            lastNodeOfChainMove(chainsExtension.lastNodeInChainOfNode(moveMove.movedPoint)),
            nextMoveGenerator,
            None,
            Long.MaxValue,
            false)
        }}) name "OneChainMove"
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
        if(maxLengthConstraints.Violation.value != 0){
          NoMoveNeighborhood
        }else{
          mu[InsertPointMove,Option[List[Long]]](
            lastNodeOfChainInsertion(chainsExtension.lastNodeInChainOfNode(insertMove.insertedPoint)),
            nextInsertGenerator,
            None,
            Long.MaxValue,
            false)
        }}) name "OneChainInsert"
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
        if(maxLengthConstraintPerVehicle(targetVehicle).value != 0){
          NoMoveNeighborhood
        }else{
          mu[InsertPointMove,Option[List[Long]]](
            lastNodeOfChainInsertion(chainsExtension.lastNodeInChainOfNode(insertMove.insertedPoint)),
            nextInsertGenerator,
            None,
            Long.MaxValue,
            false)
        }}) name "insertChainVLSN"
  }

  def moveChainVLSN(targetVehicle: Long):(Long=>Neighborhood) = {
    val nodesOfTargetVehicle = (SortedSet.empty[Long] ++ myVRP.getRouteOfVehicle(targetVehicle))

    def a(chainHeadToMove: Long): Neighborhood = {
      val relevantNodesOfTargetVehicle = nodesOfTargetVehicle intersect (relevantPredecessors(chainHeadToMove))
      val lNearestNodesOfTargetVehicle = relevantNodesOfTargetVehicle.filter(x => lClosestNeighborsByDistance(chainHeadToMove) contains x)

      val nextMoveGenerator = {
        (exploredMoves: List[OnePointMoveMove], t: Option[List[Long]]) => {
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
                () => ChainsHelper.computeRelevantNeighborsForInternalNodes(myVRP, chainsExtension),
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
          positionIndependentMoves = true, //compulsory because we are in VLSN
          neighborhoodName = "MoveChainHead"
        )

      def lastNodeOfChainMove(lastNode: Long) = onePointMove(
        () => List(lastNode),
        () => myVRP.kFirst(l,
          ChainsHelper.relevantNeighborsForLastNodeAfterHead(
            myVRP,
            chainsExtension
            //,Some(HashSet() ++ relevantPredecessors(lastNode))
          )), //TODO: takes a long time
        myVRP,
        positionIndependentMoves = true,
        neighborhoodName = "MoveChainLast")

      dynAndThen(firstNodeOfChainMove,
        (moveMove: OnePointMoveMove) => {
          if (maxLengthConstraintPerVehicle(targetVehicle).value != 0) {
            NoMoveNeighborhood
          } else {
            mu[OnePointMoveMove, Option[List[Long]]](
              lastNodeOfChainMove(chainsExtension.lastNodeInChainOfNode(moveMove.movedPoint)),
              nextMoveGenerator,
              None,
              Long.MaxValue,
              false)
          }
        }) name "OneChainMove"
    }
    a
  }

  /*
    def a(chainHeadToMove: Long): Neighborhood = {
    val relevantNodesOfTargetVehicle = nodesOfTargetVehicle intersect (relevantPredecessors(chainHeadToMove))
    val lNearestNodesOfTargetVehicle = relevantNodesOfTargetVehicle.filter(x => lClosestNeighborsByDistance(chainHeadToMove) contains x)
   */

  def moveChainWithinVehicle(vehicle: Long):Neighborhood = {
    val nodesOfTargetVehicle = (SortedSet.empty[Long] ++ myVRP.getRouteOfVehicle(vehicle))
    val chainsHeadInVehicle = nodesOfTargetVehicle.filter(chainsExtension.isHead)

    val nextMoveGenerator = {
      (exploredMoves: List[OnePointMoveMove], t: Option[List[Long]]) => {
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
              () => ChainsHelper.computeRelevantNeighborsForInternalNodes(myVRP, chainsExtension),
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
        () => chainsHeadInVehicle,
        () => _ => nodesOfTargetVehicle,
        myVRP,
        selectDestinationBehavior = Best(),
        hotRestart = false,
        positionIndependentMoves = true, //compulsory because we are in VLSN
        neighborhoodName = "MoveChainHead"
      )

    def lastNodeOfChainMove(lastNode: Long) = onePointMove(
      () => List(lastNode),
      () => myVRP.kFirst(l,
        ChainsHelper.relevantNeighborsForLastNodeAfterHead(
          myVRP,
          chainsExtension
          //,Some(HashSet() ++ relevantPredecessors(lastNode))
        )), //TODO: takes a long time
      myVRP,
      positionIndependentMoves = true,
      neighborhoodName = "MoveChainLast")

    dynAndThen(firstNodeOfChainMove,
      (moveMove: OnePointMoveMove) => {
        if (maxLengthConstraintPerVehicle(vehicle).value != 0) {
          NoMoveNeighborhood
        } else {
          mu[OnePointMoveMove, Option[List[Long]]](
            lastNodeOfChainMove(chainsExtension.lastNodeInChainOfNode(moveMove.movedPoint)),
            nextMoveGenerator,
            None,
            Long.MaxValue,
            false)
        }
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

  //TODO: speedup this 3-opt; it eats most of the run time because Precedence is SSLLOOWWW
  //for re-optimization
  def threeOptOnVehicle(vehicle:Int) = {
    val nodesOfTargetVehicle = myVRP.getRouteOfVehicle(vehicle)
    //insertions points are position where we perform the insert,
    // basically the segment will start in place of the insertion point and the insertion point will be moved upward
    val nodesOfTargetVehicleButVehicle = nodesOfTargetVehicle.filter(_ != vehicle)

    threeOpt(() => nodesOfTargetVehicle,
      () => _ => nodesOfTargetVehicleButVehicle,
      myVRP, breakSymmetry = false) filter((t:ThreeOptMove) =>
      if(t.flipSegment) t.segmentEndPosition - t.segmentStartPosition < 4
      else math.min(math.abs(t.insertionPoint - t.segmentStartPosition),math.abs(t.insertionPoint - t.segmentEndPosition)) < 6)
  }

  def vlsn(l:Int = Int.MaxValue) = {
    //VLSN neighborhood
    val nodeToAllVehicles = SortedMap.empty[Long, Iterable[Long]] ++ chainsExtension.heads.map(node => (node:Long, vehicles))
    new VLSN(
      v,
      () => SortedMap.empty[Long, SortedSet[Long]] ++
        vehicles.map((vehicle: Long) =>
          (vehicle:Long, SortedSet.empty[Long] ++ myVRP.getRouteOfVehicle(vehicle).filter(node => chainsExtension.isHead(node)))),
      () => SortedSet.empty[Long] ++ myVRP.unroutedNodes.filter(node => chainsExtension.isHead(node)),
      nodeToRelevantVehicles = () => chainHeadToxNearestVehicles,

      targetVehicleNodeToInsertNeighborhood = routeUnroutedChainVLSN,
      targetVehicleNodeToMoveNeighborhood = moveChainVLSN,
      removeChainVLSN,

      removeNodeAndReInsert = removeAndReInsertVLSN,

      reOptimizeVehicle = Some(vehicle => Some(threeOptOnVehicle(vehicle) exhaustBack moveChainWithinVehicle(vehicle))),
      useDirectInsert = true,

      objPerVehicle,
      unroutedPenaltyOBj,
      obj,

      cycleFinderAlgoSelection = CycleFinderAlgoType.Mouthuy,

      name="VLSN(" + l + ")",
      reoptimizeAtStartUp = true,
      checkObjCoherence = true
    )
  }

  // ///////////////////////////////////////////////////////////////////////////////////////////////////

  val vlsnNeighborhood = vlsn(l)
  val search = bestSlopeFirst(List(oneChainInsert,oneChainMove, onePtMove(20))) exhaustBack (vlsnNeighborhood maxMoves 1)

  search.verbose = 2
  vlsnNeighborhood.verbose = 3

  search.doAllMoves(obj=obj)

  println(myVRP)

  for(vehicle <- 0 until v){
    val l = vehiclesRouteLength(vehicle).value
    if(l !=0) println("vehicle(" + vehicle + ").length:" + l)
  }

  println("obj:" + obj.value)

}

