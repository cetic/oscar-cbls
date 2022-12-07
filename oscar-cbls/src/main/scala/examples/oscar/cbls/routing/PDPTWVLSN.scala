package examples.oscar.cbls.routing

import oscar.cbls._
import oscar.cbls.algo.generator.RoutingMatrixGenerator
import oscar.cbls.algo.search.KSmallest
import oscar.cbls.business.routing.invariants.global.RouteLength
import oscar.cbls.business.routing.invariants.vehicleCapacity.GlobalVehicleCapacityConstraint
import oscar.cbls.business.routing.model.helpers.{ChainsHelper, DistanceHelper}
import oscar.cbls.business.routing._
import oscar.cbls.core.computation.{CBLSIntVar, ChangingIntValue, Store}
import oscar.cbls.core.constraint.ConstraintSystem
import oscar.cbls.core.objective.{CascadingObjective, Objective}
import oscar.cbls.core.search.{Best, Neighborhood, NoMoveNeighborhood}
import oscar.cbls.lib.search.neighborhoods.vlsn.VLSN
import oscar.cbls.visual.SingleFrameWindow
import oscar.cbls.visual.profiling.ProfilingTree

import scala.collection.immutable.{HashSet, SortedMap, SortedSet}

/**
 * Created by fg on 12/05/17.
 */

object PDPTWVLSN extends App {
  val m = Store(noCycle = false)

  val v = 10
  val n = 500

  println(s"VLSN(PDPTW) v:$v n:$n")
  val penaltyForUnrouted = 10000
  val maxVehicleCapacity = 8
  val minVehicleCapacity = 6
  val symmetricDistance = RoutingMatrixGenerator.apply(n)._1
  val (listOfChains, precedences) = RoutingMatrixGenerator.generateChainsPrecedence(n, v, (n - v) / 2)
  val contentsFlow = RoutingMatrixGenerator.generateContentFlow(n, listOfChains, minVehicleCapacity)
  val vehiclesCapacity = RoutingMatrixGenerator.generateVehiclesSize(v, maxVehicleCapacity, minVehicleCapacity)

  val myVRP = new VRP(m, n, v)
  val vehicles = 0 until v

  val k = 10
  val l = 40
  val xNearestVehicles = 7

  println("listOfChains: \n" + listOfChains.mkString("\n"))

  // Distance
  val vehiclesRouteLength = Array.tabulate(v)(vehicle => CBLSIntVar(m, name = s"Route length of vehicle $vehicle"))
  val routeLengthInvariant = new RouteLength(myVRP.routes, n, v, vehiclesRouteLength, (from: Int, to: Int) => symmetricDistance(from)(to))

  //Chains
  val precedenceRoute = myVRP.routes.createClone()

  //TODO: we need a faster precedence constraint!
  val precedenceInvariant = precedence(precedenceRoute, precedences)
  val vehicleOfNodesNow = vehicleOfNodes(precedenceRoute, v)
  val precedencesConstraints = ConstraintSystem(m)
  for (start <- precedenceInvariant.nodesStartingAPrecedence)
    precedencesConstraints.add(vehicleOfNodesNow(start) === vehicleOfNodesNow(precedenceInvariant.nodesEndingAPrecedenceStartedAt(start).head))
  precedencesConstraints.add(0 === precedenceInvariant)
  val chainsExtension = chains(myVRP, listOfChains)

  val maxLengthConstraints = ConstraintSystem(m)
  val maxLengthConstraintPerVehicle: Array[ChangingIntValue] = Array.fill(v)(null)
  for (vehicle <- 0 until v) {
    val c = vehiclesRouteLength(vehicle) le 13000
    maxLengthConstraintPerVehicle(vehicle) = c.violation
    maxLengthConstraints.add(c)
  }

  m.registerForPartialPropagation(maxLengthConstraintPerVehicle.toIndexedSeq: _*)


  //for a chain, we want the x nearest vehicles to the chain.
  //for each node we take the summe distance to each nodes of the chain to the vehicle
  val chainHeadToSummedDistanceToVehicles = listOfChains.map((chain: List[Int]) => (chain.head, vehicles.map(vehicle => chain.map((node: Int) => symmetricDistance(node)(vehicle)).sum).toArray))
  val chainHeadToxNearestVehicles = SortedMap.empty[Int, List[Int]] ++ chainHeadToSummedDistanceToVehicles.map({ case (chainHead, vehicleToDistance) => (chainHead, KSmallest.getkSmallests(vehicles.toArray, xNearestVehicles, (v: Int) => vehicleToDistance(v))) })

  // Vehicle content
  val violationOfContentOfVehicle = Array.tabulate(v)(vehicle =>
    CBLSIntVar(myVRP.routes.model, name = s"Violation of capacity of vehicle $vehicle"))
  val capacityInvariant = GlobalVehicleCapacityConstraint(myVRP.routes, n, v, vehiclesCapacity, contentsFlow, violationOfContentOfVehicle)

  //Objective function
  val unroutedPenalty = penaltyForUnrouted * (n - length(myVRP.routes))

  val obj = new CascadingObjective(
    new CascadingObjective(maxLengthConstraints, precedencesConstraints),
    new CascadingObjective(sum(violationOfContentOfVehicle),
      sum(vehiclesRouteLength) + unroutedPenalty)
  )

  val objPerVehicle = Array.tabulate[Objective](v)(vehicle =>
    new CascadingObjective(
      new CascadingObjective(new CascadingObjective(maxLengthConstraints, precedencesConstraints), violationOfContentOfVehicle(vehicle)),
      Objective(vehiclesRouteLength(vehicle)))
  )
  val unroutedPenaltyOBj = Objective(unroutedPenalty)

  m.close()

  val relevantPredecessorsTmp: Map[Int, Iterable[Int]] = capacityInvariant.relevantPredecessorsOfNodes

  val relevantPredecessors = SortedMap.empty[Int, SortedSet[Int]] ++ relevantPredecessorsTmp.map({ case (node, v) => (node, SortedSet.empty[Int] ++ v) })

  val closestRelevantPredecessorsByDistance = Array.tabulate(n)(DistanceHelper.lazyClosestPredecessorsOfNode(symmetricDistance, relevantPredecessors)(_))

  // MOVING

  val nextMoveGenerator = {
    (exploredMoves: List[OnePointMoveMove], t: Option[List[Int]]) => {
      val chainTail: List[Int] = t match {
        case None =>
          val movedNode = exploredMoves.head.movedPoint
          chainsExtension.nextNodesInChain(chainsExtension.firstNodeInChainOfNode(movedNode))
        case Some(tail: List[Int]) => tail
      }

      chainTail match {
        case Nil => None
        case _ :: Nil => None
        case nextNodeToMove :: newTail =>
          val moveNeighborhood = onePointMove(() => Some(nextNodeToMove),
            () => ChainsHelper.computeRelevantNeighborsForInternalNodes(myVRP, chainsExtension), myVRP)
          Some(moveNeighborhood, Some(newTail))
      }
    }
  }

  val firstNodeOfChainMove = onePointMove(
    () => myVRP.routed.value.filter(chainsExtension.isHead),
    () => myVRP.kFirst(k, closestRelevantPredecessorsByDistance(_)), myVRP, neighborhoodName = "MoveHeadOfChain")

  def lastNodeOfChainMove(lastNode: Int) = onePointMove(
    () => List(lastNode),
    () => myVRP.kFirst(k,
      ChainsHelper.relevantNeighborsForLastNodeAfterHead(
        myVRP,
        chainsExtension
        , Some(HashSet() ++ relevantPredecessors(lastNode))
      )),
    myVRP,
    neighborhoodName = "MoveLastOfChain")

  val oneChainMove = {
    dynAndThen(firstNodeOfChainMove,
      (moveMove: OnePointMoveMove) => {
        if (maxLengthConstraints.Violation.value != 0) {
          NoMoveNeighborhood
        } else {
          mu[OnePointMoveMove, Option[List[Int]]](
            lastNodeOfChainMove(chainsExtension.lastNodeInChainOfNode(moveMove.movedPoint)),
            nextMoveGenerator,
            None,
            Int.MaxValue,
            intermediaryStops = false)
        }
      }) name "OneChainMove"
  }

  def onePtMove(k: Int) = onePointMove(myVRP.routed, () => myVRP.kFirst(k, closestRelevantPredecessorsByDistance(_)), myVRP)

  // INSERTING

  val nextInsertGenerator = {
    (exploredMoves: List[InsertPointMove], t: Option[List[Int]]) => {
      val chainTail: List[Int] = t match {
        case None =>
          val insertedNode = exploredMoves.head.insertedPoint
          chainsExtension.nextNodesInChain(chainsExtension.firstNodeInChainOfNode(insertedNode))
        case Some(tail: List[Int]) => tail
      }

      chainTail match {
        case Nil => None
        case head :: Nil => None
        case nextNodeToInsert :: newTail =>
          val insertNeighborhood = insertPointUnroutedFirst(() => Some(nextNodeToInsert),
            () => ChainsHelper.computeRelevantNeighborsForInternalNodes(myVRP, chainsExtension), myVRP)
          Some(insertNeighborhood, Some(newTail))
      }
    }
  }

  val firstNodeOfChainInsertion = insertPointUnroutedFirst(() => myVRP.unrouted.value.filter(chainsExtension.isHead), () => {
    myVRP.kFirst(k, closestRelevantPredecessorsByDistance(_))
  }, myVRP, neighborhoodName = "InsertUF")

  //TODO: il y a un problème si on a un noeud tout seul.
  def lastNodeOfChainInsertion(lastNode: Int) = insertPointUnroutedFirst(
    () => List(lastNode),
    () => myVRP.kFirst(
      k,
      ChainsHelper.relevantNeighborsForLastNodeAfterHead(
        myVRP,
        chainsExtension)),
    myVRP,
    neighborhoodName = "InsertUF")

  val oneChainInsert = {
    dynAndThen(firstNodeOfChainInsertion,
      (insertMove: InsertPointMove) => {
        if (maxLengthConstraints.Violation.value != 0) {
          NoMoveNeighborhood
        } else {
          mu[InsertPointMove, Option[List[Int]]](
            lastNodeOfChainInsertion(chainsExtension.lastNodeInChainOfNode(insertMove.insertedPoint)),
            nextInsertGenerator,
            None,
            Int.MaxValue,
            intermediaryStops = false)
        }
      }) name "OneChainInsert"
  }

  // //////////////////////////////////////////////////////////////////////////////////////////////////
  val relevantPredecessorsOfNodes = (node: Int) => myVRP.nodes

  val lClosestNeighborsByDistance: Array[SortedSet[Int]] = Array.tabulate(n)(node =>
    SortedSet.empty[Int] ++ myVRP.kFirst(l, (node: Int) => closestRelevantPredecessorsByDistance(node))(node))

  def routeUnroutedChainVLSN(targetVehicle: Int): Int => Neighborhood = {

    val nodesOfTargetVehicle = myVRP.getRouteOfVehicle(targetVehicle)

    (firstNodeOfUnroutedChain: Int) => {
      val lNearestNodesOfTargetVehicle = nodesOfTargetVehicle.filter(x => lClosestNeighborsByDistance(firstNodeOfUnroutedChain) contains x)

      val nextInsertGenerator = {
        (exploredMoves: List[InsertPointMove], t: Option[List[Int]]) => {
          val chainTail: List[Int] = t match {
            case None =>
              val insertedNode = exploredMoves.head.insertedPoint
              chainsExtension.nextNodesInChain(chainsExtension.firstNodeInChainOfNode(insertedNode))
            case Some(tail: List[Int]) => tail
          }

          chainTail match {
            case Nil => None
            case head :: Nil => None
            case nextNodeToInsert :: newTail =>
              val insertNeighborhood = insertPointUnroutedFirst(
                () => Some(nextNodeToInsert),
                () => ChainsHelper.computeRelevantNeighborsForInternalNodes(myVRP, chainsExtension),
                myVRP,
                selectInsertionPointBehavior = Best(),
                positionIndependentMoves = true, //compulsory because we are in VLSN
                neighborhoodName = "insertChainMiddle"
              )
              Some(insertNeighborhood, Some(newTail))
          }
        }
      }

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

      def lastNodeOfChainInsertion(lastNode: Int) = insertPointUnroutedFirst(
        () => List(lastNode),
        () => myVRP.kFirst(
          l,
          ChainsHelper.relevantNeighborsForLastNodeAfterHead( //TODO: filter in the target vehicle!!
            myVRP,
            chainsExtension)),
        myVRP,
        selectInsertionPointBehavior = Best(),
        positionIndependentMoves = true, //compulsory because we are in VLSN
        neighborhoodName = "insertChainLast")

      dynAndThen(firstNodeOfChainInsertion,
        (insertMove: InsertPointMove) => {
          if (maxLengthConstraintPerVehicle(targetVehicle).value != 0) {
            NoMoveNeighborhood
          } else {
            mu[InsertPointMove, Option[List[Int]]](
              lastNodeOfChainInsertion(chainsExtension.lastNodeInChainOfNode(insertMove.insertedPoint)),
              nextInsertGenerator,
              None,
              Int.MaxValue,
              intermediaryStops = false)
          }
        }) name "insertChainVLSN"
    }
  }

  def moveChainVLSN(targetVehicle: Int): Int => Neighborhood = {
    val nodesOfTargetVehicle = SortedSet.empty[Int] ++ myVRP.getRouteOfVehicle(targetVehicle)

    (chainHeadToMove: Int) => {
      val relevantNodesOfTargetVehicle = nodesOfTargetVehicle intersect relevantPredecessors(chainHeadToMove)
      val lNearestNodesOfTargetVehicle = relevantNodesOfTargetVehicle intersect lClosestNeighborsByDistance(chainHeadToMove)

      val nextMoveGenerator = {
        (exploredMoves: List[OnePointMoveMove], t: Option[List[Int]]) => {
          val chainTail: List[Int] = t match {
            case None =>
              val movedNode = exploredMoves.head.movedPoint
              chainsExtension.nextNodesInChain(chainsExtension.firstNodeInChainOfNode(movedNode))
            case Some(tail: List[Int]) => tail
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

      def lastNodeOfChainMove(lastNode: Int) = onePointMove(
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
            mu[OnePointMoveMove, Option[List[Int]]](
              lastNodeOfChainMove(chainsExtension.lastNodeInChainOfNode(moveMove.movedPoint)),
              nextMoveGenerator,
              None,
              Int.MaxValue,
              intermediaryStops = false)
          }
        }) name "OneChainMove"
    }
  }

  /*
    def a(chainHeadToMove: Int): Neighborhood = {
    val relevantNodesOfTargetVehicle = nodesOfTargetVehicle intersect (relevantPredecessors(chainHeadToMove))
    val lNearestNodesOfTargetVehicle = relevantNodesOfTargetVehicle.filter(x => lClosestNeighborsByDistance(chainHeadToMove) contains x)
   */

  def moveChainWithinVehicle(vehicle: Int): Neighborhood = {
    val nodesOfTargetVehicle = SortedSet.empty[Int] ++ myVRP.getRouteOfVehicle(vehicle)
    val chainsHeadInVehicle = nodesOfTargetVehicle.filter(chainsExtension.isHead)


    val nextMoveGenerator = {
      (exploredMoves: List[OnePointMoveMove], t: Option[List[Int]]) => {
        val chainTail: List[Int] = t match {
          case None =>
            val movedNode = exploredMoves.head.movedPoint
            chainsExtension.nextNodesInChain(chainsExtension.firstNodeInChainOfNode(movedNode))
          case Some(tail: List[Int]) => tail
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

    def lastNodeOfChainMove(lastNode: Int) = onePointMove(
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
          mu[OnePointMoveMove, Option[List[Int]]](
            lastNodeOfChainMove(chainsExtension.lastNodeInChainOfNode(moveMove.movedPoint)),
            nextMoveGenerator,
            None,
            Int.MaxValue,
            intermediaryStops = false)
        }
      }) name "OneChainMove"
  }

  def removeNode(node: Int) = removePoint(
    () => List(node),
    myVRP,
    positionIndependentMoves = true,
    hotRestart = false)

  def removeChainVLSN(chainHead: Int): Neighborhood = {
    mu[RemovePointMove, List[Int]](
      removeNode(chainHead),
      //(List[(MoveType)], X) => Option[(Neighborhood, X)],
      (_, chainTail: List[Int]) => chainTail match {
        case Nil => None
        case h :: t => Some((removeNode(h), t))
      },
      chainsExtension.chainOfNode(chainHead).tail,
      Int.MaxValue,
      intermediaryStops = false)
  }

  def removeAndReInsertVLSN(headOfChainToRemove: Int): () => Unit = {
    val checkpointBeforeRemove = myVRP.routes.defineCurrentValueAsCheckpoint()
    require(headOfChainToRemove >= v, s"cannot remove vehicle point: $headOfChainToRemove")

    val allNodesOfChain = chainsExtension.chainOfNode(headOfChainToRemove)
    for (nodeToRemove <- allNodesOfChain) {
      myVRP.routes.value.positionOfAnyOccurrence(nodeToRemove) match {
        case None => throw new Error(s"cannot remove non routed point: $nodeToRemove")
        case Some(positionOfPointToRemove) =>
          myVRP.routes.remove(positionOfPointToRemove)
      }
    }

    def restoreAndRelease: () => Unit = () => {
      myVRP.routes.rollbackToTopCheckpoint(checkpointBeforeRemove)
      myVRP.routes.releaseTopCheckpoint()
    }

    restoreAndRelease
  }

  //TODO: speedup this 3-opt; it eats most of the run time because Precedence is SSLLOOWWW
  //for re-optimization
  def threeOptOnVehicle(vehicle: Int) = {
    val nodesOfTargetVehicle = myVRP.getRouteOfVehicle(vehicle)
    //insertions points are position where we perform the insert,
    // basically the segment will start in place of the insertion point and the insertion point will be moved upward
    val nodesOfTargetVehicleButVehicle = nodesOfTargetVehicle.filter(_ != vehicle)

    threeOpt(() => nodesOfTargetVehicle,
      () => _ => nodesOfTargetVehicleButVehicle,
      myVRP, breakSymmetry = false) filter ((t: ThreeOptMove) =>
      if (t.flipSegment) t.segmentEndPosition - t.segmentStartPosition < 4
      else math.min(math.abs(t.insertionPoint - t.segmentStartPosition), math.abs(t.insertionPoint - t.segmentEndPosition)) < 6)
  }

  def vlsn(l: Int = Int.MaxValue) = {
    //VLSN neighborhood
    new VLSN(
      v,
      () => myVRP.getVehicleToRouteMap.view.mapValues(_.filter(node => node >= v && chainsExtension.isHead(node))).toMap,
      () => SortedSet.empty[Int] ++ myVRP.unroutedNodes.filter(node => chainsExtension.isHead(node)),
      nodeToRelevantVehicles = () => chainHeadToxNearestVehicles,

      targetVehicleNodeToInsertNeighborhood = routeUnroutedChainVLSN,
      targetVehicleNodeToMoveNeighborhood = moveChainVLSN,
      removeChainVLSN,

      removeNodeAndReInsert = removeAndReInsertVLSN,

      reOptimizeVehicle = Some(vehicle => Some(threeOptOnVehicle(vehicle) exhaustBack moveChainWithinVehicle(vehicle))),

      objPerVehicle,
      unroutedPenaltyOBj,
      obj,
      name = s"VLSN($l)",
      reoptimizeAtStartUp = true,
      debugNeighborhoodExploration = false
    ) //most of the time, you do not want incremental VLSN
  }

  // ///////////////////////////////////////////////////////////////////////////////////////////////////

  val vlsnNeighborhood = vlsn(l)
  val search = bestSlopeFirst(List(oneChainInsert, oneChainMove, onePtMove(20)))// exhaust (vlsnNeighborhood maxMoves 1)

  search.verbose = 1
  vlsnNeighborhood.verbose = 2

  search.doAllMoves(obj = obj)

  println(myVRP)
  println(search.profilingStatistics)

  for (vehicle <- 0 until v) {
    val l = vehiclesRouteLength(vehicle).value
    if (l != 0) println(s"vehicle($vehicle).length:$l")
  }

  val profilingTree = new ProfilingTree(search)
  SingleFrameWindow.show(profilingTree, "Profiling")
  profilingTree.draw()
  println(s"obj:${obj.value}")

}