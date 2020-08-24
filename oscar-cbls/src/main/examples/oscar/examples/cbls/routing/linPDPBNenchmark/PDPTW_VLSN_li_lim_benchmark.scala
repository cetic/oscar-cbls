package oscar.examples.cbls.routing


import oscar.cbls._
import oscar.cbls.algo.search.KSmallest
import oscar.cbls.business.routing._
import oscar.cbls.business.routing.invariants.global.{GlobalConstraintCore, RouteLength}
import oscar.cbls.business.routing.invariants.timeWindow.{TimeWindowConstraint, TransferFunction}
import oscar.cbls.business.routing.invariants.vehicleCapacity.GlobalVehicleCapacityConstraint
import oscar.cbls.core.search.{Best, Neighborhood, NoMoveNeighborhood}
import oscar.cbls.lib.search.neighborhoods.vlsn._

import scala.collection.immutable.{HashSet, SortedMap, SortedSet}
import scala.io.Source

object PDPTW_VLSN_li_lim_benchmark extends App {

  case class PDP(fromNode: Int, toNode: Int, demand: Int) {
    def chain: List[Int] = List(fromNode, toNode)

    def precedence: (Int, Int) = (fromNode, toNode)
  }

  def readData(fileName: String): (Int, Array[Array[Long]], List[PDP], Int, Array[TransferFunction]) = {

    case class Node(id: Int, x: Int, y: Int, demand: Int, earlyLine: Int, deadline: Int, duration: Int, pickUP: Int, delivery: Int) {
      def distance(that: Node): Long = math.sqrt((this.x - that.x) * (this.x - that.x) + (this.y - that.y) * (this.y - that.y)).ceil.toLong
    }

    val s = Source.fromFile(fileName)
    val lines = s.getLines

    val Array(v, capacity, _) = lines.next().split("\\t\\s*").map(_.toInt)


    var allNodesList: List[Node] = Nil
    while (lines.hasNext) {
      val nodeInfo = lines.next().split("\\t\\s*").map(_.toInt)
      //Node(id:Int,      x:Int,        y:Int,      demand:Int,  earlyLine:Int,deadline:Int,duration:Int,pickUP:Int,delivery:Int)
      allNodesList = Node(nodeInfo(0), nodeInfo(1), nodeInfo(2), nodeInfo(3), nodeInfo(4), nodeInfo(5), nodeInfo(6), nodeInfo(7), nodeInfo(8)) :: allNodesList
    }

    s.close()

    val allNodesArray: Array[Node] = Array.fill(allNodesList.head.id + 1)(null)
    while (allNodesList.nonEmpty) {
      val head = allNodesList.head
      allNodesList = allNodesList.tail
      allNodesArray(head.id) = head
    }

    //println(allNodesArray.mkString("\n"))

    val oscarN: Int = allNodesArray.length - 1 + v

    def oscarNodeToLinNode(oscarNode: Int): Int = if (oscarNode < v) 0 else oscarNode - v + 1

    def linNodeToOscarNode(linNode: Int): Int = {
      require(linNode != 0)
      linNode - 1 + v
    }

    val singleNodeTransferFunctions = Array.tabulate(oscarN)(node => {

      val nodeData = allNodesArray(oscarNodeToLinNode(node))
      if (node < v) TransferFunction.createFromEarliestAndLatestArrivalTime(node, nodeData.earlyLine, nodeData.deadline)
      else TransferFunction.createFromEarliestAndLatestArrivalTime(node, 0 /*nodeData.earlyLine*/ , nodeData.deadline, nodeData.duration)
      //TransferFunction.identifyTransferFunction(node) //
    })


    val distanceMatrix = Array.tabulate(oscarN)(node1 => Array.tabulate(oscarN)(node2 =>
      allNodesArray(oscarNodeToLinNode(node1)).distance(allNodesArray(oscarNodeToLinNode(node2)))
    ))

    //pickUpDeliveryChains
    val pdps = allNodesArray.toList.flatMap(nodeData => {
      if (nodeData.id == 0) None
      else if (nodeData.pickUP == 0 && nodeData.delivery != 0)
        Some(PDP(fromNode = linNodeToOscarNode(nodeData.id), toNode = linNodeToOscarNode(nodeData.delivery), demand = nodeData.demand))
      else None
    })
    (v, distanceMatrix, pdps, capacity, singleNodeTransferFunctions)
  }

  val fileName = args(0)
  val enrichment: Int = args(1).toInt
  val partition: Int = args(2).toInt
  val enrichmentSpec: Int = args(3).toInt
  val shiftInsert: Int = args(4).toInt
  runBenchmark(fileName: String, enrichment: Int, partition: Int, enrichmentSpec: Int, shiftInsert: Int)

  def runBenchmark(fileName: String, enrichment: Int, partition: Int, enrichmentSpec: Int, shiftInsert: Int): String = {


    var toReturn = s"file:$fileName\n"

    val m = new Store(noCycle = false)

    println("usage: This fileName enrichment partition enrichmentSpec shiftInsert")

    val (v, symmetricDistance, pdpList, capacity, transferFunctions) = readData(fileName)
    val n = symmetricDistance.length

    println(s"VLSN(PDPTW) v:$v n:$n pdp:${pdpList.length}")

    val penaltyForUnrouted = 10000
    val listOfChains = pdpList.map(_.chain)
    val precedences = pdpList.map(_.precedence)

    val contentsFlow = Array.fill(n)(0L)
    for (pdp <- pdpList) {
      contentsFlow(pdp.fromNode) = pdp.demand.toLong
      contentsFlow(pdp.toNode) = -pdp.demand.toLong
    }

    val vehiclesCapacity = Array.fill(v)(capacity.toLong)

    val myVRP = new VRP(m, n, v)
    val vehicles = 0 until v

    val k = 20
    val l = 1000
    val xNearestVehicles = v-1

    //println("listOfChains: \n" + listOfChains.mkString("\n"))
    // GC
    val gc = GlobalConstraintCore(myVRP.routes, v)

    // Distance
    val vehiclesRouteLength = Array.tabulate(v)(vehicle => CBLSIntVar(m, name = "Route length of vehicle " + vehicle))
    val routeLengthInvariant = new RouteLength(gc, n, v, vehiclesRouteLength, (from: Int, to: Int) => symmetricDistance(from)(to))

    //Time window constraints
    val timeWindowViolations = Array.fill(v)(new CBLSIntVar(m, 0, Domain.coupleToDomain((0, 1))))
    val timeWindowConstraint = TimeWindowConstraint(gc, n, v, transferFunctions, symmetricDistance, timeWindowViolations)
    val timeWindowConstraints = new ConstraintSystem(m)
    for (vehicle <- 0 until v) {
      timeWindowConstraints.add(timeWindowViolations(vehicle) === 0)
    }

    m.registerForPartialPropagation(timeWindowViolations: _*)

    //Chains
    val precedenceRoute = myVRP.routes.createClone()

    //TODO: we need a faster precedence constraint!
    val precedenceInvariant = precedence(precedenceRoute, precedences)
    val vehicleOfNodesNow = vehicleOfNodes(precedenceRoute, v)
    val precedencesConstraints = new ConstraintSystem(m)
    for (start <- precedenceInvariant.nodesStartingAPrecedence)
      precedencesConstraints.add(vehicleOfNodesNow(start) === vehicleOfNodesNow(precedenceInvariant.nodesEndingAPrecedenceStartedAt(start).head))
    precedencesConstraints.add(0 === precedenceInvariant)
    val chainsExtension = chains(myVRP, listOfChains)

    //for a chain, we want the x nearest vehicles to the chain.
    //for each node we take the summe distance to each nodes of the chain to the vehicle
    val chainHeadToSummedDistanceToVehicles = listOfChains.map((chain: List[Int]) => (chain.head, vehicles.map(vehicle => (chain.map((node: Int) => symmetricDistance(node)(vehicle)).sum)).toArray))
    val chainHeadToxNearestVehicles = SortedMap.empty[Int, List[Int]] ++ chainHeadToSummedDistanceToVehicles.map({ case (chainHead, vehicleToDistance) => (chainHead, KSmallest.getkSmallests(vehicles.toArray, xNearestVehicles, (v: Int) => vehicleToDistance(v))) })

    // Vehicle content
    val violationOfContentOfVehicle = Array.tabulate(v)(vehicle =>
      CBLSIntVar(myVRP.routes.model, name = "Violation of capacity of vehicle " + vehicle))
    val capacityInvariant = GlobalVehicleCapacityConstraint(gc, n, v, vehiclesCapacity, contentsFlow, violationOfContentOfVehicle)

    //Objective function
    val unroutedPenalty = penaltyForUnrouted * (n - length(myVRP.routes))

    val obj = new CascadingObjective(new CascadingObjective(timeWindowConstraints, precedencesConstraints),
      new CascadingObjective(sum(violationOfContentOfVehicle),
        sum(vehiclesRouteLength) + unroutedPenalty))

    val objPerVehicle = Array.tabulate[Objective](v)(vehicle =>
      new CascadingObjective(
        new CascadingObjective(new CascadingObjective(timeWindowConstraints, precedencesConstraints), violationOfContentOfVehicle(vehicle)),
        Objective(vehiclesRouteLength(vehicle)))
    )
    val unroutedPenaltyOBj = Objective(unroutedPenalty)

    m.close()

    val relevantPredecessorsTmp: Map[Int, Iterable[Int]] = GlobalVehicleCapacityConstraint.relevantPredecessorsOfNodes(capacityInvariant)

    val relevantPredecessors = SortedMap.empty[Int, SortedSet[Int]] ++ (relevantPredecessorsTmp.map({ case (node, v) => (node, SortedSet.empty[Int] ++ v) }))

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
          case head :: Nil => None
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
          if (timeWindowConstraints.Violation.value != 0) {
            NoMoveNeighborhood
          } else {
            mu[OnePointMoveMove, Option[List[Int]]](
              lastNodeOfChainMove(chainsExtension.lastNodeInChainOfNode(moveMove.movedPoint)),
              nextMoveGenerator,
              None,
              Int.MaxValue,
              false)
          }
        }) name "OneChainMove"
    }

    def onePtMove(k: Int) = profile(onePointMove(myVRP.routed, () => myVRP.kFirst(k, closestRelevantPredecessorsByDistance(_)), myVRP))

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
          if (timeWindowConstraints.Violation.value != 0) {
            NoMoveNeighborhood
          } else {
            mu[InsertPointMove, Option[List[Int]]](
              lastNodeOfChainInsertion(chainsExtension.lastNodeInChainOfNode(insertMove.insertedPoint)),
              nextInsertGenerator,
              None,
              Int.MaxValue,
              false)
          }
        }) name "OneChainInsert"
    }


    /*
      def nodesInRouteAfterPosition(position:Int,routes:IntSequence,v:Int):Iterable[Int] = {
        var currentPosition = routes.explorerAtPosition(position).get
        var toReturn:List[Int] = Nil
        while(currentPosition.value >= v){
          toReturn = currentPosition.value :: toReturn
          currentPosition = currentPosition.next match{
            case None => return toReturn
            case Some(x) => x
          }
        }
        toReturn
      }

      def pickUpPointToDeliveryPoint(pickUpPoint:Int):Int = ???

      def insertPickUp(pickUpPointsToInsertOpt:Option[Iterable[Int]],
                       targetVehiclesOpt:Option[Iterable[Int]]) =
        insertPointUnroutedFirst(
          () => pickUpPointsToInsertOpt match{
            case None => myVRP.unrouted.value.filter(chainsExtension.isHead)
            case Some(pickUpPoints) => pickUpPoints
          },
          () => targetVehiclesOpt match{
            case None => myVRP.kFirst(k,closestRelevantPredecessorsByDistance(_))
            case Some(vehicle) => ???
          },
          selectInsertionPointBehavior = Best(),
          vrp = myVRP,
          neighborhoodName = "InsertPickUp")

      def insertDelivery(deliveryPoint:Int,nodesInRouteAfterPickUp:Iterable[Int]) =
        insertPointUnroutedFirst(
          () => List(deliveryPoint),
          () => _ => nodesInRouteAfterPickUp,
          selectInsertionPointBehavior = Best(),
          vrp=myVRP,
          neighborhoodName = s"insertDelivery:$deliveryPoint")

      def insertPDP(pickUpPointsToInsertOpt:Option[Iterable[Int]],
                    targetVehiclesOpt:Option[Iterable[Int]]) =
        dynAndThen(
          insertPickUp(pickUpPointsToInsertOpt,targetVehiclesOpt),
          (insertMove: InsertPointMove) =>
            if(timeWindowConstraints.Violation.value != 0){
              NoMoveNeighborhood
            }else{
              val deliveryPoint = pickUpPointToDeliveryPoint(insertMove.insertedPoint)
              val nodesInRouteAfterPickUp = nodesInRouteAfterPosition(insertMove.insertAtPosition,myVRP.routes.value,v)
              insertDelivery(deliveryPoint, nodesInRouteAfterPickUp)
            }) name "insetPDP"

    */

    // //////////////////////////////////////////////////////////////////////////////////////////////////
    val relevantPredecessorsOfNodes = (node: Int) => myVRP.nodes

    val lClosestNeighborsByDistance: Array[SortedSet[Int]] = Array.tabulate(n)(node =>
      SortedSet.empty[Int] ++ myVRP.kFirst(l, (node: Int) => closestRelevantPredecessorsByDistance(node))(node))

    def routeUnroutedChainVLSN(targetVehicle: Int): (Int => Neighborhood) = {

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
            if (timeWindowViolations(targetVehicle).value != 0) {
              NoMoveNeighborhood
            } else {
              mu[InsertPointMove, Option[List[Int]]](
                lastNodeOfChainInsertion(chainsExtension.lastNodeInChainOfNode(insertMove.insertedPoint)),
                nextInsertGenerator,
                None,
                Int.MaxValue,
                false)
            }
          }) name "insertChainVLSN"
      }
    }

    def moveChainVLSN(targetVehicle: Int): (Int => Neighborhood) = {
      val nodesOfTargetVehicle = (SortedSet.empty[Int] ++ myVRP.getRouteOfVehicle(targetVehicle))

      (chainHeadToMove: Int) => {
        val relevantNodesOfTargetVehicle = nodesOfTargetVehicle intersect (relevantPredecessors(chainHeadToMove))
        val lNearestNodesOfTargetVehicle = relevantNodesOfTargetVehicle.intersect(lClosestNeighborsByDistance(chainHeadToMove))

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
            if (timeWindowViolations(targetVehicle).value != 0) {
              NoMoveNeighborhood
            } else {
              mu[OnePointMoveMove, Option[List[Int]]](
                lastNodeOfChainMove(chainsExtension.lastNodeInChainOfNode(moveMove.movedPoint)),
                nextMoveGenerator,
                None,
                Int.MaxValue,
                false)
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
      val nodesOfTargetVehicle = (SortedSet.empty[Int] ++ myVRP.getRouteOfVehicle(vehicle))
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
          if (timeWindowViolations(vehicle).value != 0) {
            NoMoveNeighborhood
          } else {
            mu[OnePointMoveMove, Option[List[Int]]](
              lastNodeOfChainMove(chainsExtension.lastNodeInChainOfNode(moveMove.movedPoint)),
              nextMoveGenerator,
              None,
              Int.MaxValue,
              false)
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
        false)
    }

    def removeAndReInsertVLSN(headOfChainToRemove: Int): (() => Unit) = {
      val checkpointBeforeRemove = myVRP.routes.defineCurrentValueAsCheckpoint(true)
      require(headOfChainToRemove >= v, "cannot remove vehicle point: " + headOfChainToRemove)

      val allNodesOfChain = chainsExtension.chainOfNode(headOfChainToRemove)
      for (nodeToRemove <- allNodesOfChain) {
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
        () => SortedMap.empty[Int, SortedSet[Int]] ++
          vehicles.map((vehicle: Int) =>
            (vehicle: Int, SortedSet.empty[Int] ++ myVRP.getRouteOfVehicle(vehicle).filter(node => chainsExtension.isHead(node)))),
        () => SortedSet.empty[Int] ++ myVRP.unroutedNodes.filter(node => chainsExtension.isHead(node)),
        nodeToRelevantVehicles = () => chainHeadToxNearestVehicles,

        targetVehicleNodeToInsertNeighborhood = routeUnroutedChainVLSN,
        targetVehicleNodeToMoveNeighborhood = moveChainVLSN,
        removeChainVLSN,

        removeNodeAndReInsert = removeAndReInsertVLSN,

        reOptimizeVehicle = None, //Some(vehicle => Some(threeOptOnVehicle(vehicle) exhaustBack moveChainWithinVehicle(vehicle))),

        objPerVehicle,
        unroutedPenaltyOBj,
        obj,

        cycleFinderAlgoSelection = CycleFinderAlgoType.Mouthuy,
        //1
        enrichmentSchemeSpec = {
          val toReturnp = enrichment match {
            case 0 =>
              println("BENCHMARK: NoEnrichment")
              NoEnrichment()
            case 1 =>

              CompositeEnrichmentSchemeSpec(
                partition match {
                  case 0 => SameSizeRandomPartitionsSpec(nbPartitions = 2)
                  case 1 => SameSizeRandomPartitionsSpec(nbPartitions = 5)
                  case 2 => SameSizeRandomPartitionsSpec(nbPartitions = 10)
                  case 3 => SameSizeRandomPartitionsSpec(nbPartitions = 15)
                  case 4 => SameSizeRandomPartitionsSpec(nbPartitions = 20)
                  case 5 => VehicleStructuredSameSizePartitionsSpreadUnroutedSpec(20)
                  case 6 => VehiclePartitionSpec()
                },
                enrichmentSpec match {
                  case 0 => LinearRandomSchemeSpec(nbEnrichmentLevels = 5)
                  case 1 => LinearRandomSchemeSpec(nbEnrichmentLevels = 10)
                  case 2 => LinearRandomSchemeSpec(nbEnrichmentLevels = 15)
                  case 3 => LinearRandomSchemeSpec(nbEnrichmentLevels = 20)
                  case 4 => DivideAndConquerSchemeSpec()
                },
                shiftInsert)
          }
          toReturn = toReturn + toReturnp + "\n"
          println("BENCHMARK:" + toReturnp)
          toReturnp
        },

        name = "VLSN(" + l + ")",
        reoptimizeAtStartUp = false,
        debugNeighborhoodExploration = false
      )
    }

    // ///////////////////////////////////////////////////////////////////////////////////////////////////

    val startTime = System.nanoTime()
    val vlsnNeighborhood = vlsn(l)
    val search = bestSlopeFirst(List(oneChainInsert, oneChainMove, onePtMove(k))) exhaust (vlsnNeighborhood maxMoves 1)
    //val search =vlsnNeighborhood
    search.verbose = 1
    vlsnNeighborhood.verbose = 2

    search.doAllMoves(obj = obj)

    val endTime = System.nanoTime()


    println(myVRP)
    for(vehicle <- 0 until v){
      val l = vehiclesRouteLength(vehicle).value
      if(l !=0) println("vehicle(" + vehicle + ").length:" + l)
    }
    println("obj:" + obj.value)

    toReturn + "\nobj:" + obj.value + "\nduration: " + ((endTime - startTime) / (1000 * 1000))
  }
}