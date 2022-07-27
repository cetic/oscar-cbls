package examples.oscar.cbls.benchmarks

import oscar.cbls._
import oscar.cbls.algo.generator.RoutingMatrixGenerator
import oscar.cbls.business.routing.invariants.global.RouteLength
import oscar.cbls.business.routing.invariants.vehicleCapacity.GlobalVehicleCapacityConstraint
import oscar.cbls.business.routing.model.helpers.DistanceHelper
import oscar.cbls.business.routing._
import oscar.cbls.core.distrib.Supervisor
import oscar.cbls.core.search.Neighborhood
import oscar.cbls.lib.search.combinators.distributed.{DistributedBest, DistributedFirst, Remote}
import oscar.cbls.modeling.ModelingAPI

import scala.collection.immutable.HashSet
import scala.collection.parallel.immutable.ParRange

object PDPDistributedBench extends ModelingAPI {
  // Inner Classes
  sealed trait NeighborhoodType {
    def withHotRestart: Boolean
  }

  case object Sequential extends NeighborhoodType {

    override def toString: String = "Sequential"

    override val withHotRestart: Boolean = false
  }

  case object DistRemote extends NeighborhoodType {
    override def toString: String = "Remote"

    override val withHotRestart: Boolean = false
  }

  case object DistRestart extends NeighborhoodType {
    override def toString: String = "Distributed Restart"

    override val withHotRestart: Boolean = false
  }

  case class DistFirst(withHotRestart: Boolean = true) extends NeighborhoodType {
    override def toString: String = "Distributed First"
  }

  case class DistBest(withHotRestart: Boolean = true) extends NeighborhoodType {
    override def toString: String = "Distributed Best"
  }

  case class BenchResult(nbName: String,
                         withHotRestart: Boolean,
                         nbWorkers: Int,
                         nbVehicles: Int,
                         nbPoints: Int,
                         timeCreatedModel: Long,
                         timeActorSysCreation: Long,
                         timeRunning: Long,
                         objective: Long,
                         nbIters: Long) {
    def toCSVLine: String = s"$nbName;$withHotRestart;$nbWorkers;$nbVehicles;$nbPoints;$timeCreatedModel;$timeActorSysCreation;$timeRunning;$objective;$nbIters"
  }

  //Nb Iterations per benchmark
  val NB_ITERS = 20

  //Max nbWorkers
  val MAX_WORKERS: Int = 8

  // Constants
  val penaltyForUnrouted = 10000
  val maxVehicleCapacity = 8
  val minVehicleCapacity = 4

  def createPDPParameters(nbVehicles: Int,
                          nbPoints: Int): (Array[Array[Long]], List[List[Int]], List[(Int, Int)], Array[Long], Array[Long]) = {
    val symmetricDistance = RoutingMatrixGenerator(nbPoints)._1
    val (listOfChains, precedences) = RoutingMatrixGenerator.generateChainsPrecedence(nbPoints, nbVehicles, (nbPoints - nbVehicles) / 2)
    val contentsFlow = RoutingMatrixGenerator.generateContentFlow(nbPoints, listOfChains, maxVehicleCapacity)
    val vehiclesCapacity = RoutingMatrixGenerator.generateVehiclesSize(nbVehicles, maxVehicleCapacity, minVehicleCapacity)
    /////
    (symmetricDistance, listOfChains, precedences, contentsFlow, vehiclesCapacity)
  }

  def createCBLSModel(nbVehicles: Int,
                      nbPoints: Int,
                      symmetricDistance: Array[Array[Long]],
                      listOfChains: List[List[Int]],
                      precedences: List[(Int, Int)],
                      contentsFlow: Array[Long],
                      vehiclesCapacity: Array[Long],
                      neighborhoodType: NeighborhoodType): (Store, Objective, Neighborhood) = {
    //CBLS Store
    val m = Store(noCycle = false)
    val myVRP = new VRP(m, nbPoints, nbVehicles)
    // Distance
    val vehiclesRouteLength = Array.tabulate(nbVehicles)(vehicle => CBLSIntVar(m, name = s"Route length of vehicle $vehicle"))
    val routeLengthInvariant = new RouteLength(myVRP.routes, nbPoints, nbVehicles, vehiclesRouteLength, (from: Int, to: Int) => symmetricDistance(from)(to))
    //Chains
    val precedenceRoute = myVRP.routes.createClone()
    val precedenceInvariant = precedence(precedenceRoute, precedences)
    val vehicleOfNodesNow = vehicleOfNodes(precedenceRoute, nbVehicles)
    val precedencesConstraints = ConstraintSystem(m)
    for (start <- precedenceInvariant.nodesStartingAPrecedence)
      precedencesConstraints.add(vehicleOfNodesNow(start) === vehicleOfNodesNow(precedenceInvariant.nodesEndingAPrecedenceStartedAt(start).head))
    precedencesConstraints.add(0 === precedenceInvariant)
    val chainsExtension = chains(myVRP, listOfChains)
    // Vehicle content
    val violationOfContentOfVehicle = Array.tabulate(nbVehicles)(vehicle =>
      CBLSIntVar(myVRP.routes.model, name = s"Violation of capacity of vehicle $vehicle"))
    val capacityInvariant = GlobalVehicleCapacityConstraint(myVRP.routes, nbPoints, nbVehicles, vehiclesCapacity, contentsFlow, violationOfContentOfVehicle)
    // Objective function
    val obj = new CascadingObjective(precedencesConstraints,
      new CascadingObjective(sum(violationOfContentOfVehicle),
        sum(vehiclesRouteLength) + (penaltyForUnrouted * (nbPoints - length(myVRP.routes)))))
    m.close()
    // Neighborhoods
    val relevantPredecessors = capacityInvariant.relevantPredecessorsOfNodes
    val closestRelevantPredecessorsByDistance = Array.tabulate(nbPoints)(DistanceHelper.lazyClosestPredecessorsOfNode(symmetricDistance, relevantPredecessors)(_))
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
      () => myVRP.kFirst(nbVehicles * 2, closestRelevantPredecessorsByDistance(_)),
      myVRP,
      neighborhoodName = "MoveHeadOfChain"
    )

    /////
    def lastNodeOfChainMove(lastNode: Int) = onePointMove(
      () => List(lastNode),
      () => myVRP.kFirst(nbVehicles * 2,
        ChainsHelper.relevantNeighborsForLastNodeAfterHead(
          myVRP,
          chainsExtension,
          Some(HashSet() ++ relevantPredecessors(lastNode)))),
      myVRP,
      neighborhoodName = "MoveLastOfChain"
    )

    // One Chain Move Neighborhood
    val oneChainMove = dynAndThen(firstNodeOfChainMove,
      (moveMove: OnePointMoveMove) => {
        mu[OnePointMoveMove, Option[List[Int]]](
          lastNodeOfChainMove(chainsExtension.lastNodeInChainOfNode(moveMove.movedPoint)),
          nextMoveGenerator,
          None,
          Long.MaxValue,
          intermediaryStops = false)
      }
    ) name "OneChainMove"

    /////
    def onePtMove(k: Int) = onePointMove(
      myVRP.routed,
      () => myVRP.kFirst(k, closestRelevantPredecessorsByDistance(_)), myVRP
    )

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
    val firstNodeOfChainInsertion = insertPointUnroutedFirst(
      () => myVRP.unrouted.value.filter(chainsExtension.isHead),
      () => {
        myVRP.kFirst(nbVehicles * 2, closestRelevantPredecessorsByDistance(_))
      },
      myVRP,
      neighborhoodName = "InsertUF"
    )

    /////
    def lastNodeOfChainInsertion(lastNode: Int) = insertPointUnroutedFirst(
      () => List(lastNode),
      () => myVRP.kFirst(
        nbVehicles * 2,
        ChainsHelper.relevantNeighborsForLastNodeAfterHead(
          myVRP,
          chainsExtension)),
      myVRP,
      neighborhoodName = "InsertUF"
    )

    // One Chain Insert Neighborhood
    val oneChainInsert = dynAndThen(
      firstNodeOfChainInsertion,
      (insertMove: InsertPointMove) => {
        mu[InsertPointMove, Option[List[Int]]](
          lastNodeOfChainInsertion(chainsExtension.lastNodeInChainOfNode(insertMove.insertedPoint)),
          nextInsertGenerator,
          None,
          Long.MaxValue,
          intermediaryStops = false)
      }
    ) name "OneChainInsert"
    // Choice of Neighborhood
    val arrayNbs: Array[Neighborhood] = Array(
      oneChainInsert,
      oneChainMove,
      onePtMove(20)
    )
    val basicNeighborhood: Neighborhood = bestSlopeFirst(arrayNbs.toList)
    val neighborhood: Neighborhood = neighborhoodType match {
      case Sequential =>
        basicNeighborhood
      case DistRemote =>
        new Remote(basicNeighborhood)
      case DistRestart =>
        throw new IllegalArgumentException("Restart not applicable")
      case DistFirst(_) =>
        new DistributedFirst(arrayNbs)
      case DistBest(_) =>
        new DistributedBest(arrayNbs)
    }
    (m, obj, neighborhood)
  }

  def runProblem(nbName: String,
                 nbWorkers: Int,
                 nbVehicles: Int,
                 nbPoints: Int,
                 symmetricDistance: Array[Array[Long]],
                 listOfChains: List[List[Int]],
                 precedences: List[(Int, Int)],
                 contentsFlow: Array[Long],
                 vehiclesCapacity: Array[Long],
                 neighborhoodType: NeighborhoodType): BenchResult = {
    // Stage 1 : Create Model
    val t0 = System.nanoTime()
    val (_, obj, nb) = createCBLSModel(nbVehicles, nbPoints, symmetricDistance,
      listOfChains, precedences, contentsFlow, vehiclesCapacity, neighborhoodType)
    val t1 = System.nanoTime()
    val timeCreation = (t1 - t0) / 1000000
    // Search Procedure Execution
    val benchResult: BenchResult = neighborhoodType match {
      case Sequential =>
        // Sequential execution
        val t2 = System.nanoTime()
        val iters = nb.doAllMoves(obj = obj)
        val t3 = System.nanoTime()
        val timeRun = (t3 - t2) / 1000000
        val objValue = obj.value
        BenchResult(nbName, neighborhoodType.withHotRestart, 0, nbVehicles, nbPoints, timeCreation, 0, timeRun, objValue, iters)
      case _ =>
        // Stage 2 : Start actor system
        val t2 = System.nanoTime()
        val supervisor: Supervisor = Supervisor.startSupervisorAndActorSystem(nb, hotRestart = neighborhoodType.withHotRestart)
        val t3 = System.nanoTime()
        val timeActSys = (t3 - t2) / 1000000
        // Stage 3 : Run procedure
        val t4 = System.nanoTime()
        var iters = 0
        for (i <- ParRange(0, nbWorkers + 1, 1, inclusive = true)) {
          if (i == 0) {
            nb.verbose = 0
            iters = nb.doAllMoves(obj = obj)
          } else {
            val (mi, _, nbi) = createCBLSModel(nbVehicles, nbPoints, symmetricDistance,
              listOfChains, precedences, contentsFlow, vehiclesCapacity, neighborhoodType)
            supervisor.createLocalWorker(mi, nbi)
            nbi.verbose = 0
          }
        }
        supervisor.shutdown()
        val t5 = System.nanoTime()
        val timeRun = (t5 - t4) / 1000000
        val objValue = obj.value
        BenchResult(nbName, neighborhoodType.withHotRestart, nbWorkers, nbVehicles, nbPoints, timeCreation, timeActSys, timeRun, objValue, iters)
    }
    benchResult
  }

  def runBenchmarkNb(j: Int,
                     nbVsI: Int,
                     nbPsI: Int,
                     symmetricDistance: Array[Array[Long]],
                     listOfChains: List[List[Int]],
                     precedences: List[(Int, Int)],
                     contentsFlow: Array[Long],
                     vehiclesCapacity: Array[Long],
                     neighborhoodType: NeighborhoodType): Unit = {
    for {_ <- 1 to NB_ITERS} {
      System.gc()
      val bench = runProblem(neighborhoodType.toString, j, nbVsI, nbPsI, symmetricDistance,
        listOfChains, precedences, contentsFlow, vehiclesCapacity, neighborhoodType)
      println(bench.toCSVLine)
    }
  }

  def main(args: Array[String]): Unit = {
    val nbVs = Array(10)
    val nbPs = Array(500)
    // Warming loop
    val (sD, lOC, prec, cF, vC) = createPDPParameters(10, 500)
    runProblem("Warming", 0, 10, 500, sD, lOC, prec, cF, vC, Sequential)
    runProblem("Warming", 1, 10, 500, sD, lOC, prec, cF, vC, DistRemote)
    println("Neighborhood Name;Hot Restart;# Workers;# Warehouses;# Delivery Points;Model Creation (ms);Actor Creation (ms);Solution Computing (ms);Objective Value;# Iterations")
    for {i <- nbVs.indices} {
      val (sDI, lOCI, precI, cFI, vCI) = createPDPParameters(nbVs(i), nbPs(i))
      // Sequential
      runBenchmarkNb(0, nbVs(i), nbPs(i), sDI, lOCI, precI, cFI, vCI, Sequential)
      // Remote
      runBenchmarkNb(1, nbVs(i), nbPs(i), sDI, lOCI, precI, cFI, vCI, DistRemote)
      for {j <- 2 to MAX_WORKERS} {
        // Distributed First with Hot restart
        runBenchmarkNb(j, nbVs(i), nbPs(i), sDI, lOCI, precI, cFI, vCI, DistFirst())
        // Distributed First without Hot restart
        runBenchmarkNb(j, nbVs(i), nbPs(i), sDI, lOCI, precI, cFI, vCI, DistFirst(false))
        // Distributed Best with Hot restart
        runBenchmarkNb(j, nbVs(i), nbPs(i), sDI, lOCI, precI, cFI, vCI, DistBest())
        // Distributed Best without Hot restart
        runBenchmarkNb(j, nbVs(i), nbPs(i), sDI, lOCI, precI, cFI, vCI, DistBest(false))
      }
    }
  }

}
