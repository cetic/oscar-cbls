package oscar.examples.cbls.graphPartition

import oscar.cbls
import oscar.cbls._
import oscar.cbls.lib.invariant.logic.DenseCount
import oscar.cbls.lib.search.combinators.GuidedLocalSearch3
import oscar.cbls.modeling._

import scala.util.Random

object GraphPartitionGLS extends CBLSModel with App {

  val nbNodes:Int = 500
  val nbEdges:Int = nbNodes * 3 // 500000 //nbNodes * nbNodes / 1000

  require(nbNodes % 2 == 0, "nbNodes must be even")

  println("nbNodes:" + nbNodes + " nbEdges:" + nbEdges)

  def generateRandomEdges(nbNodes:Int,nbEdges:Int):(List[(Int,Int)],Array[List[Int]]) = {
    val adjacencyLists:Array[List[Int]] = Array.fill(nbNodes)(List.empty)
    val allEdges = List.tabulate(nbEdges)(_ => {
      val nodeA = Random.nextInt(nbNodes)
      val nodeB = Random.nextInt(nbNodes)
      adjacencyLists(nodeA) = nodeB :: adjacencyLists(nodeA)
      adjacencyLists(nodeB) = nodeA :: adjacencyLists(nodeB)
      (nodeA,nodeB)
    })
    (allEdges,adjacencyLists)
  }

  val (edges,adjacencyLists) = generateRandomEdges(nbNodes,nbEdges)

  val degree = adjacencyLists.map(_.length)

  //nodes are randomly distributed into the two partitions, so they might be of different sizes
  val nodeToPartition = Array.tabulate(nbNodes)((nodeID:Int) => CBLSIntVar(if(Random.nextBoolean()) 1 else 0, 0 to 1, "partitionOfNode_" + nodeID))

  val noCrossingConstraints = new ConstraintSystem(s)
  for((nodeA,nodeB) <- edges){
    noCrossingConstraints.post(nodeToPartition(nodeA) === nodeToPartition(nodeB))
  }
  noCrossingConstraints.close()

  val Array(nbNodesInCluster0,nbNodesInCluster1) = DenseCount.makeDenseCount(nodeToPartition).counts
  val sameSizeConstraint = nbNodesInCluster0 === nbNodesInCluster1

  val sameSizeObj = Objective(sameSizeConstraint.violation)
  val noCrossingObj = Objective(noCrossingConstraints.violation)

  val nodeToViolation = noCrossingConstraints.violations(nodeToPartition)
  val mostViolatedNodes = argMax(nodeToViolation)
  val violatedNodes = filter(nodeToViolation)
  val nonViolatedNodes = filter(nodeToViolation,_==0)

  close()

  val searchNeighborhood =
    bestSlopeFirst(
      List(
        assignNeighborhood(nodeToPartition, "moveAll") guard (() => sameSizeConstraint.violation.value != 0),
        //profile(swapsNeighborhood(nodeToPartition, "swapAll")),
        swapsNeighborhood(nodeToPartition,
          symmetryCanBeBrokenOnIndices = false,
          searchZone1 = () => mostViolatedNodes.value,
          name = "swap1Most"),
        swapsNeighborhood(nodeToPartition,
          symmetryCanBeBrokenOnIndices = false,
          searchZone2 = () => { val v = mostViolatedNodes.value; (_,_) => v},
          name = "swapAny1Most"),
        //profile(swapsNeighborhood(nodeToPartition, //this one is the most complete of swaps, but highly inefficient compared tpo the others,and I think that it does not bring in more connexity than others (althrough I am not so suer...)
        //  symmetryCanBeBrokenOnIndices = false,
        //  searchZone1 = () => violatedNodes.value, name = "swap1Viol")),
        //profile(swapsNeighborhood(nodeToPartition,
        //  symmetryCanBeBrokenOnIndices = false,
        //  searchZone1 = () => violatedNodes.value,
        //  searchZone2 = (_,_) => violatedNodes.value,
        //  name = "swap2Viol")),
        swapsNeighborhood(nodeToPartition,
          symmetryCanBeBrokenOnIndices = false,
          searchZone1 = mostViolatedNodes,
          searchZone2 = () => {val v = violatedNodes.value; (_,_) => v},
          name = "swap1Most1Viol"),
        swapsNeighborhood(nodeToPartition,
          symmetryCanBeBrokenOnIndices = false,
          searchZone1 = mostViolatedNodes,
          searchZone2 = () => {val v = mostViolatedNodes.value; (_,_) => v},
          name = "swap1Most1Most"),
        swapsNeighborhood(nodeToPartition,
          searchZone1 = mostViolatedNodes,
          searchZone2 = () => (firstNode:Int, itsPartition:Int) => adjacencyLists(firstNode).filter(n => nodeToPartition(n).newValue != itsPartition),
          hotRestart = false,
          symmetryCanBeBrokenOnIndices = false,
          name = "swap1MostVAdj"),
        swapsNeighborhood(nodeToPartition,
          searchZone1 = violatedNodes,
          searchZone2 = () => (firstNode, itsPartition) => adjacencyLists(firstNode).filter(n => nodeToPartition(n).newValue != itsPartition),
          hotRestart = true,
          symmetryCanBeBrokenOnIndices = false,
          name = "swap1ViolAdj"),

        //profile(swapsNeighborhood(nodeToPartition,
        //  searchZone1 = swappableNodes,
        //  searchZone2 = () => {val v = swappableNodes.value; (_,_) => v},
        //  hotRestart = true,
        //  symmetryCanBeBrokenOnIndices = false,
        //  name = "swapSwappableNodes")),

        //swapsNeighborhood(nodeToPartition,   this one does not help, actually
        //  searchZone1 = () => violatedNodes.value,
        //  symmetryCanBeBrokenOnIndices = false,
        //  name = "swap1Viol")

        //profile(swapsNeighborhood(nodeToPartition,
        //  symmetryCanBeBrokenOnIndices = false,
        //  searchZone2 = (firstNode, itsPartition) => adjacencyLists(firstNode).filter(n => nodeToPartition(n).value != itsPartition),
        //  name = "swapAdjacent"))
      ).map(profile(_)),refresh = nbNodes/10)

  val metaHeuristicSearch = (
      GuidedLocalSearch3.progressiveGuidedLocalSearch(
        searchNeighborhood,
        sameSizeObj,
        200,
        100,
        5,
        10,
        consecutiveFailsBeforeDivByTwo = 1,
        maxAttemptsBeforeStop = 1,
        tryWeight2WhenNoMoveFound=false)
      onExhaustRestartAfter(randomizeNeighborhood(nodeToPartition, () => nbNodes/100, name = "randomize" + nbNodes/100), 3, noCrossingObj)
      onExhaustRestartAfter(randomizeNeighborhood(nodeToPartition, () => nbNodes/100, name = "randomize" + nbNodes/10), 3, noCrossingObj)
      saveBestAndRestoreOnExhaust(noCrossingObj,() => sameSizeObj.isZero) //conditional saveBest because there is a condition for the solution to be acceptable.
      showObjectiveFunction(noCrossingObj,"noCrossingObj") showObjectiveFunction(sameSizeObj,"sameSizeObj"))

  metaHeuristicSearch.verboseWithExtraInfo(2, () => Console.GREEN + "sameSizeObj:" + sameSizeObj.value + " noCrossingObj:" + noCrossingObj.value + Console.RESET)

  metaHeuristicSearch.doAllMoves(_ >= nbNodes + nbEdges, noCrossingObj)

  println(metaHeuristicSearch.profilingStatistics)

  println("violation on sameSize(partitions): " + sameSizeConstraint.violation.value)
  println("global violation: " + noCrossingObj.value + "/" + nbEdges)
}

