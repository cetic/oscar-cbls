package oscar.cbls.test.graph

import org.scalacheck.Gen
import oscar.cbls.algo.graph.{ConditionalGraphWithIntegerNodeCoordinates, Edge, Node}
import oscar.cbls.business.routing.neighborhood.InsertPointMove
import oscar.cbls.lib.search.neighborhoods.vlsn._
import oscar.cbls.core.search._
import oscar.cbls.lib.search.neighborhoods._
import oscar.cbls.test.invariants.bench.{InvBench, ToMax}

import scala.util.Random

object RandomGraphGenerator {
  def generatePseudoPlanarConditionalGraph(nbNodes : Int,
                                           nbConditionalEdges:Int,
                                           nbNonConditionalEdges:Int,
                                           nbTransitNodes:Int,
                                           mapSide:Int = 1000,
                                           seed : Option[Long] = None) : ConditionalGraphWithIntegerNodeCoordinates = {
    //closest edges first
    val totalEdges = nbConditionalEdges + nbNonConditionalEdges

    val rand = new Random(seed match {
      case None => System.currentTimeMillis()
      case Some(s) => s
    })

    def randomXY: Int = rand.nextInt(mapSide)
    val pointPosition: Array[(Int, Int)] = Array.tabulate(nbNodes)(_ => (randomXY, randomXY))


    val nodes = 0 until nbNodes

    def distance(fromNode: Int,toNode:Int) = {
      val fromCoord = pointPosition(fromNode)
      val tooCoord = pointPosition(toNode)
      2 + math.sqrt(math.pow(fromCoord._1 - tooCoord._1, 2) + math.pow(fromCoord._2 - tooCoord._2, 2)).toInt
    }

    val allDistances = nodes.flatMap(node1 => (0 until node1).map(node2 => (node1,node2,distance(node1,node2))))

    val sortedDistances = allDistances.sortBy(_._3).toList

    val subDistance = sortedDistances.take(totalEdges)
    val scrambled = rand.shuffle(subDistance).iterator

    val isTransitAllowed = rand.shuffle(nodes.toList.map(i => i < nbTransitNodes)).toArray

    val nodeArray = Array.tabulate(nbNodes)(nodeId => new Node(nodeId,isTransitAllowed(nodeId)))

    new ConditionalGraphWithIntegerNodeCoordinates(
      coordinates = pointPosition,
      nodes = nodeArray,
      edges = Array.tabulate(subDistance.size)(edgeId => {
        val edgeData = scrambled.next
        new Edge(
          edgeId,
          nodeA = nodeArray(edgeData._1),
          nodeB = nodeArray(edgeData._2),
          length = edgeData._3,
          conditionID = if(edgeId < nbConditionalEdges) Some(edgeId) else None
        )
      }),
      nbConditions = nbConditionalEdges)
  }

  def generateVLSN(nbNodes :Int, nbEdges :Int) : VLSNGraph = {


    val bench = new InvBench(0, List(ToMax()))
    val moveTypeEnumGen: Gen[VLSNMoveType.Value] = Gen.frequency(
      (10, VLSNMoveType.InsertNoEject),
      (10, VLSNMoveType.InsertWithEject),
      (10, VLSNMoveType.MoveNoEject),
      (10, VLSNMoveType.MoveWithEject),
      (10, VLSNMoveType.Remove),
      (10, VLSNMoveType.SymbolicTrashToInsert),
      (10, VLSNMoveType.SymbolicVehicleToTrash),
      (10, VLSNMoveType.SymbolicTrashToNodeForEject)
    )

    val moveTypeObjectGen: Gen[Move] = Gen.frequency(
      (1, AddToSetMove(bench.genIntSetVar(5), 0, 0L)),
      (1, AssignMove(bench.genIntVar(0 to 100), 0L, 0, 0L)),
      (1, CallBackMove(() => {}, 0L, "")),
      (1, CompositeMove(List(), 0L)),
      (1, DoNothingMove(0L)),
      (1, FlipMove(0, 1, bench.genIntVars().toArray, 0L)),
      (1, GradientMove(List(), 0L, Nil, 0L)),
      (1, InsertPointMove(0, 0, 0, positionIndependentMoves = true, 0L, null, null)),
      (1, InstrumentedMove(DoNothingMove(0L))),
      (1, LoadSolutionMove(null, 0L)),
      (1, NamedMove(DoNothingMove(0L))),
      (1, RemoveFromSetMove(bench.genIntSetVar(), 0, 0L)),
      (1, RollMove(bench.genIntVars(), 0, 0L)),
      (1, ShiftMove(0, 0, 0, null, 0L)),
      (1, SwapMove(null, null, 0, 0, adjustIfNotInProperDomain = false, 0))
    )

    val nodeTypeGen: Gen[VLSNSNodeType.Value] = Gen.frequency(
      (10, VLSNSNodeType.RegularNode),
      (10, VLSNSNodeType.VehicleNode),
      (2, VLSNSNodeType.UnroutedNode),
      (1, VLSNSNodeType.FictiveNode),
    )

    val tempGraph = generatePseudoPlanarConditionalGraph(nbNodes, 0, nbEdges, 0)

    val nodes = Array.tabulate(nbNodes)(nodeID =>
      new oscar.cbls.lib.search.neighborhoods.vlsn.Node(nodeID, nbNodes + nodeID, nodeTypeGen.sample.get, nodeID, nodeID))

    val builder = new VLSNEdgeBuilder(nodes: Array[oscar.cbls.lib.search.neighborhoods.vlsn.Node], nbNodes, 2) //nbLAbel is set here to nbNodes

    for (tempEdge <- tempGraph.edges) {

      val randomMove = moveTypeObjectGen.sample.get
      val randomType = moveTypeEnumGen.sample.get

      val (from, to) = if (Random.nextBoolean()) (tempEdge.nodeIDA, tempEdge.nodeIDB) else (tempEdge.nodeIDB, tempEdge.nodeIDA)
      builder.addEdge(
        nodes(from),
        nodes(to),
        Gen.choose(-10, 10).sample.get,
        randomMove,
        randomType)
    }
    builder.finish()
  }
/*
  val bench = new InvBench(0,List(ToMax()))
  val moveTypeEnumGen :Gen[VLSNMoveType.Value] = Gen.frequency(
    (10, VLSNMoveType.InsertNoEject),
    (10, VLSNMoveType.InsertWithEject),
    (10, VLSNMoveType.MoveNoEject),
    (10, VLSNMoveType.MoveWithEject),
    (10, VLSNMoveType.Remove),
    (10, VLSNMoveType.SymbolicTrashToInsert),
    (10, VLSNMoveType.SymbolicVehicleToTrash),
    (10, VLSNMoveType.SymbolicTrashToNodeForEject)
  )

  val moveTypeObjectGen :Gen[Move] = Gen.frequency(
    (1,AddToSetMove(bench.genIntSetVar(5),0,0L)),
    (1,AssignMove(bench.genIntVar(0 to 100),0L,0,0L)),
    (1,CallBackMove(() => {},0L,"")),
    (1,CompositeMove(List(),0L)),
    (1,DoNothingMove(0L)),
    (1,FlipMove(0,1,bench.genIntVars().toArray,0L)),
    (1,GradientMove(List(),0L,Nil,0L)),
    (1,InsertPointMove(0,0,0,true,0L,null,null)),
    (1,InstrumentedMove(new DoNothingMove(0L))),
    (1,LoadSolutionMove(null,0L)),
    (1,NamedMove(new DoNothingMove(0L))),
    (1,RemoveFromSetMove(bench.genIntSetVar(),0,0L)),
    (1,RollMove(bench.genIntVars(),0,0L)),
    (1,ShiftMove(0,0,0,null,0L)),
    (1,SwapMove(null,null,0,0,false,0))
  )
*/
  val nodeTypeGen :Gen[VLSNSNodeType.Value] = Gen.frequency(
    (10,VLSNSNodeType.RegularNode),
    (10,VLSNSNodeType.VehicleNode),
    (2,VLSNSNodeType.UnroutedNode),
    (1,VLSNSNodeType.FictiveNode),
  )
}


