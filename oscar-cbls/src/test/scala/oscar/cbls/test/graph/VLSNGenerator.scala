package oscar.cbls.test.graph

import oscar.cbls.lib.search.neighborhoods.vlsn._
import oscar.cbls.test.invariants.bench.InvBench
import oscar.cbls.test.invariants.bench.ToMax
import org.scalacheck.Gen
import oscar.cbls.core.search._
import oscar.cbls.lib.search.neighborhoods._
import oscar.cbls.business.routing.neighborhood._
import oscar.cbls.algo.generator.RandomGraphGenerator
import scala.util.Random




object VLSNGenerator {
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
      (1, InsertPointMove(0, 0, 0, positionIndependentMoves = true, 0, 0L, null, null)),
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

    val tempGraph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes, 0, nbEdges, 0)

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
    builder.buildGraph()
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
