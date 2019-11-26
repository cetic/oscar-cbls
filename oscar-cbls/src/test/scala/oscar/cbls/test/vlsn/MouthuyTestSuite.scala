package oscar.cbls.test.vlsn

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}
import oscar.cbls.lib.search.vlsn.CycleFinderAlgoType._
import oscar.cbls.lib.search.vlsn._
import oscar.cbls.test.graph.RandomGraphGenerator._

class MouthuyTestSuite extends FunSuite with Matchers with GeneratorDrivenPropertyChecks {


  /**
    * Will test wether a negative weight cycle is found in the graph.
    * If Mouthuy finds one, DFS must yield a cycle as well.
    * However, a cycle may not be found by Mouthuy, yet DFS finds one.
    */
  test("Mouthuy is coherent with DFS search (standard & pruned)") {
    forAll(genVlsn,minSuccessful(500)) { vlsnGraph =>

      val mouthuyAlgo = CycleFinderAlgo(vlsnGraph, Mouthuy)
      val dfsAlgo = CycleFinderAlgo(vlsnGraph, DFS)
      val dfsAlgoPruned = CycleFinderAlgo(vlsnGraph, DFSPruned)

      val size = vlsnGraph.nodes.maxBy(_.nodeID).nodeID.toInt
      val liveNodes = Array.tabulate(size + 1)(_ => true)
      val cycle = mouthuyAlgo.findCycle(liveNodes)

      if (cycle.isDefined) {
        dfsAlgo.findCycle(liveNodes) shouldBe defined
        dfsAlgoPruned.findCycle(liveNodes) shouldBe defined
        println("Mouthuy found a cycle")
      }
      else if (dfsAlgo.findCycle(liveNodes).isDefined) {
        println("DFS found a cycle")
      }
      else if (dfsAlgoPruned.findCycle(liveNodes).isDefined) {
        println("DFS pruned found a cycle")
      }
      else {
        println("Definitely no cycle in this graph.")
      }
    }
  }

  test("Cycles found by Mouthuy have negative weight"){
    forAll(genVlsn) { vlsnGraph =>

      val mouthuyAlgo = CycleFinderAlgo(vlsnGraph, Mouthuy)
      val size = vlsnGraph.nodes.maxBy(_.nodeID).nodeID.toInt
      val liveNodes = Array.tabulate(size + 1)(_ => true)
      val cycle = mouthuyAlgo.findCycle(liveNodes)

      if(cycle.isDefined){
        println("Found cycle")

        val edges = cycle.get
        val sum = edges.map(_.deltaObj).sum
        sum.toInt should be < 0
      }
    }
  }

  /*
  test("Mouthuy with deterministic values"){

    val nbNodes = 6
    val nodes = Array.tabulate(nbNodes)(nodeID  =>
      new Node(nodeID, nbNodes + nodeID,VLSNSNodeType.RegularNode,nodeID,nodeID))

    val builder = new VLSNEdgeBuilder(nodes: Array[Node], nbNodes,2) //nbLAbel is set here to nbNodes

    def edge(from: Long, to: Long, gain: Long): Unit = {
      builder.addEdge(
        nodes(from.toInt),
        nodes(to.toInt),
        gain,
        moveTypeObjectGen.sample.get,
        moveTypeEnumGen.sample.get
      )
    }

    edge(0L, 1L, 1L)
    edge(1L, 2L, 1L)
    edge(2L, 3L, 1L)
    edge(3L, 4L, 1L)
    edge(4L, 0L, -1L)
    edge(0L, 3L, 1L)
    edge(2L, 4L, -2L)
    edge(2L, 5L, 2L)
    edge(5L, 0L, 1L)
    edge(4L, 2L, -1L)

    val liveNodes = Array.tabulate(6)(_ => true)
    val graph = builder.finish()
    val mouthuyAlgo = CycleFinderAlgo(graph, Mouthuy)
    val cycle = mouthuyAlgo.findCycle(liveNodes)

    cycle shouldBe defined
  }

*/

  val genVlsn: Gen[VLSNGraph] = for{
    nbNodes <- Gen.choose(10,20)
    nbEdges <- Gen.choose(nbNodes * 2,nbNodes * 3)
  } yield generateVLSN(nbNodes,nbEdges)
}



