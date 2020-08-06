package oscar.cbls.test.graph

import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import oscar.cbls.CBLSSetVar
import oscar.cbls.algo.graph._
import oscar.cbls.lib.invariant.graph.DistanceInConditionalGraph
import oscar.cbls.test.invariants.bench._

import scala.collection.immutable.SortedSet

class DistanceInConditionalGraphTestSuite extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with Matchers{

  val verbose = 0

  test("DistanceInConditionalGraph maintains the distance between two nodes"){

    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    val nbNodes = 100
    val nbConditionalEdges = 50
    val nbNonConditionalEdges = 50
    val nbTarget = 10

    val openConditions:CBLSSetVar = bench.genIntSetVar(nbVars = 50, range = 0 until nbConditionalEdges,"OpenConditions")

    val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes,
      mapSide = 1000)

    val underApproxDistanceMatrix = FloydWarshall.buildDistanceMatrix(graph,_ => true)
    val distancesArray = Array.tabulate(nbTarget)(
      targetID => new DistanceInConditionalGraph(graph,
        from = nbTarget,
        to = targetID,
        openConditions = openConditions,
        distanceIfNotConnected = Long.MaxValue)
      (underApproximatingDistance = (a:Int,b:Int) => underApproxDistanceMatrix(a)(b)))

    bench.run()
  }

  test("DistanceInConditionalGraph maintains the distance between two nodes (with transit)"){

    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))

    val nbNodes = 100
    val nbConditionalEdges = 50
    val nbNonConditionalEdges = 50
    val nbTarget = 10

    val openConditions:CBLSSetVar = bench.genIntSetVar(nbVars = 50, range = 0 until nbConditionalEdges)

    val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes/2,
      mapSide = 1000)

    val underApproxDistanceMatrix = FloydWarshall.buildDistanceMatrix(graph,_ => true)
    val distancesArray = Array.tabulate(nbTarget)(
      targetID => new DistanceInConditionalGraph(graph,
        from = nbTarget,
        to = targetID,
        openConditions = openConditions,
        distanceIfNotConnected = Long.MaxValue)
      (underApproximatingDistance = (a:Int,b:Int) => underApproxDistanceMatrix(a)(b)))

    bench.run()
  }


  test("DistanceInConditionalGraph from a node to itself should be 0"){

    val bench = new InvBench(verbose,List(ToZero()))
    val nbNodes = 10
    val nbConditionalEdges = 0
    val nbNonConditionalEdges = 15
    val openConditions:CBLSSetVar = bench.genIntSetVar(nbVars = 0, range = 0 to 0)
    val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes/2,
      mapSide = 1000)

    val underApproxDistanceMatrix = FloydWarshall.buildDistanceMatrix(graph,_ => true)
    val distancesArray = Array.tabulate(nbNodes)(
      nodeID => new DistanceInConditionalGraph(graph,
        from = nodeID,
        to = nodeID,
        openConditions = openConditions,
        distanceIfNotConnected = Long.MaxValue)
      (underApproximatingDistance = (a:Int,b:Int) => underApproxDistanceMatrix(a)(b)))

    bench.model.close()
    bench.model.propagate()

    distancesArray.foreach(d => d.value should (be(Long.MaxValue) or be(0)))
  }

  test("DistanceInConditionalGraph from a node to an unreachable node should be 'distanceIfNotConnected'"){

    val bench = new InvBench(verbose,List(ToZero()))
    val nbNodes = 10
    val nbConditionalEdges = 0
    val nbNonConditionalEdges = 15
    val openConditions:CBLSSetVar = bench.genIntSetVar(nbVars = nbConditionalEdges, range = 0 to 0)
    val graphTemp = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes/2,
      mapSide = 1000)

    val lonelyNode = new Node(nbNodes,true)

    val graph = new ConditionalGraphWithIntegerNodeCoordinates(
      coordinates = graphTemp.coordinates :+ (0,0),
      nodes = graphTemp.nodes :+ lonelyNode,
      edges = graphTemp.edges,
      nbConditions = graphTemp.nbConditions)

    val underApproxDistance = FloydWarshall.buildDistanceMatrix(graph,_ => true)
    val distancesArray = Array.tabulate(nbNodes)(
      nodeId => new DistanceInConditionalGraph(graph,
        from = nodeId,
        to = lonelyNode.id,
        openConditions = openConditions,
        distanceIfNotConnected = Long.MaxValue)(underApproximatingDistance = (a:Int,b:Int) => underApproxDistance(a)(b))
    )

    bench.model.close()
    bench.model.propagate()

    distancesArray.foreach(d => d.value should (be(Long.MaxValue) or be(0)))
  }

  test("DistanceInConditionalGraph with all edges closed should be 'distanceIfNotConnected' or 0"){
    val bench = new InvBench(verbose,List(ToZero()))
    val nbNodes = 10
    val nbConditionalEdges = 15
    val nbNonConditionalEdges = 0
    val openConditions:CBLSSetVar = new CBLSSetVar(bench.model,SortedSet[Int](), 0 to 0, "openConditions")
    val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes/2,
      mapSide = 1000)

    val underApproxDistanceMatrix = FloydWarshall.buildDistanceMatrix(graph,_ => true)
    val distancesArray = Array.tabulate(nbNodes,nbNodes)(
      (nodeId1,nodeId2) => new DistanceInConditionalGraph(graph,
        from = nodeId1,
        to = nodeId2,
        openConditions = openConditions,
        distanceIfNotConnected = Long.MaxValue)
      (underApproximatingDistance = (a:Int,b:Int) => underApproxDistanceMatrix(a)(b))).flatten


    bench.model.close()
    bench.model.propagate()

    distancesArray.foreach(d => d.value should (be(Long.MaxValue) or be(0)))
  }
}
