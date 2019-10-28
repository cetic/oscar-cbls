package oscar.cbls.test.graph

import org.scalatest.{FunSuite, Matchers}
import oscar.cbls.algo.graph.{ConditionalGraphWithIntegerNodeCoordinates, FloydWarshall, Node}

class FloydWarshallTestSuite extends FunSuite with Matchers{

  test("diagonal of matrix from Floyd Warshall should be 0"){
    val nbNodes = 10
    val nbConditionalEdges = 0
    val nbNonConditionalEdges = 15

    val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes/2,
      mapSide = 1000)

    val underApproxDistanceMatrix = FloydWarshall.buildDistanceMatrix(graph,_ => true)
    for(i <- underApproxDistanceMatrix.indices){
      underApproxDistanceMatrix(i)(i) should be (0)
    }
  }

  test("everything but diagonal of matrix from Floyd Warshall should be != 0"){
    val nbNodes = 10
    val nbConditionalEdges = 0
    val nbNonConditionalEdges = 15

    val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes/2,
      mapSide = 1000)

    val underApproxDistanceMatrix = FloydWarshall.buildDistanceMatrix(graph,_ => true)
    for(i <- underApproxDistanceMatrix.indices){
      for(j <- underApproxDistanceMatrix.indices){
        if(i != j){
          underApproxDistanceMatrix(i)(j) should not be 0
        }
      }
    }
  }

  test("distances from node to unreachable node should be Long.MaxValue"){
    val nbNodes = 10
    val nbConditionalEdges = 0
    val nbNonConditionalEdges = 15
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

    val last = graphTemp.nodes.length

    val underApproxDistanceMatrix = FloydWarshall.buildDistanceMatrix(graph,_ => true)
    for(i <- 0 until underApproxDistanceMatrix.length-1){

      underApproxDistanceMatrix(i)(last) should be(Long.MaxValue)
    }
  }
}
