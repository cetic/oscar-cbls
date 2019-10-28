package oscar.cbls.test.graph

import org.scalacheck.Gen
import org.scalactic.anyvals.PosInt
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}
import oscar.cbls.algo.graph._

import scala.util.Random

class DijkstraMTTestSuite extends FunSuite with GeneratorDrivenPropertyChecks with Matchers{

  test("Astar confirms the result from DijkstraMT is the closest terminal"){

    val nbNodes = 50
    val nbConditionalEdges = 90
    val nbNonConditionalEdges = 20
    val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes/2,
      mapSide = 1000)

    val open = Array.tabulate((nbConditionalEdges * 0.5).toInt)(_ => 1L).toList
    val closed = Array.tabulate((nbConditionalEdges * 0.5).toInt)(_ => 0L).toList
    val openConditions = Random.shuffle(open ::: closed)
    val underApproxDistanceMatrix = FloydWarshall.buildDistanceMatrix(graph,_ => true)
    val aStar = new RevisableAStar(graph, underApproximatingDistance = (a:Int,b:Int) => underApproxDistanceMatrix(a)(b))
    val dijkstra = new DijkstraMT(graph)

    val gen = for{
      nbTargets <- Gen.chooseNum(1,10)
      node <- Gen.oneOf(graph.nodes)
      targets <- Gen.listOfN(nbTargets,Gen.oneOf(graph.nodes.filter(_ != node)))
    } yield(node,targets)

    val iterations = PosInt.from(Math.pow(graph.nodes.length,2).toInt).get

    forAll(gen,minSuccessful(iterations)){
      nodeAndTargets =>
        val node = nodeAndTargets._1
        val targets = nodeAndTargets._2

        val dijkstraResult = dijkstra.search(node,targets,openConditions(_) == 1)
        val aStarResults = targets.map(target => {
          aStar.search(node,target,openConditions(_) == 1)
        })

        dijkstraResult match{

          // If dikjstraResult is Unreachable, all aStar should be NeverConnected or NotConnected
          case Unreachable => aStarResults.foreach(_ should (be (a[NeverConnected]) or be (a[NotConnected])))

          // If dijkstraResult is a VoronoiZone, the minimum distance of aStar should match dijkstraResult
          case VoronoiZone(_,distance) =>
            aStarResults
              .filter(r => r.isInstanceOf[Distance])
              .minBy(_.asInstanceOf[Distance].distance)
              .asInstanceOf[Distance].distance should be (distance)
        }
    }
  }

  test("DijkstraMT with only closed conditional edges should yield Unreachable for any nodes"){

    val nbNodes = 50
    val nbConditionalEdges = 90
    val nbNonConditionalEdges = 0
    val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes/2,
      mapSide = 1000)

    val dijkstra = new DijkstraMT(graph)

    val gen = for{
      nbTargets <- Gen.chooseNum(1,10)
      node <- Gen.oneOf(graph.nodes)
      targets <- Gen.listOfN(nbTargets,Gen.oneOf(graph.nodes.filter(_ != node)))
    } yield(node,targets)

    val iterations = PosInt.from(Math.pow(graph.nodes.length,2).toInt).get

    forAll(gen,minSuccessful(iterations)){
      nodeAndTargets =>
        val node = nodeAndTargets._1
        val targets = nodeAndTargets._2

        val dijkstraResult = dijkstra.search(node,targets,_ => false)
        dijkstraResult should be (a [Unreachable.type])
    }
  }
}
