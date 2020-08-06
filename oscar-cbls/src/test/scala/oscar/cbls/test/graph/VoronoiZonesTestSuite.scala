package oscar.cbls.test.graph

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.Checkers
import oscar.cbls._
import oscar.cbls.algo.quick.QList
import oscar.cbls.lib.invariant.graph.VoronoiZones
import oscar.cbls.test.invariants.bench._

class VoronoiZonesTestSuite extends AnyFunSuite with Matchers with Checkers {

  val verbose = 0

  test("Voronoi Zones in conditional graph"){

    val bench = new InvBench(verbose,List(PlusOne()))

    val nbNodes = 50
    val nbConditionalEdges = 50
    val nbNonConditionalEdges = 50

    val nbCentroids = 10

    val openConditions:CBLSSetVar = bench.genIntSetVar(nbVars = 50, range = 0 until nbConditionalEdges,"openConditions")
    val centroids:CBLSSetVar = bench.genIntSetVar(nbVars = nbCentroids, range = 0 until nbCentroids,"centroids")

    val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes,
      mapSide = 1000)

    VoronoiZones(graph,
      graphDiameterOverApprox = Long.MaxValue-1,
      openConditions,
      centroids:SetValue,
      trackedNodes = nbCentroids until nbNodes,
      openConditions.model,
      defaultDistanceForUnreachableNodes = Long.MaxValue)

    bench.run()
  }

  test("Voronoi Zones in conditional graph with non transit nodes"){

    val bench = new InvBench(verbose,List(PlusOne()))

    val nbNodes = 50
    val nbConditionalEdges = 50
    val nbNonConditionalEdges = 50
    val nbTarget = 10

    val nbCentroids = 10

    val openConditions:CBLSSetVar = bench.genIntSetVar(nbVars = 50, range = 0 until nbConditionalEdges, name="openConditions")
    val centroids:CBLSSetVar = bench.genIntSetVar(nbVars = nbCentroids, range = 0 until nbCentroids, name = "Centroids")

    val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes/2,
      mapSide = 1000)

    VoronoiZones(graph,
      graphDiameterOverApprox = Long.MaxValue-1,
      openConditions,
      centroids:SetValue,
      trackedNodes = nbCentroids until nbNodes,
      openConditions.model,
      defaultDistanceForUnreachableNodes = Long.MaxValue)

    bench.run()
  }

  /**
    * Given an arbitrary graph and it's set of nodes (N), calling voronoi.spanningTree(N)
    * should yield a list of edges (E) for which any node (n) of N is in E, except if
    * - n is a centroid
    * - n has no path to its centroid
    */
  test("Voronoi.SpanningTree contains all the given nodes"){

    val bench = new InvBench(verbose, List(PlusOne()))

    val nbNodes = 30
    val nbConditionalEdges = 0
    val nbNonConditionalEdges = 40
    val nbCentroids = 3

    val openConditions: CBLSSetVar = bench.genIntSetVar(nbVars = nbConditionalEdges, range = 0 to nbConditionalEdges, name = "openConditions")
    val centroids: CBLSSetVar = bench.genIntSetVar(nbVars = nbCentroids, range = 0 until nbCentroids, name = "Centroids")

    val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes,
      mapSide = 1000)

    val voronoi = VoronoiZones(graph,
      graphDiameterOverApprox = Long.MaxValue - 1,
      openConditions,
      centroids,
      trackedNodes = nbCentroids until nbNodes,
      openConditions.model,
      defaultDistanceForUnreachableNodes = Long.MaxValue)

    val tree = voronoi.spanningTree(QList.buildFromIterable(graph.nodes))
    val edgeList = tree.toList

    graph.nodes.forall(n => {
      val count = edgeList.count(e => e.nodeIDA == n.id || e.nodeIDB == n.id)
      val isCentroid = centroids.value.contains(n.id)
      val pathToCentroid = voronoi.pathToCentroid(n)

      count > 0 || isCentroid || pathToCentroid.isEmpty
    }) should be(true)
  }
}
