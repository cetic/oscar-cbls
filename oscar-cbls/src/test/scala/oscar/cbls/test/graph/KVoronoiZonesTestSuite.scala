package oscar.cbls.test.graph

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.Checkers
import oscar.cbls.CBLSSetVar
import oscar.cbls.lib.invariant.graph.KVoronoiZones
import oscar.cbls.test.invariants.bench._

class KVoronoiZonesTestSuite extends AnyFunSuite with Matchers with Checkers {

  val verbose = 0

  test("Voronoi Zones in conditional graph"){
    val nbTests = 5

    for (_ <- 0 until nbTests) {

      val bench = new InvBench(verbose,List(Random(),Shuffle(),MultipleMove()))

      val nbNodes = 1000
      val nbCentroids = 200

      val nbConditionalEdges = (nbNodes + nbCentroids) * 3
      val nbNonConditionalEdges = (nbNodes + nbCentroids) * 3

      val k = 5

      val openConditions:CBLSSetVar = bench.genIntSetVar(nbVars = nbConditionalEdges, range = 0 until nbConditionalEdges,"openConditions")
      val centroids:CBLSSetVar = bench.genIntSetVar(nbVars = nbCentroids, range = 0 until nbCentroids,"centroids")

      val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
        nbConditionalEdges,
        nbNonConditionalEdges,
        nbTransitNodes = nbNodes,
        mapSide = 1000,
        seed = Some(2))

      KVoronoiZones(graph,
        openConditions,
        centroids,
        k,
        trackedNodes = nbCentroids until nbNodes,
        openConditions.model,
        defaultDistanceForUnreachableNode = Long.MaxValue
      )

      bench.run()
    }
    }

    test("Voronoi Zones in conditional graph with non transit nodes"){
    val nbTests = 5

    for (_ <- 0 until nbTests) {

      val bench = new InvBench(verbose,List(Random(),Shuffle(),MultipleMove()))

      val nbNodes = 50
      val nbCentroids = 10

      val nbConditionalEdges = (nbNodes + nbCentroids) * 3
      val nbNonConditionalEdges = (nbNodes + nbCentroids) * 3

      val k = 5

      val openConditions:CBLSSetVar = bench.genIntSetVar(nbVars = nbConditionalEdges, range = 0 until nbConditionalEdges, name="openConditions")
      val centroids:CBLSSetVar = bench.genIntSetVar(nbVars = nbCentroids, range = 0 until nbCentroids, name = "Centroids")

      val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
        nbConditionalEdges,
        nbNonConditionalEdges,
        nbTransitNodes = nbNodes/2,
        mapSide = 1000)

      KVoronoiZones(graph,
        openConditions,
        centroids,
        k,
        trackedNodes = nbCentroids until nbNodes,
        openConditions.model,
        defaultDistanceForUnreachableNode = Long.MaxValue
      )

      bench.run()
    }
    }


  }
