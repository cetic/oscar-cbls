package oscar.cbls.test.graph

import oscar.cbls._
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.Checkers
import oscar.cbls.{CBLSSetVar, SetValue}
import oscar.cbls.lib.invariant.graph.KVoronoiZones
import oscar.cbls.test.invariants.bench._

class KVoronoiZonesTestSuite extends FunSuite with Matchers with Checkers {

    val verbose = 0

    test("Voronoi Zones in conditional graph"){

      val bench = new InvBench(verbose,List(Random(),Shuffle(),MultipleMove()))

      val nbNodes = 1000
      val nbCentroids = 200L

      val nbConditionalEdges = (nbNodes + nbCentroids) * 3
      val nbNonConditionalEdges = (nbNodes + nbCentroids) * 3

      val k = 5

      val openConditions:CBLSSetVar = bench.genIntSetVar(nbVars = 50, range = 0 until nbConditionalEdges,"openConditions")
      val centroids:CBLSSetVar = bench.genIntSetVar(nbVars = nbCentroids, range = 0 until nbCentroids,"centroids")

      val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
        nbConditionalEdges,
        nbNonConditionalEdges,
        nbTransitNodes = nbNodes,
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

    test("Voronoi Zones in conditional graph with non transit nodes"){

      val bench = new InvBench(verbose,List(Random()))

      val nbNodes = 50
      val nbCentroids = 10L

      val nbConditionalEdges = (nbNodes + nbCentroids) * 3
      val nbNonConditionalEdges = (nbNodes + nbCentroids) * 3

      val k = 5

      val openConditions:CBLSSetVar = bench.genIntSetVar(nbVars = 50, range = 0 until nbConditionalEdges, name="openConditions")
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
