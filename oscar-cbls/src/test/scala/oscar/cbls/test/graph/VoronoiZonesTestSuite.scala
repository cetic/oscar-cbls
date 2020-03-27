package oscar.cbls.test.graph

import org.scalatest.prop.Checkers
import org.scalatest.{FunSuite, Matchers}
import oscar.cbls._
import oscar.cbls.algo.graph._
import oscar.cbls.algo.quick.QList
import oscar.cbls.lib.invariant.graph.VoronoiZones
import oscar.cbls.test.invariants.bench._


class VoronoiZonesTestSuite extends FunSuite with Matchers with Checkers {

  val verbose = 0

  val nbTest = 100
  test("Voronoi Zones in conditional graph"){



    for (i <- (0 until nbTest)) {

      val bench = new InvBench(verbose,List(PlusOne(),Random(),Shuffle(),MultipleMove()))

      val nbNodes = 1000
      val nbCentroids = 200L
      val nbConditionalEdges = (nbNodes + nbCentroids) * 3
      val nbNonConditionalEdges = (nbNodes + nbCentroids) * 3



      val openConditions:CBLSSetVar = bench.genIntSetVar(nbVars = nbConditionalEdges, range = 0 until nbConditionalEdges,"openConditions")
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
  }

  test("Voronoi Zones in conditional graph with non transit nodes"){
    for (i <- (0 until nbTest)) {

      val bench = new InvBench(verbose,List(PlusOne(),Random(),Shuffle(),MultipleMove()))

      val nbNodes = 1000
      val nbCentroids = 200L
      val nbConditionalEdges = (nbNodes + nbCentroids) * 3
      val nbNonConditionalEdges = (nbNodes + nbCentroids) * 3

<<<<<<< HEAD
=======
    val nbCentroids = 10
>>>>>>> Finish the modification so that everything compiles.


      val openConditions:CBLSSetVar = bench.genIntSetVar(nbVars = nbConditionalEdges, range = 0 until nbConditionalEdges, name="openConditions")
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
  }

  test("Voronoi Zones work with centroids in the same point") {
    for (i <- (0 until nbTest)) {

      val bench = new InvBench(verbose,List(PlusOne(),Random(),Shuffle(),MultipleMove()))


      val nbNodes = 1000
      val nbCentroids = 200L
      val nbConditionalEdges = (nbNodes + nbCentroids) * 3
      val nbNonConditionalEdges = (nbNodes + nbCentroids) * 3



      val openConditions:CBLSSetVar = bench.genIntSetVar(nbVars = 50, range = 0 until nbConditionalEdges, name="openConditions")
      val centroids:CBLSSetVar = bench.genIntSetVar(nbVars = nbCentroids, range = 0 until nbCentroids, name = "Centroids")

      val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
        nbConditionalEdges,
        nbNonConditionalEdges,
        nbTransitNodes = nbNodes/2,
        mapSide = 1000)

      val newNodes = graph.nodes.flatMap(n => {
        if (n.id < nbCentroids/2)
          List(new Node(n.id * 2,n.transitAllowed),new Node(n.id * 2 + 1,n.transitAllowed))
        else
          List(new Node(n.id + nbCentroids/2))

      })

      // println("Old:\n" + graph.nodes.mkString("\n"))
      // println("New:\n" + newNodes.mkString("\n"))

      val newCoordinates = Array.tabulate(graph.nbNodes)(i => if (i < nbCentroids / 2) Array(graph.coordinates(i),graph.coordinates(i)) else Array(graph.coordinates(i))).flatten
      // println("Old:\n" + graph.coordinates.mkString("\n"))
      // println("New:\n" + newCoordinates.mkString("\n"))


      var nbDuplicateEdges = 0
      var nbDuplicateCondition = 0
      val nbEdges = graph.edges.length

      val newEdgesNested : Array[Array[Edge]] = for (e <- graph.edges) yield {
        if (e.nodeA.id < nbCentroids/2 || e.nodeB.id < nbCentroids/2) {
          // println(e.nodeA + "<->" + e.nodeB)
          val e1NodeAId = if (e.nodeA.id < nbCentroids / 2) 2 * e.nodeA.id else e.nodeA.id + nbCentroids/2
          val e1NodeBId = if (e.nodeB.id < nbCentroids / 2) 2 * e.nodeB.id else e.nodeB.id + nbCentroids/2
          val e1 = new Edge(e.id,newNodes(e1NodeAId),newNodes(e1NodeBId),e.length,e.conditionID)

          val e2NodeAId = if (e.nodeA.id < nbCentroids / 2) 2 * e.nodeA.id + 1 else e.nodeA.id + nbCentroids/2
          val e2NodeBId = if (e.nodeB.id < nbCentroids / 2) 2 * e.nodeB.id + 1 else e.nodeB.id + nbCentroids/2
          val newCondId = e.conditionID match {
            case None => None
            case Some(e) => nbDuplicateCondition += 1
              Some(nbConditionalEdges.toInt + nbDuplicateCondition - 1)
          }
          val e2 = new Edge(nbEdges + nbDuplicateEdges,newNodes(e2NodeAId),newNodes(e2NodeBId),e.length,newCondId)

          nbDuplicateEdges += 1
          Array[Edge](e1,e2)
        } else {
          Array[Edge](new Edge(e.id,newNodes(e.nodeA.id + nbCentroids/2),newNodes(e.nodeB.id + nbCentroids/2),e.length,e.conditionID))
        }
      }
      val newEdges = newEdgesNested.flatten
      // println("Old:\n" + graph.edges.mkString("\n"))
      // println("New:\n" + newEdges.mkString("\n"))


      val graphWithDuplicatesCentroids = new ConditionalGraphWithIntegerNodeCoordinates(
        newNodes,
        newEdges,
        graph.nbConditions + nbDuplicateCondition,
        newCoordinates
      )


      VoronoiZones(graphWithDuplicatesCentroids,
        graphDiameterOverApprox = Long.MaxValue-1,
        openConditions,
        centroids:SetValue,
        trackedNodes = nbCentroids until nbNodes,
        openConditions.model,
        defaultDistanceForUnreachableNodes = Long.MaxValue)

      bench.run()
    }

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

    val openConditions: CBLSSetVar = bench.genIntSetVar(nbVars = nbConditionalEdges, range = 0 until nbConditionalEdges, name = "openConditions")
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
