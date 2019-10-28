package oscar.cbls.test.graph

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}
import oscar.cbls.algo.clique.Clique

import scala.util.Random

class CliqueTestSuite extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

  //First test with madeup results
  test("Reported clique is expected"){
    val nbNodes = 10
    val adjacencyList:List[(Long,Long)] = List((1,2),(1,3),(4,2),(3,4),(3,6),(6,5),(4,5),(3,5),(4,6),(6,8),(8,5),(5,7))

    val adjacencyDico = adjacencyList ++ adjacencyList.map{case (a,b) => (b,a)}
    def isNeighbor(a:Long,b:Long) = adjacencyDico.contains((a,b))

    val cliques = Clique.bronKerbosch2(nbNodes,isNeighbor:(Long,Long)=>Boolean)

    // Should be only one clique of size 4
    cliques.count(set => set.size == 4) should be (1)

    // This clique should be (3,6,4,5)
    val maxclique = cliques.filter(set => set.size == 4).head
    List(3L,6,4,5).forall(maxclique.contains) should be (true)
  }

  test("Empty graph of N nodes has N cliques each of size 1"){
    val nbNodes = 10
    val adjacencyList:List[(Long,Long)] = List()

    val cliques = Clique.bronKerbosch2(nbNodes,(a,b) => false)

    // Should be only N cliques of size 1
    cliques.count(set => set.size == 1) should be (nbNodes)

    // All cliques should be size 1
    cliques.forall(c => c.size == 1) should be (true)
  }


  test("Reported cliques are strongly connected"){
    forAll(graphGen){gen => {

      val graph = gen._2
      val nbNodes = gen._1

      val adjacencyDico = graph ++ graph.map{case (a,b) => (b,a)}
      def isNeighbor(a:Long,b:Long) = adjacencyDico.contains((a,b))

      val cliques = Clique.bronKerbosch2(nbNodes,isNeighbor:(Long,Long)=>Boolean)

      for(clique <- cliques){
        // All vertexes of the clique are neighbor with eachother (or are equal to themselves)
        clique.forall(v1 => clique.forall(v2 => isNeighbor(v1,v2) || v2 == v1)) should be (true)
      }
    }}
  }

  // Generates a list of 0 to 20 unique tuples
  val graphGen: Gen[(Int, Array[(Long, Long)])] = for {
    v <- Gen.choose(0, 30) // Number of tuples
    p <- Gen.choose(80,90) // Probability of 2 nodes to be neighbor
  } yield (v,Array.tabulate(v)(item => {
    Array.tabulate(v-1)(item2 => {
      if(Random.nextDouble() < p.toFloat/100 && item != item2)
        if(item > item2) (item2 :Long,item :Long) else (item :Long,item2 :Long)
      else
        null
    })
  }).flatMap(_.toList).filter(t => t != null).distinct)
}
