package oscar.cbls.test.graph

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}
import oscar.cbls.algo.dag.{ConcreteDAG, ConcreteDAGNode, CycleException}

import scala.util.Random

class DAGTestSuite extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

  test("notifyAddEdge throws CycleException when autoSort activated"){

    val nodes = (0 to 10).map(new ConcreteDAGNode(_))
    val dag = new ConcreteDAG(nodes)
    dag.autoSort = true

    nodes(0).setAsPrecedingNodeKnownNotYetPreceding(nodes(1))
    dag.notifyAddEdge(nodes(0),nodes(1))

    nodes(1).setAsPrecedingNodeKnownNotYetPreceding(nodes(2))
    dag.notifyAddEdge(nodes(1),nodes(2))

    nodes(2).setAsPrecedingNodeKnownNotYetPreceding(nodes(3))
    dag.notifyAddEdge(nodes(2),nodes(3))

    nodes(3).setAsPrecedingNodeKnownNotYetPreceding(nodes(4))
    dag.notifyAddEdge(nodes(3),nodes(4))

    nodes(4).setAsPrecedingNodeKnownNotYetPreceding(nodes(5))
    dag.notifyAddEdge(nodes(4),nodes(5))

    nodes(5).setAsPrecedingNodeKnownNotYetPreceding(nodes(6))
    dag.notifyAddEdge(nodes(5),nodes(6))

    nodes(6).setAsPrecedingNodeKnownNotYetPreceding(nodes(3))
    an [CycleException] should be thrownBy dag.notifyAddEdge(nodes(6),nodes(3))
  }

  test("doDagSort throws CycleException when called on cycle"){
    val nodes = (0 to 10).map(new ConcreteDAGNode(_))
    val dag = new ConcreteDAG(nodes)

    nodes(0).setAsPrecedingNodeKnownNotYetPreceding(nodes(1))
    nodes(1).setAsPrecedingNodeKnownNotYetPreceding(nodes(2))
    nodes(2).setAsPrecedingNodeKnownNotYetPreceding(nodes(3))
    nodes(4).setAsSucceedingNodeKnownNotYetSucceeding(nodes(3))
    nodes(4).setAsPrecedingNodeKnownNotYetPreceding(nodes(5))
    nodes(6).setAsSucceedingNodeKnownNotYetSucceeding(nodes(5))
    nodes(6).setAsPrecedingNodeKnownNotYetPreceding(nodes(3))

    an [CycleException] should be thrownBy dag.doDAGSort()
  }

  test("getCycle returns expected nodes"){

    val nodes = (0 to 10).map(new ConcreteDAGNode(_))
    val dag = new ConcreteDAG(nodes)

    nodes(1).setAsSucceedingNodeKnownNotYetSucceeding(nodes(0))
    nodes(1).setAsPrecedingNodeKnownNotYetPreceding(nodes(2))
    nodes(2).setAsPrecedingNodeKnownNotYetPreceding(nodes(3))
    nodes(4).setAsSucceedingNodeKnownNotYetSucceeding(nodes(3))
    nodes(5).setAsSucceedingNodeKnownNotYetSucceeding(nodes(4))
    nodes(5).setAsPrecedingNodeKnownNotYetPreceding(nodes(6))
    nodes(6).setAsPrecedingNodeKnownNotYetPreceding(nodes(3))

    dag.getCycle() should contain allOf (nodes(3),nodes(4),nodes(5),nodes(6))
  }

  test("CheckSort does not throw with acyclic graph"){
    forAll(acyclicGraphGen){ graph => {

      val nodes = (0 to graph._1).map(new ConcreteDAGNode(_))
      val dag = new ConcreteDAG(nodes)

      for(tuple <- graph._2){
        nodes(tuple._1).setAsPrecedingNodeKnownNotYetPreceding(nodes(tuple._2))
        dag.notifyAddEdge(nodes(tuple._1),nodes(tuple._2))
      }

      dag.doDAGSort()

      noException should be thrownBy dag.checkSort()
    }}
  }

  test("CheckGraph does not throw with acyclic graph"){
    forAll(acyclicGraphGen){ graph => {

      val nodes = (0 to graph._1).map(new ConcreteDAGNode(_))
      val dag = new ConcreteDAG(nodes)

      for(tuple <- graph._2){
        nodes(tuple._1).setAsPrecedingNodeKnownNotYetPreceding(nodes(tuple._2))
        dag.notifyAddEdge(nodes(tuple._1),nodes(tuple._2))
      }

      dag.doDAGSort()

      noException should be thrownBy dag.checkGraph()
    }}
  }

  test("GetCycle returns null when graph is acyclic (initialized with setAsPrecedingNode)"){
    forAll(acyclicGraphGen){ graph => {

      val shuffledGraph = Random.shuffle(graph._2.toList)
      val nodes = (0 to graph._1).map(new ConcreteDAGNode(_))

      val dag = new ConcreteDAG(nodes)
      dag.autoSort = true

      for(tuple <- shuffledGraph){
        nodes(tuple._1).setAsPrecedingNodeKnownNotYetPreceding(nodes(tuple._2))
        dag.notifyAddEdge(nodes(tuple._1),nodes(tuple._2))
      }

      dag.doDAGSort()
      dag.getCycle() should be (Nil)
    }}
  }


  test("GetCycle returns null when graph is acyclic (initialized with setAsSucceedingNode)"){
    forAll(acyclicGraphGen){ graph => {

      val shuffledGraph = Random.shuffle(graph._2.toList)
      val nodes = (0 to graph._1).map(new ConcreteDAGNode(_))

      val dag = new ConcreteDAG(nodes)
      dag.autoSort = true

      for(tuple <- shuffledGraph){
        nodes(tuple._1).setAsSucceedingNodeKnownNotYetSucceeding(nodes(tuple._2))
        dag.notifyAddEdge(nodes(tuple._2),nodes(tuple._1))
      }

      dag.doDAGSort()
      dag.getCycle() should be (Nil)
    }}
  }

  // Generates a list of 0 to 30 unique tuples, guaranteed to form an acyclic graph
  val acyclicGraphGen: Gen[(Int, Array[(Int, Int)])] = for {
    v <- Gen.choose(1, 30) // Number of tuples
    p <- Gen.choose(10,50) // Probability of 2 nodes to be neighbor
    d <- Gen.choose(v,100) // Density of precedences
  } yield (v,Array.tabulate(v)(item => (item,item+1)) ++ Array.tabulate(d)(count => {
      var i = count % v
      var target = i + Random.nextInt(v - i)
      (i,target)
  }).filter(t => t._2 != t._1))
}
