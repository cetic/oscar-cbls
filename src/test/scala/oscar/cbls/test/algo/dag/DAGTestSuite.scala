package oscar.cbls.test.algo.dag

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import oscar.cbls.algo.dag.{ConcreteDAG, ConcreteDAGNode}
import oscar.cbls.util.exceptions.DAGExceptions

import scala.util.Random

class DAGTestSuite extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with Matchers {
  // Generates a list of 0 to 30 unique tuples, guaranteed to form an acyclic graph
  val acyclicGraphGen: Gen[(Int, Array[(Int, Int)])] = for {
    v <- Gen.choose(1, 30) // Number of tuples
    d <- Gen.choose(v,100) // Density of precedences
  } yield (v,Array.tabulate(v)(item => (item,item+1)) ++ Array.tabulate(d)(count => {
    val i = count % v
    val target = i + Random.nextInt(v - i)
    (i,target)
  }).filter(t => t._2 != t._1))

  test("A DAGException is thrown upon adding a incorrect edge when auto-sort is active."){

    val nodes = (0 to 10).map(new ConcreteDAGNode(_))
    val dag = new ConcreteDAG(nodes)
    dag.incrementalSort = true

    nodes(0).setAsANewPredecessorOf(nodes(1))
    dag.notifyAddEdge(nodes(0),nodes(1))

    nodes(1).setAsANewPredecessorOf(nodes(2))
    dag.notifyAddEdge(nodes(1),nodes(2))

    nodes(2).setAsANewPredecessorOf(nodes(3))
    dag.notifyAddEdge(nodes(2),nodes(3))

    nodes(3).setAsANewPredecessorOf(nodes(4))
    dag.notifyAddEdge(nodes(3),nodes(4))

    nodes(4).setAsANewPredecessorOf(nodes(5))
    dag.notifyAddEdge(nodes(4),nodes(5))

    nodes(5).setAsANewPredecessorOf(nodes(6))
    dag.notifyAddEdge(nodes(5),nodes(6))

    // At this point we have the following graph : 0 --> 1 --> 2 --> 3 --> 4 --> 5 --> 6
    // We want to add 6 --> 3
    nodes(6).setAsANewPredecessorOf(nodes(3))
    an [DAGExceptions] should be thrownBy dag.notifyAddEdge(nodes(6),nodes(3))
  }

  test("Sorting a dag with a cycle throws a CycleException."){
    val nodes = (0 to 10).map(new ConcreteDAGNode(_))
    val dag = new ConcreteDAG(nodes)

    nodes(0).setAsANewPredecessorOf(nodes(1))
    nodes(1).setAsANewPredecessorOf(nodes(2))
    nodes(2).setAsANewPredecessorOf(nodes(3))
    nodes(4).setAsANewSuccessorOf(nodes(3))
    nodes(4).setAsANewPredecessorOf(nodes(5))
    nodes(6).setAsANewSuccessorOf(nodes(5))
    nodes(6).setAsANewPredecessorOf(nodes(3))

    an [DAGExceptions] should be thrownBy dag.doDAGSort()
  }

  test("Finding a cycle works as expected"){

    val nodes = (0 to 10).map(new ConcreteDAGNode(_))
    val dag = new ConcreteDAG(nodes)

    nodes(1).setAsANewSuccessorOf(nodes(0))
    nodes(1).setAsANewPredecessorOf(nodes(2))
    nodes(2).setAsANewPredecessorOf(nodes(3))
    nodes(4).setAsANewSuccessorOf(nodes(3))
    nodes(5).setAsANewSuccessorOf(nodes(4))
    nodes(5).setAsANewPredecessorOf(nodes(6))
    nodes(6).setAsANewPredecessorOf(nodes(3))

    // At this point we have the following graph : 0 --> 1 --> 2 --> 3 --> 4 --> 5 --> 6
    // We want to add 6 --> 3 ==> Cycle will be 3, 4, 5, 6, 3
    dag.getCycle() should contain.allOf(nodes(3),nodes(4),nodes(5),nodes(6))
  }

  test("No ordering exception are thrown with acyclic graph"){
    forAll(acyclicGraphGen){ graph => {

      val nodes = (0 to graph._1).map(new ConcreteDAGNode(_))
      val dag = new ConcreteDAG(nodes)

      for(tuple <- graph._2){
        nodes(tuple._1).setAsANewPredecessorOf(nodes(tuple._2))
        dag.notifyAddEdge(nodes(tuple._1),nodes(tuple._2))
      }

      dag.doDAGSort()

      noException should be thrownBy dag.checkSort()
    }}
  }

  test("No graph incoherence are detected with acyclic graph"){
    forAll(acyclicGraphGen){ graph => {

      val nodes = (0 to graph._1).map(new ConcreteDAGNode(_))
      val dag = new ConcreteDAG(nodes)

      for(tuple <- graph._2){
        nodes(tuple._1).setAsANewPredecessorOf(nodes(tuple._2))
        dag.notifyAddEdge(nodes(tuple._1),nodes(tuple._2))
      }

      dag.doDAGSort()

      noException should be thrownBy dag.checkGraph()
    }}
  }

  test("No cycle are detected when a graph is acyclic (initialized with setAsPrecedingNode)"){
    forAll(acyclicGraphGen){ graph => {

      val shuffledGraph = Random.shuffle(graph._2.toList)
      val nodes = (0 to graph._1).map(new ConcreteDAGNode(_))

      val dag = new ConcreteDAG(nodes)
      dag.incrementalSort = true

      for(tuple <- shuffledGraph){
        nodes(tuple._1).setAsANewPredecessorOf(nodes(tuple._2))
        dag.notifyAddEdge(nodes(tuple._1),nodes(tuple._2))
      }

      dag.doDAGSort()
      dag.getCycle() should be (Nil)
    }}
  }

  test("No cycle are detected when a graph is acyclic (initialized with setAsSucceedingNode)"){
    forAll(acyclicGraphGen){ graph => {

      val shuffledGraph = Random.shuffle(graph._2.toList)
      val nodes = (0 to graph._1).map(new ConcreteDAGNode(_))

      val dag = new ConcreteDAG(nodes)
      dag.incrementalSort = true

      for(tuple <- shuffledGraph){
        nodes(tuple._1).setAsANewSuccessorOf(nodes(tuple._2))
        dag.notifyAddEdge(nodes(tuple._2),nodes(tuple._1))
      }

      dag.doDAGSort()
      dag.getCycle() should be (Nil)
    }}
  }
}
