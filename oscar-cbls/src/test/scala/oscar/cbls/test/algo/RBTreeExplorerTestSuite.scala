package oscar.cbls.test.algo

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import oscar.cbls.algo.rb.RedBlackTreeMap

class RBTreeExplorerTestSuite extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with Matchers {
  val intGenerator: Gen[Int] = for (n <- Gen.choose(10, 1000)) yield n

  // Generates a list of key-value tuples with incremental key (in order) and random values
  val sequentialTuplesList: Gen[List[(Int, Int)]] =  for {
    numElems <- Gen.choose(1, 500)
    valuesList <- Gen.listOfN(numElems,intGenerator)
  } yield valuesList.zipWithIndex.map(tuple => (tuple._2,tuple._1))

  test("tree.biggestPosition returns expected value"){
    forAll(sequentialTuplesList){list =>{
      val tree = RedBlackTreeMap.makeFromSorted(list)

      val expectedKey = list.last._1
      val expectedValue = list.last._2

      tree.biggestPosition.get.key should be(expectedKey)
      tree.biggestPosition.get.value should be(expectedValue)
    }}
  }

  test("tree.smallestPosition returns expected value"){
    forAll(sequentialTuplesList){list =>{
      val tree = RedBlackTreeMap.makeFromSorted(list)

      val expectedKey = list.head._1
      val expectedValue = list.head._2

      tree.smallestPosition.get.key should be(expectedKey)
      tree.smallestPosition.get.value should be(expectedValue)
    }}
  }

  test("tree.positionOf on unexisting key returns None"){
    forAll(sequentialTuplesList){list =>{
      val tree = RedBlackTreeMap.makeFromSorted(list)

      tree.positionOf(-1) should be (None)
    }}
  }

  test("Bottom-up iteration of explorer "){
    forAll(sequentialTuplesList){list =>{

      var explorationList = List[(Int,Int)]()
      val tree = RedBlackTreeMap.makeFromSorted(list)
      var explorer = tree.smallestPosition
      while(explorer.isDefined){
        explorationList = explorationList ::: List((explorer.get.key,explorer.get.value))
        explorer = explorer.get.next
      }

      explorationList should be (tree.content)
    }}
  }

  test("Top-down iteration of explorer "){
    forAll(sequentialTuplesList){list =>{

      var explorationList = List[(Int,Int)]()
      val tree = RedBlackTreeMap.makeFromSorted(list)
      var explorer = tree.biggestPosition
      while(explorer.isDefined){
        explorationList = List((explorer.get.key,explorer.get.value)) ::: explorationList
        explorer = explorer.get.prev
      }

      explorationList should be (tree.content)
    }}
  }
}
