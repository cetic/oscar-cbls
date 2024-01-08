package oscar.cbls.test.algo.sequence

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls.algo.sequence.IntSequenceExplorer
import oscar.cbls.algo.sequence.concrete.RootIntSequenceExplorer
import oscar.cbls.algo.sequence.stackedUpdate.RemovedIntSequenceExplorer

import scala.util.Random

object ExplorerTester extends AnyFunSuite with Matchers {

  /** Exhaustively checks the consistency of an explorer with its reference sequence
    * @param rootExplorer
    *   A starting explorer of the Int
    * @param startingPosition
    *   The position of the starting explorer in the IntSequence
    * @param referenceList
    *   The reference list
    */
  def testExplorers(rootExplorer: IntSequenceExplorer, referenceList: List[Int]): Unit = {
    rootExplorer.position should be(-1)

    testPrevNextGlobalBehavior(rootExplorer, referenceList)
    testToPosition(rootExplorer, referenceList)
    if (referenceList.nonEmpty) {
      testForeach(rootExplorer, referenceList)
      testUntil(rootExplorer, referenceList)
    }

  }

  private def testToPosition(explorer: IntSequenceExplorer, referenceList: List[Int]): Unit = {
    val explorerAtRoot = explorer.goToPosition(-1)
    explorerAtRoot.isDefined should be(true)
    explorerAtRoot.get.position should be(-1)

    val explorerAtEnd = explorerAtRoot.get.goToPosition(referenceList.size - 1)
    explorerAtEnd.isDefined should be(true)
    explorerAtEnd.get.position should be(referenceList.size - 1)

    val exception = intercept[IllegalArgumentException](explorerAtEnd.get.goToPosition(Int.MinValue))
    assert(exception.getMessage.contains("requirement failed"))
    explorerAtRoot.get.goToPosition(Int.MaxValue).isDefined should be(false)

    for (testPosition <- 1 until referenceList.size - 1) {
      val reachedExplorer = explorerAtRoot.get.goToPosition(testPosition)
      reachedExplorer.isDefined should be(true)
      reachedExplorer.get.position should be(testPosition)
      reachedExplorer.get.value should be(referenceList(testPosition))
    }

    for (testPosition <- 1 until referenceList.size - 1) {
      val reachedExplorer = explorerAtEnd.get.goToPosition(testPosition)
      reachedExplorer.isDefined should be(true)
      reachedExplorer.get.position should be(testPosition)
      reachedExplorer.get.value should be(referenceList(testPosition))
    }
  }

  private def testUntil(explorer: IntSequenceExplorer, referenceList: List[Int]): Unit = {
    val nonExistingValue: Int    = referenceList.max + 1
    val randomExistingValue: Int = referenceList(Random.nextInt(referenceList.size))
    val firstValue: Int          = referenceList.head
    val lastValue: Int           = referenceList.last

    val explorerAtStart: IntSequenceExplorer = explorer.goToPosition(0).get
    val explorerAtEnd: IntSequenceExplorer   = explorer.goToPosition(referenceList.size - 1).get

    var searchResult: Option[IntSequenceExplorer] = None

    // nextUntilValue(...)
    explorerAtStart.nextUntilValue(nonExistingValue) should be(None)
    searchResult = explorerAtStart.nextUntilValue(randomExistingValue)
    searchResult.isDefined should be(true)
    searchResult.get.value should be(randomExistingValue)

    searchResult = explorerAtStart.nextUntilValue(firstValue)
    searchResult.isDefined should be(true)
    searchResult.get.value should be(firstValue)
    searchResult.get.position should be(0)

    searchResult = explorerAtStart.nextUntilValue(lastValue)
    searchResult.isDefined should be(true)
    searchResult.get.value should be(lastValue)

    // prevUntilValue(...)
    explorerAtEnd.prevUntilValue(nonExistingValue) should be(None)
    searchResult = explorerAtEnd.prevUntilValue(randomExistingValue)
    searchResult.isDefined should be(true)
    searchResult.get.value should be(randomExistingValue)

    searchResult = explorerAtEnd.prevUntilValue(firstValue)
    searchResult.isDefined should be(true)
    searchResult.get.value should be(firstValue)

    searchResult = explorerAtEnd.prevUntilValue(lastValue)
    searchResult.isDefined should be(true)
    searchResult.get.value should be(lastValue)
    searchResult.get.position should be(referenceList.size - 1)

    // nextUntil(...)
    explorerAtStart.nextUntil(x => x.value == nonExistingValue) should be(None)
    searchResult = explorerAtStart.nextUntil(x => x.value == randomExistingValue)
    searchResult.isDefined should be(true)
    searchResult.get.value should be(randomExistingValue)

    searchResult = explorerAtStart.nextUntil(x => x.value == firstValue)
    searchResult.isDefined should be(true)
    searchResult.get.value should be(firstValue)
    searchResult.get.position should be(0)

    searchResult = explorerAtStart.nextUntil(x => x.value == lastValue)
    searchResult.isDefined should be(true)
    searchResult.get.value should be(lastValue)

    // prevUntilValue(...)
    explorerAtEnd.prevUntil(x => x.value == nonExistingValue) should be(None)
    searchResult = explorerAtEnd.prevUntil(x => x.value == randomExistingValue)
    searchResult.isDefined should be(true)
    searchResult.get.value should be(randomExistingValue)

    searchResult = explorerAtEnd.prevUntil(x => x.value == firstValue)
    searchResult.isDefined should be(true)
    searchResult.get.value should be(firstValue)

    searchResult = explorerAtEnd.prevUntil(x => x.value == lastValue)
    searchResult.isDefined should be(true)
    searchResult.get.value should be(lastValue)
    searchResult.get.position should be(referenceList.size - 1)

  }

  // Test the foreach method
  private def testForeach(explorer: IntSequenceExplorer, referenceList: List[Int]): Unit = {
    var listForEach: List[Int] = List.empty
    explorer
      .goToPosition(0)
      .get
      .foreach(e => {
        listForEach = listForEach :+ e.value
      })
    listForEach shouldBe referenceList.drop(explorer.position)
  }

  private def testPrevNextGlobalBehavior(
    startingExplorer: IntSequenceExplorer,
    referenceList: List[Int]
  ): Unit = {
    // RootExplorer test
    val explorerAtStart = startingExplorer.goToPosition(0)
    explorerAtStart.get.value should be(referenceList.head)
    val explorerAtRoot = explorerAtStart.get.prev
    explorerAtRoot.isDefined should be(true)
    explorerAtRoot.get.isInstanceOf[RootIntSequenceExplorer] should be(true)
    val explorerAtStart2 = explorerAtRoot.get.next
    explorerAtStart2.isDefined should be(true)
    explorerAtStart2.get.value should be(referenceList.head)

    var prevExplorer: Option[IntSequenceExplorer] = Some(startingExplorer)
    var prevPos                                   = -1
    for (_ <- 0 until 100) {
      // true == next
      if (Random.nextBoolean()) {
        val currentExplorer = prevExplorer.get.next
        val currentPos      = prevPos + 1
        if (currentPos == referenceList.size) {
          currentExplorer should be(None)
        } else {
          currentExplorer.get.value should be(referenceList(currentPos))
          prevExplorer = currentExplorer
          prevPos = currentPos
        }
      } else {
        val currentExplorer = prevExplorer.get.prev
        val currentPos      = prevPos - 1
        if (currentPos == -2) {
          currentExplorer should be(None)
        } else {
          if (currentPos == -1) {
            val exception = intercept[NoSuchElementException](currentExplorer.get.value)
            assert(exception.getMessage.contains("not part of the sequence"))
            currentExplorer.get.isInstanceOf[RootIntSequenceExplorer] should be(true)
          } else {
            currentExplorer.get.value should be(referenceList(currentPos))
          }
          prevExplorer = currentExplorer
          prevPos = currentPos
        }
      }
    }
  }
}
