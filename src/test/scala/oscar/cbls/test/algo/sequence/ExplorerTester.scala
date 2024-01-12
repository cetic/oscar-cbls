package oscar.cbls.test.algo.sequence

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls.algo.sequence.{IntSequenceExplorer, RootIntSequenceExplorer}

import scala.util.Random

object ExplorerTester extends AnyFunSuite with Matchers {

  /** Exhaustively checks the consistency of an explorer with its reference sequence
    * @param rootExplorer
    *   A starting explorer of the Int
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

    val explorerAtRootEnd = explorerAtRoot.get.goToPosition(referenceList.size)
    explorerAtRootEnd.isDefined should be(true)
    explorerAtRootEnd.get.position should be(referenceList.size)
    explorerAtRootEnd.get.isInstanceOf[RootIntSequenceExplorer] should be(true)
    explorerAtRootEnd.get.asInstanceOf[RootIntSequenceExplorer].backward should be(false)

    explorerAtEnd.get.goToPosition(Int.MinValue).isEmpty should be(true)
    explorerAtRoot.get.goToPosition(Int.MaxValue).isEmpty should be(true)

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
    // Backward
    val explorerAtStart = startingExplorer.goToPosition(0)
    explorerAtStart.get.value should be(referenceList.head)
    val explorerAtRoot = explorerAtStart.get.prev
    explorerAtRoot.isInstanceOf[RootIntSequenceExplorer] should be(true)
    explorerAtRoot.asInstanceOf[RootIntSequenceExplorer].backward should be(true)
    val explorerAtStart2 = explorerAtRoot.next
    explorerAtStart2.value should be(referenceList.head)
    // Forward
    val explorerAtEnd = startingExplorer.goToPosition(referenceList.size - 1)
    explorerAtEnd.get.value should be(referenceList.last)
    val explorerAtRoot2 = explorerAtEnd.get.next
    explorerAtRoot2.isInstanceOf[RootIntSequenceExplorer] should be(true)
    explorerAtRoot2.asInstanceOf[RootIntSequenceExplorer].backward should be(false)
    val explorerAtEnd2 = explorerAtRoot2.prev
    explorerAtEnd2.value should be(referenceList.last)

    var prevExplorer: IntSequenceExplorer = startingExplorer
    var prevPos                           = -1
    for (_ <- 0 until 100) {
      // true == next
      if (Random.nextBoolean()) {
        val currentExplorer = prevExplorer.next
        val currentPos      = Math.min(prevPos + 1, referenceList.size)
        if (currentPos == referenceList.size) {
          currentExplorer.isInstanceOf[RootIntSequenceExplorer] should be(true)
          currentExplorer.asInstanceOf[RootIntSequenceExplorer].backward should be(false)
          val exception = intercept[NoSuchElementException](currentExplorer.value)
          assert(exception.getMessage.contains("not part of the sequence"))
          currentExplorer.next.isInstanceOf[RootIntSequenceExplorer] should be(true)
          currentExplorer.next.asInstanceOf[RootIntSequenceExplorer].backward should be(false)
        } else {
          currentExplorer.value should be(referenceList(currentPos))
          prevExplorer = currentExplorer
          prevPos = currentPos
        }
      } else {
        val currentExplorer = prevExplorer.prev
        val currentPos      = Math.max(prevPos - 1, -1)
        if (currentPos == -1) {
          currentExplorer.isInstanceOf[RootIntSequenceExplorer] should be(true)
          currentExplorer.asInstanceOf[RootIntSequenceExplorer].backward should be(true)
          val exception = intercept[NoSuchElementException](currentExplorer.value)
          assert(exception.getMessage.contains("not part of the sequence"))
          currentExplorer.prev.isInstanceOf[RootIntSequenceExplorer] should be(true)
          currentExplorer.prev.asInstanceOf[RootIntSequenceExplorer].backward should be(true)
        } else {
          currentExplorer.value should be(referenceList(currentPos))
          prevExplorer = currentExplorer
          prevPos = currentPos
        }
      }
    }
  }
}
