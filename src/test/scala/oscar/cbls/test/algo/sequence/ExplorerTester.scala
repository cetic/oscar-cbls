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
    testExploreToPosition(rootExplorer, referenceList)
    if (referenceList.nonEmpty) {
      testForeach(rootExplorer, referenceList)
      testExploreUntil(rootExplorer, referenceList)
    }

  }

  private def testExploreToPosition(explorer: IntSequenceExplorer, referenceList: List[Int]): Unit = {
    val explorerAtRoot = explorer.exploreToPosition(-1)
    explorerAtRoot.isDefined should be(true)
    explorerAtRoot.get.position should be(-1)

    val explorerAtEnd = explorerAtRoot.get.exploreToPosition(referenceList.size - 1)
    explorerAtEnd.isDefined should be(true)
    explorerAtEnd.get.position should be(referenceList.size - 1)

    val explorerAtRootEnd = explorerAtRoot.get.exploreToPosition(referenceList.size)
    explorerAtRootEnd.isDefined should be(true)
    explorerAtRootEnd.get.position should be(referenceList.size)
    explorerAtRootEnd.get.isInstanceOf[RootIntSequenceExplorer] should be(true)
    explorerAtRootEnd.get.asInstanceOf[RootIntSequenceExplorer].beforeStart should be(false)

    explorerAtEnd.get.exploreToPosition(Int.MinValue).isEmpty should be(true)
    explorerAtRoot.get.exploreToPosition(Int.MaxValue).isEmpty should be(true)

    for (testPosition <- 1 until referenceList.size - 1) {
      val reachedExplorer = explorerAtRoot.get.exploreToPosition(testPosition)
      reachedExplorer.isDefined should be(true)
      reachedExplorer.get.position should be(testPosition)
      reachedExplorer.get.value should be(referenceList(testPosition))
    }

    for (testPosition <- 1 until referenceList.size - 1) {
      val reachedExplorer = explorerAtEnd.get.exploreToPosition(testPosition)
      reachedExplorer.isDefined should be(true)
      reachedExplorer.get.position should be(testPosition)
      reachedExplorer.get.value should be(referenceList(testPosition))
    }
  }

  private def testExploreUntil(explorer: IntSequenceExplorer, referenceList: List[Int]): Unit = {
    val nonExistingValue: Int    = referenceList.max + 1
    val randomExistingValue: Int = referenceList(Random.nextInt(referenceList.size))
    val firstValue: Int          = referenceList.head
    val lastValue: Int           = referenceList.last

    val explorerAtStart: IntSequenceExplorer = explorer.exploreToPosition(0).get
    val explorerAtEnd: IntSequenceExplorer   = explorer.exploreToPosition(referenceList.size - 1).get

    var searchResult: Option[IntSequenceExplorer] = None

    // nextUntilValue(...)
    explorerAtStart.exploreForwardUntilValue(nonExistingValue) should be(None)
    searchResult = explorerAtStart.exploreForwardUntilValue(randomExistingValue)
    searchResult.isDefined should be(true)
    searchResult.get.value should be(randomExistingValue)

    searchResult = explorerAtStart.exploreForwardUntilValue(firstValue)
    searchResult.isDefined should be(true)
    searchResult.get.value should be(firstValue)
    searchResult.get.position should be(0)

    searchResult = explorerAtStart.exploreForwardUntilValue(lastValue)
    searchResult.isDefined should be(true)
    searchResult.get.value should be(lastValue)

    // prevUntilValue(...)
    explorerAtEnd.exploreBackwardUntilValue(nonExistingValue) should be(None)
    searchResult = explorerAtEnd.exploreBackwardUntilValue(randomExistingValue)
    searchResult.isDefined should be(true)
    searchResult.get.value should be(randomExistingValue)

    searchResult = explorerAtEnd.exploreBackwardUntilValue(firstValue)
    searchResult.isDefined should be(true)
    searchResult.get.value should be(firstValue)

    searchResult = explorerAtEnd.exploreBackwardUntilValue(lastValue)
    searchResult.isDefined should be(true)
    searchResult.get.value should be(lastValue)
    searchResult.get.position should be(referenceList.size - 1)

    // nextUntil(...)
    explorerAtStart.exploreForwardUntil(x => x.value == nonExistingValue) should be(None)
    searchResult = explorerAtStart.exploreForwardUntil(x => x.value == randomExistingValue)
    searchResult.isDefined should be(true)
    searchResult.get.value should be(randomExistingValue)

    searchResult = explorerAtStart.exploreForwardUntil(x => x.value == firstValue)
    searchResult.isDefined should be(true)
    searchResult.get.value should be(firstValue)
    searchResult.get.position should be(0)

    searchResult = explorerAtStart.exploreForwardUntil(x => x.value == lastValue)
    searchResult.isDefined should be(true)
    searchResult.get.value should be(lastValue)

    // prevUntilValue(...)
    explorerAtEnd.exploreBackwardUntil(x => x.value == nonExistingValue) should be(None)
    searchResult = explorerAtEnd.exploreBackwardUntil(x => x.value == randomExistingValue)
    searchResult.isDefined should be(true)
    searchResult.get.value should be(randomExistingValue)

    searchResult = explorerAtEnd.exploreBackwardUntil(x => x.value == firstValue)
    searchResult.isDefined should be(true)
    searchResult.get.value should be(firstValue)

    searchResult = explorerAtEnd.exploreBackwardUntil(x => x.value == lastValue)
    searchResult.isDefined should be(true)
    searchResult.get.value should be(lastValue)
    searchResult.get.position should be(referenceList.size - 1)

  }

  // Test the IntSequenceExplorerIterator
  private def testForeach(explorer: IntSequenceExplorer, referenceList: List[Int]): Unit = {
    // At RootPosition
    val explorerAtRoot1 = explorer.exploreToPosition(-1)
    val rootIterator1Backward = explorerAtRoot1.get.backward
    intercept[NoSuchElementException](rootIterator1Backward.toValue(4).next())
    intercept[NoSuchElementException](rootIterator1Backward.to(_ => true).next())
    intercept[NoSuchElementException](rootIterator1Backward.untilValue(4).next())
    intercept[NoSuchElementException](rootIterator1Backward.until(_ => true).next())
    val rootIterator1Forward = explorerAtRoot1.get.forward
    intercept[NoSuchElementException](rootIterator1Forward.toValue(4).next())
    intercept[NoSuchElementException](rootIterator1Forward.to(_ => true).next())
    intercept[NoSuchElementException](rootIterator1Forward.untilValue(4).next())
    intercept[NoSuchElementException](rootIterator1Forward.until(_ => true).next())

    val explorerAtRoot2 = explorer.exploreToPosition(referenceList.size)
    val rootIterator2Backward = explorerAtRoot2.get.backward
    intercept[NoSuchElementException](rootIterator2Backward.toValue(4).next())
    intercept[NoSuchElementException](rootIterator2Backward.to(_ => true).next())
    intercept[NoSuchElementException](rootIterator2Backward.untilValue(4).next())
    intercept[NoSuchElementException](rootIterator2Backward.until(_ => true).next())
    val rootIterator2Forward = explorerAtRoot2.get.forward
    intercept[NoSuchElementException](rootIterator2Forward.toValue(4).next())
    intercept[NoSuchElementException](rootIterator2Forward.to(_ => true).next())
    intercept[NoSuchElementException](rootIterator2Forward.untilValue(4).next())
    intercept[NoSuchElementException](rootIterator2Forward.until(_ => true).next())


    def compareSubLists(explorer: Iterator[IntSequenceExplorer], reference: List[Int]): Unit = {
      var explorerList: List[Int] = List.empty
      for(expl <- explorer)  explorerList = explorerList ::: List(expl.value)
      explorerList should be(reference)
    }

    val randomValue = Random.shuffle(referenceList).head
    val explorerAt0 = explorer.exploreToPosition(0).get
    compareSubLists(explorerAt0.forward.untilValue(randomValue), referenceList.takeWhile(x => x != randomValue))
    compareSubLists(explorerAt0.forward.until(x => x.value == randomValue), referenceList.takeWhile(x => x != randomValue))
    compareSubLists(explorerAt0.forward.toValue(randomValue), referenceList.takeWhile(x => x != randomValue) :+ randomValue)
    compareSubLists(explorerAt0.forward.to(x => x.value == randomValue), referenceList.takeWhile(x => x != randomValue) :+ randomValue)

    val explorerAtEnd = explorer.exploreToPosition(referenceList.size-1).get
    compareSubLists(explorerAtEnd.backward.untilValue(randomValue), referenceList.reverse.takeWhile(x => x != randomValue))
    compareSubLists(explorerAtEnd.backward.until(x => x.value == randomValue), referenceList.reverse.takeWhile(x => x != randomValue))
    compareSubLists(explorerAtEnd.backward.toValue(randomValue), referenceList.reverse.takeWhile(x => x != randomValue) :+ randomValue)
    compareSubLists(explorerAtEnd.backward.to(x => x.value == randomValue), referenceList.reverse.takeWhile(x => x != randomValue) :+ randomValue)

    var explorerValuePlus10List: List[Int] = List.empty
    for(expl <- explorerAt0.forward){
      explorerValuePlus10List :+= expl.value + 10
    }

    explorerValuePlus10List should be(referenceList.map(x => x+10))

  }

  private def testPrevNextGlobalBehavior(
    startingExplorer: IntSequenceExplorer,
    referenceList: List[Int]
  ): Unit = {
    // RootExplorer test
    // Backward
    val explorerAtStart = startingExplorer.exploreToPosition(0)
    explorerAtStart.get.value should be(referenceList.head)
    val explorerAtRoot = explorerAtStart.get.prev
    explorerAtRoot.isInstanceOf[RootIntSequenceExplorer] should be(true)
    explorerAtRoot.asInstanceOf[RootIntSequenceExplorer].beforeStart should be(true)
    val explorerAtStart2 = explorerAtRoot.next
    explorerAtStart2.value should be(referenceList.head)
    // Forward
    val explorerAtEnd = startingExplorer.exploreToPosition(referenceList.size - 1)
    explorerAtEnd.get.value should be(referenceList.last)
    val explorerAtRoot2 = explorerAtEnd.get.next
    explorerAtRoot2.isInstanceOf[RootIntSequenceExplorer] should be(true)
    explorerAtRoot2.asInstanceOf[RootIntSequenceExplorer].beforeStart should be(false)
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
          currentExplorer.asInstanceOf[RootIntSequenceExplorer].beforeStart should be(false)
          val exception = intercept[NoSuchElementException](currentExplorer.value)
          assert(exception.getMessage.contains("not part of the sequence"))
          currentExplorer.next.isInstanceOf[RootIntSequenceExplorer] should be(true)
          currentExplorer.next.asInstanceOf[RootIntSequenceExplorer].beforeStart should be(false)
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
          currentExplorer.asInstanceOf[RootIntSequenceExplorer].beforeStart should be(true)
          val exception = intercept[NoSuchElementException](currentExplorer.value)
          assert(exception.getMessage.contains("not part of the sequence"))
          currentExplorer.prev.isInstanceOf[RootIntSequenceExplorer] should be(true)
          currentExplorer.prev.asInstanceOf[RootIntSequenceExplorer].beforeStart should be(true)
        } else {
          currentExplorer.value should be(referenceList(currentPos))
          prevExplorer = currentExplorer
          prevPos = currentPos
        }
      }
    }
  }
}
