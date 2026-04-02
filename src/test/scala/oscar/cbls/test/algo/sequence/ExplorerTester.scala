package oscar.cbls.test.algo.sequence

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer, RootIntSequenceExplorer}

private[sequence] object ExplorerTester extends AnyFunSuite with Matchers {

  /** Exhaustively checks the consistency of explorers from a given sequence with its reference
    * sequence
    * @param seq
    *   The sequence under tests.
    * @param referenceList
    *   The reference list.
    */
  def testExplorer(seq: IntSequence, referenceList: List[Int]): Unit = {
    if (referenceList.nonEmpty) {
      seq.explorerAtPosition(-1).get.isInstanceOf[RootIntSequenceExplorer] should be(true)
      seq.explorerAtPosition(-1).get.asInstanceOf[RootIntSequenceExplorer].beforeStart should be(
        true
      )
      testExplorerInternal(seq.explorerAtPosition(-1).get, referenceList)
    }
  }

  /** Exhaustively checks the consistency of an explorer with its reference sequence.
    * @param rootExplorer
    *   A starting explorer of the Int.
    * @param referenceList
    *   The reference list.
    */
  private def testExplorerInternal(
    rootExplorer: IntSequenceExplorer,
    referenceList: List[Int]
  ): Unit = {

    rootExplorer.position should be(-1)

    testPrevNextGlobalBehavior(rootExplorer, referenceList)
    testExploreToPosition(rootExplorer, referenceList)
    if (referenceList.nonEmpty) {
      testForeach(rootExplorer, referenceList)
      testExploreUntil(rootExplorer, referenceList)
    }

  }

  private def testExploreToPosition(
    explorer: IntSequenceExplorer,
    referenceList: List[Int]
  ): Unit = {
    val explorerAtRoot = explorer.exploreToPosition(-1)
    val explorerAtEnd  = explorerAtRoot.get.exploreToPosition(referenceList.size - 1)

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
    val nonExistingValue: Int                = referenceList.max + 1
    val explorerAtStart: IntSequenceExplorer = explorer.exploreToPosition(0).get
    val explorerAtEnd: IntSequenceExplorer = explorer.exploreToPosition(referenceList.size - 1).get

    var searchResult: Option[IntSequenceExplorer] = None

    for (existingValue: Int <- referenceList) {
      // nextUntilValue(...)
      explorerAtStart.exploreForwardUntilValue(nonExistingValue) should be(None)
      searchResult = explorerAtStart.exploreForwardUntilValue(existingValue)
      searchResult.isDefined should be(true)
      searchResult.get.value should be(existingValue)

      // prevUntilValue(...)
      explorerAtEnd.exploreBackwardUntilValue(nonExistingValue) should be(None)
      searchResult = explorerAtEnd.exploreBackwardUntilValue(existingValue)
      searchResult.isDefined should be(true)
      searchResult.get.value should be(existingValue)

      // nextUntil(...)
      explorerAtStart.exploreForwardUntil(x => x.value == nonExistingValue) should be(None)
      searchResult = explorerAtStart.exploreForwardUntil(x => x.value == existingValue)
      searchResult.isDefined should be(true)
      searchResult.get.value should be(existingValue)

      // prevUntil(...)
      explorerAtEnd.exploreBackwardUntil(x => x.value == nonExistingValue) should be(None)
      searchResult = explorerAtEnd.exploreBackwardUntil(x => x.value == existingValue)
      searchResult.isDefined should be(true)
      searchResult.get.value should be(existingValue)
    }

  }

  // Test the IntSequenceExplorerIterator
  private def testForeach(explorer: IntSequenceExplorer, referenceList: List[Int]): Unit = {

    def compareSubLists(explorer: Iterator[IntSequenceExplorer], reference: List[Int]): Unit = {
      var explorerList: List[Int] = List.empty
      for (expl <- explorer) explorerList = explorerList ::: List(expl.value)
      explorerList should be(reference)
    }

    val explorerAt0 = explorer.exploreToPosition(0).get
    for (value <- referenceList) {
      compareSubLists(
        explorerAt0.forward.untilValue(value),
        referenceList.takeWhile(x => x != value)
      )
      compareSubLists(
        explorerAt0.forward.until(x => x.value == value),
        referenceList.takeWhile(x => x != value)
      )
      compareSubLists(
        explorerAt0.forward.toValue(value),
        referenceList.takeWhile(x => x != value) :+ value
      )
      compareSubLists(
        explorerAt0.forward.to(x => x.value == value),
        referenceList.takeWhile(x => x != value) :+ value
      )

      val explorerAtEnd = explorer.exploreToPosition(referenceList.size - 1).get
      compareSubLists(
        explorerAtEnd.backward.untilValue(value),
        referenceList.reverse.takeWhile(x => x != value)
      )
      compareSubLists(
        explorerAtEnd.backward.until(x => x.value == value),
        referenceList.reverse.takeWhile(x => x != value)
      )
      compareSubLists(
        explorerAtEnd.backward.toValue(value),
        referenceList.reverse.takeWhile(x => x != value) :+ value
      )
      compareSubLists(
        explorerAtEnd.backward.to(x => x.value == value),
        referenceList.reverse.takeWhile(x => x != value) :+ value
      )
    }
    var explorerValuePlus10List: List[Int] = List.empty
    for (expl <- explorerAt0.forward) {
      explorerValuePlus10List :+= expl.value + 10
    }

    explorerValuePlus10List should be(referenceList.map(x => x + 10))

  }

  private def testNextGlobalBehavior(
    rootExplorer: IntSequenceExplorer,
    referenceList: List[Int]
  ): IntSequenceExplorer = {
    var currentExplorer: IntSequenceExplorer = rootExplorer.next
    var currentPos                           = 0

    while (currentPos < referenceList.size) {
      currentExplorer.value should be(referenceList(currentPos))
      currentExplorer = currentExplorer.next
      currentPos += 1
    }

    currentExplorer.isInstanceOf[RootIntSequenceExplorer] should be(true)
    currentExplorer.asInstanceOf[RootIntSequenceExplorer].beforeStart should be(false)
    val exception = intercept[NoSuchElementException](currentExplorer.value)
    assert(exception.getMessage.contains("not part of the sequence"))
    currentExplorer.next.isInstanceOf[RootIntSequenceExplorer] should be(true)
    currentExplorer.next.asInstanceOf[RootIntSequenceExplorer].beforeStart should be(false)

    currentExplorer
  }

  private def testPrevGlobalBehavior(
    rootExplorer: IntSequenceExplorer,
    referenceList: List[Int]
  ): Unit = {
    var currentExplorer: IntSequenceExplorer = rootExplorer.prev
    var currentPos                           = referenceList.size - 1

    while (currentPos > -1) {
      currentExplorer.value should be(referenceList(currentPos))
      currentExplorer = currentExplorer.prev
      currentPos -= 1
    }

    currentExplorer.isInstanceOf[RootIntSequenceExplorer] should be(true)
    currentExplorer.asInstanceOf[RootIntSequenceExplorer].beforeStart should be(true)
    val exception = intercept[NoSuchElementException](currentExplorer.value)
    assert(exception.getMessage.contains("not part of the sequence"))
    currentExplorer.prev.isInstanceOf[RootIntSequenceExplorer] should be(true)
    currentExplorer.prev.asInstanceOf[RootIntSequenceExplorer].beforeStart should be(true)
  }

  private def testPrevNextGlobalBehavior(
    startingExplorer: IntSequenceExplorer,
    referenceList: List[Int]
  ): Unit = {
    val endExplorer = testNextGlobalBehavior(startingExplorer, referenceList)

    endExplorer.position should be(referenceList.size)

    testPrevGlobalBehavior(endExplorer, referenceList)
  }
}
