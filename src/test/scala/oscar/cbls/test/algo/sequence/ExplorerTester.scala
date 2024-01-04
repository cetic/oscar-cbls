package oscar.cbls.test.algo.sequence

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls.algo.sequence.IntSequenceExplorer
import oscar.cbls.algo.sequence.concrete.RootIntSequenceExplorer

import scala.util.Random

object ExplorerTester extends AnyFunSuite with Matchers {

  /** Exhaustively checks the consistency of an explorer with its reference sequence
    * @param startingExplorer
    *   A starting explorer of the Int
    * @param startingPosition
    *   The position of the starting explorer in the IntSequence
    * @param referenceList
    *   The reference list
    */
  def testExplorers(startingExplorer: Option[IntSequenceExplorer], startingPosition: Int, referenceList: List[Int]): Unit = {
    startingExplorer.get.position should be(startingPosition)

    testPrevNextGlobalBehavior(startingPosition, startingExplorer, referenceList)

    val value    = startingExplorer.get.value

    // To Position
    for (p <- referenceList.indices)
      startingExplorer.get.toPosition(p).get.position should be(p)

    val listBeforePosReversed = referenceList.take(startingPosition).reverse
    val listAfterPos          = referenceList.drop(startingPosition + 1)

    // Next/Prev until ...
    // Backward
    val firstValueBackward = listBeforePosReversed.find(x => x == value)
    firstValueBackward match {
      case None =>
        val prevExplorer = startingExplorer.get.prev
        if (prevExplorer.isEmpty) prevExplorer should be(None)
        else {
          prevExplorer.get.prevUntil(e => e.value == value) should be(None)
          prevExplorer.get.prevUntilValue(value) should be(None)
        }
      case Some(value) =>
        startingExplorer.get.prev.get.prevUntil(e => e.value == value).get.value should be(value)
        startingExplorer.get.prev.get.prevUntilValue(value).get.value should be(value)
    }
    // Forward
    val firstValueForward = listAfterPos.find(x => x == value)
    firstValueForward match {
      case None =>
        val nextExplorer = startingExplorer.get.next
        if (nextExplorer.isEmpty) nextExplorer should be(None)
        else {
          nextExplorer.get.nextUntil(e => e.value == value) should be(None)
          nextExplorer.get.nextUntilValue(value) should be(None)
        }
      case Some(value) =>
        startingExplorer.get.next.get.nextUntil(e => e.value == value).get.value should be(value)
        startingExplorer.get.next.get.nextUntilValue(value).get.value should be(value)
    }

    // Foreach
    var listForEach: List[Int] = List.empty
    startingExplorer.get.foreach(e => {
      listForEach = listForEach :+ e.value
    })
    listForEach shouldBe referenceList.drop(startingPosition)

  }

  private def testPrevNextGlobalBehavior(startingPos: Int, startingExplorer: Option[IntSequenceExplorer], referenceList: List[Int]): Unit = {
    var prevExplorer = startingExplorer
    var prevPos = startingPos
    for(_ <- 0 until 1000){
      // true == next
      if(Random.nextBoolean()){
        val currentExplorer = prevExplorer.get.next
        val currentPos = prevPos + 1
        if(currentPos == referenceList.size) {
          currentExplorer should be(None)
        } else {
          currentExplorer.get.value should be(referenceList(currentPos))
          prevExplorer = currentExplorer
          prevPos = currentPos
        }
      } else {
        val currentExplorer = prevExplorer.get.prev
        val currentPos = prevPos - 1
        if(currentPos == -2) {
          currentExplorer should be(None)
        } else {
          if(currentPos == -1){
            currentExplorer.get.value should be(Int.MinValue)
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
