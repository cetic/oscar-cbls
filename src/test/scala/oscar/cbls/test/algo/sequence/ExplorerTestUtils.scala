package oscar.cbls.test.algo.sequence

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls.algo.sequence.IntSequenceExplorer

object ExplorerTestUtils {
  private val myTestUtils = new ExplorerTestUtils()
  def compareAllAttributes(exp: Option[IntSequenceExplorer], pos: Int, list: List[Int]): Unit =
    myTestUtils.compareExplorer(exp, pos, list)
}

class ExplorerTestUtils extends AnyFunSuite with Matchers {

  /** Exhaustively checks the consistency of an explorer with its reference sequence
    * @param exp
    *   The explorer to check
    * @param pos
    *   The position of the explorer in the list
    * @param list
    *   The original list
    */
  def compareExplorer(exp: Option[IntSequenceExplorer], pos: Int, list: List[Int]): Unit = {
    exp.get.position should be(pos)
    var explorer = exp
    val value    = exp.get.value

    // Checking prev
    list
      .take(pos + 1)
      .reverse
      // Adding the RootIntSequenceExplorer value
      .appendedAll(List(Int.MinValue))
      .foreach(e => {
        e should be(explorer.get.value)
        explorer = explorer.get.prev
      })
    // Reached the first entry
    explorer should be(None)

    // Checking next
    explorer = exp
    list
      .takeRight(list.size - pos)
      .foreach(e => {
        e should be(explorer.get.value)
        explorer = explorer.get.next
      })
    // Reached the last entry
    explorer should be(None)

    // To Position
    for (p <- list.indices)
      exp.get.toPosition(p).get.position should be(p)

    val listBeforePosReversed = list.take(pos).reverse
    val listAfterPos          = list.drop(pos + 1)

    // Next/Prev until ...
    // Backward
    val firstValueBackward = listBeforePosReversed.find(x => x == value)
    firstValueBackward match {
      case None =>
        val prevExplorer = exp.get.prev
        if (prevExplorer.isEmpty) prevExplorer should be(None)
        else {
          prevExplorer.get.prevUntil(e => e.value == value) should be(None)
          prevExplorer.get.prevUntilValue(value) should be(None)
        }
      case Some(value) =>
        exp.get.prev.get.prevUntil(e => e.value == value).get.value should be(value)
        exp.get.prev.get.prevUntilValue(value).get.value should be(value)
    }
    // Forward
    val firstValueForward = listAfterPos.find(x => x == value)
    firstValueForward match {
      case None =>
        val nextExplorer = exp.get.next
        if (nextExplorer.isEmpty) nextExplorer should be(None)
        else {
          nextExplorer.get.nextUntil(e => e.value == value) should be(None)
          nextExplorer.get.nextUntilValue(value) should be(None)
        }
      case Some(value) =>
        exp.get.next.get.nextUntil(e => e.value == value).get.value should be(value)
        exp.get.next.get.nextUntilValue(value).get.value should be(value)
    }

    // Foreach
    var listForEach: List[Int] = List.empty
    exp.get.foreach(e => {
      listForEach = listForEach :+ e.value
    })
    listForEach shouldBe list.drop(pos)

  }
}
