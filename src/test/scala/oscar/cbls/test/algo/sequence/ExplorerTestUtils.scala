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
    var explorer = exp

    // Checking prev
    list
      .take(pos + 1)
      .reverse
      .foreach(e => {
        e should be(explorer.get.value)
        explorer = explorer.get.prev
      })
    // Reached the first entry
    explorer should be(None)

    //Checking next
    explorer = exp
    list
      .takeRight(list.size - pos)
      .foreach(e => {
        e should be(explorer.get.value)
        explorer = explorer.get.next
      })
    // Reached the last entry
    explorer should be(None)

    exp.get.position should be(pos)
  }
}
