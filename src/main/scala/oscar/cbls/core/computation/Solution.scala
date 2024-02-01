package oscar.cbls.core.computation

case class Solution(savedValues: Iterable[SavedValue],
                    model: Store) {

  def restoreSolution(): Unit = {
    savedValues.foreach(_.restoreValue())
  }
}
