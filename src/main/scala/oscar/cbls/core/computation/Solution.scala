package oscar.cbls.core.computation

case class Solution(savedValues: Iterable[SavedValue],
                    model: Store,
                    solutionNb: Int) {

  /** Restores the model it's previous saved state by restoring each decision variable. */
  def restoreSolution(): Unit = {
    savedValues.foreach(sv => sv.restoreValue())
  }

  /** Displays the solution as a human-readable string */
  override def toString:String = {
    "Solution(\n\t" + savedValues.map(_.toString()).mkString(",\n\t") + "\n)"
  }
}
