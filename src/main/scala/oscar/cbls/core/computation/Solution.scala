package oscar.cbls.core.computation

/** The state of the model at a given time.
  *
  * This solution can be restored using the restoreSolution method.
  *
  * @param savedValues
  *   The values of the saved decision variable
  * @param model
  *   The Store holding those variables
  * @param solutionNb
  *   The identification of this solution.
  */
case class Solution(savedValues: Iterable[SavedValue], model: Store, solutionNb: Int) {

  /** Restores the model it's previous saved state by restoring each decision variable. */
  def restoreSolution(): Unit = {
    savedValues.foreach(sv => sv.restoreValue())
  }

  /** Displays the solution as a human-readable string */
  override def toString: String = {
    "Solution(\n\t" + savedValues.map(_.toString()).mkString(",\n\t") + "\n)"
  }
}
