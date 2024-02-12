package oscar.cbls.core.computation

/** The value of a decision [[Variable]] that is part of a [[Solution]]
  * @param variable
  *   The variable whose value is being saved
  */
abstract class SavedValue(val variable: Variable) {
  /** Restores the variable current value to the saved one */
  def restoreValue(): Unit
}
