package oscar.cbls.core.computation.integer

import oscar.cbls.core.computation.SavedValue

/** A saved state of a [[IntVariable]]
  *
  * @param intVariable
  *   The IntVariable whose state is saved
  */
class SavedIntValue(intVariable: IntVariable) extends SavedValue(intVariable) {

  private val savedValue: Long = intVariable.value()

  /** Restores the variable current value to the saved one */
  override def restoreValue(): Unit = intVariable := savedValue
}
