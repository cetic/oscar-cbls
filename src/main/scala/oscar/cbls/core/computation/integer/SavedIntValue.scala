package oscar.cbls.core.computation.integer

import oscar.cbls.core.computation.SavedValue

class SavedIntValue(intVariable: IntVariable) extends SavedValue(intVariable){

  private val savedValue: Long = intVariable.value()

  /** Restores the variable current value to the saved one */
  override def restoreValue(): Unit = intVariable := savedValue
}
