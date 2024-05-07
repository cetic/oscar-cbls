package oscar.cbls.core.computation.set

import oscar.cbls.core.computation.SavedValue

class SetSavedValue(v: SetVariable) extends SavedValue(v) {

  /** Restores the variable current value to the saved one */
  def restoreValue(): Unit = ???
}
