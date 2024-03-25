package oscar.cbls.core.computation.integer

trait IntNotificationTarget {

  /** Notifies the listening [[oscar.cbls.core.propagation.PropagationElement]] that the listened
    * [[IntVariable]] has changed.
    *
    * Implemented by the listening [[oscar.cbls.core.propagation.PropagationElement]]. Called by the
    * listened [[IntVariable]]
    *
    * @param intVariable
    *   The listened IntVariable
    * @param oldVal
    *   The previous value of the variable
    * @param newVal
    *   The new value of the variable
    */
  def notifyIntChanged(intVariable: IntVariable, oldVal: Long, newVal: Long): Unit
}
