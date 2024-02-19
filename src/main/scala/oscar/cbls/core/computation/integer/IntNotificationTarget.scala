package oscar.cbls.core.computation.integer

trait IntNotificationTarget {
  // Note: In the legacy code there is an id information, corresponding to the id in the ddl of listening element.
  // I don't think it's a good idea to use it.

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
