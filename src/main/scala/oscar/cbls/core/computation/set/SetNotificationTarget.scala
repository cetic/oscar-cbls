package oscar.cbls.core.computation.set

trait SetNotificationTarget {

  /** Notifies the listening [[oscar.cbls.core.propagation.PropagationElement]] that the listened
    * [[SetVariable]] has changed. This method has to be implemented by the listening element, and
    * will be called by the listened variable.
    *
    * This method uses parameters providing incremental information (`addedValues` and
    * `removedValues`), as well as information on the entire value (`oldValue` and `newValue`),
    * since different propagation elements may make use of either or both kinds.
    *
    * @param v
    *   The listened SetVariable
    * @param index
    *   * The index of the IntVariable in the context of the listening Invariant
    * @param addedValues
    *   The values added to the SetVariable
    * @param removedValues
    *   The values removed from the SetVariable
    * @param oldValue
    *   The old value of the SetVariable
    * @param newValue
    *   The new value of the SetVariable
    */
  def notifySetChanges(
    v: ChangingSetValue,
    index: Int,
    addedValues: Iterable[Int],
    removedValues: Iterable[Int],
    oldValue: Set[Int],
    newValue: Set[Int]
  ): Unit
}
