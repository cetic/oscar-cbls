package oscar.cbls.core.computation.set

trait SetNotificationTarget extends PropagationElement {

  /** this method will be called just before the variable "v" is actually updated.
    *
    * @param v
    * @param id
    *   d is always MinValue when notified for a valueWiseKey
    * @param addedValues
    * @param removedValues
    * @param oldValue
    * @param newValue
    */
  def notifySetChanges(
    v: ChangingSetValue,
    id: Int,
    addedValues: Iterable[Int],
    removedValues: Iterable[Int],
    oldValue: SortedSet[Int],
    newValue: SortedSet[Int]
  ): Unit

  def registerDynamicValueWiseDependency(s: SetValue): ValueWiseKey = {
    s match {
      case c: ChangingSetValue =>
        val key          = registerDynamicallyListenedElement(c, Int.MinValue)
        val valueWiseKey = c.instrumentKeyToValueWiseKey(key)
        valueWiseKey.target = this
        valueWiseKey
      case _ =>
        DoNothingValueWiseKey
    }
  }
}
