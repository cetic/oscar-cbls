package oscar.cbls.core.computation.integer

trait IntNotificationTarget {
  // Note: In the legacy code there is a id information, corresponding to the id in the ddl of listening element.
  // I don't think it's a good idea to use it.
  def notifyIntChanged(intVariable: IntVariable, oldVal: Long, newVal: Long): Unit
}
