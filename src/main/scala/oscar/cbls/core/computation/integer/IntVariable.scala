package oscar.cbls.core.computation.integer

import oscar.cbls.core.computation.{SavedValue, Store, Variable}

class IntVariable(model:Store, isConstant: Boolean = false) extends Variable(model,isConstant){

  override def save(): SavedValue = ???

  /** this is the propagation method that should be overridden by propagation elements. notice that
   * it is only called in a propagation wave if: 1L: it has been registered for propagation since
   * the last time it was propagated 2L: it is included in the propagation wave: partial
   * propagation wave do not propagate all propagation elements; it only propagates the ones that
   * come in the predecessors of the targeted propagation element overriding this method is
   * optional, so an empty body is provided by default
   */
  override def performPropagation(): Unit = ???

  /** This is the debug procedure through which propagation element can redundantly check that the
   * incremental computation they perform through the performPropagation method is correct
   * overriding this method is optional, so an empty body is provided by default
   */
  override def checkInternals(): Unit = ???
}
