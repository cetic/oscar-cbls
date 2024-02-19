package oscar.cbls.core.computation.integer

import oscar.cbls.core.computation.Store

/** A constant IntVariable.
  *
  * The only difference is that we cannot change it's value.
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this variable is linked.
  * @param value
  *   The value of this constant variable
  */
class IntConstant(model: Store, value: Long) extends IntVariable(model, value, true) {
  override protected def setValue(value: Long): Unit = {
    require(false, "The value of a constant variable can not be changed")
  }
}
