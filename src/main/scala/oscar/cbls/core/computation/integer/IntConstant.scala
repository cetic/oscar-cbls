package oscar.cbls.core.computation.integer

import oscar.cbls.core.computation.Store

class IntConstant(model: Store, value: Long) extends IntVariable(model,value,true){
  override protected def setValue(value: Long): Unit = {
    require(false, "The value of a constant variable can not be changed")
  }
}
