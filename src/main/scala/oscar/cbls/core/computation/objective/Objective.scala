package oscar.cbls.core.computation.objective

import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable

object Objective {
  def apply(objectiveValue: IntVariable): Objective = {
    require(!objectiveValue.isConstant, "An Objective value can not be constant")
    new Objective(objectiveValue.model, objectiveValue)
  }
}

class Objective(model: Store, objectiveValue: IntVariable)
    extends AbstractObjective(model) {

  model.registerForPartialPropagation(objectiveValue)

  override def value: Long = objectiveValue.value()

  override def detailedString(short: Boolean): String = objectiveValue.toString
}
