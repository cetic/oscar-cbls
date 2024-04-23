package oscar.cbls.core.computation.objective

import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable

object Objective{
  def apply(objectiveValue: IntVariable): Objective = {
    require(objectiveValue.is)
  }
}


class Objective(model: Store) extends IntVariable(model,){

}
