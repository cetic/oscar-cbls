package oscar.cbls.test.core.computation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.objective.Minimize

class ObjectiveTestSuite extends AnyFunSuite {

  test("Can't create an Objective with a constant Variable") {
    val store = new Store()
    val exception1 =
      intercept[IllegalArgumentException](Minimize(IntVariable(store, 10, isConstant = true)))

    assert(exception1.getMessage.contains("An Objective value can not be constant"))
  }

}
