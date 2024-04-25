package oscar.cbls.test.core.computation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.objective.{CascadingObjective, Objective}

class ObjectiveTestSuite extends AnyFunSuite{

  test("Can't create a CascadingObjective with less than 2 Objective"){
    val store = new Store()
    val objective = Objective(IntVariable(store,10))

    val exception1 = intercept[IllegalArgumentException](CascadingObjective(List.empty))
    assert(exception1.getMessage.contains("Building CascadingObjective with an empty list of Objectives is forbidden"))

    val exception2 = intercept[IllegalArgumentException](CascadingObjective(List(objective)))
    assert(exception2.getMessage.contains("Building CascadingObjective with less than 2 Objectives is forbidden"))
  }

  test("Can't create an Objective with a constant Variable"){
    val store = new Store()
    val exception1 = intercept[IllegalArgumentException](Objective(IntVariable(store,10,isConstant = true)))

    assert(exception1.getMessage.contains("An Objective value can not be constant"))
  }

  test("If the mustBeZeroObjective is not equal to zero the resulting value is Long.MaxValue"){
    val store = new Store()
    val objective1 = Objective(IntVariable(store,10))
    val objective2 = Objective(IntVariable(store,20))
    val cascading = CascadingObjective(List(objective1,objective2))

    store.close()

    cascading.value should be(Long.MaxValue)
  }

  test("If the mustBeZeroObjective is equal to zero the resulting value is the secondObjective value"){
    val store = new Store()
    val objective1 = Objective(IntVariable(store,0))
    val objective2 = Objective(IntVariable(store,20))
    val cascading = CascadingObjective(List(objective1,objective2))

    store.close()

    cascading.value should be(20)
  }

}
