package oscar.cbls.test.core.computation

import oscar.cbls.IntVariable
import oscar.cbls.core.computation.integer.IntNotificationTarget
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.Invariant
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class DynamicLinkUnitTestSuite extends AnyFunSuite with Matchers{

  private class TestInvWorking(var1: IntVariable, var2: IntVariable, out: IntVariable)
      extends Invariant(var1.model, Some("test invariant"))
      with IntNotificationTarget {

    var1.registerStaticallyAndDynamicallyListeningElement(this)
    var2.registerStaticallyAndDynamicallyListeningElement(this)
    out.setDefiningInvariant(this)

    override def notifyIntChanges(
      intVariable: IntVariable,
      contextualVarIndex: Int,
      oldVal: Long,
      newVal: Long
    ): Unit = {
      scheduleForPropagation()
    }

    override def performPropagation() = {
      out := var1.value() + var2.value()
    }

    override def checkInternals(): Unit = {}
  }

  private class TestInvNotWorking(var1: IntVariable, var2: IntVariable, out: IntVariable)
      extends Invariant(var1.model, Some("test invariant"))
      with IntNotificationTarget {

    var1.registerDynamicallyListeningElement(this)
    var2.registerDynamicallyListeningElement(this)
    out.setDefiningInvariant(this)

    override def notifyIntChanges(
      intVariable: IntVariable,
      contextualVarIndex: Int,
      oldVal: Long,
      newVal: Long
    ): Unit = {
      scheduleForPropagation()
    }

    override def performPropagation() = {
      out := var1.value() + var2.value()
    }

    override def checkInternals(): Unit = {}
  }

  private val store = new Store()

  private val var1       = new IntVariable(store, 0, false)
  private val var2       = new IntVariable(store, 0, false)
  private val out1       = new IntVariable(store, 0, false)
  private val invWorking = new TestInvWorking(var1, var2, out1)

  private val var3          = new IntVariable(store, 0, false)
  private val var4          = new IntVariable(store, 0, false)
  private val out2          = new IntVariable(store, 0, false)
  private val invNotWorking = new TestInvNotWorking(var3, var4, out2)

  store.close()

  test("Propagation succeeds when static link is properly defined.") {
    var1 := 2
    assert(out1.value() == 2)
  }

  test("Propagation fails when static link is not properly defined.") {
    var3 := 2
    an[java.lang.IllegalArgumentException] should be thrownBy out2.value()
  }
}
