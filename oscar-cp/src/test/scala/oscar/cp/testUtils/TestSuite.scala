package oscar.cp.testUtils

import org.scalatest.FunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.Assertions
import oscar.cp.{CPStore, Constraint, isInconsistent}

/** @author Renaud Hartert ren.hartert@gmail.com */
abstract class TestSuite extends FunSuite with Matchers with Assertions {
  
  /** Perform the test only if the condition is true. */
  protected def test(testCondition: Boolean, testName: String)(testFun: => Unit): Unit = {
    if (testCondition) test(testName)(testFun)
  }

  protected def postAndCheckSuspend(store: CPStore, c: Constraint): Unit = {
    store.post(c)
    c.isActive should be (true)
  }

  protected def postAndCheckSuccess(store: CPStore, c: Constraint): Unit = {
    store.post(c)
    c.isActive should be (false)
  }

  protected def postAndCheckFailure(store: CPStore, c: Constraint): Unit = {
    isInconsistent(store.post(c)) should be (true)
  }
}