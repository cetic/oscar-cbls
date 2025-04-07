package oscar.cbls.test

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls.util.AssertionDetector

class ConfigurationTest extends AnyFunSuite with Matchers {

  test("Assertions are activated in tests files.") {
    var shouldBeTrue: Boolean = false

    assert(
      { shouldBeTrue = true; shouldBeTrue },
      s"shouldBeTrue should be true. Got $shouldBeTrue."
    )

    shouldBeTrue shouldBe true
  }

  test("Assertions are activated in source files.") {
    AssertionDetector.isAssertionActivated() shouldBe true
  }

}
