package oscar.cbls.test.algo.sequence

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import oscar.cbls.algo.sequence.UnitaryAffineFunction

class UnitaryAffineFunctionSuite extends AnyFunSuite {

  test("When flip is false, UnitaryAffineFunction works as expected") {
    // Positive offset
    val lt_pos = UnitaryAffineFunction(10, flip = false)
    // apply(x: Int)
    lt_pos(10) should be(20)
    lt_pos(0) should be(10)
    lt_pos(-10) should be(0)
    // unApply(x: Int)
    lt_pos.unApply(20) should be(10)
    lt_pos.unApply(10) should be(0)
    lt_pos.unApply(0) should be(-10)
    // Negative offset
    val lt_neg = UnitaryAffineFunction(-10, flip = false)
    // apply(x: Int)
    lt_neg(10) should be(0)
    lt_neg(0) should be(-10)
    lt_neg(-10) should be(-20)
    // unApply(x: Int)
    lt_neg.unApply(0) should be(10)
    lt_neg.unApply(-10) should be(0)
    lt_neg.unApply(-20) should be(-10)
  }

  test("When flip is true, UnitaryAffineFunction works as expected") {
    // Positive offset
    val lt_pos = UnitaryAffineFunction(10, flip = true)
    // apply(x: Int)
    lt_pos(10) should be(0)
    lt_pos(0) should be(10)
    lt_pos(-10) should be(20)
    // unApply(x: Int)
    lt_pos.unApply(0) should be(10)
    lt_pos.unApply(10) should be(0)
    lt_pos.unApply(20) should be(-10)
    // Negative offset
    val lt_neg = UnitaryAffineFunction(-10, flip = true)
    // apply(x: Int)
    lt_neg(10) should be(-20)
    lt_neg(0) should be(-10)
    lt_neg(-10) should be(0)
    // unApply(x: Int)
    lt_neg.unApply(-20) should be(10)
    lt_neg.unApply(-10) should be(0)
    lt_neg.unApply(0) should be(-10)
  }

  test("Composition of UnitaryAffineFunction works as expected") {
    val lt_list = List(
      UnitaryAffineFunction(10, flip = true),
      UnitaryAffineFunction(20, flip = true),
      UnitaryAffineFunction(10, flip = false),
      UnitaryAffineFunction(20, flip = false)
    )

    for (f <- lt_list; g <- lt_list) {
      f(g)(5) should be(f(g(5)))
    }
  }

  test("Invert of a UnitaryAffineFunction works as expected") {
    val lt_list = List(
      UnitaryAffineFunction(10, flip = true),
      UnitaryAffineFunction(20, flip = true),
      UnitaryAffineFunction(10, flip = false),
      UnitaryAffineFunction(20, flip = false)
    )

    for (lt <- lt_list) {
      lt.invert(5) should be(lt.unApply(5))
    }
  }

  test("Identity UnitaryAffineFunction works as expected") {
    val identity    = UnitaryAffineFunction.identity
    val notIdentity = UnitaryAffineFunction(5, flip = true)
    notIdentity.isIdentity should be(false)
    identity.isIdentity should be(true)
    identity(84) should be(84)
  }

}
