package oscar.cbls.test.algo.fun

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import oscar.cbls.algo.fun.LinearTransform

class LinearTransformSuite extends AnyFunSuite {

  test("When opposite is false, LinearTransform works as expected") {
    // Positive offset
    val lt_pos = LinearTransform(10, opposite = false)
    // apply(x: Int)
    lt_pos(10) should be(20)
    lt_pos(0) should be(10)
    lt_pos(-10) should be(0)
    // unApply(x: Int)
    lt_pos.unApply(20) should be(10)
    lt_pos.unApply(10) should be(0)
    lt_pos.unApply(0) should be(-10)
    // Negative offset
    val lt_neg = LinearTransform(-10, opposite = false)
    // apply(x: Int)
    lt_neg(10) should be(0)
    lt_neg(0) should be(-10)
    lt_neg(-10) should be(-20)
    // unApply(x: Int)
    lt_neg.unApply(0) should be(10)
    lt_neg.unApply(-10) should be(0)
    lt_neg.unApply(-20) should be(-10)
  }

  test("When opposite is true, LinearTransform works as expected") {
    // Positive offset
    val lt_pos = LinearTransform(10, opposite = true)
    // apply(x: Int)
    lt_pos(10) should be(0)
    lt_pos(0) should be(10)
    lt_pos(-10) should be(20)
    // unApply(x: Int)
    lt_pos.unApply(0) should be(10)
    lt_pos.unApply(10) should be(0)
    lt_pos.unApply(20) should be(-10)
    // Negative offset
    val lt_neg = LinearTransform(-10, opposite = true)
    // apply(x: Int)
    lt_neg(10) should be(-20)
    lt_neg(0) should be(-10)
    lt_neg(-10) should be(0)
    // unApply(x: Int)
    lt_neg.unApply(-20) should be(10)
    lt_neg.unApply(-10) should be(0)
    lt_neg.unApply(0) should be(-10)
  }

  test("Composition of LinearFunction works as expected") {
    val lt_list = List(
      LinearTransform(10, opposite = true),
      LinearTransform(20, opposite = true),
      LinearTransform(10, opposite = false),
      LinearTransform(20, opposite = false)
    )

    for (f <- lt_list; g <- lt_list) {
      f(g)(5) should be(f(g(5)))
    }
  }

  test("Invert of a LinearFunction works as expected") {
    val lt_list = List(
      LinearTransform(10, opposite = true),
      LinearTransform(20, opposite = true),
      LinearTransform(10, opposite = false),
      LinearTransform(20, opposite = false)
    )

    for (lt <- lt_list) {
      lt.invert(5) should be(lt.unApply(5))
    }
  }

  test("Identity LinearFunction works as expected") {
    val identity    = LinearTransform.identity
    val notIdentity = LinearTransform(5, opposite = true)
    notIdentity.isIdentity should be(false)
    identity.isIdentity should be(true)
    identity(84) should be(84)
  }

}
