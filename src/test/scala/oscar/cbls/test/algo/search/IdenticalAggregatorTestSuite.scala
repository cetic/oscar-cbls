package oscar.cbls.test.algo.search

import org.scalacheck.Gen
import oscar.cbls.algo.search.IdenticalAggregator
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class IdenticalAggregatorTestSuite extends AnyFunSuite with ScalaCheckPropertyChecks with Matchers {
  // picking a smaller range to avoid overflow in the example class mod3
  private val list         = Gen.containerOf[List, Int] { Gen.chooseNum(-1000, 1000) }
  private def mod3(x: Int) = ((x % 3) + 3) % 3

  test("RemoveIdentical can eliminates duplicates") {
    forAll { l: List[Int] =>
      val expected = l.distinct.sorted
      val result   = IdenticalAggregator.removeIdentical[Int](l, (a, b) => a == b).sorted
      result shouldEqual expected
    }
  }

  test("RemoveIdentical can be used to pick equivalence classes representatives") {
    // a ~= b iff (a - b) % 3 == 0
    forAll(list) { l: List[Int] =>
      // remainder (%) is not exactly equivalent to a mod operator:
      // -1 % 3 == -1, and 2 % 3 == 2, but -1 == 2 mod 3.
      // Hence this ugly workaround
      val expected = l.map(mod3).distinct.sorted
      val result = IdenticalAggregator
        .removeIdentical[Int](l, (a, b) => (a - b) % 3 == 0)
        .map(mod3)
        .sorted
      result shouldEqual expected
    }
  }

  test("RemoveIdenticalClasses works as expected") {
    forAll(list) { l: List[Int] =>
      val expected = l.map(mod3).distinct.sorted
      val result = IdenticalAggregator
        .removeIdenticalClasses[Int](l, mod3)
        .map(mod3)
        .sorted
      result shouldEqual expected
    }
  }

  test("RemoveIdenticalClassesLazily works as expected") {
    forAll(list) { l: List[Int] =>
      val expected = l.map(mod3).distinct.sorted
      val result = IdenticalAggregator
        .removeIdenticalClassesLazily[Int, Int](l, mod3)
        .map(mod3)
        .toList
        .sorted
      result shouldEqual expected
    }
  }
}
