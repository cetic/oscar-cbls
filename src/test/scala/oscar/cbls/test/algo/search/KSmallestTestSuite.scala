package oscar.cbls.test.algo.search

import oscar.cbls.algo.search.KSmallest

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class KSmallestTestSuite extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with Matchers {

  private val list = Gen.containerOf[List, Int](Gen.chooseNum(Int.MinValue, Int.MaxValue))

  private val num: Gen[Int] = for (l <- list; n <- Gen.choose(0, l.length)) yield n

  test("getKSmallest returns the smallest k elements of a collection") {
    forAll(list, num) { (l: List[Int], k: Int) =>
      whenever(k >= 0 && k <= l.length) {
        val expected = l.sorted.take(k)
        val res      = KSmallest.getKSmallest(k, l)
        res shouldEqual expected
      }
    }
  }

  test("Key affects ordering in getKSmallest") {
    forAll(list, num) { (l: List[Int], k: Int) =>
      whenever(k >= 0 && k <= l.length) {
        val expected = l.sorted.reverse.take(k)
        val result   = KSmallest.getKSmallest(k, l, x => -x.toLong)
        result shouldEqual expected
      }
    }
  }

  test("Apply method creates object that can return smallest elements") {
    forAll(list, num) { (l: List[Int], k: Int) =>
      whenever(k >= 0 && k <= l.length) {
        val expected = l.sorted.take(k)
        val ks       = KSmallest(l)
        val result   = ks(k)
        result shouldEqual expected
      }
    }
  }

  test("Key ordering affects created object") {
    forAll(list, num) { (l: List[Int], k: Int) =>
      whenever(k >= 0 && k <= l.length) {
        val expected = l.sorted.reverse.take(k)
        val ks       = KSmallest(l, x => -x.toLong)
        val result   = ks(k)
        result shouldEqual expected
      }
    }
  }

  test("Apply method creates object that can return smallest elements that satisfy a filter") {
    // testing if the numbers are even, good as any predicate
    forAll(list, num) { (l: List[Int], k: Int) =>
      whenever(k >= 0 && k <= l.length) {
        val expected = l.filter(_ % 2 == 0).sorted.take(k)
        val ks       = KSmallest(l)
        val result   = ks(k, _ % 2 == 0)
        result shouldEqual expected
      }
    }
  }

  test("getKFirst returns first elements in the given collection that satisfy a predicate") {
    forAll(list, num) { (l: List[Int], k: Int) =>
      whenever(k >= 0 && k <= l.length) {
        val expected = l.filter(_ % 2 == 0).take(k)
        val result   = KSmallest.getKFirst(k, l, _ % 2 == 0)
        result shouldEqual expected
      }
    }
  }
}
