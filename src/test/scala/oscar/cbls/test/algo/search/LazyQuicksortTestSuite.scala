package oscar.cbls.test.algo.search

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import oscar.cbls.algo.search.{KSmallest, LazyQuicksort}

class LazyQuicksortTestSuite extends AnyFunSuite with ScalaCheckPropertyChecks with Matchers {

  test("Lazy quicksort can sort the whole array") {
    forAll { array: Array[Int] =>
      val lqs = LazyQuicksort(array, x => x)
      whenever(array.length > 0) {
        lqs.sortUntil(array.length - 1)
        val max = lqs(array.length - 1)
        max shouldEqual array.max
      }
      array shouldBe sorted
    }
  }

  private val arr = Gen.containerOf[Array, Int](
    Gen.chooseNum(Int.MinValue, Int.MaxValue)
  )

  private val idx: Gen[Int] = for (a <- arr; n <- Gen.choose(0, a.length)) yield n

  test("Lazy quicksort sorts until at least the required element") {
    forAll (arr, idx) { (array: Array[Int], i: Int) =>
      val aSorted = array.clone().sorted
      val lqs = LazyQuicksort(array, x => x)
      whenever(i >= 0 && i <= array.length - 1) {
        lqs(i) shouldEqual aSorted(i)
      }
    }
  }

  test("Lazy quicksort iterator works properly") {
    forAll { array: Array[Int] =>
      val sortedList = array.clone().sorted.toList
      val lqs = LazyQuicksort(array, x => x)
      val listFromIterator = {
        val b = List.newBuilder[Int]
        for (i <- lqs) b += i
        b.result()
      }
      listFromIterator shouldEqual sortedList
    }
  }

  test("Lazy quicksort key affects ordering") {
    forAll { array: Array[Int] =>
      val lqs = LazyQuicksort(array, x => -x.toLong)
      whenever(array.length > 0) {
        lqs.sortUntil(array.length - 1)
        val min = lqs(array.length - 1)
        min shouldEqual array.min
      }
      array.reverse shouldBe sorted
    }
  }

  // not asserting anything, since timing in a test can be iffy
  ignore("Performance check for LazyQuicksort") {
    val n            = 10000000
    val k            = 500
    val randomValues = Array.tabulate(n)(_ => (math.random() * Int.MaxValue).toInt)

    val time1  = System.currentTimeMillis()
    KSmallest.getKSmallest(k, randomValues, x => x)
    val watch1 = System.currentTimeMillis() - time1

    val time2 = System.currentTimeMillis()
    val it    = new LazyQuicksort(randomValues).iterator
    for (_ <- 1 to k) {
      it.next()
    }
    val watch2 = System.currentTimeMillis() - time2

    println(s"nonLazy:$watch1")
    println(s"lazy:$watch2")
    watch2 should be < watch1
  }
}
