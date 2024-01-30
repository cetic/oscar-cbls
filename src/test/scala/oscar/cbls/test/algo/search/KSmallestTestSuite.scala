package oscar.cbls.test.algo.search

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import oscar.cbls.algo.search.{KSmallest, LazyQuicksort}

class KSmallestTestSuite extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with Matchers {

  // not asserting anything, since timing in a test can be iffy
  test("Performance check for LazyQuicksort") {
    val n            = 10000000
    val k            = 500
    val randomValues = Array.tabulate(n)(_ => (math.random() * Int.MaxValue).toInt)

    val time1  = System.currentTimeMillis()
    KSmallest.getkSmallests(randomValues, k, x => x)
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
