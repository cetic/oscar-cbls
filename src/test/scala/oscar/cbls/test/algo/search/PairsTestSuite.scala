package oscar.cbls.test.algo.search

import oscar.cbls.algo.search.Pairs
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class PairsTestSuite extends AnyFunSuite with ScalaCheckPropertyChecks with Matchers {

  private def arrange(x: (Int, Int)) = if (x._1 > x._2) x.swap else x

  test("makeAllUnsortedPairs works correctly") {
    forAll { l: List[Int] =>
      val result = Pairs.makeAllUnsortedPairs(l).map(arrange).toSet
      val expected: Set[(Int, Int)] = {
        if (l.size <= 1) Set.empty
        else {
          val a   = l.toArray
          val out = for (i <- a.indices; j <- i until a.length if j != i) yield (a(i), a(j))
          out.toSet.map(arrange)
        }
      }
      result shouldEqual expected
    }
  }

  test("makeAllSortedPairs works with no filter") {
    forAll { l: List[Int] =>
      val result = Pairs.makeAllSortedPairs(l).toSet
      val expected: Set[(Int, Int)] = {
        if (l.size <= 1) Set.empty
        else {
          val a   = l.toArray
          val out = for (i <- a.indices; j <- i until a.length if j != i) yield (a(i), a(j))
          out.toSet
        }
      }
      result shouldEqual expected
    }
  }

  test("makeAllSortedPairs works with a filter") {
    // using inequality as filter, meaning that no tuple has two identical elements
    forAll { l: List[Int] =>
      val result = Pairs.makeAllSortedPairs(l, (a: Int, b: Int) => a != b).toSet
      val expected: Set[(Int, Int)] = {
        if (l.size <= 1) Set.empty
        else {
          val a = l.toArray
          val out = for {
            i <- a.indices
            j <- i until a.length if j != i && a(i) != a(j)
          } yield (a(i), a(j))
          out.toSet
        }
      }
      result shouldEqual expected
    }
  }

  test("makeAllHeadsAndTails works correctly") {
    forAll { l: List[Int] =>
      val result = Pairs.makeAllHeadAndTails(l)
      val expected: List[(Int, List[Int])] = {
        for (i <- l.indices) yield (l(i), l.drop(i + 1))
      }.toList
      result shouldEqual expected
    }
  }

  test("zipIntoAllPossiblePairs works with no filter") {
    forAll { (l1: List[Int], l2: List[String]) =>
      val result = Pairs.zipIntoAllPossiblePairs(l1, l2).sorted
      val expected: List[(Int, String)] = {
        for (i <- l1; j <- l2) yield (i, j)
      }.sorted
      result shouldEqual expected
    }
  }

  test("zipIntoAllPossiblePairs works with a filter") {
    forAll { (l1: List[Int], l2: List[Int]) =>
      val result = Pairs.zipIntoAllPossiblePairs(l1, l2, (a: Int, b: Int) => a != b).sorted
      val expected: List[(Int, Int)] = {
        for (i <- l1; j <- l2 if i != j) yield (i, j)
      }.sorted
      result shouldEqual expected
    }
  }

  test("nextPair works correctly") {
    forAll { l: List[Int] =>
      val result = Pairs.nextPair(l)
      val expected: List[(Int, Int)] = {
        if (l.size <= 1) Nil
        else (for (i <- 0 until l.size - 1) yield (l(i), l(i + 1))).toList
      }
      result shouldEqual expected
    }
  }
}
