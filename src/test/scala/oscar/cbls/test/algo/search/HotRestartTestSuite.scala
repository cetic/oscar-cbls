// OscaR is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 2.1 of the License, or
// (at your option) any later version.
//
// OscaR is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License  for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with OscaR.
// If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html

package oscar.cbls.test.algo.search

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import oscar.cbls.algo.search.HotRestart

import scala.collection.immutable.SortedSet

class HotRestartTestSuite extends AnyFunSuite with ScalaCheckPropertyChecks with Matchers {

  test("Hot restart on empty iterable is empty iterable") {
    val res: Iterable[Int] = Nil
    forAll { i: Int =>
      HotRestart(res, i) shouldEqual res
    }
  }

  private val triplet =
    for (
      i <- Gen.choose(-1000, 1000);
      j <- Gen.choose(i, 1000);
      k <- Gen.choose(j, 1000)
    ) yield (i, j, k)

  test("Hot restart works on ranges") {
    forAll(triplet) { t: (Int, Int, Int) =>
      val (start, pivot, end) = t
      whenever(start <= pivot && pivot <= end) {
        val r      = Range.inclusive(start, end)
        val result = HotRestart(r, pivot).toList
        val expected: List[Int] =
          Range.inclusive(pivot, r.last).toList ::: Range(r.start, pivot).toList
        result shouldEqual expected
      }
    }
  }

  test("Hot restart works on sorted sets") {
    forAll { (s: SortedSet[Int], pivot: Int) =>
      whenever(s.nonEmpty) {
        val result = HotRestart(s, pivot).toList
        val expected: List[Int] = {
          if (pivot <= s.min || pivot > s.max) s.toList
          else s.filter(_ >= pivot).toList ::: s.filter(_ < pivot).toList
        }
        result shouldEqual expected
      }
    }
  }

  test("Hot restart works on lists") {
    forAll { (l: List[Int], pivot: Int) =>
      whenever(l.nonEmpty) {
        val result = HotRestart(l, pivot).toList
        val expected: List[Int] = {
          if (pivot <= l.min || pivot > l.max) l.sorted
          else l.filter(_ >= pivot).sorted ::: l.filter(_ < pivot).sorted
        }
        result shouldEqual expected
      }
    }
  }

  test("Hot restart works with generic sets") {
    forAll { (s: Set[Int], pivot: Int) =>
      whenever(s.nonEmpty) {
        val result = HotRestart(s, pivot).toList
        val expected: List[Int] = {
          if (pivot <= s.min || pivot > s.max) s.toList.sorted
          else s.filter(_ >= pivot).toList.sorted ::: s.filter(_ < pivot).toList.sorted
        }
        result shouldEqual expected
      }
    }
  }

  test("Preserve sequence works on lists") {
    forAll { (l: List[Int], pivot: Int) =>
      whenever(l.nonEmpty) {
        val result = HotRestart.preserveSequence(l, pivot).toList
        val expected: List[Int] = {
          if (!l.contains(pivot)) l
          else l.dropWhile(_ != pivot) ::: l.takeWhile(_ != pivot)
        }
        result shouldEqual expected
      }
    }
  }
}
