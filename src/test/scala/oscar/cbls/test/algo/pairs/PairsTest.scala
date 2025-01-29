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

package oscar.cbls.test.algo.pairs

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import oscar.cbls.algo.pairs.Pairs._
import oscar.cbls.algo.sequence.IntSequence

class PairsTest extends AnyFunSuite with Matchers {

  test("Test pairsOfAdjacent with list") {
    val l: List[Int] = List.from(1 to 5)
    val adj          = pairsOfAdjacent(l)

    adj must equal(List((1, 2), (2, 3), (3, 4), (4, 5)))
  }

  test("Test pairsOfAdjacent with sequence") {
    val seq = IntSequence(List.from(1 to 5))
    val adj = pairsOfAdjacent(seq)

    adj must equal(List((1, 2), (2, 3), (3, 4), (4, 5)))
  }

  test("Test pairsOfAdjacent with routes") {
    val seq = IntSequence(List(0, 3, 6, 1, 4, 7, 2, 5, 8))
    val adj = pairsOfAdjacentInRoute(seq, 3)

    val expected = List((0, 3), (3, 6), (6, 0), (1, 4), (4, 7), (7, 1), (2, 5), (5, 8), (8, 2))
    adj must equal(expected)
  }

  test("Test headAndTail") {
    val l  = List(1, 2, 3, 4)
    val ht = headAndTail(l)

    val expected = List((1, List(2, 3, 4)), (2, List(3, 4)), (3, List(4)), (4, List()))

    ht must equal(expected)
  }

  test("Test allPairs") {
    val l     = List(1, 2, 3, 4)
    val pairs = allPairs(l)

    pairs must contain only ((1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4))
  }

  test("Test allPairs with filter") {
    val l     = List(1, 2, 3, 4)
    val pairs = allPairs(l, (a: Int, b: Int) => (a + b) % 2 == 0)

    pairs must contain only ((1, 3), (2, 4))
  }

  test("Test zip") {
    val l = List(1, 2, 3, 4)
    val r = List('a', 'b', 'c')

    val pairs = zipAllPairs(l, r)

    pairs must contain only (
      (1, 'a'),
      (1, 'b'),
      (1, 'c'),
      (2, 'a'),
      (2, 'b'),
      (2, 'c'),
      (3, 'a'),
      (3, 'b'),
      (3, 'c'),
      (4, 'a'),
      (4, 'b'),
      (4, 'c')
    )
  }

  test("Test zip with filter") {
    val l = List('a', 'b', 'c')
    val r = List('A', 'B', 'C', 'D')

    // Makes pairs lower-case and upper-case
    val pairs = zipAllPairs(l, r, (lowerCase: Char, upperCase: Char) => lowerCase - upperCase == 32)

    pairs must contain only (('a', 'A'), ('b', 'B'), ('c', 'C'))
  }

}
