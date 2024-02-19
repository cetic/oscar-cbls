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

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import oscar.cbls.algo.search.HotRestart

import scala.collection.immutable.SortedSet

class HotRestartTestSuite extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with Matchers {

  oldTest()

  private def oldTest(): Unit = {
//    println(new InstrumentedRange(NumericRange.inclusive[Int](0, 9, 1)) startBy 5)

    val s: SortedSet[Int] = SortedSet(1, 2, 3, 4, 7, 8, 9)
    val it                = s.iteratorFrom(7)
    while (it.hasNext) println("next:" + it.next())

    println(oscar.cbls.algo.search.HotRestart(s, 0))
  }

  test("Start by works with instrumented range") {

  }

  test("App in source") {
    val it = (0 until 100).filter(_ % 3 == 1)
    println("nonHotRestart" + it)
    val setM = HotRestart(it, 31)
    println("hotRestart" + setM)

    val it2 = (0 until 100)
    println("nonHotRestart2" + it2)
    val setM2 = HotRestart(it2, 31)
    println("hotRestart2" + setM2)

    val it3: Range = 0 until 0
    println("nonHotRestart3: " + it3)
    val setM3 = HotRestart(it3, 0)
    println("hotRestart3: " + setM3)
  }

}
