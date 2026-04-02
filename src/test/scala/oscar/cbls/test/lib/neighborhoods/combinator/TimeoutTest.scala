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

package oscar.cbls.test.lib.neighborhoods.combinator

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls._
import oscar.cbls.core.search.loop.LoopBehavior
import oscar.cbls.lib.invariant.numeric.Int2Int
import oscar.cbls.lib.neighborhoods.Assign
import oscar.cbls.lib.neighborhoods.combinator.SoftTimeout

class TimeoutTest extends AnyFunSuite with Matchers {

  test("SoftTimeout works as expected") {
    implicit val m: Model = model("TimeoutTest test")

    val a      = m.intVar(50, 0, 100, "a")
    val objVar = m.intVar(50, 0, 100, "objVar")

    new Int2Int(
      m.store,
      a,
      objVar,
      (x: Long) => {
        Thread.sleep(1) // millisecond
        -x
      }
    )

    val input = Array(a)

    val obj = m.minimize(objVar)

    m.close()

    val search = SoftTimeout(
      Assign(
        input,
        selectVariableBehavior = LoopBehavior.first(),
        selectValueBehavior = LoopBehavior.first()
      ),
      1000L * 1000 * 1000
    )

    // search.verbosityLevel = 2
    val startTime = System.nanoTime()
    search.doAllMoves(obj)

    val endTime = System.nanoTime()
    // No more move
    val duration = endTime - startTime
    require(duration > 1000L * 1000 * 1000, s"Timed out after $duration ns")
    require(duration <= 1500L * 1000 * 1000, s"Timed out after $duration ns")
  }

  test("SoftTimeout.global works as expected") {
    implicit val m: Model = model("TimeoutTest test")

    val a      = m.intVar(50, 0, 100, "a")
    val objVar = m.intVar(50, 0, 100, "objVar")

    new Int2Int(
      m.store,
      a,
      objVar,
      (x: Long) => {
        Thread.sleep(1) // millisecond
        -x
      }
    )

    val input = Array(a)

    val obj = m.minimize(objVar)

    m.close()

    val search = SoftTimeout.global(
      Assign(
        input,
        selectVariableBehavior = LoopBehavior.first(),
        selectValueBehavior = LoopBehavior.first()
      ),
      1000L * 1000 * 1000
    )

    // search.verbosityLevel = 2
    val startTime = System.nanoTime()
    search.doAllMoves(obj)

    val endTime = System.nanoTime()
    // No more move
    val duration = endTime - startTime
    require(duration > 1000L * 1000 * 1000, s"Timed out after $duration ns")
    require(duration <= 1500L * 1000 * 1000, s"Timed out after $duration ns")
  }
}
