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
import oscar.cbls._
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.search.loop.LoopBehavior
import oscar.cbls.lib.invariant.numeric.Abs
import oscar.cbls.lib.neighborhoods.combinator.BestSlopeFirst
import oscar.cbls.lib.neighborhoods.{AssignMove, AssignNeighborhood, SwapMove, SwapNeighborhood}

class BestSlopeFirstTest extends AnyFunSuite with Matchers {

  ignore("BestSlopFirst works as expected") {
    implicit val m: Model = model("BestSlope test")

    val a = m.intVar(70L, 0L, 100L, "a")
    val b = m.intVar(20L, 0L, 100L, "b")
    val c = m.intVar(80L, 0L, 100L, "c")
    val d = m.intVar(50L, 0L, 100L, "d")

    val input    = Array(a, b, c, d)
    val distVars = Array.fill(input.length - 1)(m.intVar(0L, 0L, 100L))
    for (i <- distVars.indices) {
      Abs(m.store, input(i + 1) - input(i), distVars(i))
    }

    val objVal = sum(distVars)
    val obj    = m.minimize(objVal)

    m.close()

    val swap = SwapNeighborhood(
      input,
      selectFirstVariableBehavior = LoopBehavior.first(),
      selectSecondVariableBehavior = LoopBehavior.first(),
      hotRestart = true
    )

    val assignableValue = (v: IntVariable) => {
      if (v == a) 60L to 70L
      else if (v == b) 50L to 55L
      else if (v == c) 65L to 90L
      else if (v == d) 30L to 65L
      else 0L to 100L
    }

    val assign = AssignNeighborhood(
      input,
      (v, _) => assignableValue(v),
      selectVariableBehavior = LoopBehavior.first(),
      selectValueBehavior = LoopBehavior.best(),
      hotRestart = true
    )

    val search = BestSlopeFirst(List(swap, assign), 4, 3)
    search.profileSearch()

    var moves = search.getAllMoves(obj)
    // The first 3 moves are a Swap
    moves.head must be(SwapMove(a, b, 90, swap.name))
    moves = moves.tail
    moves.head must be(SwapMove(b, d, 70, swap.name))
    moves = moves.tail
    moves.head must be(SwapMove(c, d, 60, swap.name))

    // Swap is exhausted, the search tries Assign
    moves = moves.tail
    moves.head must be(AssignMove(a, 60, 40, assign.name))
    moves = moves.tail
    moves.head must be(AssignMove(b, 55, 30, assign.name))
    moves = moves.tail
    moves.head must be(AssignMove(d, 65, 25, assign.name))
    moves = moves.tail
    moves.head must be(AssignMove(c, 65, 15, assign.name))

    // Assign is exhausted but Swap is no more tabu
    moves = moves.tail
    moves.head must be(SwapMove(a, b, 10, swap.name))

    // Swap is exhausted, but we can override the tabu of Assign
    moves = moves.tail
    moves.head must be(AssignMove(a, 60, 5, assign.name))

    // No more move
    moves.tail mustBe empty

  }

  ignore("BestSlopFirst verbosity") {
    implicit val m: Model = model("BestSlope test")

    val a = m.intVar(70L, 0L, 100L, "a")
    val b = m.intVar(20L, 0L, 100L, "b")
    val c = m.intVar(80L, 0L, 100L, "c")
    val d = m.intVar(50L, 0L, 100L, "d")

    val input    = Array(a, b, c, d)
    val distVars = Array.fill(input.length - 1)(m.intVar(0L, 0L, 100L))
    for (i <- distVars.indices) {
      Abs(m.store, input(i + 1) - input(i), distVars(i))
    }

    val objVal = sum(distVars)
    val obj    = m.minimize(objVal)

    m.close()

    println(objVal)

    val swap = SwapNeighborhood(
      input,
      selectFirstVariableBehavior = LoopBehavior.first(),
      selectSecondVariableBehavior = LoopBehavior.first(),
      hotRestart = true
    )

    val assignableValue = (v: IntVariable) => {
      if (v == a) 60L to 70L
      else if (v == b) 50L to 55L
      else if (v == c) 65L to 90L
      else if (v == d) 30L to 65L
      else 0L to 100L
    }

    val assign = AssignNeighborhood(
      input,
      (v, _) => assignableValue(v),
      selectVariableBehavior = LoopBehavior.first(),
      selectValueBehavior = LoopBehavior.best(),
      hotRestart = true
    )

    val search = BestSlopeFirst(List(swap, assign), 4, 3)
    search.profileSearch()
    search.verbosityLevel = 2

    search.doAllMoves(obj)
    search.displayProfiling()
    println(input.mkString(", "))

  }

}
