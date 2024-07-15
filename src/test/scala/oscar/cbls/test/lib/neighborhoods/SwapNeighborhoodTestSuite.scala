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

package oscar.cbls.test.lib.neighborhoods

import org.scalatest.funsuite.AnyFunSuite
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.objective.Minimize
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.core.search.loop.LoopBehavior
import oscar.cbls.lib.invariant.numeric.{Abs, Minus2, Sum}
import oscar.cbls.lib.neighborhoods.SwapNeighborhood

import scala.util.Random

class SwapNeighborhoodTestSuite extends AnyFunSuite {

  private def getTestBasicModel(optSeed: Option[Long] = None): (Array[IntVariable], Minimize) = {
    val seed: Long = optSeed match {
      case Some(s) => s
      case None    => Random.nextLong()
    }
    val rng = Random
    rng.setSeed(seed)
    println(s"\nSeed: $seed")

    val store                     = new Store(debugLevel = 3)
    val input: Array[IntVariable] = Array.fill(5)(IntVariable(store, rng.between(1L, 11L)))
    val objValue: IntVariable = IntVariable(
      store,
      1000L,
      name = Some(s"Sum of distances of ${input.map(v => v.value()).mkString("[", ", ", "]")}")
    )

    // The distance between two number x and y is |x - y|
    // We compute the distance between each adjacent variables in input
    val distVars: Array[IntVariable] = Array.fill(input.length - 1)(IntVariable(store, 0L))
    for (i <- distVars.indices) {
      val diffVar: IntVariable = IntVariable(store, 0L)
      Minus2(store, input(i), input(i + 1), diffVar)
      Abs(store, diffVar, distVars(i))
    }
    // The objective is to minimize the sum of distances of the input variables
    Sum(store, distVars, SetVariable(store, distVars.indices.toSet), objValue)
    val objective: Minimize = Minimize(objValue)
    store.close()

    (input, objective)
  }

  test("SwapNeighborhood works as expected with First loop") {
    val (input, objective) = getTestBasicModel()

    val search = SwapNeighborhood(input)
    search.verbosityLevel = 4
    search.doAllMoves(objective)
    println()
    println(s"Input variables after search: ${input.map(v => v.value()).mkString("[", ", ", "]")}")
  }

  test("SwapNeighborhood works as expected with Best loop") {
    val (input, objective) = getTestBasicModel()

    val search = SwapNeighborhood(
      input,
      selectFirstVariableBehavior = LoopBehavior.best(),
      selectSecondVariableBehavior = LoopBehavior.best()
    )
    search.verbosityLevel = 4
    search.doAllMoves(objective)
    println()
    println(s"Input variables after search: ${input.map(v => v.value()).mkString("[", ", ", "]")}")
  }

}
