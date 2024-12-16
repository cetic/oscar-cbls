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
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.objective.Minimize
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.lib.invariant.numeric.{Abs, Minus2, Sum}
import oscar.cbls.lib.neighborhoods.combinator.RoundRobin
import oscar.cbls.lib.neighborhoods.{AssignNeighborhood, SwapNeighborhood}
import oscar.cbls.test.lib.neighborhoods.ToolsForTestingNeighborhood.generateRandomDomain

import scala.util.Random

class RoundRobinCombinatorTestSuite extends AnyFunSuite {

  private def getDataForTest: (Array[IntVariable], (IntVariable, Int) => List[Long], Minimize) = {
    val seed: Long  = Random.nextLong()
    val rng: Random = new Random(seed)
    println(s"\nSeed: $seed")

    val store                     = new Store(debugLevel = 3)
    val domains                   = Array.fill(4)(generateRandomDomain(rng))
    val input: Array[IntVariable] = domains.map(d => IntVariable(store, d.head))

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

    val domainOf: (IntVariable, Int) => List[Long] = (v: IntVariable, i: Int) => {
      if (v == input(0)) domains(0)
      else if (v == input(1)) domains(1)
      else if (v == input(2)) domains(2)
      else if (v == input(3)) domains(3)
      else (0L to 10L).toList
    }

    (input, domainOf, objective)
  }

  ignore("RoundRobin combinator works as expected") {
    val (input, domain, objective) = getDataForTest
    input.foreach(v => println(s"$v - ${domain(v, 0)}"))
    println()

    val n1 = AssignNeighborhood(input.slice(0, 2), domain, name = "Assign(0, 1)")
    val n2 = SwapNeighborhood(input)
    val n3 = AssignNeighborhood(input.slice(2, 4), domain, name = "Assign(2, 3)")

    val search = RoundRobin(Array((n1, 1), (n2, 1), (n3, 2)))
    search.verbosityLevel = 3
    search.profileSearch()
    search.doAllMoves(objective)
    search.displayProfiling()

    input.foreach(v => println(v))
  }

}
