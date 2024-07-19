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
import oscar.cbls.core.computation.objective.Maximize
import oscar.cbls.core.search.loop.LoopBehavior
import oscar.cbls.lib.invariant.numeric.IntInt2Int
import oscar.cbls.lib.neighborhoods.AssignNeighborhood
import oscar.cbls.lib.neighborhoods.combinator.Best
import oscar.cbls.test.lib.neighborhoods.ToolsForTestingNeighborhood.generateRandomDomain

import scala.util.Random

class BestCombinatorTestSuite extends AnyFunSuite {

  private def getTestBasicModel
    : (IntVariable, IntVariable, IntVariable, (IntVariable, Int) => List[Long], Maximize) = {
    val seed: Long  = Random.nextLong()
    val rng: Random = new Random(seed)
    println(s"\nSeed: $seed")

    val store: Store          = new Store(debugLevel = 3)
    val domainA               = generateRandomDomain(rng)
    val a: IntVariable        = IntVariable(store, domainA.head, name = Some("A"))
    val domainB               = generateRandomDomain(rng)
    val b: IntVariable        = IntVariable(store, domainB.head, name = Some("B"))
    val domainC               = generateRandomDomain(rng)
    val c: IntVariable        = IntVariable(store, domainC.head, name = Some("C"))
    val middle: IntVariable   = IntVariable(store, 1000L, name = Some(s"($a)^2 + ($b)^2 "))
    val objValue: IntVariable = IntVariable(store, 1000L, name = Some(s"($a)^2 + ($b)^2 + ($c)^2 "))
    val objective: Maximize   = Maximize(objValue)
    new IntInt2Int(store, a, b, middle, (x, y) => x * x + y * y)
    new IntInt2Int(store, middle, c, objValue, (x, y) => x + y * y)

    store.close()

    val domain = (v: IntVariable) => {
      if (v == a) domainA
      else if (v == b) domainB
      else if (v == c) domainC
      else (0L to 10L).toList
    }

    (a, b, c, (v: IntVariable, index: Int) => domain(v), objective)
  }

  test("BestCombinator works as expected") {
    val (a, b, c, domains, objective) = getTestBasicModel
    val nA = AssignNeighborhood(
      Array(a),
      domains,
      name = "NA",
      selectVariableBehavior = LoopBehavior.best(),
      selectValueBehavior = LoopBehavior.best()
    )
    val nB = AssignNeighborhood(
      Array(b),
      domains,
      name = "NB",
      selectVariableBehavior = LoopBehavior.best(),
      selectValueBehavior = LoopBehavior.best()
    )
    val nC = AssignNeighborhood(
      Array(c),
      domains,
      name = "NC",
      selectVariableBehavior = LoopBehavior.best(),
      selectValueBehavior = LoopBehavior.best()
    )

    val search = Best(List(nA, nB, nC))
    search.verbosityLevel = 3
    search.profileSearch()
    search.doAllMoves(objective)
    search.displayProfiling()
  }

}
