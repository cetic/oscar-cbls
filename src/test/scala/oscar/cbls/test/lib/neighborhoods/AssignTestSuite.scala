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
import oscar.cbls.core.search.loop.LoopBehavior
import oscar.cbls.lib.invariant.numeric.IntInt2Int
import oscar.cbls.lib.neighborhoods.Assign
import oscar.cbls.test.lib.neighborhoods.ToolsForTestingNeighborhood.generateRandomDomain
import oscar.cbls._
import oscar.cbls.modeling.{Neighborhoods => Nrs}

import scala.util.Random
import org.scalatest.matchers.must.Matchers
import oscar.cbls.core.search.NoMoveFound

class AssignTestSuite extends AnyFunSuite with Matchers {

  private def getTestBasicModel
    : (Array[IntVariable], (IntVariable, Int) => Iterable[Long], Minimize) = {
    val seed: Long  = Random.nextLong()
    val rng: Random = Random
    rng.setSeed(seed)
    println(s"\nSeed: $seed")

    val store: Store   = new Store(debugLevel = 3)
    val domainA        = generateRandomDomain(rng)
    val a: IntVariable = IntVariable(store, domainA.head, name = Some("A"))
    a.setDomain(domainA.min, domainA.max)
    val domainB        = generateRandomDomain(rng)
    val b: IntVariable = IntVariable(store, domainB.head, name = Some("B"))
    b.setDomain(domainB.min, domainB.max)
    val objValue: IntVariable = IntVariable(store, 1000L, name = Some(s"($a)^2 + ($b)^2 "))
    val objective: Minimize   = Minimize(objValue)
    new IntInt2Int(store, a, b, objValue, (x, y) => x * x + y * y)
    store.close()

    val domain = (v: IntVariable) => {
      if (v == a) domainA
      else if (v == b) domainB
      else (0L to 10L).toList
    }

    (Array(a, b), (v: IntVariable, _: Int) => domain(v), objective)
  }

  test("Assign cannot assign same value") {
    implicit val m : Model = model("Test assign")
    val a = intVar(10,0,20)
    val obj = m.minimize(a)
    m.close()

    val assign = Nrs.assign(Array(a),
      varsDomain = (_,_) => Array(10))
    Nrs.combinator.acceptAll(assign).getMove(obj) mustBe NoMoveFound
  }

  ignore("AssignNeighborhoods works as expected with First loop") {
    val (vars, domain, objective) = getTestBasicModel

    val search = Assign(vars, domain)
    search.verbosityLevel = 4
    search.doAllMoves(objective)
  }

  ignore("AssignNeighborhoods works as expected with Best loop") {
    val (vars, domain, objective) = getTestBasicModel

    val search = Assign(
      vars,
      domain,
      selectVariableBehavior = LoopBehavior.best(),
      selectValueBehavior = LoopBehavior.best()
    )
    search.verbosityLevel = 4
    search.doAllMoves(objective)
  }

  ignore("AssignNeighborhoods works as expected with First loop, without specifying domain") {
    val (vars, _, objective) = getTestBasicModel

    val search = Assign(vars)
    search.verbosityLevel = 0
    search.doAllMoves(objective)
  }

  ignore("AssignNeighborhoods works as expected with Best loop, without specifying domain") {
    val (vars, _, objective) = getTestBasicModel

    val search = Assign(
      vars,
      selectVariableBehavior = LoopBehavior.best(),
      selectValueBehavior = LoopBehavior.best()
    )
    search.verbosityLevel = 0
    search.doAllMoves(objective)
  }
}
