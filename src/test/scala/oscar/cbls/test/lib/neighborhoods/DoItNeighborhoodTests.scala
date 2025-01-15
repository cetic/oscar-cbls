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
import org.scalatest.matchers.must.Matchers
import oscar.cbls._
import oscar.cbls.core.search.{MoveFound, NoMoveFound}
import oscar.cbls.lib.neighborhoods.DoItNeighborhood

class DoItNeighborhoodTests extends AnyFunSuite with Matchers {

  test("DoIt works as expected") {
    implicit val m: Model = model("Do it tests")
    val a                 = intVar(5, 0, 10)
    val b                 = intVar(3, 0, 10)
    val obj               = m.minimize(a + b)
    m.close()

    val search = DoItNeighborhood(m.store, () => a := 10)
    search.getMove(obj) match {
      case NoMoveFound   => require(requirement = false, "Should not happen")
      case mf: MoveFound => mf.move.commit()
    }

    a.value() must be(10)
    b.value() must be(3)
    obj.objValue.value() must be(13)
  }

  test("DoIt.doAllMoves works as expected") {
    implicit val m: Model = model("Do it tests")
    val a                 = intVar(5, 0, 10)
    val b                 = intVar(3, 0, 10)
    val obj               = m.minimize(a + b)
    m.close()

    val search = DoItNeighborhood(m.store, () => a := 10)
    noException mustBe thrownBy(search.doAllMoves(obj, _ >= 3))
  }
}
