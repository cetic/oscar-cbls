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

package oscar.cbls.test.lib.invariants

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.lib.invariant.minmax._

import scala.util.Random
class MinMaxInvariantsTestSuite extends AnyFunSuite {

  test("Max on array"){
    val store                       = new Store()
    val vars                        = Array.fill(5)(new IntVariable(store, Random.between(-1000, 1000)))
    val output                      = new IntVariable(store, Long.MinValue)
    val inv                         = Max(store, vars, output, "MaxBulk")
    store.close()

    inv.checkInternals()

    vars(Random.between(0, 5)) :+= Random.between(-1000, 1000)
    vars(Random.between(0, 5)) :-= Random.between(-1000, 1000)
    vars(Random.between(0, 5)) := Random.between(-1000, 1000)
    vars(Random.between(0, 5)) :*= Random.between(-1000, 1000)
    vars(Random.between(0, 5)) :/= Random.between(-1000, 1000)
    vars(Random.between(0, 5)) :+= Random.between(-1000, 1000)
    vars(Random.between(0, 5)).:++()
    vars(Random.between(0, 5)).:--()

    store.propagate()
    inv.checkInternals()
  }

  test("Min on array"){
    val store                       = new Store()
    val vars                        = Array.fill(5)(new IntVariable(store, Random.between(-1000, 1000)))
    val output                      = new IntVariable(store, Long.MaxValue)
    val inv                         = Min(store, vars, output, "MinBulk")
    store.close()

    inv.checkInternals()

    vars(Random.between(0, 5)) :+= Random.between(-1000, 1000)
    vars(Random.between(0, 5)) :-= Random.between(-1000, 1000)
    vars(Random.between(0, 5)) := Random.between(-1000, 1000)
    vars(Random.between(0, 5)) :*= Random.between(-1000, 1000)
    vars(Random.between(0, 5)) :/= Random.between(-1000, 1000)
    vars(Random.between(0, 5)) :+= Random.between(-1000, 1000)
    vars(Random.between(0, 5)).:++()
    vars(Random.between(0, 5)).:--()

    store.propagate()
    inv.checkInternals()
  }

}
