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
  test("Max2 initialization"){
    val store     = new Store(2)
    val a : Long  = Random.between(-1000, 1000)
    val b : Long  = Random.between(-1000, 1000)
    val input1    = new IntVariable(store, a)
    val input2    = new IntVariable(store, b)
    val output    = new IntVariable(store, a)
    val inv       = Max2(store, input1, input2, output, "Max2")
    store.close()

    inv.name() should be("Max2")
    inv.checkInternals()
  }

  test("Max2 works as attended"){
    val store                       = new Store(2)
    val input1                      = new IntVariable(store, Random.between(-1000, 1000))
    val input2                      = new IntVariable(store, Random.between(-1000, 1000))
    val output                      = new IntVariable(store, Long.MinValue)
    Random.between(-1000, 1000)
    val inv = Max2(store, input1, input2, output)
    store.close()

    input1 :+= Random.between(-1000, 1000)
    input2 :+= Random.between(-1000, 1000)

    input1 :-= Random.between(-1000, 1000)
    input2 :-= Random.between(-1000, 1000)

    input1 := Random.between(-1000, 1000)
    input2 := Random.between(-1000, 1000)

    input1 :*= Random.between(-1000, 1000)
    input2 :*= Random.between(-1000, 1000)

    input1 :/= Random.between(-1000, 1000)
    input2 :/=Random.between(-1000, 1000)

    input1 :*= Random.between(-1000, 1000)
    input2 :*= Random.between(-1000, 1000)

    input1 :+= Random.between(-1000, 1000)
    input2 :+= Random.between(-1000, 1000)

    input1.:--()
    input2.:--()

    input1.:++()
    input2.:++()

    store.propagate()
    inv.checkInternals()
  }

  test("Exchange max"){
    val store     = new Store(2)
    var a : Long  = Random.between(-1000, 1000)
    var b : Long  = Random.between(-1000, 1000)
    val input1    = new IntVariable(store, a)
    val input2    = new IntVariable(store, b)
    val output    = new IntVariable(store, b)
    val inv       = Max2(store, input1, input2, output)
    store.close()

    // The smaller value become the bigger
    val dist = math.abs(b - a)
    if (a < b) {
      input1 :+= (dist + 1)
      a += (dist + 1)
    } else {
      input2 :+= (dist + 1)
      b += (dist + 1)
    }

    inv.checkInternals()
  }

  test("Min2 initialization"){
    val store     = new Store(2)
    val a : Long  = Random.between(-1000, 1000)
    val b : Long  = Random.between(-1000, 1000)
    val input1    = new IntVariable(store, a)
    val input2    = new IntVariable(store, b)
    val output    = new IntVariable(store, a)
    val inv       = Min2(store, input1, input2, output, "Min2")
    store.close()

    inv.name() should be("Min2")
    inv.checkInternals()
  }

  test("Min2 works as attended"){
    val store                       = new Store()
    val input1                      = new IntVariable(store, Random.between(-1000, 1000))
    val input2                      = new IntVariable(store, Random.between(-1000, 1000))
    val output                      = new IntVariable(store, Long.MaxValue)
    val inv                         = Min2(store, input1, input2, output)
    store.close()

    input1 :+= Random.between(-1000, 1000)
    input2 :+= Random.between(-1000, 1000)

    input1 :-= Random.between(-1000, 1000)
    input2 :-= Random.between(-1000, 1000)

    input1 := Random.between(-1000, 1000)
    input2 := Random.between(-1000, 1000)

    input1 :*= Random.between(-1000, 1000)
    input2 :*= Random.between(-1000, 1000)

    input1 :/= Random.between(-1000, 1000)
    input2 :/= Random.between(-1000, 1000)

    input1 :*= Random.between(-1000, 1000)
    input2 :*= Random.between(-1000, 1000)

    input1 :+= Random.between(-1000, 1000)
    input2 :+= Random.between(-1000, 1000)

    input1.:--()
    input2.:--()

    input1.:++()
    input2.:++()

    store.propagate()
    inv.checkInternals()
  }

  test("Exchange min"){
    val store     = new Store()
    var a : Long  = Random.between(-1000, 1000)
    var b : Long  = Random.between(-1000, 1000)
    val input1    = new IntVariable(store, a)
    val input2    = new IntVariable(store, b)
    val output    = new IntVariable(store, Long.MaxValue)
    val inv       = Min2(store, input1, input2, output)
    store.close()

    // The bigger value become the smaller
    val dist = math.abs(b - a)
    if (a > b) {
      input1 :-= (dist + 1)
      a -= (dist + 1)
    } else {
      input2 :+= (dist + 1)
      b += (dist + 1)
    }

    store.propagate()
    inv.checkInternals()
  }

  test("Max on array"){
    val store                       = new Store()
    val vars                        = Array.fill(5)(new IntVariable(store, Random.between(-1000, 1000)))
    val output                      = new IntVariable(store, Long.MinValue)
    val inv                         = Max(store, vars, output)
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
    val inv                         = Min(store, vars, output)
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
