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
    val store     = new Store()
    val a : Long  = Random.between(-1000, 1000)
    val b : Long  = Random.between(-1000, 1000)
    val input1    = new IntVariable(store, a)
    val input2    = new IntVariable(store, b)
    val output    = new IntVariable(store, a)
    val inv       = Max2(store, input1, input2, output, "Max2")
    store.close()

    output.value() should be >= a
    output.value() should be >= b
    inv.name() should be("Max2")
  }

  test("Max2 works as attended"){
    val store                       = new Store()
    var randomValues1: List[Long]   = List.fill(8)(Random.between(-1000, 1000))
    var randomValues2: List[Long]   = List.fill(8)(Random.between(-1000, 1000))
    var refInt1                     = randomValues1.head
    var refInt2                     = randomValues2.head
    val input1                      = new IntVariable(store, randomValues1.head)
    val input2                      = new IntVariable(store, randomValues2.head)
    val output                      = new IntVariable(store, randomValues1.head)
    randomValues1  = randomValues1.tail
    randomValues2 = randomValues2.tail
    Max2(store, input1, input2, output)
    store.close()

    input1 :+= randomValues1.head
    input2 :+= randomValues2.head
    refInt1 += randomValues1.head
    refInt2 += randomValues2.head
    randomValues1 = randomValues1.tail
    randomValues2 = randomValues2.tail

    input1 :-= randomValues1.head
    input2 :-= randomValues2.head
    refInt1 -= randomValues1.head
    refInt2 -= randomValues2.head
    randomValues1 = randomValues1.tail
    randomValues2 = randomValues2.tail

    input1 := randomValues1.head
    input2 := randomValues2.head
    refInt1 = randomValues1.head
    refInt2 = randomValues2.head
    randomValues1 = randomValues1.tail
    randomValues2 = randomValues2.tail

    input1 :*= randomValues1.head
    input2 :*= randomValues2.head
    refInt1 *= randomValues1.head
    refInt2 *= randomValues2.head
    randomValues1 = randomValues1.tail
    randomValues2 = randomValues2.tail

    input1 :/= randomValues1.head
    input2 :/= randomValues2.head
    refInt1 /= randomValues1.head
    refInt2 /= randomValues2.head
    randomValues1 = randomValues1.tail
    randomValues2 = randomValues2.tail

    input1 :*= randomValues1.head
    input2 :*= randomValues2.head
    refInt1 *= randomValues1.head
    refInt2 *= randomValues2.head
    randomValues1 = randomValues1.tail
    randomValues2 = randomValues2.tail

    input1 :+= randomValues1.head
    input2 :+= randomValues2.head
    refInt1 += randomValues1.head
    refInt2 += randomValues2.head
    randomValues1 = randomValues1.tail
    randomValues2 = randomValues2.tail

    input1.:--()
    input2.:--()
    refInt1 -= 1
    refInt2 -= 1

    input1.:++()
    input2.:++()
    refInt1 += 1
    refInt2 += 1

    output.value() should be >= refInt1
    output.value() should be >= refInt2
  }

  test("Exchange max"){
    val store = new Store()
    var a : Long = Random.between(-1000, 1000)
    var b : Long = Random.between(-1000, 1000)
    val input1 = new IntVariable(store, a)
    val input2 = new IntVariable(store, b)
    val output = new IntVariable(store, b)
    Max2(store, input1, input2, output)
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
    output.value() should be >= a
    output.value() should be >= b
  }

  test("Min2 initialization"){
    val store     = new Store()
    val a : Long  = Random.between(-1000, 1000)
    val b : Long  = Random.between(-1000, 1000)
    val input1    = new IntVariable(store, a)
    val input2    = new IntVariable(store, b)
    val output    = new IntVariable(store, a)
    val inv       = Min2(store, input1, input2, output, "Min2")
    store.close()

    output.value() should be <= a
    output.value() should be <= b
    inv.name() should be("Min2")
  }

  test("Min2 works as attended"){
    val store                       = new Store()
    var randomValues1: List[Long]   = List.fill(8)(Random.between(-1000, 1000))
    var randomValues2: List[Long]   = List.fill(8)(Random.between(-1000, 1000))
    var refInt1                     = randomValues1.head
    var refInt2                     = randomValues2.head
    val input1                      = new IntVariable(store, randomValues1.head)
    val input2                      = new IntVariable(store, randomValues2.head)
    val output                      = new IntVariable(store, randomValues1.head)
    randomValues1  = randomValues1.tail
    randomValues2 = randomValues2.tail
    Min2(store, input1, input2, output)
    store.close()

    input1 :+= randomValues1.head
    input2 :+= randomValues2.head
    refInt1 += randomValues1.head
    refInt2 += randomValues2.head
    randomValues1 = randomValues1.tail
    randomValues2 = randomValues2.tail

    input1 :-= randomValues1.head
    input2 :-= randomValues2.head
    refInt1 -= randomValues1.head
    refInt2 -= randomValues2.head
    randomValues1 = randomValues1.tail
    randomValues2 = randomValues2.tail

    input1 := randomValues1.head
    input2 := randomValues2.head
    refInt1 = randomValues1.head
    refInt2 = randomValues2.head
    randomValues1 = randomValues1.tail
    randomValues2 = randomValues2.tail

    input1 :*= randomValues1.head
    input2 :*= randomValues2.head
    refInt1 *= randomValues1.head
    refInt2 *= randomValues2.head
    randomValues1 = randomValues1.tail
    randomValues2 = randomValues2.tail

    input1 :/= randomValues1.head
    input2 :/= randomValues2.head
    refInt1 /= randomValues1.head
    refInt2 /= randomValues2.head
    randomValues1 = randomValues1.tail
    randomValues2 = randomValues2.tail

    input1 :*= randomValues1.head
    input2 :*= randomValues2.head
    refInt1 *= randomValues1.head
    refInt2 *= randomValues2.head
    randomValues1 = randomValues1.tail
    randomValues2 = randomValues2.tail

    input1 :+= randomValues1.head
    input2 :+= randomValues2.head
    refInt1 += randomValues1.head
    refInt2 += randomValues2.head
    randomValues1 = randomValues1.tail
    randomValues2 = randomValues2.tail

    input1.:--()
    input2.:--()
    refInt1 -= 1
    refInt2 -= 1

    input1.:++()
    input2.:++()
    refInt1 += 1
    refInt2 += 1

    output.value() should be <= refInt1
    output.value() should be <= refInt2
  }

  test("Exchange min"){
    val store = new Store()
    var a : Long = Random.between(-1000, 1000)
    var b : Long = Random.between(-1000, 1000)
    val input1 = new IntVariable(store, a)
    val input2 = new IntVariable(store, b)
    val output = new IntVariable(store, b)
    Min2(store, input1, input2, output)
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
    output.value() should be <= a
    output.value() should be <= b
  }

}
