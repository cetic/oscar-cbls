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

package oscar.cbls.test.lib.invariant

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.lib.invariant.numeric._
import oscar.cbls.lib.invariant.minmax.{Max2, Min2}

import scala.util.Random

class BasicNumericInvariantsTestSuite extends AnyFunSuite {

  private val random     = Random
  private val seed: Long = random.nextLong()
  random.setSeed(seed)

  // function to make a bunch of operation on a IntVariables
  private def modifyIntVariable(x: IntVariable, minVal: Long = -1000, maxVal: Long = 1000): Unit = {
    x := random.between(minVal, maxVal)
    x :+= random.between(minVal, maxVal)
    x :-= random.between(minVal, maxVal)
    x :*= random.between(minVal, maxVal)
    if (minVal <= 0 && maxVal >= 0)
      x :/= random.between(minVal.max(1), maxVal)
    else
      x :/= random.between(minVal, maxVal)
    x.:++()
    x.:--()
  }

  test(s"Absolute value of positive number (seed: $seed)") {
    val store  = new Store(debugLevel = 3)
    val input  = IntVariable(store, 42)
    val output = IntVariable(store, 0)
    Abs(store, input, output)
    store.close()

    output.value() should be(42)
  }

  test(s"Absolute value of negative number (seed: $seed)") {
    val store  = new Store(debugLevel = 3)
    val input  = IntVariable(store, -42)
    val output = IntVariable(store, 0)
    Abs(store, input, output)
    store.close()

    output.value() should be(42)
  }

  test(s"Absolute value of 0 (seed: $seed)") {
    val store  = new Store(debugLevel = 3)
    val input  = IntVariable(store, 0)
    val output = IntVariable(store, 1)
    Abs(store, input, output)
    store.close()

    output.value() should be(0)
  }

  test(s"Abs invariant works as expected (seed: $seed)") {
    val store  = new Store(debugLevel = 3)
    val rndVal = random.between(-1000, 1000)
    val input  = IntVariable(store, rndVal)
    val output = IntVariable(store, rndVal)
    val absInv = Abs(store, input, output)
    store.close()

    modifyIntVariable(input)

    absInv.checkInternals()
  }

  test(s"Opposite invariant works as expected (seed: $seed)") {
    val store       = new Store(debugLevel = 3)
    val rndVal      = random.between(-1000, 1000)
    val input       = IntVariable(store, rndVal)
    val output      = IntVariable(store, rndVal)
    val oppositeInv = Opposite(store, input, output)
    store.close()

    modifyIntVariable(input)

    oppositeInv.checkInternals()
  }

  test(s"Square invariant works as expected (seed: $seed)") {
    val store     = new Store(debugLevel = 3)
    val rndVal    = random.between(-1000, 1000)
    val input     = IntVariable(store, rndVal)
    val output    = IntVariable(store, rndVal)
    val squareInv = Square(store, input, output)
    store.close()

    modifyIntVariable(input)

    squareInv.checkInternals()
  }

  test(s"Sqrt invariant works as expected (seed: $seed)") {
    val store   = new Store(debugLevel = 3)
    val rndVal  = random.between(1, 2000)
    val input   = IntVariable(store, rndVal)
    val output  = IntVariable(store, rndVal)
    val sqrtInv = Sqrt(store, input, output)
    store.close()

    modifyIntVariable(input, 1, 2000)

    sqrtInv.checkInternals()
  }

  test(s"Sum2 invariant works as expected (seed: $seed)") {
    val store   = new Store(debugLevel = 3)
    val rndVal1 = random.between(-1000, 1000)
    val rndVal2 = random.between(-1000, 1000)
    val input1  = IntVariable(store, rndVal1)
    val input2  = IntVariable(store, rndVal2)
    val output  = IntVariable(store, rndVal1)
    val sum2Inv = Sum2(store, input1, input2, output)
    store.close()

    modifyIntVariable(input1)
    modifyIntVariable(input2)

    sum2Inv.checkInternals()
  }

  test(s"Minus2 invariant works as expected (seed: $seed)") {
    val store     = new Store(debugLevel = 3)
    val rndVal1   = random.between(-1000, 1000)
    val rndVal2   = random.between(-1000, 1000)
    val input1    = IntVariable(store, rndVal1)
    val input2    = IntVariable(store, rndVal2)
    val output    = IntVariable(store, rndVal1)
    val minus2Inv = Minus2(store, input1, input2, output)
    store.close()

    modifyIntVariable(input1)
    modifyIntVariable(input2)

    minus2Inv.checkInternals()
  }

  test(s"Prod2 invariant works as expected (seed: $seed)") {
    val store    = new Store(debugLevel = 3)
    val rndVal1  = random.between(-1000, 1000)
    val rndVal2  = random.between(-1000, 1000)
    val input1   = IntVariable(store, rndVal1)
    val input2   = IntVariable(store, rndVal2)
    val output   = IntVariable(store, rndVal1)
    val prod2Inv = Prod2(store, input1, input2, output)
    store.close()

    modifyIntVariable(input1)
    modifyIntVariable(input2)

    prod2Inv.checkInternals()
  }

  test(s"Div2 invariant works as expected (seed: $seed)") {
    val store   = new Store(debugLevel = 3)
    val rndVal1 = random.between(-1000, 1000)
    val rndVal2 = random.between(500, 2000) // Smaller domain to avoid input2 be equal to 0
    val input1  = IntVariable(store, rndVal1)
    val input2  = IntVariable(store, rndVal2)
    val output  = IntVariable(store, rndVal1)
    val div2Inv = Div2(store, input1, input2, output)
    store.close()

    modifyIntVariable(input1)

    modifyIntVariable(input2, 1, 500) // We don't want input2 to be reduce to 0 by a subtraction.

    div2Inv.checkInternals()
  }

  test(s"Mod invariant works as expected (seed: $seed)") {
    val store   = new Store(debugLevel = 3)
    val rndVal1 = random.between(-1000, 1000)
    val rndVal2 = random.between(-1000, 1000)
    val input1  = IntVariable(store, rndVal1)
    val input2  = IntVariable(store, rndVal2)
    val output  = IntVariable(store, rndVal1)
    val modInv  = Mod(store, input1, input2, output)
    store.close()

    modifyIntVariable(input1)
    modifyIntVariable(input2)

    modInv.checkInternals()
  }

  test(s"Pow invariant works as expected (seed: $seed)") {
    val store   = new Store(debugLevel = 3)
    val rndVal1 = random.between(-1000, 1000)
    val rndVal2 = random.between(-1000, 1000)
    val input1  = IntVariable(store, rndVal1)
    val input2  = IntVariable(store, rndVal2)
    val output  = IntVariable(store, rndVal1)
    val powInv  = Pow(store, input1, input2, output)
    store.close()

    modifyIntVariable(input1)
    modifyIntVariable(input2)

    powInv.checkInternals()
  }

  test(s"Max2 invariant works as expected (seed: $seed)") {
    val store   = new Store(debugLevel = 3)
    val rndVal1 = random.between(-1000, 1000)
    val rndVal2 = random.between(-1000, 1000)
    val input1  = IntVariable(store, rndVal1)
    val input2  = IntVariable(store, rndVal2)
    val output  = IntVariable(store, rndVal1)
    val max2Inv = Max2(store, input1, input2, output)
    store.close()

    modifyIntVariable(input1)
    modifyIntVariable(input2)

    max2Inv.checkInternals()
  }

  test(s"Min2 invariant works as expected (seed: $seed)") {
    val store   = new Store(debugLevel = 3)
    val rndVal1 = random.between(-1000, 1000)
    val rndVal2 = random.between(-1000, 1000)
    val input1  = IntVariable(store, rndVal1)
    val input2  = IntVariable(store, rndVal2)
    val output  = IntVariable(store, rndVal1)
    val min2Inv = Min2(store, input1, input2, output)
    store.close()

    modifyIntVariable(input1)
    modifyIntVariable(input2)

    min2Inv.checkInternals()
  }
}
