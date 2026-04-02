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

package oscar.cbls.test.lib.invariant.numeric

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.lib.invariant.minmax.{Max2, Min2}
import oscar.cbls.lib.invariant.numeric._
import oscar.cbls.modeling.Model
import oscar.cbls.util.invBench.{InvTestBench, TestBenchSut}

class BasicNumericInvariantsTestSuite extends AnyFunSuite {

  test(s"Absolute value of positive number") {
    val store  = new Store(debugLevel = 3)
    val input  = IntVariable(store, 42)
    val output = IntVariable(store, 0)
    Abs(store, input, output)
    store.close()

    output.value() should be(42)
  }

  test(s"Absolute value of negative number") {
    val store  = new Store(debugLevel = 3)
    val input  = IntVariable(store, -42)
    val output = IntVariable(store, 0)
    Abs(store, input, output)
    store.close()

    output.value() should be(42)
  }

  test(s"Absolute value of 0") {
    val store  = new Store(debugLevel = 3)
    val input  = IntVariable(store, 0)
    val output = IntVariable(store, 1)
    Abs(store, input, output)
    store.close()

    output.value() should be(0)
  }

  test(s"Abs invariant works as expected") {
    def createInv(model: Model): TestBenchSut = {
      val input  = model.intVar(0, Int.MinValue, Int.MaxValue)
      val output = model.intVar(0, 0, Int.MaxValue)
      val inv    = Abs(model.store, input, output)

      TestBenchSut(inv, Array(input), Array(output))
    }

    val bench: InvTestBench = InvTestBench(createInv, "Test Abs inv")
    bench.test()
  }

  test(s"Opposite invariant works as expected") {
    def createInv(model: Model): TestBenchSut = {
      val input  = model.intVar(0, Int.MinValue, Int.MaxValue)
      val output = model.intVar(0, Int.MinValue, Int.MaxValue)
      val inv    = Opposite(model.store, input, output)

      TestBenchSut(inv, Array(input), Array(output))
    }

    val bench: InvTestBench = InvTestBench(createInv, "Test Opposite inv")
    bench.test()
  }

  test(s"Square invariant works as expected ") {
    def createInv(model: Model): TestBenchSut = {
      val input  = model.intVar(0, Int.MinValue, Int.MaxValue)
      val output = model.intVar(0, 0, Long.MaxValue)
      val inv    = Square(model.store, input, output)

      TestBenchSut(inv, Array(input), Array(output))
    }

    val bench: InvTestBench = InvTestBench(createInv, "Test Square inv")
    bench.test()
  }

  test(s"Sqrt invariant works as expected") {
    def createInv(model: Model): TestBenchSut = {
      val input  = model.intVar(0, 0, Long.MaxValue)
      val output = model.intVar(0, 0, Long.MaxValue)
      val inv    = Sqrt(model.store, input, output)

      TestBenchSut(inv, Array(input), Array(output))
    }

    val bench: InvTestBench = InvTestBench(createInv, "Test Sqrt inv")
    bench.test()
  }

  test(s"Sum2 invariant works as expected") {
    def createInv(model: Model): TestBenchSut = {
      val a      = model.intVar(0, Int.MinValue, Int.MaxValue)
      val b      = model.intVar(0, Int.MinValue, Int.MaxValue)
      val output = model.intVar(0, Long.MinValue, Long.MaxValue)
      val inv    = Sum2(model.store, a, b, output)

      TestBenchSut(inv, Array(a, b), Array(output))
    }

    val bench: InvTestBench = InvTestBench(createInv, "Test Sum2 inv")
    bench.test()
  }

  test(s"Minus2 invariant works as expected") {
    def createInv(model: Model): TestBenchSut = {
      val a      = model.intVar(0, Int.MinValue, Int.MaxValue)
      val b      = model.intVar(0, Int.MinValue, Int.MaxValue)
      val output = model.intVar(0, Long.MinValue, Long.MaxValue)
      val inv    = Minus2(model.store, a, b, output)

      TestBenchSut(inv, Array(a, b), Array(output))
    }

    val bench: InvTestBench = InvTestBench(createInv, "Test Minus2 inv")
    bench.test()
  }

  test(s"Prod2 invariant works as expected") {
    def createInv(model: Model): TestBenchSut = {
      val a      = model.intVar(0, Int.MinValue, Int.MaxValue)
      val b      = model.intVar(0, Int.MinValue, Int.MaxValue)
      val output = model.intVar(0, Long.MinValue, Long.MaxValue)
      val inv    = Prod2(model.store, a, b, output)

      TestBenchSut(inv, Array(a, b), Array(output))
    }

    val bench: InvTestBench = InvTestBench(createInv, "Test Prod2 inv")
    bench.test()
  }

  test(s"Div2 invariant works as expected with positive divisor") {
    def createInv(model: Model): TestBenchSut = {
      val a      = model.intVar(0, Long.MinValue, Long.MaxValue)
      val b      = model.intVar(1L, 1L, Long.MaxValue)
      val output = model.intVar(0, Long.MinValue, Long.MaxValue)
      val inv    = Div2(model.store, a, b, output)

      TestBenchSut(inv, Array(a, b), Array(output))
    }

    val bench: InvTestBench = InvTestBench(createInv, "Test Div2 inv")
    bench.test()
  }

  test(s"Div2 invariant works as expected with negative divisor") {
    def createInv(model: Model): TestBenchSut = {
      val a      = model.intVar(0, Long.MinValue, Long.MaxValue)
      val b      = model.intVar(-1L, Long.MinValue, -1L)
      val output = model.intVar(0, Long.MinValue, Long.MaxValue)
      val inv    = Div2(model.store, a, b, output)

      TestBenchSut(inv, Array(a, b), Array(output))
    }

    val bench: InvTestBench = InvTestBench(createInv, "Test Div2 inv")
    bench.test()
  }

  test(s"Mod invariant works as expected with positive divisor") {
    def createInv(model: Model): TestBenchSut = {
      val a      = model.intVar(0, Long.MinValue, Long.MaxValue)
      val b      = model.intVar(1L, 1L, Long.MaxValue)
      val output = model.intVar(0, Long.MinValue, Long.MaxValue)
      val inv    = Mod(model.store, a, b, output)

      TestBenchSut(inv, Array(a, b), Array(output))
    }

    val bench: InvTestBench = InvTestBench(createInv, "Test Mod inv")
    bench.test()
  }

  test(s"Mod invariant works as expected with negative divisor") {
    def createInv(model: Model): TestBenchSut = {
      val a      = model.intVar(0, Long.MinValue, Long.MaxValue)
      val b      = model.intVar(-1L, Long.MinValue, -1L)
      val output = model.intVar(0, Long.MinValue, Long.MaxValue)
      val inv    = Mod(model.store, a, b, output)

      TestBenchSut(inv, Array(a, b), Array(output))
    }

    val bench: InvTestBench = InvTestBench(createInv, "Test Mod inv")
    bench.test()
  }

  test(s"Pow invariant works as expected") {
    def createInv(model: Model): TestBenchSut = {
      val a      = model.intVar(0, Long.MinValue, Long.MaxValue)
      val b      = model.intVar(0, Long.MinValue, Long.MaxValue)
      val output = model.intVar(0, Long.MinValue, Long.MaxValue)
      val inv    = Pow(model.store, a, b, output)

      TestBenchSut(inv, Array(a, b), Array(output))
    }

    val bench: InvTestBench = InvTestBench(createInv, "Test Pow inv")
    bench.test()
  }

  test(s"Max2 invariant works as expected") {
    def createInv(model: Model): TestBenchSut = {
      val a      = model.intVar(0, Long.MinValue, Long.MaxValue)
      val b      = model.intVar(0, Long.MinValue, Long.MaxValue)
      val output = model.intVar(0, Long.MinValue, Long.MaxValue)
      val inv    = Max2(model.store, a, b, output)

      TestBenchSut(inv, Array(a, b), Array(output))
    }

    val bench: InvTestBench = InvTestBench(createInv, "Test Max2 inv")
    bench.test()
  }

  test(s"Min2 invariant works as expected") {
    def createInv(model: Model): TestBenchSut = {
      val a      = model.intVar(0, Long.MinValue, Long.MaxValue)
      val b      = model.intVar(0, Long.MinValue, Long.MaxValue)
      val output = model.intVar(0, Long.MinValue, Long.MaxValue)
      val inv    = Min2(model.store, a, b, output)

      TestBenchSut(inv, Array(a, b), Array(output))
    }

    val bench: InvTestBench = InvTestBench(createInv, "Test Min2 inv")
    bench.test()
  }
}
