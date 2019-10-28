package oscar.cbls.test.invariants

import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import oscar.cbls.lib.invariant.numeric._
import oscar.cbls.lib.invariant.set.SetProd
import oscar.cbls.test.invariants.bench._

class ArithmeticInvariantsTestSuite extends FunSuite with Checkers{

  val verbose = 0

  test("SumElements maintains the sum of variables of which indices are in the given set.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    SumElements(
      bench.genIntVarsArray(10, 0 to 100),
      bench.genIntSetVar(5, 0 to 9))
    bench.run()

  }

  test("ProdElements maintains the product of variables of which indices are in the given set.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    ProdElements(
      bench.genIntVarsArray(10, 0 to 10),
      bench.genIntSetVar(2, 0 to 9))
    bench.run()
  }

  test("Sum maintains the sum of input variables.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    Sum(bench.genIntVarsArray(10, 0 to 100))
    bench.run()
  }

  test("Prod maintains the product of input variables.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    Prod(bench.genIntVarsArray(3, 0 to 100))
    bench.run()
  }

  test("Minus maintains the difference between two variables.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    Minus(bench.genIntVar(0 to 100), bench.genIntVar(0 to 100))
    bench.run()
  }

  test("Sum2 maintains the sum of two variables.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    Sum2(bench.genIntVar(0 to 100), bench.genIntVar(0 to 100))
    bench.run()
  }

  test("Prod2 maintains the product of two variables") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    Prod2(bench.genIntVar(0 to 100), bench.genIntVar(0 to 100))
    bench.run()
  }

  test("Div maintains the division of two variables.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    Div(bench.genIntVar(0 to 100),
      bench.genIntVar(1 to 100, true, (v: Int) => v != 0))
    bench.run()
  }

  test("Mod maintains the modulo of two variables.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    Mod(bench.genIntVar(0 to 100),
      bench.genIntVar(1 to 100, true, (v: Int) => v != 0))
    bench.run()
  }

  test("Abs maintains the absolute value of a variable.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    Abs(bench.genIntVar(-100 to 100))
    bench.run()
  }

  test("Step maintains a step function of the input var.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    Step(bench.genIntVar(-100 to 100))
    bench.run()
  }

  test("RoundUpModulo") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    RoundUpModulo(
      bench.genIntVar(0 to 10),
      bench.genIntVar(0 to 5),
      Gen.choose(10, 20).sample.get,
      Gen.choose(1, 5).sample.get,
      Gen.choose(0, 10).sample.get)
    bench.run()
  }


  /**
    * Won't pass when the product products an overflow.
    */
  test("SetProd maintains the product of variables") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    SetProd(bench.genIntSetVar(10, -3 to 3))
    bench.run()
  }

}
