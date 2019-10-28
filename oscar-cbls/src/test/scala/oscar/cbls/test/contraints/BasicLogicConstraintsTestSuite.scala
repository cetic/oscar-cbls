package oscar.cbls.test.contraints

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import oscar.cbls.lib.constraint._
import oscar.cbls.test.invariants.bench._

class BasicLogicConstraintsTestSuite extends FunSuite with Checkers{

  val verbose = 0

  test("LE maintains the violation of a lesser or equal test.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
      Random(), RandomDiff()))
    val c = LE(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10))
    bench.run(c)
  }

  test("GE maintains the violation of a greater or equal test.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
      Random(), RandomDiff()))
    val c = GE(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10))
    bench.run(c)
  }

  test("L maintains the violation of a strict lesser test.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
      Random(), RandomDiff()))
    val c = L(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10))
    bench.run(c)
  }

  test("G maintains the violation of a strict greater test.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
      Random(), RandomDiff()))
    val c = G(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10))
    bench.run(c)
  }

  test("NE maintains the violation of a inequality test.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
      Random(), RandomDiff()))
    val c = NE(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10))
    bench.run(c)
  }

  test("EQ maintains the violation of an equality test.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
      Random(), RandomDiff()))
    val c = EQ(bench.genIntVar(-10 to 10),bench.genIntVar(-10 to 10))
    bench.run(c)
  }

}
