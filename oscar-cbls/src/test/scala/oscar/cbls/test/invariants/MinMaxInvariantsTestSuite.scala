package oscar.cbls.test.invariants

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import oscar.cbls.lib.invariant.minmax._
import oscar.cbls.test.invariants.bench._

class MinMaxInvariantsTestSuite extends FunSuite with Checkers{

  val verbose = 0

  def randomArray(size:Int,values:Range):Array[Long] = {
    def randomValue(r:Range):Int = {
      r.start + (r.length * scala.math.random).toInt
    }
    Array.tabulate(size)(_ => randomValue(values))
  }

  // TODO test also with the other parameters of ArgMinArray
  test("ArgMinArray maintains the set of min variables of the array") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    ArgMin(bench.genIntVarsArray(20, 0 to 30))
    bench.run()
  }

  // TODO test also with the other parameters of ArgMaxArray
  test("ArgMaxArray maintains the set of max variables of the array") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    ArgMax(bench.genIntVarsArray(20, 0 to 30))
    bench.run()
  }

  test("MaxLin") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    MaxLin(bench.genSortedIntVars(6, -10 to 10))
    bench.run()
  }

  test("MinLin") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    MinLin(bench.genSortedIntVars(6, 0 to 10))
    bench.run()
  }
  /*
    test("Min") {
      val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
      new Min(bench.genSortedIntVars(5, -10 to 10))
      bench.run()
    }

    test("Max") {
      val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
      new Max(bench.genSortedIntVars(5, -10 to 10))
      bench.run()
    }
  */
  test("Min2") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    Min2(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10))
    bench.run()
  }

  test("Max2") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    Max2(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10))
    bench.run()
  }

  test("MinArray maintains the minimum from an array of variables.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    MinArray(bench.genIntVarsArray(4, 0 to 100))
    bench.run()
  }

  test("MaxArray maintains the maximum from an array of variables.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    MaxArray(bench.genIntVarsArray(2, 0 to 50))
    bench.run()
  }

  test("MinArrayLazy maintains the minimum from an array of variables.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new MinConstArrayLazy(randomArray(30,0 to 100),bench.genIntSetVar(range = 0 until 30))
    bench.run()
  }

  test("MaxArrayLazy maintains the maximum from an array of variables.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new MaxConstArrayLazy(randomArray(30,0 to 100),bench.genIntSetVar(range = 0 until 30))
    bench.run()
  }

  test("MinArrayValueWise maintains the minimum from an array of variables.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new MinConstArrayValueWise(randomArray(30,0 to 100),bench.genIntSetVar(range = 0 until 30),Int.MinValue)
    bench.run()
  }

  test("MinSet maintains the minimum of a set.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    MinSet(bench.genIntSetVar(),Default=100)
    bench.run()
  }

  test("MaxSet maintains the maximum of a set") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    MaxSet(bench.genIntSetVar(),Default=0)
    bench.run()
  }

  test("NthSmallest (true) maintains the nth smallest element of a set"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    val array = bench.genIntVarsArray()
    NthSmallest(array,scala.util.Random.nextInt(array.length),true)
    bench.run()
  }

  test("NthSmallest (false) maintains the nth biggest element of a set"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    val array = bench.genIntVarsArray()
    NthSmallest(array,scala.util.Random.nextInt(array.length),false)
    bench.run()
  }

  test("MaxConstArray maintains Max(Var(i) | i in cond) "){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    MaxConstArray(randomArray(30,0 to 100),bench.genIntSetVar(range = 0 until 30))
    bench.run()
  }

  test("MinConstArray maintains Min(Var(i) | i in cond) "){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    MinConstArray(randomArray(30,0 to 100),bench.genIntSetVar(range = 0 until 30))
    bench.run()
  }
}
