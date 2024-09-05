package oscar.cbls.test.lib.invariant.minmax

import org.scalatest.funsuite.AnyFunSuite
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.test.invBench.TestBenchData
import oscar.cbls.lib.invariant.minmax._
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.core.computation.Variable
import oscar.cbls.test.invBench.InvTestBench
import oscar.cbls.core.computation.integer.IntConstant

class MinMaxTestWithBench extends AnyFunSuite {

  test("Min invariant is working in test bench") {
    def createMin(model: Store): TestBenchData = {
      val nbValues   = 1000
      val inputArray = Array.tabulate(nbValues)(i => IntVariable(model, 0))
      val listened   = SetVariable(model, Set.empty[Int])
      listened.setDomain(0, nbValues - 1)
      val output                 = IntVariable(model, 0)
      val min                    = Min(model, inputArray, listened, output)
      val input: Array[Variable] = (listened :: inputArray.toList).toArray
      TestBenchData(min, input, Array(output))
    }

    val bench = InvTestBench(createMin, "Test Min Invariant")
    bench.test()

  }

  test("Max invariant is working in test bench") {
    def createMax(model: Store): TestBenchData = {
      val nbValues   = 1000
      val inputArray = Array.tabulate(nbValues)(i => IntVariable(model, 0))
      val listened   = SetVariable(model, Set.empty[Int])
      listened.setDomain(0, nbValues - 1)
      val output                 = IntVariable(model, 0)
      val min                    = Max(model, inputArray, listened, output)
      val input: Array[Variable] = (listened :: inputArray.toList).toArray
      TestBenchData(min, input, Array(output))
    }

    val bench = InvTestBench(createMax, "Test Max Invariant")
    bench.test()

  }

  test("MinConst invariant is working in test bench") {
    def createMin(model: Store): TestBenchData = {
      val nbValues = 1000
      val inputArray =
        Array.tabulate(nbValues)(i => new IntConstant(model, scala.util.Random.nextLong()))
      val listened = SetVariable(model, Set.empty[Int])
      listened.setDomain(0, nbValues - 1)
      val output                 = IntVariable(model, 0)
      val min                    = MinConst(model, inputArray, listened, output)
      val input: Array[Variable] = Array(listened)

      TestBenchData(min, input, Array(output))
    }

    val bench = InvTestBench(createMin, "Test MinConst Invariant")
    bench.test()

  }

  test("MaxConst invariant is working in test bench") {
    def createMax(model: Store): TestBenchData = {
      val nbValues = 1000
      val inputArray =
        Array.tabulate(nbValues)(i => new IntConstant(model, scala.util.Random.nextLong()))
      val listened = SetVariable(model, Set.empty[Int])
      listened.setDomain(0, nbValues - 1)
      val output                 = IntVariable(model, 0)
      val min                    = MaxConst(model, inputArray, listened, output)
      val input: Array[Variable] = Array(listened)

      TestBenchData(min, input, Array(output))
    }

    val bench = InvTestBench(createMax, "Test MaxConst Invariant")
    bench.test()

  }

  test("MinSet invariant is working in test bench") {
    def createMin(model: Store): TestBenchData = {
      val nbValues               = 1000
      val inputSet               = SetVariable(model, Set.empty[Int])
      val output                 = IntVariable(model, 0)
      val min                    = MinSet(model, inputSet, output)
      val input: Array[Variable] = Array(inputSet)

      TestBenchData(min, input, Array(output))
    }

    val bench = InvTestBench(createMin, "Test MinSet Invariant")
    bench.test()

  }

  test("MaxSet invariant is working in test bench") {
    def createMin(model: Store): TestBenchData = {
      val nbValues               = 1000
      val inputSet               = SetVariable(model, Set.empty[Int])
      val output                 = IntVariable(model, 0)
      val min                    = MaxSet(model, inputSet, output)
      val input: Array[Variable] = Array(inputSet)

      TestBenchData(min, input, Array(output))
    }

    val bench = InvTestBench(createMin, "Test MaxSet Invariant")
    bench.test()

  }

}
