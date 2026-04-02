package oscar.cbls.test.lib.invariant.minmax

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import oscar.cbls.core.computation.Variable
import oscar.cbls.lib.invariant.minmax._
import oscar.cbls.modeling.Model
import oscar.cbls.util.invBench.{InvTestBench, InvTestBenchWithConstGen, TestBenchSut}

class MinMaxTestWithBench extends AnyFunSuite {

  test("Min invariant is working in test bench") {
    def createMin(model: Model): TestBenchSut = {
      val nbValues   = 1000
      val inputArray = Array.fill(nbValues)(model.intVar(0, Long.MinValue, Long.MaxValue))
      val listened   = model.setVar(Set.empty, 0, nbValues - 1)
      val output     = model.intVar(0, Long.MinValue, Long.MaxValue)
      val min        = Min(model.store, inputArray, listened, output)
      val input: Array[Variable] = (listened :: inputArray.toList).toArray
      TestBenchSut(min, input, Array(output))
    }

    val bench = InvTestBench(createMin, "Test Min Invariant")
    bench.test()

  }

  test("Max invariant is working in test bench") {
    def createMax(model: Model): TestBenchSut = {
      val nbValues   = 1000
      val inputArray = Array.fill(nbValues)(model.intVar(0, Long.MinValue, Long.MaxValue))
      val listened   = model.setVar(Set.empty, 0, nbValues - 1)
      val output     = model.intVar(0, Long.MinValue, Long.MaxValue)
      val max        = Max(model.store, inputArray, listened, output)
      val input: Array[Variable] = (listened :: inputArray.toList).toArray
      TestBenchSut(max, input, Array(output))
    }

    val bench = InvTestBench(createMax, "Test Max Invariant")
    bench.test()

  }

  test("MinConst invariant is working in test bench") {
    class MinConstTestBench extends InvTestBenchWithConstGen[Array[Long]]("MinConst Test Bench") {

      override def genConst(): Gen[Array[Long]] = {
        for {
          size  <- Gen.choose(1, 100)
          array <- Gen.sequence[Array[Long], Long](Array.fill(size)(Arbitrary.arbitrary[Long]))
        } yield array
      }

      override def createTestBenchSut(model: Model, inputData: Array[Long]): TestBenchSut = {
        val listened               = model.setVar(Set.empty, 0, inputData.length - 1)
        val output                 = model.intVar(0, Long.MinValue, Long.MaxValue)
        val min                    = MinConst(model.store, inputData, listened, output)
        val input: Array[Variable] = Array(listened)

        TestBenchSut(min, input, Array(output))
      }

      override def typeTToString(elem: Array[Long]) =
        s"(${Array.tabulate(elem.length)(i => s"$i: ${elem(i)}").mkString(";")})"

    }

    val bench = new MinConstTestBench
    bench.test()
  }

  test("MaxConst invariant is working in test bench") {
    class MaxConstTestBench extends InvTestBenchWithConstGen[Array[Long]]("MaxConst Test Bench") {

      override def genConst(): Gen[Array[Long]] = {
        for {
          size <- Gen.choose(1, 100)
          array <- Gen.sequence[Array[Long], Long](
            Array.fill(size)(Gen.choose(Long.MinValue + 1, Long.MaxValue))
          )
        } yield array
      }

      override def createTestBenchSut(model: Model, inputData: Array[Long]): TestBenchSut = {
        val listened               = model.setVar(Set.empty, 0, inputData.length - 1)
        val output                 = model.intVar(0, Long.MinValue, Long.MaxValue)
        val max                    = MaxConst(model.store, inputData, listened, output)
        val input: Array[Variable] = Array(listened)

        TestBenchSut(max, input, Array(output))
      }

      override def typeTToString(elem: Array[Long]) =
        s"(${Array.tabulate(elem.length)(i => s"$i: ${elem(i)}").mkString(";")})"

    }

    val bench = new MaxConstTestBench
    bench.test()
  }

  test("MinSet invariant is working in test bench") {
    def createMin(model: Model): TestBenchSut = {
      val inputSet               = model.setVar(Set.empty, Int.MinValue, Int.MaxValue)
      val output                 = model.intVar(0, Int.MinValue, Int.MaxValue)
      val min                    = MinSet(model.store, inputSet, output)
      val input: Array[Variable] = Array(inputSet)

      TestBenchSut(min, input, Array(output))
    }

    val bench = InvTestBench(createMin, "Test MinSet Invariant")
    bench.test()
  }

  test("MaxSet invariant is working in test bench") {
    def createMin(model: Model): TestBenchSut = {
      val inputSet               = model.setVar(Set.empty, Int.MinValue, Int.MaxValue)
      val output                 = model.intVar(0, Int.MinValue, Int.MaxValue)
      val max                    = MaxSet(model.store, inputSet, output)
      val input: Array[Variable] = Array(inputSet)

      TestBenchSut(max, input, Array(output))
    }

    val bench = InvTestBench(createMin, "Test MaxSet Invariant")
    bench.test()
  }
}
