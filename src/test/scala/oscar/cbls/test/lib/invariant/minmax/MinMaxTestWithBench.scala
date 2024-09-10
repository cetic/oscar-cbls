package oscar.cbls.test.lib.invariant.minmax

import org.scalatest.funsuite.AnyFunSuite
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.test.invBench.TestBenchData
import oscar.cbls.lib.invariant.minmax._
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.core.computation.Variable
import oscar.cbls.test.invBench.{InvTestBench, InvTestBenchWithConstGen}
import oscar.cbls.core.computation.integer.IntConstant
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.rng.Seed

class MinMaxTestWithBench extends AnyFunSuite {

  test("Min invariant is working in test bench") {
    def createMin(model: Store): TestBenchData = {
      val nbValues   = 1000
      val inputArray = Array.fill(nbValues)(IntVariable(model, 0))
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
      val inputArray = Array.fill(nbValues)(IntVariable(model, 0))
      val listened   = SetVariable(model, Set.empty[Int])
      listened.setDomain(0, nbValues - 1)
      val output                 = IntVariable(model, 0)
      val max                    = Max(model, inputArray, listened, output)
      val input: Array[Variable] = (listened :: inputArray.toList).toArray
      TestBenchData(max, input, Array(output))
    }

    val bench = InvTestBench(createMax, "Test Max Invariant")
    bench.test()

  }

  test("MinConst invariant is working in test bench") {
    class MinConstTestBench extends InvTestBenchWithConstGen[Array[Long]]("MinConst Test Bench") {

      override def createConstData(): Gen[Array[Long]] = {
        for {
          size  <- Gen.choose(1, 100)
          array <- Gen.sequence[Array[Long], Long](Array.fill(size)(Arbitrary.arbitrary[Long]))
        } yield array
      }

      override def createTestData(model: Store, inputData: Array[Long]): TestBenchData = {
        val listened = SetVariable(model, Set.empty[Int])
        listened.setDomain(0, inputData.length - 1)
        val output                 = IntVariable(model, 0)
        val min                    = MinConst(model, inputData, listened, output)
        val input: Array[Variable] = Array(listened)

        TestBenchData(min, input, Array(output))
      }

      override def typeTToString(elem: Array[Long]) =
        s"(${Array.tabulate(elem.length)(i => s"$i: ${elem(i)}").mkString(";")})"

    }

    val bench = new MinConstTestBench

    bench.test()

  }

  test("MaxConst invariant is working in test bench") {
    class MaxConstTestBench extends InvTestBenchWithConstGen[Array[Long]]("MaxConst Test Bench") {

      override def createConstData(): Gen[Array[Long]] = {
        for {
          size <- Gen.choose(1, 100)
          array <- Gen.sequence[Array[Long], Long](
            Array.fill(size)(Gen.choose(Long.MinValue + 1, Long.MaxValue))
          )
        } yield array
      }

      override def createTestData(model: Store, inputData: Array[Long]): TestBenchData = {
        val listened = SetVariable(model, Set.empty[Int])
        listened.setDomain(0, inputData.length - 1)
        val output                 = IntVariable(model, 0)
        val max                    = MaxConst(model, inputData, listened, output)
        val input: Array[Variable] = Array(listened)

        TestBenchData(max, input, Array(output))
      }

      override def typeTToString(elem: Array[Long]) =
        s"(${Array.tabulate(elem.length)(i => s"$i: ${elem(i)}").mkString(";")})"

    }

    val bench = new MaxConstTestBench

    bench.test()

  }

  test("MinSet invariant is working in test bench") {
    def createMin(model: Store): TestBenchData = {
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
      val inputSet               = SetVariable(model, Set.empty[Int])
      val output                 = IntVariable(model, 0)
      val max                    = MaxSet(model, inputSet, output)
      val input: Array[Variable] = Array(inputSet)

      TestBenchData(max, input, Array(output))
    }

    val bench = InvTestBench(createMin, "Test MaxSet Invariant")
    bench.test()

  }

}
