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

package oscar.cbls.test.lib.invariant.routing

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import oscar.cbls.algo.sequence.IntSequence
import oscar.cbls.core.computation.Store
import oscar.cbls.lib.invariant.routing.timeWindows.{TimeWindow, TimeWindowsConstraint}
import oscar.cbls.modeling.routing.VRS
import oscar.cbls.test.invBench.{InvTestBenchWithConstGen, TestBenchSut}

class TimeWindowsConstraintTest extends AnyFunSuite with Matchers {

  private def generateTSPTWDATA(): (Int => Int => Long, Array[TimeWindow]) = {

    val timeMatrix: Array[Array[Long]] = Array.fill(4)(new Array[Long](4))
    timeMatrix(0)(1) = 4
    timeMatrix(0)(2) = 3
    timeMatrix(0)(3) = 5
    timeMatrix(1)(2) = 5
    timeMatrix(1)(3) = 3
    timeMatrix(2)(3) = 4

    val timeFunction: Int => Int => Long = i =>
      j => {
        if (j > i) timeMatrix(i)(j)
        else timeMatrix(j)(i)
      }

    val timeWindows: Array[TimeWindow] = new Array[TimeWindow](4)
    timeWindows(0) = TimeWindow(0, 30, 0)
    timeWindows(1) = TimeWindow(5, 16, 2)
    timeWindows(2) = TimeWindow(0, 10, 1)
    timeWindows(3) = TimeWindow(8, 14, 2)

    (timeFunction, timeWindows)

  }

  test("TimeWindowsConstraint: initialization works as expected") {
    val model                       = new Store(debugLevel = 3)
    val vrs                         = VRS(model, 4, 1)
    val (timeFunction, timeWindows) = generateTSPTWDATA()
    val inv                         = TimeWindowsConstraint(vrs, timeFunction, timeWindows)
    model.close()

    inv(0).value() must be(0)
  }

  test("TimeWindowsConstraint: assigns works and no violation is detected") {
    val model                       = new Store(debugLevel = 3)
    val vrs                         = VRS(model, 4, 1)
    val (timeFunction, timeWindows) = generateTSPTWDATA()
    val inv                         = TimeWindowsConstraint(vrs, timeFunction, timeWindows)
    model.close()

    vrs.routes := IntSequence(List(0, 2, 3, 1))
    model.propagate()

    inv(0).value() must be(0)
  }

  test("TimeWindowsConstraint: assigns works and violation is detected") {
    val model                       = new Store(debugLevel = 3)
    val vrs                         = VRS(model, 4, 1)
    val (timeFunction, timeWindows) = generateTSPTWDATA()
    val inv                         = TimeWindowsConstraint(vrs, timeFunction, timeWindows)
    model.close()

    vrs.routes := IntSequence(List(0, 3, 2, 1))
    model.propagate()

    inv(0).value() must be(1)
  }

  test("TimeWindowsConstraint: leaving time is correctly computed") {
    val model                       = new Store(debugLevel = 3)
    val vrs                         = VRS(model, 4, 1)
    val (timeFunction, timeWindows) = generateTSPTWDATA()
    val inv                         = TimeWindowsConstraint(vrs, timeFunction, timeWindows)
    model.close()

    vrs.routes := IntSequence(List(0, 3, 2, 1))

    inv.leavingTimeWhenArrivingAt(3, 5) must be(10)  // arriving too early
    inv.leavingTimeWhenArrivingAt(3, 10) must be(12) // arriving on time
    inv.leavingTimeWhenArrivingAt(3, 15) must be(-1) // arriving too late

  }

  test("Classic TimeWindowsConstraint: test bench") {
    val n = 25
    val v = 5

    val classicBench =
      new TimeWindowsConstraintTestBench(n, v, false, false, "ClassicTimeWindowsConstraint")

    classicBench.test()
  }

  test("TimeWindowsConstraint with log reduction: test bench") {
    val n = 25
    val v = 5

    val lrBench =
      new TimeWindowsConstraintTestBench(n, v, true, false, "LogReducedTimeWindowsConstraint")

    lrBench.test()
  }

  test("TimeWindowsConstraint with log reduction and extremes: test bench") {
    val n = 25
    val v = 5

    val lrWEBench =
      new TimeWindowsConstraintTestBench(
        n,
        v,
        true,
        true,
        "LogReducedWithExtremesTimeWindowsConstraint"
      )

    lrWEBench.test()
  }

  private class TimeWindowsConstraintTestBench(
    n: Int,
    v: Int,
    withLogReduction: Boolean,
    withExtremesPC: Boolean,
    name: String,
    additionalSeeds: List[String] = List()
  ) extends InvTestBenchWithConstGen[(Array[Array[Long]], Array[TimeWindow])](
        s"$name test bench",
        additionalSeeds
      ) {

    override def genConst(): Gen[(Array[Array[Long]], Array[TimeWindow])] = {

      for {
        timeMatrix  <- genTimeMatrix()
        timeWindows <- Gen.sequence[Array[TimeWindow], TimeWindow](Array.tabulate(n)(i => genTW(i)))
      } yield (timeMatrix, timeWindows)
    }

    override def createTestBenchSut(
      model: Store,
      inputData: (Array[Array[Long]], Array[TimeWindow])
    ): TestBenchSut = {
      val (timeMatrix, timeWindows) = inputData
      val vrs                       = VRS(model, n, v)
      val inv = TimeWindowsConstraint.fromMatrix(
        vrs,
        timeMatrix,
        timeWindows,
        withLogReduction,
        withExtremesPC
      )

      TestBenchSut(inv, Array(vrs.routes), inv(), Some(vrs))
    }

    override def typeTToString(elem: (Array[Array[Long]], Array[TimeWindow])): String = {
      s"""Data:
         |TimeMatrix:
         |${elem._1.map(_.mkString("Array(", ", ", ")")).mkString("Array(", ",\n ", ")")}
         |Time Windows: ${elem._2.mkString("Array(", ",\n ", ")")}
         |""".stripMargin
    }

    private def genTimeMatrix(): Gen[Array[Array[Long]]] = {
      val arrayGen: Gen[Array[Long]] =
        Gen.sequence[Array[Long], Long](Array.fill(n)(Gen.choose(0L, 50L)))
      val matrixGen: Gen[Array[Array[Long]]] =
        Gen.sequence[Array[Array[Long]], Array[Long]](Array.fill(n)(arrayGen))

      matrixGen
    }

    private def genTW(node: Int): Gen[TimeWindow] = {
      for {
        earliestArrival <- if (node < v) Gen.choose(0, 2) else Gen.choose(0, 20)
        latestArrival   <- if (node < v) Gen.const(100) else Gen.choose(earliestArrival + 1, 40)
        duration        <- if (node < v) Gen.const(0) else Gen.choose(1, 4)
      } yield TimeWindow(earliestArrival, latestArrival, duration)
    }
  }

}
