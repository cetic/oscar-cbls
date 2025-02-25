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

package oscar.cbls.test.lib.invariant.logic

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls.core.computation.{Store, Variable}
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.lib.invariant.logic.{Cluster, DenseCluster}
import oscar.cbls.test.invBench.{InvTestBench, TestBenchSut}

class DenseClusterTestSuite extends AnyFunSuite with Matchers {

  def testDenseCluster(): (Store, Array[IntVariable], Array[SetVariable], DenseCluster) = {
    val store                     = new Store(debugLevel = 3)
    val values: Array[Long]       = Array(4, 4, 4, 5, 4, 5, 5, 4)
    val input: Array[IntVariable] = values.map(x => IntVariable(store, x))
    val inv: DenseCluster         = Cluster.makeDense(store, input, 10)
    store.close()

    (store, input, inv(), inv)
  }

  test("DenseCluster: initialization works as expected") {
    val (_, _, output, _) = testDenseCluster()

    output should have length 10
    for (i <- 0 to 3) output(i).value() shouldBe empty
    for (i <- 6 until 10) output(i).value() shouldBe empty

    output(4).value() should contain only (0, 1, 2, 4, 7)
    output(5).value() should contain only (3, 5, 6)
  }

  test("DenseCluster: move a value to another non empty cluster") {
    val (_, input, output, _) = testDenseCluster()
    input(0) := 5

    for (i <- 0 to 3) output(i).value() shouldBe empty
    for (i <- 6 until 10) output(i).value() shouldBe empty

    output(4).value() should contain only (1, 2, 4, 7)
    output(5).value() should contain only (0, 3, 5, 6)
  }

  test("DenseCluster: move a value to an empty cluster") {
    val (_, input, output, _) = testDenseCluster()
    input(0) := 7

    for (i <- 1 to 3) output(i).value() shouldBe empty
    output(6).value() shouldBe empty
    for (i <- 8 until 10) output(i).value() shouldBe empty

    output(4).value() should contain only (1, 2, 4, 7)
    output(5).value() should contain only (3, 5, 6)
    output(7).value() should contain only 0
  }

  test("DenseCluster: empty a cluster") {
    val (_, input, output, _) = testDenseCluster()
    input(3) := 0
    input(5) := 0
    input(6) := 0

    for (i <- 1 to 3) output(i).value() shouldBe empty
    for (i <- 5 until 10) output(i).value() shouldBe empty

    output(4).value() should contain only (0, 1, 2, 4, 7)
    output(0).value() should contain only (3, 5, 6)
  }

  test("DenseCluster: checkInternal doesn't fail") {
    val (store, input, _, inv) = testDenseCluster()
    input(0) := 9
    input(3) := 4
    input(5) := 9
    input(1) := 5
    store.propagate()

    noException should be thrownBy inv.checkInternals()
  }

  test("DenseCluster: checkInternals should fail when a variable is not in expected cluster") {
    val (store, _, output, inv) = testDenseCluster()
    store.propagate()

    output(5) := Set(3, 5)
    val e = the[IllegalArgumentException] thrownBy inv.checkInternals()

    e.getMessage should include("Found a variable that is not in expected cluster.")
  }

  test("DenseCluster: checkInternals should fail when a variable is not in the good cluster") {
    val (store, _, output, inv) = testDenseCluster()
    store.propagate()

    output(5) := Set(3, 5, 6, 0)
    val e = the[IllegalArgumentException] thrownBy inv.checkInternals()

    e.getMessage should include("A variable has not the same value than its cluster's key")
  }

  test("DenseCluster: test bench") {
    def createDenseCluster(model: Store): TestBenchSut = {
      val nbValues                  = 1000
      val upperBound                = 10
      val input: Array[IntVariable] = Array.fill(nbValues)(IntVariable(model, 0))
      input.foreach(v => v.setDomain(0, upperBound - 1))
      val inv                          = Cluster.makeDense(model, input, upperBound)
      val inputArray: Array[Variable]  = input.toArray
      val outputArray: Array[Variable] = inv().toArray
      TestBenchSut(inv, inputArray, outputArray)
    }

    val bench = InvTestBench(createDenseCluster, "Test DenseCluster Invariant")
    bench.test()
  }

}
