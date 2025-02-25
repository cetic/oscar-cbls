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

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.core.computation.{Store, Variable}
import oscar.cbls.lib.invariant.logic.{Cluster, SparseCluster}
import oscar.cbls.test.invBench.{InvTestBenchWithConstGen, TestBenchSut}

import scala.collection.immutable.HashMap

class SparseClusterTestSuite extends AnyFunSuite with Matchers {

  private def testSparseCluster()
    : (Store, Array[IntVariable], HashMap[Long, SetVariable], SparseCluster) = {
    val store                     = new Store(debugLevel = 3)
    val values: Array[Long]       = Array(1, 2, 2, 3, 3, 3, 4, 4, 5, 5, 5)
    val input: Array[IntVariable] = values.map(x => IntVariable(store, x))
    val inv: SparseCluster        = Cluster.makeSparse(store, input, Array(2L, 5L))
    store.close()

    (store, input, inv(), inv)
  }

  test("SparseCluster: initialization works as expected") {
    val (_, _, output, _) = testSparseCluster()

    output.keys should contain only (2, 5)
    output(2).value() should contain only (1, 2)
    output(5).value() should contain only (8, 9, 10)
  }

  test("SparseCuster: add a new value to a cluster") {
    val (_, input, output, _) = testSparseCluster()
    input(0) := 5

    output.keys should contain only (2, 5)
    output(2).value() should contain only (1, 2)
    output(5).value() should contain only (0, 8, 9, 10)
  }

  test("SparseCluster: remove value from a cluster") {
    val (_, input, output, _) = testSparseCluster()
    input(10) := 3

    output.keys should contain only (2, 5)
    output(2).value() should contain only (1, 2)
    output(5).value() should contain only (8, 9)
  }

  test("SparseCluster: empty a cluster") {
    val (_, input, output, _) = testSparseCluster()
    input(1) := 0
    input(2) := 0

    output.keys should contain only (2, 5)
    output(2).value() shouldBe empty
    output(5).value() should contain only (8, 9, 10)
  }

  test("SparseCluster: move from a cluster to another") {
    val (_, input, output, _) = testSparseCluster()
    input(1) := 5

    output.keys should contain only (2, 5)
    output(2).value() should contain only 2
    output(5).value() should contain only (1, 8, 9, 10)
  }

  test("SparseCluster: checkInternals doesn't fail") {
    val (store, input, _, inv) = testSparseCluster()
    input(0)  := 5
    input(8)  := 2
    input(10) := 3
    store.propagate()

    noException should be thrownBy inv.checkInternals()
  }

  test("SparseCuster: checkInternals should fail when a variable is not in expected cluster") {
    val (store, _, output, inv) = testSparseCluster()
    store.propagate()

    output(2) := Set(2)
    val e = the[IllegalArgumentException] thrownBy inv.checkInternals()

    e.getMessage should include("Found a variable that is not in expected cluster.")
  }

  test("SparseCuster: checkInternals should fail when a variable is not in the good cluster") {
    val (store, _, output, inv) = testSparseCluster()
    store.propagate()

    output(2) := Set(1, 2, 3)
    val e = the[IllegalArgumentException] thrownBy inv.checkInternals()

    e.getMessage should include("A variable has not the same value than its cluster's key")
  }

  test("SparseCluster: test bench") {
    class SparseClusterTestBench
        extends InvTestBenchWithConstGen[Array[Long]]("SparseCluster Test Bench") {

      override def genConst(): Gen[Array[Long]] = {
        for {
          size <- Gen.choose(0, 100)
          set  <- Gen.listOfN(size, Gen.choose(0L, 100L))
        } yield set.toArray
      }

      override def createTestBenchSut(model: Store, inputData: Array[Long]): TestBenchSut = {
        val nbValues                  = 1000
        val input: Array[IntVariable] = Array.fill(nbValues)(IntVariable(model, 0))
        input.foreach(v => v.setDomain(0, 100))
        val inv                          = Cluster.makeSparse(model, input, inputData)
        val output                       = inv()
        val inputArray: Array[Variable]  = input.toArray
        val outputArray: Array[Variable] = output.values.toArray

        TestBenchSut(inv, inputArray, outputArray)
      }

      override def typeTToString(elem: Array[Long]): String =
        s"Clusters key: ${elem.mkString(", ")}"
    }

    val bench = new SparseClusterTestBench
    bench.test()
  }

}
