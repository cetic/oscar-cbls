package oscar.cbls.test.core.computation

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.core.computation.{Store, Variable}
import oscar.cbls.core.computation.integer.{IntConstant, IntVariable}
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.lib.invariant.minmax.{Max, Min}
import oscar.cbls.lib.invariant.numeric.Sum2
import oscar.cbls.test.invBench.{InvTestBenchWithConstGen, TestBenchSut}

import scala.util.Random

class ConstantsWithInvariantsTestSuite extends AnyFunSuite {

  val random: Random     = Random
  private val seed: Long = random.nextLong()
  random.setSeed(seed)

  private def rn() = random.between(-1000, 1000)

  test(s"Sum2 works between a variable and a constant. Seed: $seed") {
    val store  = new Store(debugLevel = 3)
    val (a, b) = (rn(), rn())
    val aVar   = IntVariable(store, a)
    val bConst = new IntConstant(store, b)

    val res = IntVariable(store, 0)
    val _   = Sum2(store, aVar, bConst, res, Some("sum"))
    store.close()

    res.value() should be(a + b)
  }

  test(s"Min invariant works with a mix of variables and constants. Seed: $seed") {
    val store = new Store(debugLevel = 3)
    val arr   = Array.tabulate(1000) { _ => rn() }
    val vars: Array[IntVariable] = Array.tabulate(arr.length) { i =>
      if (random.nextBoolean()) {
        IntVariable(store, arr(i))
      } else {
        new IntConstant(store, arr(i))
      }
    }
    val filterSet            = vars.indices.toSet
    val indices: SetVariable = SetVariable(store, filterSet)
    val out                  = IntVariable(store, 0)
    val _                    = Min(store, vars, indices, out)
    store.close()

    out.value() should be(arr.min)

    for (_ <- 0 until 1000) {
      val i = random.shuffle(vars.indices.toList).head
      if (!vars(i).isConstant) {
        val newN = rn() - 1000
        vars(i) := newN
        arr(i) = newN

        out.value() should be(arr.min)
      }
    }
  }

  test("MinSet works with a mix of variable and constant") {
    class MinTestBench
        extends InvTestBenchWithConstGen[Array[Long]]("Min test with constant and variables") {
      val nbValues = 100

      override def createTestBenchSut(model: Store, inputData: Array[Long]): TestBenchSut = {
        val inputArray = Array.tabulate(nbValues)(i =>
          if (i < nbValues / 2) IntConstant(model, inputData(i)) else IntVariable(model, 0)
        )
        val listened = SetVariable(model, Set.empty[Int])
        listened.setDomain(0, nbValues - 1)
        val output = IntVariable(model, 0)
        val max    = Max(model, inputArray, listened, output)
        val input: Array[Variable] =
          (listened :: inputArray.filterNot(_.isInstanceOf[IntConstant]).toList).toArray
        TestBenchSut(max, input, Array(output))

      }

      override def genConst(): Gen[Array[Long]] = {
        Gen.sequence[Array[Long], Long](Array.fill(nbValues / 2)(Arbitrary.arbitrary[Long]))
      }
    }

    val bench = new MinTestBench
    bench.test()
  }

}
