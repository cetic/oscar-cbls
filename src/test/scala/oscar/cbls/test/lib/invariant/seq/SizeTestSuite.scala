package oscar.cbls.test.lib.invariant.seq

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.seq.SeqVariable
import oscar.cbls.lib.invariant.seq.Size
import oscar.cbls.modeling.Model
import oscar.cbls.util.invBench.{InvTestBench, TestBenchSut}

class SizeTestSuite extends AnyFunSuite {

  test("Size keeps seq size using TestBench") {

    val sizeTestData = (model: Model) => {
      val input  = model.seqVar(Nil, Int.MinValue, Int.MaxValue, "Seq")
      val output = model.intVar(0, 0, Int.MaxValue, "Size of Seq")
      val inv    = Size(model.store, input, output)
      TestBenchSut(inv, Array(input), Array(output))
    }

    val testBench = InvTestBench(sizeTestData, "Size of Seq test bench")
    testBench.test()
  }

  test("Size is equal to 0 when initiated with an empty SeqVariable") {
    val model: Store = new Store(debugLevel = 3)
    val input        = SeqVariable(model, List.empty, "Seq")
    val output       = IntVariable(model, 0, name = Some("Size of Seq"))
    val _            = Size(model, input, output)
    model.close()
    output.value() should be(0)
  }

  test("Size is correct when initiated with a non empty SeqVariable") {
    val model: Store = new Store(debugLevel = 3)
    val input        = SeqVariable(model, List(0, 1, 2, 3, 4, 5), "Seq")
    val output       = IntVariable(model, 0, name = Some("Size of Seq"))
    val _            = Size(model, input, output)
    model.close()
    output.value() should be(6)
  }

}
