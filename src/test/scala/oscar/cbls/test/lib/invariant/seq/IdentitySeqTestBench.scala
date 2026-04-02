package oscar.cbls.test.lib.invariant.seq

import org.scalatest.funsuite.AnyFunSuite
import oscar.cbls.core.computation.seq.SeqIdentityInvariant
import oscar.cbls.modeling.Model
import oscar.cbls.util.invBench.{InvTestBench, TestBenchSut}

class IdentitySeqTestBench extends AnyFunSuite {

  test("IdentitySeq keeps identity using TestBench") {

    val testDataGen = (model: Model) => {
      val input  = model.seqVar(Nil, Int.MinValue, Int.MaxValue, "Original Seq")
      val output = model.seqVar(Nil, Int.MinValue, Int.MaxValue, "Copy")
      val inv    = SeqIdentityInvariant(model.store, input, output)
      TestBenchSut(inv, Array(input), Array(output))
    }

    val testBench = InvTestBench(testDataGen, "SeqIdentityVariable test bench")
    testBench.test()
  }

}
