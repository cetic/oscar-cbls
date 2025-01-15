package oscar.cbls.test.lib.invariant.seq

import org.scalatest.funsuite.AnyFunSuite
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.seq.{SeqIdentityInvariant, SeqVariable}
import oscar.cbls.test.invBench.{InvTestBench, TestBenchSut}

class IdentitySeqTestBench extends AnyFunSuite {

  test("IdentitySeq keeps identity using TestBench") {

    val testDataGen = (model: Store) => {
      val input  = SeqVariable(model, List.empty, "Original Seq")
      val output = SeqVariable(model, List.empty, "Copy")
      val inv    = SeqIdentityInvariant(model, input, output)
      TestBenchSut(inv, Array(input), Array(output))
    }

    val testBench = InvTestBench(testDataGen, "SeqIdentityVariable test bench")
    testBench.test()
  }

}
