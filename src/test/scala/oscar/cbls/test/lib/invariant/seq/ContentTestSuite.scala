package oscar.cbls.test.lib.invariant.seq

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.seq.SeqVariable
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.lib.invariant.seq.Content
import oscar.cbls.test.invBench.{InvTestBench, TestBenchSut}

import scala.collection.immutable.HashSet

class ContentTestSuite extends AnyFunSuite {

  test("Content keeps seq content using TestBench") {

    val contentTestData = (model: Store) => {
      val input  = SeqVariable(model, List.empty, "Seq")
      val output = SetVariable(model, HashSet.empty[Int], name = Some("Content_of_Seq"))
      val inv    = Content(model, input, output)
      TestBenchSut(inv, Array(input), Array(output))
    }

    val testBench = InvTestBench(contentTestData, "Content of Seq test bench")
    testBench.test()
  }

  test("Content is empty when initiated with an empty SeqVariable") {
    val model: Store = new Store(debugLevel = 3)
    val input        = SeqVariable(model, List.empty, "Seq")
    val output       = SetVariable(model, HashSet.empty[Int], name = Some("Content_of_Seq"))
    val _            = Content(model, input, output)
    model.close()
    output.value() should be(Set.empty[Int])
  }

  test("Content contents the content of SeqVariable when initiated with a non empty SeqVariable") {
    val model: Store = new Store(debugLevel = 3)
    val input        = SeqVariable(model, List(0, 1, 2, 3, 4, 5), "Seq")
    val output       = SetVariable(model, HashSet.empty[Int], name = Some("Content_of_Seq"))
    val _            = Content(model, input, output)
    model.close()
    output.value() should be(HashSet(0, 1, 2, 3, 4, 5))
  }

}
