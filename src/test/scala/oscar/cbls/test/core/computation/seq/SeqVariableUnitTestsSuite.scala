package oscar.cbls.test.core.computation.seq

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.algo.sequence.IntSequence
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.seq.SeqVariable

import scala.util.Random

class SeqVariableUnitTestsSuite extends AnyFunSuite{

  test("A SeqAssign can not be applied when a CheckPoint has been defined"){
    val model: Store = new Store(3)
    val seqVar = new SeqVariable(model, List(0,1,2,3,4,5))
    model.close()

    seqVar.defineCurrentValueAsCheckpoint()
    val exception = intercept[IllegalArgumentException](
      seqVar.setValue(IntSequence(List(5,4,3,2,1,0)))
    )
    assert(
      exception.getMessage.contains(
        "Sequences cannot be assigned when a checkpoint has been defined"
      )
    )
  }

  test("Applying multiples consecutive assign has the expected behavior"){
    val model: Store = new Store(3)
    val seqVar = new SeqVariable(model, List(0,1,2,3,4,5))
    val clone = seqVar.createClone()
    model.close()

    val lastValue = List(5,4,3,2,1,0)
    for(_ <- 0 until 10)
      seqVar.setValue(IntSequence(List.fill(10)(Random.nextInt)))

    seqVar.setValue(IntSequence(lastValue))
    clone.value.toList == lastValue should be(true)
  }

  test("Assigning a new value after releasing a checkpoint works as expected"){
    val model: Store = new Store(3)
    val seqVar = new SeqVariable(model, List(0,1,2,3,4,5))
    val clone = seqVar.createClone()
    model.close()

    val lastValue = List(5,4,3,2,1,0)
    val checkpoint = seqVar.defineCurrentValueAsCheckpoint()
    seqVar.insertAfterPosition(78,checkpoint.explorerAtPosition(1).get)
    seqVar.rollbackToTopCheckpoint()
    seqVar.releaseTopCheckpoint()

    seqVar.setValue(IntSequence(lastValue))
    clone.value.toList == lastValue should be(true)
  }

  test("Assigning a new value after a propagation works as expected"){
    val model: Store = new Store(3)
    val seqVar = new SeqVariable(model, List(0,1,2,3,4,5))
    val clone = seqVar.createClone()
    model.close()

    val lastValue = List(5,4,3,2,1,0)
    seqVar.insertAfterPosition(78,seqVar.value.explorerAtPosition(1).get)
    clone.value.toList == List(0,1,78,2,3,4,5)

    seqVar.setValue(IntSequence(lastValue))
    clone.value.toList == lastValue should be(true)
  }

  test("Cannot define a new checkpoint if no updates has been done since last one."){
    val model: Store = new Store(3)
    val seqVar = new SeqVariable(model, List(0,1,2,3,4,5))
    model.close()

    seqVar.defineCurrentValueAsCheckpoint()
    val exception = intercept[IllegalArgumentException](
      seqVar.defineCurrentValueAsCheckpoint()
    )
    assert(
      exception.getMessage.contains(
        "Sequences cannot be assigned when a checkpoint has been defined"
      )
    )
  }

}
