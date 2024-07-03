package oscar.cbls.test.core.computation.seq

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.algo.sequence.IntSequence
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.seq.SeqVariable

import scala.List
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
        "Can't create a new checkpoint if no update were done in the previous one."
      )
    )
  }

  test("IdentitySeq : Sending a list of updates without checkpoint works as expected"){
    val model: Store = new Store(3)
    val seqVar = new SeqVariable(model, List(0,1,2,3,4,5))
    val clone = seqVar.createClone()
    model.close()

    clone.value.toList == List(0,1,2,3,4,5) should be(true)

    seqVar.insertAfterPosition(6,seqVar.value.explorerAtPosition(5).get)
    seqVar.move(seqVar.value.explorerAtPosition(1).get,seqVar.value.explorerAtPosition(2).get,seqVar.value.explorerAtPosition(-1).get,false)
    seqVar.remove(seqVar.value.explorerAtPosition(5).get)
    clone.value.toList should be(List(1,2,0,3,4,6))
  }

  test("IdentitySeq : Sending a list of updates with checkpoints works as expected"){
    val model: Store = new Store(3)
    val seqVar = new SeqVariable(model, List(0,1,2,3,4,5))
    val clone = seqVar.createClone()
    model.close()

    clone.value.toList == List(0,1,2,3,4,5) should be(true)

    seqVar.insertAfterPosition(6,seqVar.value.explorerAtPosition(5).get)
    seqVar.move(seqVar.value.explorerAtPosition(1).get,seqVar.value.explorerAtPosition(2).get,seqVar.value.explorerAtPosition(-1).get,false)
    seqVar.remove(seqVar.value.explorerAtPosition(5).get)
    seqVar.defineCurrentValueAsCheckpoint()
    seqVar.insertAfterPosition(7,seqVar.value.explorerAtPosition(5).get)
    seqVar.move(seqVar.value.explorerAtPosition(1).get,seqVar.value.explorerAtPosition(2).get,seqVar.value.explorerAtPosition(-1).get,false)
    seqVar.remove(seqVar.value.explorerAtPosition(5).get)

    clone.value.toList should be(List(2,0,1,3,4,7))
  }

  test("IdentitySeq : Sending a list of updates with checkpoints and rollback works as expected"){
    val model: Store = new Store(3)
    val seqVar = new SeqVariable(model, List(0,1,2,3,4,5))
    val clone = seqVar.createClone()
    model.close()

    clone.value.toList == List(0,1,2,3,4,5) should be(true)

    seqVar.defineCurrentValueAsCheckpoint()
    seqVar.insertAfterPosition(6,seqVar.value.explorerAtPosition(5).get)
    seqVar.move(seqVar.value.explorerAtPosition(1).get,seqVar.value.explorerAtPosition(2).get,seqVar.value.explorerAtPosition(-1).get,false)
    seqVar.remove(seqVar.value.explorerAtPosition(5).get)
    seqVar.rollbackToTopCheckpoint()
    seqVar.insertAfterPosition(7,seqVar.value.explorerAtPosition(5).get)
    seqVar.move(seqVar.value.explorerAtPosition(1).get,seqVar.value.explorerAtPosition(2).get,seqVar.value.explorerAtPosition(-1).get,false)
    seqVar.remove(seqVar.value.explorerAtPosition(5).get)


    clone.value.toList should be(List(1,2,0,3,4,7))
  }

  test("IdentitySeq : Sending a list of updates with checkpoints and rollback works as expected. (propagation before rollback)"){
    val model: Store = new Store(3)
    val seqVar = new SeqVariable(model, List(0,1,2,3,4,5))
    val clone = seqVar.createClone()
    model.close()

    clone.value.toList == List(0,1,2,3,4,5) should be(true)

    seqVar.defineCurrentValueAsCheckpoint()
    seqVar.insertAfterPosition(6,seqVar.value.explorerAtPosition(5).get)
    seqVar.move(seqVar.value.explorerAtPosition(1).get,seqVar.value.explorerAtPosition(2).get,seqVar.value.explorerAtPosition(-1).get,false)
    seqVar.remove(seqVar.value.explorerAtPosition(5).get)
    clone.value.toList should be(List(1,2,0,3,4,6))
    seqVar.rollbackToTopCheckpoint()
    seqVar.insertAfterPosition(7,seqVar.value.explorerAtPosition(5).get)
    seqVar.move(seqVar.value.explorerAtPosition(1).get,seqVar.value.explorerAtPosition(2).get,seqVar.value.explorerAtPosition(-1).get,false)
    seqVar.remove(seqVar.value.explorerAtPosition(5).get)


    clone.value.toList should be(List(1,2,0,3,4,7))
  }

  test("IdentitySeq : Define + Updates + propagate + rollback + release + assign works as expected. (propagation before rollback)"){
    val model: Store = new Store(3)
    val seqVar = new SeqVariable(model, List(0,1,2,3,4,5))
    val clone = seqVar.createClone()
    model.close()

    clone.value.toList == List(0,1,2,3,4,5) should be(true)

    seqVar.defineCurrentValueAsCheckpoint()
    seqVar.insertAfterPosition(6,seqVar.value.explorerAtPosition(5).get)
    seqVar.move(seqVar.value.explorerAtPosition(1).get,seqVar.value.explorerAtPosition(2).get,seqVar.value.explorerAtPosition(-1).get,false)
    seqVar.remove(seqVar.value.explorerAtPosition(5).get)
    clone.value.toList should be(List(1,2,0,3,4,6))
    seqVar.rollbackToTopCheckpoint()
    seqVar.releaseTopCheckpoint()
    seqVar := IntSequence(List(10,9,8,7,6,5,4,3,2,1))


    clone.value.toList should be(List(10,9,8,7,6,5,4,3,2,1))
  }

  test("IdentitySeq : Define + Updates + rollback + release + propagate + assign works as expected. (propagation before rollback)"){
    val model: Store = new Store(3)
    val seqVar = new SeqVariable(model, List(0,1,2,3,4,5))
    val clone = seqVar.createClone()
    model.close()

    clone.value.toList == List(0,1,2,3,4,5) should be(true)

    seqVar.defineCurrentValueAsCheckpoint()
    seqVar.insertAfterPosition(6,seqVar.value.explorerAtPosition(5).get)
    seqVar.move(seqVar.value.explorerAtPosition(1).get,seqVar.value.explorerAtPosition(2).get,seqVar.value.explorerAtPosition(-1).get,false)
    seqVar.remove(seqVar.value.explorerAtPosition(5).get)
    seqVar.rollbackToTopCheckpoint()
    seqVar.releaseTopCheckpoint()
    clone.value.toList should be(List(0,1,2,3,4,5))
    seqVar := IntSequence(List(10,9,8,7,6,5,4,3,2,1))


    clone.value.toList should be(List(10,9,8,7,6,5,4,3,2,1))
  }

  test("Insert + propagate + rollback a move on empty sequence works as expected"){
    val model: Store = new Store(3)
    val seqVar = new SeqVariable(model, List())
    val clone = seqVar.createClone()
    model.close()

    seqVar.defineCurrentValueAsCheckpoint()
    seqVar.insertAfterPosition(1,seqVar.newValue.explorerAtPosition(-1).get)
    seqVar.insertAfterPosition(2,seqVar.newValue.explorerAtPosition(-1).get)
    clone.value.toList should be(List(2,1))
    seqVar.rollbackToTopCheckpoint()
    clone.value.toList should be(List.empty)
  }


  test("IdentitySeq : Special case"){
    val model: Store = new Store(3)
    val seqVar = new SeqVariable(model, List(0,1,2,3,4,5,6,7,8,9))
    val clone = seqVar.createClone()
    model.close()

    clone.value.toList == List(0,1,2,3,4,5,6,7,8,9) should be(true)

    seqVar.defineCurrentValueAsCheckpoint()
    seqVar.move(seqVar.value.explorerAtPosition(4).get,seqVar.value.explorerAtPosition(4).get,seqVar.value.explorerAtPosition(8).get,false)
    seqVar.move(seqVar.value.explorerAtPosition(9).get,seqVar.value.explorerAtPosition(9).get,seqVar.value.explorerAtPosition(3).get,false)
    clone.value.toList should be(List(0,1,2,3,9,5,6,7,8,4))
    seqVar.rollbackToTopCheckpoint()
    clone.value.toList should be(List(0,1,2,3,4,5,6,7,8,9))
  }

}
