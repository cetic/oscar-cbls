package oscar.cbls.test.core.computation.seq

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.algo.sequence.IntSequence
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.seq.SeqVariable

import scala.util.Random

class SeqVariableUnitTestsSuite extends AnyFunSuite {

  private val random: Random = Random
  private val seed: Long     = random.nextLong()
  random.setSeed(seed)

  private def generateSeq(
    size: Int = 0,
    myInitList: Option[List[Int]] = None
  ): (SeqVariable, SeqVariable) = {
    val model: Store = new Store(3)
    val seq: SeqVariable =
      new SeqVariable(model, myInitList.getOrElse(List.fill(size)(random.nextInt(100))), "Test Seq")
    val copy: SeqVariable = seq.createClone()
    model.close()
    (seq, copy)
  }

  test(s"The 'one checkpoint' usage of a TSP works as expected") {
    val (route, copyForRouteLength) = generateSeq()

    route.defineCurrentValueAsCheckpoint()
    route.insertAfterPosition(5, route.value.explorerAtPosition(-1).get)
    copyForRouteLength.value.toList should be(List(5))
    route.rollbackToTopCheckpoint()
    route.insertAfterPosition(8, route.value.explorerAtPosition(-1).get)
    copyForRouteLength.value.toList should be(List(8))
    route.rollbackToTopCheckpoint()
    route.releaseTopCheckpoint()
    route.insertAfterPosition(8, route.value.explorerAtPosition(-1).get)
    copyForRouteLength.value.toList should be(List(8))

    val exception =
      intercept[IllegalArgumentException](route.rollbackToTopCheckpoint())
    assert(
      exception.getMessage
        .contains("Can not rollback to top checkpoint since no checkpoint has been defined")
    )
  }

  test(s"The 'two checkpoint' usage of a TSP works as expected") {
    val (route, copyForRouteLength) = generateSeq()

    // Testing inserting 5 and then 8 (after and before 5)
    route.defineCurrentValueAsCheckpoint()
    route.insertAfterPosition(5, route.value.explorerAtPosition(-1).get)
    route.defineCurrentValueAsCheckpoint()
    route.insertAfterPosition(8, route.value.explorerAtPosition(0).get)
    copyForRouteLength.value.toList should be(List(5, 8))
    route.rollbackToTopCheckpoint()
    route.insertAfterPosition(8, route.value.explorerAtPosition(-1).get)
    copyForRouteLength.value.toList should be(List(8, 5))
    route.rollbackToTopCheckpoint()
    route.releaseTopCheckpoint()
    route.rollbackToTopCheckpoint()

    // Testing and validating insertion of 4 and then 7 (after and before 4)
    route.insertAfterPosition(4, route.value.explorerAtPosition(-1).get)
    route.defineCurrentValueAsCheckpoint()
    route.insertAfterPosition(7, route.value.explorerAtPosition(0).get)
    copyForRouteLength.value.toList should be(List(4, 7))
    route.rollbackToTopCheckpoint()
    route.insertAfterPosition(7, route.value.explorerAtPosition(-1).get)
    copyForRouteLength.value.toList should be(List(7, 4))
    route.rollbackToTopCheckpoint()
    route.releaseTopCheckpoint()
    route.rollbackToTopCheckpoint()
    route.releaseTopCheckpoint()
    route.insertAfterPosition(4, route.value.explorerAtPosition(-1).get)
    route.insertAfterPosition(7, route.value.explorerAtPosition(-1).get)
    copyForRouteLength.value.toList should be(List(7, 4))

    var exception =
      intercept[IllegalArgumentException](route.rollbackToTopCheckpoint())
    assert(
      exception.getMessage
        .contains("Can not rollback to top checkpoint since no checkpoint has been defined")
    )

    exception = intercept[IllegalArgumentException](copyForRouteLength.rollbackToTopCheckpoint())
    assert(
      exception.getMessage
        .contains("Can not rollback to top checkpoint since no checkpoint has been defined")
    )
  }

  test("A SeqAssign can not be applied when a CheckPoint has been defined") {
    val model: Store = new Store(3)
    val seqVar       = new SeqVariable(model, List(0, 1, 2, 3, 4, 5))
    model.close()

    seqVar.defineCurrentValueAsCheckpoint()
    val exception =
      intercept[IllegalArgumentException](seqVar.setValue(IntSequence(List(5, 4, 3, 2, 1, 0))))
    assert(
      exception.getMessage
        .contains("Sequences cannot be assigned when a checkpoint has been defined")
    )
  }

  test("Applying multiples consecutive assign has the expected behavior") {
    val (seqVar, clone) = generateSeq(myInitList = Some(List(0, 1, 2, 3, 4, 5)))

    val lastValue = List(5, 4, 3, 2, 1, 0)
    for (_ <- 0 until 10)
      seqVar.setValue(IntSequence(List.fill(10)(Random.nextInt())))

    seqVar.setValue(IntSequence(lastValue))
    clone.value.toList == lastValue should be(true)
  }

  test("Assigning a new value after releasing a checkpoint works as expected") {
    val (seqVar, clone) = generateSeq(myInitList = Some(List(0, 1, 2, 3, 4, 5)))

    val lastValue  = List(5, 4, 3, 2, 1, 0)
    val checkpoint = seqVar.defineCurrentValueAsCheckpoint()
    seqVar.insertAfterPosition(78, checkpoint.explorerAtPosition(1).get)
    seqVar.rollbackToTopCheckpoint()
    seqVar.releaseTopCheckpoint()

    seqVar.setValue(IntSequence(lastValue))
    clone.value.toList == lastValue should be(true)
  }

  test("IdentitySeq : Sending a list of updates without checkpoint works as expected") {
    val (seqVar, clone) = generateSeq(myInitList = Some(List(0, 1, 2, 3, 4, 5)))
    clone.value.toList == List(0, 1, 2, 3, 4, 5) should be(true)

    seqVar.insertAfterPosition(6, seqVar.value.explorerAtPosition(5).get)
    seqVar.move(
      seqVar.value.explorerAtPosition(1).get,
      seqVar.value.explorerAtPosition(2).get,
      seqVar.value.explorerAtPosition(-1).get,
      flip = false
    )
    seqVar.remove(seqVar.value.explorerAtPosition(5).get)
    clone.value.toList should be(List(1, 2, 0, 3, 4, 6))
  }

  test("IdentitySeq : Sending a list of updates with checkpoints works as expected") {
    val (seqVar, clone) = generateSeq(myInitList = Some(List(0, 1, 2, 3, 4, 5)))

    clone.value.toList == List(0, 1, 2, 3, 4, 5) should be(true)

    seqVar.insertAfterPosition(6, seqVar.value.explorerAtPosition(5).get)
    seqVar.move(
      seqVar.value.explorerAtPosition(1).get,
      seqVar.value.explorerAtPosition(2).get,
      seqVar.value.explorerAtPosition(-1).get,
      flip = false
    )
    seqVar.remove(seqVar.value.explorerAtPosition(5).get)
    seqVar.defineCurrentValueAsCheckpoint()
    seqVar.insertAfterPosition(7, seqVar.value.explorerAtPosition(5).get)
    seqVar.move(
      seqVar.value.explorerAtPosition(1).get,
      seqVar.value.explorerAtPosition(2).get,
      seqVar.value.explorerAtPosition(-1).get,
      flip = false
    )
    seqVar.remove(seqVar.value.explorerAtPosition(5).get)

    clone.value.toList should be(List(2, 0, 1, 3, 4, 7))
  }

  test("IdentitySeq : Sending a list of updates with checkpoints and rollback works as expected") {
    val (seqVar, clone) = generateSeq(myInitList = Some(List(0, 1, 2, 3, 4, 5)))

    clone.value.toList == List(0, 1, 2, 3, 4, 5) should be(true)

    seqVar.defineCurrentValueAsCheckpoint()
    seqVar.insertAfterPosition(6, seqVar.value.explorerAtPosition(5).get)
    seqVar.move(
      seqVar.value.explorerAtPosition(1).get,
      seqVar.value.explorerAtPosition(2).get,
      seqVar.value.explorerAtPosition(-1).get,
      flip = false
    )
    seqVar.remove(seqVar.value.explorerAtPosition(5).get)
    seqVar.rollbackToTopCheckpoint()
    seqVar.insertAfterPosition(7, seqVar.value.explorerAtPosition(5).get)
    seqVar.move(
      seqVar.value.explorerAtPosition(1).get,
      seqVar.value.explorerAtPosition(2).get,
      seqVar.value.explorerAtPosition(-1).get,
      flip = false
    )
    seqVar.remove(seqVar.value.explorerAtPosition(5).get)

    clone.value.toList should be(List(1, 2, 0, 3, 4, 7))
  }

  test(
    "IdentitySeq : Sending a list of updates with checkpoints and rollback works as expected. (propagation before rollback)"
  ) {
    val (seqVar, clone) = generateSeq(myInitList = Some(List(0, 1, 2, 3, 4, 5)))

    clone.value.toList == List(0, 1, 2, 3, 4, 5) should be(true)

    seqVar.defineCurrentValueAsCheckpoint()
    seqVar.insertAfterPosition(6, seqVar.value.explorerAtPosition(5).get)
    seqVar.move(
      seqVar.value.explorerAtPosition(1).get,
      seqVar.value.explorerAtPosition(2).get,
      seqVar.value.explorerAtPosition(-1).get,
      flip = false
    )
    seqVar.remove(seqVar.value.explorerAtPosition(5).get)
    clone.value.toList should be(List(1, 2, 0, 3, 4, 6))
    seqVar.rollbackToTopCheckpoint()
    seqVar.insertAfterPosition(7, seqVar.value.explorerAtPosition(5).get)
    seqVar.move(
      seqVar.value.explorerAtPosition(1).get,
      seqVar.value.explorerAtPosition(2).get,
      seqVar.value.explorerAtPosition(-1).get,
      flip = false
    )
    seqVar.remove(seqVar.value.explorerAtPosition(5).get)

    clone.value.toList should be(List(1, 2, 0, 3, 4, 7))
  }

  test(
    "IdentitySeq : Define + Updates + propagate + rollback + release + propagate + assign works as expected. (propagation before rollback)"
  ) {
    val (seqVar, clone) = generateSeq(myInitList = Some(List(0, 1, 2, 3, 4, 5)))

    clone.value.toList == List(0, 1, 2, 3, 4, 5) should be(true)

    seqVar.defineCurrentValueAsCheckpoint()
    seqVar.insertAfterPosition(6, seqVar.value.explorerAtPosition(5).get)
    seqVar.move(
      seqVar.value.explorerAtPosition(1).get,
      seqVar.value.explorerAtPosition(2).get,
      seqVar.value.explorerAtPosition(-1).get,
      flip = false
    )
    seqVar.remove(seqVar.value.explorerAtPosition(5).get)
    clone.value.toList should be(List(1, 2, 0, 3, 4, 6))
    seqVar.rollbackToTopCheckpoint()
    seqVar.releaseTopCheckpoint()
    clone.value.toList should be(List(0, 1, 2, 3, 4, 5))
    seqVar := IntSequence(List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1))

    clone.value.toList should be(List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1))
  }

  test("Insert + propagate + rollback a move on empty sequence works as expected") {
    val (seqVar, clone) = generateSeq()

    seqVar.defineCurrentValueAsCheckpoint()
    seqVar.insertAfterPosition(1, seqVar.pendingValue.explorerAtPosition(-1).get)
    seqVar.insertAfterPosition(2, seqVar.pendingValue.explorerAtPosition(-1).get)
    clone.value.toList should be(List(2, 1))
    seqVar.rollbackToTopCheckpoint()
    clone.value.toList should be(List.empty)
  }

  // Moves leading to identical sequence should be discarded
  test("IdentitySeq : Applying moves leading to identical Sequence works as expected") {
    val initialList: List[Int] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    val (seqVar, clone)        = generateSeq(myInitList = Some(initialList))

    clone.value.toList should be(initialList)
    seqVar.defineCurrentValueAsCheckpoint()

    // Flipping in place
    seqVar.flip(seqVar.value.explorerAtPosition(2).get, seqVar.value.explorerAtPosition(2).get)
    clone.value.toList should be(initialList)

    // Moving in place
    seqVar.move(
      seqVar.value.explorerAtPosition(2).get,
      seqVar.value.explorerAtPosition(2).get,
      seqVar.value.explorerAtPosition(1).get,
      flip = false
    )
    clone.value.toList should be(initialList)

    // Moving in place 2
    seqVar.move(
      seqVar.value.explorerAtPosition(2).get,
      seqVar.value.explorerAtPosition(2).get,
      seqVar.value.explorerAtPosition(1).get,
      flip = true
    )
    clone.value.toList should be(initialList)

  }

  test("IdentitySeq : The SeqUpdate are propagated along with it's resulting IntSequence") {
    val (seqVar, clone) = generateSeq(myInitList = Some(List(0, 1, 2, 3, 4, 5)))

    (seqVar.value equals clone.value) should be(true)
    seqVar.insertAfterPosition(10, seqVar.value.explorerAtPosition(-1).get)
    (seqVar.value equals clone.value) should be(true)
    seqVar.move(
      seqVar.value.explorerAtPosition(0).get,
      seqVar.value.explorerAtPosition(1).get,
      seqVar.value.explorerAtPosition(2).get,
      flip = false
    )
    (seqVar.value equals clone.value) should be(true)
    seqVar.swapSegments(
      seqVar.value.explorerAtPosition(0).get,
      seqVar.value.explorerAtPosition(1).get,
      flipFirstSegment = false,
      seqVar.value.explorerAtPosition(4).get,
      seqVar.value.explorerAtPosition(5).get,
      flipSecondSegment = true
    )
    (seqVar.value equals clone.value) should be(true)

    seqVar.defineCurrentValueAsCheckpoint()
    (seqVar.value equals clone.value) should be(true)
  }

  test("Define and rollback on empty sequence works as expected") {
    val (seqVar, clone) = generateSeq()

    seqVar.defineCurrentValueAsCheckpoint()
    seqVar.rollbackToTopCheckpoint()
    clone.value.toList should be(List.empty)
  }

  test("HowToRollback instruction are actually correct") {
    val model: Store     = new Store(3)
    val seq: SeqVariable = new SeqVariable(model, List.fill(10)(random.nextInt(100)), "Test Seq")
    val myCopy           = new SeqVariable(model, List.empty, "my test copy")
    new TestSeqInvariant(model, seq, myCopy)
    model.close()

    val initValue = seq.value.toList

    (myCopy.value.toList equals seq.value.toList) should be(true)

    // First scenario, nothing is propagated before rollback ==> my copy receive nothing
    seq.defineCurrentValueAsCheckpoint()
    seq.insertAfterPosition(random.nextInt(100), seq.value.explorerAtPosition(2).get)
    seq.flip(seq.value.explorerAtPosition(2).get, seq.value.explorerAtPosition(7).get)
    seq.remove(seq.value.explorerAtPosition(2).get)
    seq.remove(seq.value.explorerAtPosition(2).get)
    seq.move(
      seq.value.explorerAtPosition(0).get,
      seq.value.explorerAtPosition(2).get,
      seq.value.explorerAtPosition(8).get,
      flip = true
    )
    seq.rollbackToTopCheckpoint()
    (myCopy.value.toList equals seq.value.toList) should be(true)

    // Second scenario, everything is propagated before rollback ==>
    // my copy receive a batch of 5 moves (move,insert,move,remove,remove)
    seq.insertAfterPosition(random.nextInt(100), seq.value.explorerAtPosition(2).get)
    seq.insertAfterPosition(random.nextInt(100), seq.value.explorerAtPosition(2).get)
    seq.flip(seq.value.explorerAtPosition(2).get, seq.value.explorerAtPosition(7).get)
    seq.remove(seq.value.explorerAtPosition(2).get)
    seq.move(
      seq.value.explorerAtPosition(0).get,
      seq.value.explorerAtPosition(2).get,
      seq.value.explorerAtPosition(8).get,
      flip = true
    )
    (myCopy.value.toList equals seq.value.toList) should be(true)
    seq.rollbackToTopCheckpoint()
    (myCopy.value.toList equals seq.value.toList) should be(true)

    // Third scenario, only a part is propagated before rollback ==>
    // my copy receive a batch of 5 moves (move,insert,move,remove,remove)
    seq.insertAfterPosition(random.nextInt(100), seq.value.explorerAtPosition(2).get)
    seq.insertAfterPosition(random.nextInt(100), seq.value.explorerAtPosition(2).get)
    seq.flip(seq.value.explorerAtPosition(2).get, seq.value.explorerAtPosition(7).get)
    (myCopy.value.toList equals seq.value.toList) should be(true)
    seq.remove(seq.value.explorerAtPosition(2).get)
    seq.move(
      seq.value.explorerAtPosition(0).get,
      seq.value.explorerAtPosition(2).get,
      seq.value.explorerAtPosition(8).get,
      flip = true
    )
    seq.rollbackToTopCheckpoint()
    (myCopy.value.toList equals seq.value.toList) should be(true)
    seq.releaseTopCheckpoint()
    (myCopy.value.toList equals seq.value.toList) should be(true)
    (myCopy.value.toList equals initValue) should be(true)
    (seq.value.toList equals initValue) should be(true)
  }

}
