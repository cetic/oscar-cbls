package oscar.cbls.test.core.computation.seq

import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.computation.seq.SeqVariable

import scala.util.Random

case class SeqOperations(random: Random, seqVariable: SeqVariable, cloneVariable: SeqVariable) {

  private val minValue: Int = seqVariable.domain.get._1.toInt
  private val maxValue: Int = seqVariable.domain.get._2.toInt

  private var lastCheckpoint: Option[IntSequence] = None

  def seqInsertOperation(refListStack: List[List[Int]]): List[List[Int]] = {
    val value: Int    = random.nextInt(maxValue)
    val position: Int = random.between(-1, refListStack.head.size)
    val insertAfterPositionExplorer: IntSequenceExplorer =
      seqVariable.newValue.explorerAtPosition(position).get
    seqVariable.insertAfterPosition(value, insertAfterPositionExplorer)
    val newRefList: List[Int] =
      refListStack.head.slice(0, position) ::: List(value) ::: refListStack.head.slice(
        position,
        refListStack.head.size
      )

    List(newRefList) ::: refListStack.tail
  }

  def seqMoveOperation(refListStack: List[List[Int]]): List[List[Int]] = {
    val lastRefList: List[Int] = refListStack.head
    val from: Int              = random.between(0, lastRefList.size)
    val to: Int                = random.between(from, lastRefList.size)
    val after: Int = random
      .shuffle(
        List(-1) ::: List.tabulate(from)(identity) ::: List.tabulate(lastRefList.size - to - 1)(x =>
          x + to
        )
      )
      .head
    val flip: Boolean                      = random.nextBoolean()

    println(s"Move: $from,$to,$after,$flip")
    val fromExplorer: IntSequenceExplorer  = seqVariable.newValue.explorerAtPosition(from).get
    val toExplorer: IntSequenceExplorer    = seqVariable.newValue.explorerAtPosition(to).get
    val afterExplorer: IntSequenceExplorer = seqVariable.newValue.explorerAtPosition(after).get

    seqVariable.move(fromExplorer, toExplorer, afterExplorer, flip, None)
    val movedSubList: List[Int] =
      if (flip) lastRefList.slice(from, to + 1).reverse else lastRefList.slice(from, to + 1)
    val refListWithoutSubList: List[Int] = lastRefList.take(from) ::: lastRefList.drop(to + 1)
    val newRefList: List[Int] = if (after < from) {
      refListWithoutSubList.take(after + 1) ::: movedSubList ::: refListWithoutSubList.drop(
        after + 1
      )
    } else {
      refListWithoutSubList.take(
        after + 1 - movedSubList.size
      ) ::: movedSubList ::: refListWithoutSubList.drop(after + 1 - movedSubList.size)
    }
    List(newRefList) ::: refListStack.tail
  }

  def seqFlipOperation(refListStack: List[List[Int]]): List[List[Int]] = {
    val lastRefList: List[Int] = refListStack.head
    val from: Int              = random.between(0, lastRefList.size)
    val to: Int                = random.between(from, lastRefList.size)
    println(s"Flip: $from,$to")

    val fromExplorer: IntSequenceExplorer = seqVariable.newValue.explorerAtPosition(from).get
    val toExplorer: IntSequenceExplorer   = seqVariable.newValue.explorerAtPosition(to).get

    seqVariable.flip(fromExplorer, toExplorer, None)
    val movedSubList: List[Int]          = lastRefList.slice(from, to + 1).reverse
    val newRefList: List[Int] =
      lastRefList.take(from) ::: movedSubList ::: lastRefList.drop(to + 1)
    List(newRefList) ::: refListStack.tail
  }

  def seqSwapSegmentsOperation(refListStack: List[List[Int]]): List[List[Int]] = {
    val lastRefList: List[Int] = refListStack.head
    val from1: Int             = random.between(0, lastRefList.size - 1)
    val to1: Int               = random.between(from1, lastRefList.size - 1)
    val flip1: Boolean         = random.nextBoolean()

    val from2: Int     = random.between(to1 + 1, lastRefList.size)
    val to2: Int       = random.between(from2, lastRefList.size)
    val flip2: Boolean = random.nextBoolean()

    println(s"Swap: $from1,$to1,$flip1,$from2,$to2,$flip2")

    val from1Explorer: IntSequenceExplorer = seqVariable.newValue.explorerAtPosition(from1).get
    val to1Explorer: IntSequenceExplorer   = seqVariable.newValue.explorerAtPosition(to1).get
    val from2Explorer: IntSequenceExplorer = seqVariable.newValue.explorerAtPosition(from2).get
    val to2Explorer: IntSequenceExplorer   = seqVariable.newValue.explorerAtPosition(to2).get

    seqVariable.swapSegments(
      from1Explorer,
      to1Explorer,
      flip1,
      from2Explorer,
      to2Explorer,
      flip2,
      None
    )
    val movedSubList1: List[Int] =
      if (flip1) lastRefList.slice(from1, to1 + 1).reverse else lastRefList.slice(from1, to1 + 1)
    val movedSubList2: List[Int] =
      if (flip2) lastRefList.slice(from2, to2 + 1).reverse else lastRefList.slice(from2, to2 + 1)
    val newRefList: List[Int] =
      lastRefList.take(from1) ::: movedSubList1 :::
        lastRefList.slice(to1 + 1, from2) ::: movedSubList2 ::: lastRefList.drop(to2 + 1)
    List(newRefList) ::: refListStack.tail
  }

  def seqRemoveOperation(refListStack: List[List[Int]]): List[List[Int]] = {
    val lastRefList: List[Int] = refListStack.head
    val removePos              = random.between(0, lastRefList.size)
    val removeExplorer         = seqVariable.newValue.explorerAtPosition(removePos).get
    seqVariable.remove(removeExplorer)
    val newRefList: List[Int] = lastRefList.take(removePos) ::: lastRefList.drop(removePos + 1)
    List(newRefList) ::: refListStack.tail
  }

  def seqDefineCheckPointOperation(refListStack: List[List[Int]]): List[List[Int]] = {
    lastCheckpoint = Some(seqVariable.defineCurrentValueAsCheckpoint())
    List(refListStack.head) ::: refListStack
  }

  def seqRollBackToCheckPointOperation(refListStack: List[List[Int]]): List[List[Int]] = {
    seqVariable.rollbackToTopCheckpoint(Some(lastCheckpoint.get))
    List(refListStack.tail.head) ::: refListStack.tail
  }

  def seqAssignOperation(refListStack: List[List[Int]]): List[List[Int]] = {
    val brandNewList = List.fill(random.nextInt(100))(random.nextInt(maxValue))
    seqVariable := IntSequence(brandNewList)
    List(brandNewList)
  }

  def seqPerformPropagation(refListStack: List[List[Int]]): List[List[Int]] = {
    require(cloneVariable.value == seqVariable.value, s"Should be ${seqVariable.value} got ${cloneVariable.value}")
    refListStack
  }

  def seqReleaseTopCheckpoint(refListStack: List[List[Int]]): List[List[Int]] = {
    lastCheckpoint = Some(seqVariable.releaseTopCheckpoint())
    refListStack.tail
  }

}
