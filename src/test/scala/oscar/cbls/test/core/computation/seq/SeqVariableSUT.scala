package oscar.cbls.test.core.computation.seq

import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.computation.seq.SeqVariable

/** Defines the System Under Test (SUT)
  *
  * In the SeqVariable case, the SUT contains a [[oscar.cbls.core.computation.seq.SeqVariable]] and
  * a classical list that acts as witness. Both collection are modified the same way and methods
  * allow to compare the collections to ensure that the two collections are identical after the
  * movements
  *
  * @param seq
  *   The SeqVariable we are modifying
  * @param copy
  *   The copy of seq maintained with a [[oscar.cbls.core.computation.seq.SeqIdentityInvariant]]
  * @param l
  *   The witness list to compare with
  */
case class SeqVariableSUT(seq: SeqVariable, copy: SeqVariable, var refListStack: List[List[Int]] = List(List.empty)) {

  private def lastRefList: List[Int]              = refListStack.head
  private var lastCheckpoint: Option[IntSequence] = None

  def insert(elem: Int, afterPos: Int): Unit = {
    val insertAfterExplorer = seq.newValue.explorerAtPosition(afterPos)
    seq.insertAfterPosition(elem, insertAfterExplorer.get)

    val newRefList: List[Int] =
      lastRefList
        .slice(0, afterPos + 1) ::: List(elem) ::: lastRefList.slice(afterPos + 1, lastRefList.size)
    refListStack = List(newRefList) ::: refListStack.tail
  }

  def move(fromPos: Int, toPos: Int, afterPos: Int, flip: Boolean): Unit = {
    val fromExplorer: IntSequenceExplorer  = seq.newValue.explorerAtPosition(fromPos).get
    val toExplorer: IntSequenceExplorer    = seq.newValue.explorerAtPosition(toPos).get
    val afterExplorer: IntSequenceExplorer = seq.newValue.explorerAtPosition(afterPos).get
    seq.move(fromExplorer, toExplorer, afterExplorer, flip, None)

    val movedSubList: List[Int] =
      if (flip) lastRefList.slice(fromPos, toPos + 1).reverse
      else lastRefList.slice(fromPos, toPos + 1)
    val refListWithoutSubList: List[Int] = lastRefList.take(fromPos) ::: lastRefList.drop(toPos + 1)
    val newRefList: List[Int] =
      if (afterPos < fromPos)
        refListWithoutSubList.take(afterPos + 1) ::: movedSubList :::
          refListWithoutSubList.drop(afterPos + 1)
      else
        refListWithoutSubList.take(afterPos + 1 - movedSubList.size) ::: movedSubList :::
          refListWithoutSubList.drop(afterPos + 1 - movedSubList.size)
    refListStack = List(newRefList) ::: refListStack.tail
  }

  def flip(fromPos: Int, toPos: Int): Unit = {
    val fromExplorer: IntSequenceExplorer = seq.newValue.explorerAtPosition(fromPos).get
    val toExplorer: IntSequenceExplorer   = seq.newValue.explorerAtPosition(toPos).get
    seq.flip(fromExplorer, toExplorer, None)

    val movedSubList: List[Int] = lastRefList.slice(fromPos, toPos + 1).reverse
    val newRefList: List[Int] =
      lastRefList.take(fromPos) ::: movedSubList ::: lastRefList.drop(toPos + 1)
    refListStack = List(newRefList) ::: refListStack.tail
  }

  def swap(
    fromPos_1: Int,
    toPos_1: Int,
    flip_1: Boolean,
    fromPos_2: Int,
    toPos_2: Int,
    flip_2: Boolean
  ): Unit = {
    val from1Explorer: IntSequenceExplorer = seq.newValue.explorerAtPosition(fromPos_1).get
    val to1Explorer: IntSequenceExplorer   = seq.newValue.explorerAtPosition(toPos_1).get
    val from2Explorer: IntSequenceExplorer = seq.newValue.explorerAtPosition(fromPos_2).get
    val to2Explorer: IntSequenceExplorer   = seq.newValue.explorerAtPosition(toPos_2).get

    seq.swapSegments(from1Explorer, to1Explorer, flip_1, from2Explorer, to2Explorer, flip_2, None)
    val movedSubList1: List[Int] =
      if (flip_1) lastRefList.slice(fromPos_1, toPos_1 + 1).reverse
      else lastRefList.slice(fromPos_1, toPos_1 + 1)
    val movedSubList2: List[Int] =
      if (flip_2) lastRefList.slice(fromPos_2, toPos_2 + 1).reverse
      else lastRefList.slice(fromPos_2, toPos_2 + 1)
    val newRefList: List[Int] =
      lastRefList.take(fromPos_1) ::: movedSubList1 :::
        lastRefList.slice(toPos_1 + 1, fromPos_2) ::: movedSubList2 ::: lastRefList.drop(
          toPos_2 + 1
        )
    refListStack = List(newRefList) ::: refListStack.tail
  }

  def remove(pos: Int): Unit = {
    val removeExplorer = seq.newValue.explorerAtPosition(pos).get
    seq.remove(removeExplorer)
    val newRefList: List[Int] = lastRefList.take(pos) ::: lastRefList.drop(pos + 1)
    refListStack = List(newRefList) ::: refListStack.tail
  }

  def assign(newValues: List[Int]): Unit = {
    if (newValues.isEmpty) seq := IntSequence.empty() else seq := IntSequence(newValues)
    refListStack = List(newValues)
  }

  def defineCheckpoint(): Unit = {
    lastCheckpoint = Some(seq.defineCurrentValueAsCheckpoint())
    refListStack = List(refListStack.head) ::: refListStack
  }

  def rollBackToTopCheckpoint(): Unit = {
    seq.rollbackToTopCheckpoint(Some(lastCheckpoint.get))
    refListStack = List(refListStack.tail.head) ::: refListStack.tail
  }

  def releaseTopCheckPoint(): Unit = {
    lastCheckpoint = Some(seq.releaseTopCheckpoint())
    refListStack = refListStack.tail
  }

  def performPropagation(): Unit = {
    require(compare(withCopy = true), s"Should be ${seq.value} got ${copy.value}")
  }

  def compare(withCopy: Boolean = false): Boolean =
    if (withCopy) {
      copy.value.toList == refListStack.head &&
      seq.value.toList == refListStack.head
    } else {
      seq.newValue.toList == refListStack.head
    }

}

// -1 == no checkpoint defined
class SeqVariableState(
  val checkpointLevel: Int = -1,
  val previousState: Option[SeqVariableState] = None
) {
  var length: Int = 0
  // List empty == no checkpoint defined
  // list(0) == checkpoint defined but no operation since it
  var operationsSinceLastCheckpoint: Int = 0

  override def toString: String =
    s"Checkpoint level : $checkpointLevel - length : $length - " +
      s"operations since checkpoint : $operationsSinceLastCheckpoint " +
      s"${if (previousState.isDefined) s"\n PreviousState : ${previousState.get}"
        else ""}"
}
