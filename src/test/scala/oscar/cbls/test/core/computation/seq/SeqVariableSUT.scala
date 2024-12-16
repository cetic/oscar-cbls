package oscar.cbls.test.core.computation.seq

import oscar.cbls.algo.sequence.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.computation.seq.SeqVariable

/** Defines the System Under Test (SUT)
  *
  * In the SeqVariable case, the SUT contains a [[oscar.cbls.core.computation.seq.SeqVariable]] and
  * a copy of it. The copy is used to ensure the proper behavior of
  * [[oscar.cbls.core.computation.seq.SeqIdentityInvariant]]. Each modification methods return the
  * new resulting list. The copy is updated only when using the propagation method.
  *
  * @param seq
  *   The SeqVariable we are modifying.
  * @param copy
  *   The copy of seq maintained with a [[oscar.cbls.core.computation.seq.SeqIdentityInvariant]].
  */
case class SeqVariableSUT(seq: SeqVariable, copy: SeqVariable) {

  def insert(elem: Int, afterPos: Int): List[Int] = {
    val insertAfterExplorer = seq.pendingValue.explorerAtPosition(afterPos)
    seq.insertAfterPosition(elem, insertAfterExplorer.get)
    seq.value().toList
  }

  def move(fromPos: Int, toPos: Int, afterPos: Int, flip: Boolean): List[Int] = {
    val fromExplorer: IntSequenceExplorer  = seq.pendingValue.explorerAtPosition(fromPos).get
    val toExplorer: IntSequenceExplorer    = seq.pendingValue.explorerAtPosition(toPos).get
    val afterExplorer: IntSequenceExplorer = seq.pendingValue.explorerAtPosition(afterPos).get
    seq.move(fromExplorer, toExplorer, afterExplorer, flip, None)
    seq.value().toList
  }

  def flip(fromPos: Int, toPos: Int): List[Int] = {
    val fromExplorer: IntSequenceExplorer = seq.pendingValue.explorerAtPosition(fromPos).get
    val toExplorer: IntSequenceExplorer   = seq.pendingValue.explorerAtPosition(toPos).get
    seq.flip(fromExplorer, toExplorer, None)
    seq.value().toList
  }

  def swap(
    fromPos_1: Int,
    toPos_1: Int,
    flip_1: Boolean,
    fromPos_2: Int,
    toPos_2: Int,
    flip_2: Boolean
  ): List[Int] = {
    val from1Explorer: IntSequenceExplorer = seq.pendingValue.explorerAtPosition(fromPos_1).get
    val to1Explorer: IntSequenceExplorer   = seq.pendingValue.explorerAtPosition(toPos_1).get
    val from2Explorer: IntSequenceExplorer = seq.pendingValue.explorerAtPosition(fromPos_2).get
    val to2Explorer: IntSequenceExplorer   = seq.pendingValue.explorerAtPosition(toPos_2).get

    seq.swapSegments(from1Explorer, to1Explorer, flip_1, from2Explorer, to2Explorer, flip_2, None)
    seq.value().toList
  }

  def remove(pos: Int): List[Int] = {
    val removeExplorer = seq.pendingValue.explorerAtPosition(pos).get
    seq.remove(removeExplorer)
    seq.value().toList
  }

  def assign(newValues: List[Int]): List[Int] = {
    if (newValues.isEmpty) seq := IntSequence.empty() else seq := IntSequence(newValues)
    seq.value().toList
  }

  def defineCheckpoint(): List[Int] = {
    seq.defineCurrentValueAsCheckpoint()
    seq.value().toList
  }

  def rollBackToTopCheckpoint(): List[Int] = {
    seq.rollbackToTopCheckpoint()
    seq.value().toList
  }

  def releaseTopCheckPoint(): List[Int] = {
    seq.releaseTopCheckpoint()
    seq.value().toList
  }

  def performPropagation(): List[Int] = {
    require(compareCopy(), s"Should be ${seq.value()} got ${copy.value()}")
    seq.value().toList
  }

  def compareCopy(): Boolean = {
    copy.value().equals(seq.value())
  }

}

/** Defines the State.
  *
  * Basically it's a small structure that mimic's the
  * [[oscar.cbls.core.computation.seq.SeqVariable]].
  *
  * @param checkpointLevel
  *   The assumed checkpoint level of the SeqVariable.
  * @param refList
  *   The assumed current value of the SeqVariable.
  * @param operationsSinceLastCheckpoint
  *   The assumed number of operations since last checkpoint of the SeqVariable.
  * @param previousState
  *   The assumed previous checkpoint of the SeqVariable.
  */
class SeqVariableState(
  val checkpointLevel: Int = -1,
  val refList: List[Int] = List.empty,
  val operationsSinceLastCheckpoint: Int = 0,
  val previousState: Option[SeqVariableState] = None
) {
  require(
    checkpointLevel > -1 || previousState.isEmpty,
    "Can't have a previous state with no defined checkpoint"
  )

  def length: Int = refList.size

  override def toString: String =
    s"Checkpoint level : $checkpointLevel - refList (${refList.length}) : $refList - " +
      s"operations since checkpoint : $operationsSinceLastCheckpoint " +
      s"${if (previousState.isDefined) s"\n PreviousState : ${previousState.get}"
        else ""}"
}
