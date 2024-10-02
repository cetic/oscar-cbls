package oscar.cbls.test.invBench
import oscar.cbls.algo.sequence.IntSequence
import oscar.cbls.core.computation.Variable
import oscar.cbls.core.computation.seq.SeqVariable

/** Abstract class holding basic methods to modify a SeqVariable and update it's
  * [[SeqVariableState]]
  */
abstract class SeqVariableMove(varId: Int) extends VariableMove(varId) {

  override def mkMove(testVar: Variable): Unit = {
    testVar match {
      case seqVar: SeqVariable => mkSeqMove(seqVar)
      case _ => throw new Error("Seq Movement can only update variable of type Seq")
    }
  }

  /** Does the move (avoiding pattern matching for each concrete class) */
  protected def mkSeqMove(seqVar: SeqVariable): Unit

  override def updateState(state: VariableState): VariableState = {
    state match {
      case seqState: SeqVariableState => updateSeqState(seqState)
      case _ => throw new Error("Seq Movement can only update state of type SeqVariableState")
    }
  }

  /** Updates the State (avoiding pattern matching for each concrete class) */
  protected def updateSeqState(state: SeqVariableState): SeqVariableState

}

/** Move that inserts a value into the SeqVariable.
  *
  * @param varId
  *   The test id of the SeqVariable.
  * @param value
  *   The value to insert.
  * @param after
  *   The position after which the value must be inserted.
  */
case class SeqInsertUpdate(override val varId: Int, value: Int, after: Int)
    extends SeqVariableMove(varId) {

  override protected def mkSeqMove(seqVar: SeqVariable): Unit = {
    seqVar.insertAfterPosition(value, seqVar.value().explorerAtPosition(after).get)
  }

  override def updateSeqState(state: SeqVariableState): SeqVariableState =
    SeqVariableState(
      varId,
      state.currentState.pushOp(Some(state.currentState.seqSize + 1)),
      state.domain
    )

  override def toString: String = s"Input var $varId | Inserts $value after pos $after"
}

/** Move that moves a sub-sequence of values at another place in the SeqVariable.
  *
  * @param varId
  *   The test id of the SeqVariable.
  * @param from
  *   The start (included) of the sub-sequence.
  * @param to
  *   The end (included) of the sub-sequence.
  * @param after
  *   The position after which the sub-sequence must be moved.
  * @param flip
  *   Whether the sub-sequence must be flipped or not.
  */
case class SeqMoveUpdate(override val varId: Int, from: Int, to: Int, after: Int, flip: Boolean)
    extends SeqVariableMove(varId) {

  override protected def mkSeqMove(seqVar: SeqVariable): Unit = {
    seqVar.move(
      seqVar.value().explorerAtPosition(from).get,
      seqVar.value().explorerAtPosition(to).get,
      seqVar.value().explorerAtPosition(after).get,
      flip
    )
  }

  override def updateSeqState(state: SeqVariableState): SeqVariableState =
    SeqVariableState(varId, state.currentState.pushOp(), state.domain)

  override def toString: String =
    s"Input var $varId | Moves from $from to $to after $after ${if (flip) "flip" else "no flip"}"

}

/** Move that flips a sub-sequence of values.
  *
  * @param varId
  *   The test id of the SeqVariable.
  * @param from
  *   The start (included) of the sub-sequence.
  * @param to
  *   The end (included) of the sub-sequence.
  */
case class SeqFlipUpdate(override val varId: Int, from: Int, to: Int)
    extends SeqVariableMove(varId) {

  override protected def mkSeqMove(seqVar: SeqVariable): Unit = {
    seqVar.flip(
      seqVar.value().explorerAtPosition(from).get,
      seqVar.value().explorerAtPosition(to).get
    )
  }

  override def updateSeqState(state: SeqVariableState): SeqVariableState =
    SeqVariableState(varId, state.currentState.pushOp(), state.domain)

  override def toString: String = s"Input var $varId | Flips sub-sequence from $from to $to"

}

/** Move that swaps two sub-sequences of values within the SeqVariable.
  *
  * @param varId
  *   The test id of the SeqVariable.
  * @param from_1
  *   The start (included) of the first sub-sequence.
  * @param to_1
  *   The end (included) of the first sub-sequence.
  * @param flip_1
  *   Whether the first sub-sequence must be flipped or not.
  * @param from_2
  *   The start (included) of the second sub-sequence.
  * @param to_2
  *   The end (included) of the second sub-sequence.
  * @param flip_2
  *   Whether the second sub-sequence must be flipped or not.
  */
case class SeqSwapUpdate(
  override val varId: Int,
  from_1: Int,
  to_1: Int,
  flip_1: Boolean,
  from_2: Int,
  to_2: Int,
  flip_2: Boolean
) extends SeqVariableMove(varId) {

  override protected def mkSeqMove(seqVar: SeqVariable): Unit = {
    seqVar.swapSegments(
      seqVar.value().explorerAtPosition(from_1).get,
      seqVar.value().explorerAtPosition(to_1).get,
      flip_1,
      seqVar.value().explorerAtPosition(from_2).get,
      seqVar.value().explorerAtPosition(to_2).get,
      flip_2
    )
  }

  override def updateSeqState(state: SeqVariableState): SeqVariableState =
    SeqVariableState(varId, state.currentState.pushOp(), state.domain)

  override def toString: String =
    s"Input var $varId | Swaps from $from_1 to $to_1 ${if (flip_1) "flip"
      else "no flip"} and from $from_2 to $to_2 ${if (flip_2) "flip" else "no flip"}"

}

/** Move that removes a value from the SeqVariable.
  *
  * @param varId
  *   The test id of the SeqVariable.
  * @param position
  *   The position of the value to remove.
  */
case class SeqRemoveUpdate(override val varId: Int, position: Int) extends SeqVariableMove(varId) {

  override protected def mkSeqMove(seqVar: SeqVariable): Unit = {
    seqVar.remove(seqVar.value().explorerAtPosition(position).get)
  }

  override def updateSeqState(state: SeqVariableState): SeqVariableState =
    SeqVariableState(
      varId,
      state.currentState.pushOp(Some(state.currentState.seqSize - 1)),
      state.domain
    )

  override def toString: String = s"Input var $varId | Removes value at pos $position"
}

/** Move that defines a new checkpoint in the SeqVariable.
  *
  * @param varId
  *   The test id of the SeqVariable.
  */
case class SeqDefineCheckpointUpdate(override val varId: Int) extends SeqVariableMove(varId) {

  override protected def mkSeqMove(seqVar: SeqVariable): Unit =
    seqVar.defineCurrentValueAsCheckpoint()

  override def updateSeqState(state: SeqVariableState): SeqVariableState =
    SeqVariableState(
      state.id,
      SeqVariableStackableState(state.currentState.seqSize, 0, Some(state.currentState)),
      state.domain
    )

  override def toString: String = s"Input var $varId | Defines new checkpoint"
}

/** Move that rolls back all the modifications of the SeqVariable since the last checkpoint.
  *
  * @param varId
  *   The test id of the SeqVariable.
  */
case class SeqRollBackToTopCheckpointUpdate(override val varId: Int)
    extends SeqVariableMove(varId) {

  override protected def mkSeqMove(seqVar: SeqVariable): Unit = seqVar.rollbackToTopCheckpoint()

  override def updateSeqState(state: SeqVariableState): SeqVariableState = {
    val prev = state.currentState.previousStackableState.get
    SeqVariableState(state.id, SeqVariableStackableState(prev.seqSize, 0, Some(prev)), state.domain)
  }

  override def toString: String = s"Input var $varId | Rolls back to top checkpoint"
}

/** Move that releases the top checkpoint.
  *
  * @param varId
  *   The test id of the SeqVariable.
  */
case class SeqReleaseTopCheckpointUpdate(override val varId: Int) extends SeqVariableMove(varId) {

  override protected def mkSeqMove(seqVar: SeqVariable): Unit = seqVar.releaseTopCheckpoint()

  override def updateSeqState(state: SeqVariableState): SeqVariableState =
    SeqVariableState(state.id, state.currentState.previousStackableState.get, state.domain)

  override def toString: String = s"Input var $varId | Releases top checkpoint"
}

/** Move that assigns a new value to the SeqVariable.
  *
  * @param varId
  *   The test id of the SeqVariable.
  */
case class SeqAssignUpdate(override val varId: Int, newSeq: List[Int])
    extends SeqVariableMove(varId) {

  override protected def mkSeqMove(seqVar: SeqVariable): Unit = {
    if (newSeq.isEmpty)
      seqVar := IntSequence.empty()
    else
      seqVar := IntSequence(newSeq)
  }

  override def updateSeqState(state: SeqVariableState): SeqVariableState = {
    SeqVariableState(state.id, SeqVariableStackableState(newSeq.size, 0, None), state.domain)
  }

  override def toString: String = s"Input var $varId | Assigns new value : $newSeq"
}
