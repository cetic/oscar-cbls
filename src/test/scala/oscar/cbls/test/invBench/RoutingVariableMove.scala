// OscaR is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 2.1 of the License, or
// (at your option) any later version.
//
// OscaR is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License  for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with OscaR.
// If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html

package oscar.cbls.test.invBench

import oscar.cbls.algo.sequence.IntSequence
import oscar.cbls.core.computation.Variable
import oscar.cbls.core.computation.seq.SeqVariable

import scala.annotation.tailrec
import scala.collection.immutable.HashSet

abstract class RoutingVariableMove(varId: Int) extends VariableMove(varId) {

  /** Used to display the routes and unrouted node of the current state in [[toString]]. */
  protected var currentRoutesAndUnrouted: String = ""

  override def mkMove(testVar: Variable): Unit = {
    testVar match {
      case seqVar: SeqVariable => mkSeqMove(seqVar)
      case _ => throw new Error("Routing Movement can only update variable of type Seq")
    }
  }

  /** Does the move (avoiding pattern matching for each concrete class) */
  protected def mkSeqMove(seqVar: SeqVariable): Unit

  override def updateState(state: VariableState): VariableState = {
    state match {
      case seqState: RoutingVariableState => updateSeqState(seqState)
      case _ => throw new Error("Routing Movement can only update state of type SeqVariableState")
    }
  }

  /** Updates the State (avoiding pattern matching for each concrete class) */
  protected def updateSeqState(state: RoutingVariableState): RoutingVariableState

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
case class RoutingInsertUpdate(override val varId: Int, value: Int, after: Int)
    extends RoutingVariableMove(varId) {

  override protected def mkSeqMove(seqVar: SeqVariable): Unit = {
    seqVar.insertAfterPosition(value, seqVar.value().explorerAtPosition(after).get)
  }

  override def updateSeqState(state: RoutingVariableState): RoutingVariableState = {
    currentRoutesAndUnrouted =
      s"Current routes: ${state.routes} | Current unrouted: ${state.unrouted}"
    val explorer    = state.routes.explorerAtPosition(after).get
    val newRouted   = state.routes.insertAfterPosition(value, explorer)
    val newUnrouted = state.unrouted - value
    RoutingVariableState(
      varId,
      state.currentState.pushOp(Some(state.currentState.seqSize + 1)),
      state.vrs,
      newUnrouted,
      newRouted
    )
  }

  override def toString: String =
    s"Input var $varId | Inserts $value after pos $after | $currentRoutesAndUnrouted"
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
case class RoutingMoveUpdate(override val varId: Int, from: Int, to: Int, after: Int, flip: Boolean)
    extends RoutingVariableMove(varId) {

  override protected def mkSeqMove(seqVar: SeqVariable): Unit = {
    seqVar.move(
      seqVar.value().explorerAtPosition(from).get,
      seqVar.value().explorerAtPosition(to).get,
      seqVar.value().explorerAtPosition(after).get,
      flip
    )
  }

  override def updateSeqState(state: RoutingVariableState): RoutingVariableState = {
    currentRoutesAndUnrouted =
      s"Current routes: ${state.routes} | Current unrouted: ${state.unrouted}"

    val fromExplorer  = state.routes.explorerAtPosition(from).get
    val toExplorer    = state.routes.explorerAtPosition(to).get
    val afterExplorer = state.routes.explorerAtPosition(after).get
    val newRouted     = state.routes.moveAfter(fromExplorer, toExplorer, afterExplorer, flip)

    RoutingVariableState(varId, state.currentState.pushOp(), state.vrs, state.unrouted, newRouted)
  }

  override def toString: String =
    s"Input var $varId | Moves from $from to $to after $after ${if (flip) "flip" else "no flip"} | $currentRoutesAndUnrouted"
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
case class RoutingFlipUpdate(override val varId: Int, from: Int, to: Int)
    extends RoutingVariableMove(varId) {

  override protected def mkSeqMove(seqVar: SeqVariable): Unit = {
    seqVar.flip(
      seqVar.value().explorerAtPosition(from).get,
      seqVar.value().explorerAtPosition(to).get
    )
  }

  override def updateSeqState(state: RoutingVariableState): RoutingVariableState = {
    currentRoutesAndUnrouted =
      s"Current routes: ${state.routes} | Current unrouted: ${state.unrouted}"

    val fromExplorer = state.routes.explorerAtPosition(from).get
    val toExplorer   = state.routes.explorerAtPosition(to).get
    val newRouted = state.routes.moveAfter(fromExplorer, toExplorer, fromExplorer.prev, flip = true)
    RoutingVariableState(varId, state.currentState.pushOp(), state.vrs, state.unrouted, newRouted)
  }

  override def toString: String =
    s"Input var $varId | Flips sub-sequence from $from to $to | $currentRoutesAndUnrouted"

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
case class RoutingSwapUpdate(
  override val varId: Int,
  from_1: Int,
  to_1: Int,
  flip_1: Boolean,
  from_2: Int,
  to_2: Int,
  flip_2: Boolean
) extends RoutingVariableMove(varId) {

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

  override def updateSeqState(state: RoutingVariableState): RoutingVariableState = {
    currentRoutesAndUnrouted =
      s"Current routes: ${state.routes} | Current unrouted: ${state.unrouted}"

    val newRouted = swap(state)

    RoutingVariableState(varId, state.currentState.pushOp(), state.vrs, state.unrouted, newRouted)
  }

  override def toString: String =
    s"Input var $varId | Swaps from $from_1 to $to_1 ${if (flip_1) "flip"
      else "no flip"} and from $from_2 to $to_2 ${if (flip_2) "flip" else "no flip"} | $currentRoutesAndUnrouted"

  @tailrec
  private def swap(state: RoutingVariableState): IntSequence = {
    val from1Explorer = state.routes.explorerAtPosition(from_1).get
    val to1Explorer   = state.routes.explorerAtPosition(to_1).get
    val from2Explorer = state.routes.explorerAtPosition(from_2).get
    val to2Explorer   = state.routes.explorerAtPosition(to_2).get
    if (to_1 == from_2 - 1) {
      if (flip_1) {
        val tmp =
          state.routes.moveAfter(from1Explorer, to1Explorer, from1Explorer.prev, flip = true)
        tmp.moveAfter(
          tmp.explorerAtPosition(from2Explorer.position).get,
          tmp.explorerAtPosition(to2Explorer.position).get,
          tmp.explorerAtPosition(from1Explorer.prev.position).get,
          flip_2
        )
      } else {
        state.routes.moveAfter(from2Explorer, to2Explorer, from1Explorer.prev, flip_2)
      }
    } else if (to_1 < from_2) {
      val tmp = state.routes.moveAfter(from1Explorer, to1Explorer, from2Explorer.prev, flip_1)
      tmp.moveAfter(
        tmp.explorerAtPosition(from2Explorer.position).get,
        tmp.explorerAtPosition(to2Explorer.position).get,
        tmp.explorerAtPosition(from1Explorer.prev.position).get,
        flip_2
      )
    } else {
      RoutingSwapUpdate(varId, from_2, to_2, flip_2, from_1, to_1, flip_1).swap(state)
    }
  }

}

/** Move that removes a value from the SeqVariable.
  *
  * @param varId
  *   The test id of the SeqVariable.
  * @param position
  *   The position of the value to remove.
  */
case class RoutingRemoveUpdate(override val varId: Int, position: Int)
    extends RoutingVariableMove(varId) {

  override protected def mkSeqMove(seqVar: SeqVariable): Unit = {
    seqVar.remove(seqVar.value().explorerAtPosition(position).get)
  }

  override def updateSeqState(state: RoutingVariableState): RoutingVariableState = {
    currentRoutesAndUnrouted =
      s"Current routes: ${state.routes} | Current unrouted: ${state.unrouted}"

    val toRemoveExplorer = state.routes.explorerAtPosition(position).get
    val newRouted        = state.routes.remove(toRemoveExplorer)
    val newUnrouted      = state.unrouted + toRemoveExplorer.value
    RoutingVariableState(
      varId,
      state.currentState.pushOp(Some(state.currentState.seqSize - 1)),
      state.vrs,
      newUnrouted,
      newRouted
    )
  }

  override def toString: String =
    s"Input var $varId | Removes value at pos $position |$currentRoutesAndUnrouted"
}

/** Move that defines a new checkpoint in the SeqVariable.
  *
  * @param varId
  *   The test id of the SeqVariable.
  */
case class RoutingDefineCheckpointUpdate(override val varId: Int)
    extends RoutingVariableMove(varId) {

  override protected def mkSeqMove(seqVar: SeqVariable): Unit =
    seqVar.defineCurrentValueAsCheckpoint()

  override def updateSeqState(state: RoutingVariableState): RoutingVariableState = {
    currentRoutesAndUnrouted =
      s"Current routes: ${state.routes} | Current unrouted: ${state.unrouted}"

    RoutingVariableState(
      state.id,
      SeqVariableStackableState(
        state.currentState.seqSize,
        0,
        Some(state.currentState),
        Some(state.unrouted),
        Some(state.routes)
      ),
      state.vrs,
      state.unrouted,
      state.routes
    )
  }

  override def toString: String =
    s"Input var $varId | Defines new checkpoint | $currentRoutesAndUnrouted"
}

/** Move that rolls back all the modifications of the SeqVariable since the last checkpoint.
  *
  * @param varId
  *   The test id of the SeqVariable.
  */
case class RoutingRollBackToTopCheckpointUpdate(override val varId: Int)
    extends RoutingVariableMove(varId) {

  override protected def mkSeqMove(seqVar: SeqVariable): Unit = seqVar.rollbackToTopCheckpoint()

  override def updateSeqState(state: RoutingVariableState): RoutingVariableState = {
    currentRoutesAndUnrouted =
      s"Current routes: ${state.routes} | Current unrouted: ${state.unrouted}"

    val prev         = state.currentState.previousStackableState.get
    val prevRouted   = state.currentState.previousStackableRouted.get
    val prevUnrouted = state.currentState.previousStackableUnrouted.get
    RoutingVariableState(
      state.id,
      SeqVariableStackableState(prev.seqSize, 0, Some(prev), Some(prevUnrouted), Some(prevRouted)),
      state.vrs,
      prevUnrouted,
      prevRouted
    )
  }

  override def toString: String =
    s"Input var $varId | Rolls back to top checkpoint | $currentRoutesAndUnrouted"
}

/** Move that releases the top checkpoint.
  *
  * @param varId
  *   The test id of the SeqVariable.
  */
case class RoutingReleaseTopCheckpointUpdate(override val varId: Int)
    extends RoutingVariableMove(varId) {

  override protected def mkSeqMove(seqVar: SeqVariable): Unit = seqVar.releaseTopCheckpoint()

  override def updateSeqState(state: RoutingVariableState): RoutingVariableState = {
    currentRoutesAndUnrouted =
      s"Current routes: ${state.routes} | Current unrouted: ${state.unrouted}"

    RoutingVariableState(
      state.id,
      state.currentState.previousStackableState.get,
      state.vrs,
      state.currentState.previousStackableUnrouted.get,
      state.currentState.previousStackableRouted.get
    )
  }

  override def toString: String =
    s"Input var $varId | Releases top checkpoint | $currentRoutesAndUnrouted"
}

/** Move that assigns a new value to the SeqVariable.
  *
  * @param varId
  *   The test id of the SeqVariable.
  */
case class RoutingAssignUpdate(override val varId: Int, newSeq: List[Int])
    extends RoutingVariableMove(varId) {

  override protected def mkSeqMove(seqVar: SeqVariable): Unit = {
    if (newSeq.isEmpty)
      seqVar := IntSequence.empty()
    else
      seqVar := IntSequence(newSeq)
  }

  override def updateSeqState(state: RoutingVariableState): RoutingVariableState = {
    currentRoutesAndUnrouted =
      s"Current routes: ${state.routes} | Current unrouted: ${state.unrouted}"

    val newRouted = IntSequence(newSeq)
    val newUnrouted: HashSet[Int] =
      HashSet.from(state.vrs.v until state.vrs.n).filter(!newRouted.contains(_))

    RoutingVariableState(
      state.id,
      SeqVariableStackableState(newSeq.size, 0, None),
      state.vrs,
      newUnrouted,
      newRouted
    )
  }

  override def toString: String =
    s"Input var $varId | Assigns new value : $newSeq | $currentRoutesAndUnrouted"
}
