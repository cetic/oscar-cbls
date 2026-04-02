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

package oscar.cbls.util.invBench

import org.scalacheck.Gen

/** This class holds the internal state of a [[oscar.cbls.core.computation.seq.SeqVariable]].
  *
  * It is used to determine the applicable movements given this state.
  *
  * @param id
  *   The test id of the SeqVariable to which this state is linked.
  * @param currentState
  *   The current state of the SeqVariable.
  */
case class SeqVariableState(id: Int, currentState: SeqVariableStackableState, domain: (Int, Int))
    extends VariableState(id) {

  private val (minValue, maxValue) = domain
  private val size                 = currentState.seqSize

  // Flags determining if a given move is allowed or not
  private val swapAndMoveAllowed: Boolean   = currentState.seqSize >= 2
  private val flipAndRemoveAllowed: Boolean = currentState.seqSize >= 1
  private val releaseAllowed: Boolean =
    currentState.seqOperationSinceLastCheckpoint == 0 && currentState.seqCheckpointLevel > -1
  private val rollBackAllowed: Boolean = currentState.seqCheckpointLevel > -1
  private val assignAllowed: Boolean   = currentState.seqCheckpointLevel == -1

  private def genSeqInsert: Gen[SeqInsertUpdate] = {
    for {
      value <- Gen.choose(minValue, maxValue)
      after <- Gen.choose(-1, size - 1)
    } yield SeqInsertUpdate(id, value, after)
  }

  private def genSeqMove: Gen[SeqMoveUpdate] = {
    for {
      from  <- Gen.choose(0, size - 1)
      to    <- Gen.choose(from, size - 1)
      after <- Gen.oneOf((-1 until from) ++ (to + 1 until size))
      flip  <- Gen.prob(0.5)
    } yield SeqMoveUpdate(id, from, to, after, flip)
  }

  private def genSeqFlip: Gen[SeqFlipUpdate] = {
    for {
      from <- Gen.choose(0, size - 1)
      to   <- Gen.choose(from, size - 1)
    } yield SeqFlipUpdate(id, from, to)
  }

  private def genSeqSwap: Gen[SeqSwapUpdate] = {
    for {
      from_1 <- Gen.choose(0, size - 2)
      to_1   <- Gen.choose(from_1, size - 2)
      flip_1 <- Gen.prob(0.5)
      from_2 <- Gen.choose(to_1 + 1, size - 1)
      to_2   <- Gen.choose(from_2, size - 1)
      flip_2 <- Gen.prob(0.5)
    } yield SeqSwapUpdate(id, from_1, to_1, flip_1, from_2, to_2, flip_2)
  }

  private def genSeqRemove: Gen[SeqRemoveUpdate] = {
    for {
      removePos <- Gen.choose(0, size - 1)
    } yield SeqRemoveUpdate(id, removePos)
  }

  private def genSeqAssign: Gen[SeqAssignUpdate] = {
    for {
      nbValues <- Gen.choose(0, 100)
      values   <- Gen.listOfN(nbValues, Gen.choose(minValue, maxValue))
    } yield SeqAssignUpdate(id, values)
  }

  private def genSeqDefineCheckpoint: Gen[SeqDefineCheckpointUpdate] =
    Gen.const(SeqDefineCheckpointUpdate(id))

  private def genSeqReleaseTopCheckpoint: Gen[SeqReleaseTopCheckpointUpdate] =
    Gen.const(SeqReleaseTopCheckpointUpdate(id))

  private def genSeqRollBack: Gen[SeqRollBackToTopCheckpointUpdate] =
    Gen.const(SeqRollBackToTopCheckpointUpdate(id))

  override def generateMove(): Gen[VariableMove] = {
    var authMoves: List[(Int, Gen[VariableMove])] =
      List((5, genSeqInsert), (1, genSeqDefineCheckpoint))
    if (swapAndMoveAllowed) authMoves = authMoves ::: List((5, genSeqMove), (5, genSeqSwap))
    if (flipAndRemoveAllowed) authMoves = authMoves ::: List((5, genSeqFlip), (3, genSeqRemove))
    if (releaseAllowed) authMoves = authMoves ::: List((2, genSeqReleaseTopCheckpoint))
    if (rollBackAllowed) authMoves = authMoves ::: List((3, genSeqRollBack))
    if (assignAllowed) authMoves = authMoves ::: List((2, genSeqAssign))

    Gen.frequency(authMoves: _*)
  }

  override def canMake(m: VariableMove): Boolean = {
    m match {
      case s: SeqSwapUpdate =>
        swapAndMoveAllowed && s.to_1 < size && s.to_2 < size
      case m: SeqMoveUpdate =>
        swapAndMoveAllowed && m.after < size && m.to < size
      case f: SeqFlipUpdate =>
        flipAndRemoveAllowed && f.to < size
      case r: SeqRemoveUpdate =>
        flipAndRemoveAllowed && r.position < size
      case _: SeqReleaseTopCheckpointUpdate    => releaseAllowed
      case _: SeqRollBackToTopCheckpointUpdate => rollBackAllowed
      case _: SeqAssignUpdate                  => assignAllowed
      case i: SeqInsertUpdate                  => i.after < size
      case _                                   => true
    }
  }

  override def toString: String =
    s"Seq $id : size $size | checkpoint lvl ${currentState.seqCheckpointLevel} | " +
      s"${currentState.seqOperationSinceLastCheckpoint} operations since last checkpoint"
}
