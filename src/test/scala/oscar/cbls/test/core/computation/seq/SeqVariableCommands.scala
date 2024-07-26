package oscar.cbls.test.core.computation.seq

import org.scalacheck.{Gen, Prop}
import org.scalacheck.commands.Commands
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.seq.SeqVariable

import scala.util.{Failure, Success, Try}

object SeqVariableCommands extends Commands {

  override type State = SeqVariableState
  override type Sut   = SeqVariableSUT

  override def canCreateNewSut(
    newState: State,
    initSuts: Iterable[State],
    runningSuts: Iterable[Sut]
  ): Boolean = { true }

  override def newSut(state: State): Sut = {
    require(
      state.checkpointLevel == -1,
      "Cannot create a SeqVariable with checkpoint already defined"
    )
    require(
      state.operationsSinceLastCheckpoint == 0,
      "Cannot create a SeqVariable with operations already registered"
    )
    val store = new Store()
    val seq   = new SeqVariable(store, List.empty[Int], "Initial seq")
    val clone = seq.createClone()
    store.close()
    SeqVariableSUT(seq, clone)
  }

  override def destroySut(sut: Sut): Unit = {}

  override def initialPreCondition(state: State): Boolean = {
    state.length == 0 && state.checkpointLevel == -1 &&
    state.previousState.isEmpty && state.operationsSinceLastCheckpoint == 0
  }

  override def genInitialState: Gen[State] = { new SeqVariableState() }

  override def genCommand(state: State): Gen[SeqVariableCommands.Command] = {
    val l: Int = state.length

    // Moves
    lazy val flipIntSeq: Gen[Command] = {
      for {
        pos <- genTwoIncIntUpTo(l - 1)
      } yield FlipIntSeq(pos.head, pos.last)
    }
    lazy val swapIntSeq: Gen[Command] = for {
      pos    <- genFourIncIntBetween(l - 1)
      flip_1 <- Gen.prob(0.5)
      flip_2 <- Gen.prob(0.5)
    } yield SwapIntSeq(pos.head, pos(1), flip_1, pos(2), pos.last, flip_2)
    lazy val moveIntSeq: Gen[Command] = for {
      pos      <- genFourIncIntBetween(l - 1, minValue = -1)
      backward <- Gen.prob(0.5)
      flip     <- Gen.prob(0.5)
    } yield MoveIntSeq(
      if (backward || pos.head == -1) pos(2) else pos.head,
      if (backward || pos.head == -1) pos(3) else pos(1),
      if (backward || pos.head == -1) pos.head else pos(2),
      flip
    )

    lazy val moves: Gen[Command] = {
      l match {
        case 1 => moveIntSeq
        case _ => Gen.oneOf(swapIntSeq, flipIntSeq, moveIntSeq)
      }
    }
    lazy val insertIntSeq: Gen[Command] = for {
      afterPos <- Gen.choose(-1, l - 1)
      value    <- Gen.choose(0, 1000)
    } yield InsertIntSeq(value, afterPos)
    lazy val removeIntSeq: Gen[Command] = for {
      pos <- Gen.choose(0, l - 1)
    } yield RemoveIntSeq(pos)

    lazy val assignIntSeq: Gen[Command] = for {
      newLength <- Gen.choose(0, 20)
      newList   <- Gen.listOfN(newLength, Gen.choose(0, 1000))
    } yield AssignIntSeq(newList)

    val defineCheckpointIntSeq: Gen[Command]        = Gen.const(DefineCheckpointIntSeq())
    val rollBackToTopCheckpointIntSeq: Gen[Command] = Gen.const(RollBackToTopCheckpointIntSeq())
    val releaseTopCheckPointIntSeq: Gen[Command]    = Gen.const(ReleaseTopCheckPointIntSeq())
    val performPropagationIntSeq: Gen[Command]      = Gen.const(PerformPropagationIntSeq())

    (state.checkpointLevel, l, state.operationsSinceLastCheckpoint) match {
      case (-1, 0, 0) =>
        Gen.oneOf(insertIntSeq, assignIntSeq, defineCheckpointIntSeq, performPropagationIntSeq)
      case (-1, _, 0) =>
        Gen.oneOf(
          insertIntSeq,
          removeIntSeq,
          moves,
          assignIntSeq,
          defineCheckpointIntSeq,
          performPropagationIntSeq
        )
      case (_, 0, 0) =>
        Gen.oneOf(insertIntSeq, releaseTopCheckPointIntSeq, performPropagationIntSeq)
      case (_, 0, _) =>
        Gen.oneOf(
          insertIntSeq,
          rollBackToTopCheckpointIntSeq,
          performPropagationIntSeq,
          defineCheckpointIntSeq
        )
      case (_, _, 0) =>
        Gen.oneOf(
          insertIntSeq,
          removeIntSeq,
          moves,
          releaseTopCheckPointIntSeq,
          performPropagationIntSeq
        )
      case (_, _, _) =>
        Gen.oneOf(
          insertIntSeq,
          removeIntSeq,
          moves,
          rollBackToTopCheckpointIntSeq,
          defineCheckpointIntSeq,
          performPropagationIntSeq
        )
    }
  }

  private def genFourIncIntBetween(maxValue: Int, minValue: Int = 0): Gen[List[Int]] = for {
    n_1 <- Gen.choose(minValue, maxValue - 1)
    n_2 <- Gen.choose(n_1, maxValue - 1)
    n_3 <- Gen.choose(n_2 + 1, maxValue)
    n_4 <- Gen.choose(n_3, maxValue)
  } yield List(n_1, n_2, n_3, n_4)

  private def genTwoIncIntUpTo(maxValue: Int): Gen[List[Int]] = for {
    n_1 <- Gen.choose(0, maxValue-1)
    n_2 <- Gen.choose(n_1+1, maxValue)
  } yield List(n_1, n_2)

  abstract class SeqVariableOperations extends Command {
    type Result = List[Int]

    override def preCondition(state: State): Boolean = {
      state.length >= 0 && state.checkpointLevel >= -1 && state.operationsSinceLastCheckpoint >= 0
    }

    override def postCondition(state: State, result: Try[Result]): Prop = {
      Success(nextState(state).refList) == result
    }

    protected def newOperationsValue(state: SeqVariableState): Int = {
      if (state.checkpointLevel >= 0) state.operationsSinceLastCheckpoint + 1
      else state.operationsSinceLastCheckpoint
    }
  }

  case class InsertIntSeq(value: Int, afterPos: Int) extends SeqVariableOperations {

    override def preCondition(state: SeqVariableState): Boolean = {
      super.preCondition(state) && state.length > afterPos
    }

    override def run(sut: SeqVariableSUT): Result = {
      sut.insert(value, afterPos)
    }

    override def nextState(state: SeqVariableState): SeqVariableState = {
      val newRefList: List[Int] =
        state.refList
          .slice(0, afterPos + 1) ::: List(value) ::: state.refList.slice(
          afterPos + 1,
          state.length
        )
      new SeqVariableState(
        state.checkpointLevel,
        newRefList,
        newOperationsValue(state),
        state.previousState
      )
    }
  }

  case class RemoveIntSeq(pos: Int) extends SeqVariableOperations {

    override def preCondition(state: SeqVariableState): Boolean = {
      super.preCondition(state) && state.length > pos
    }

    override def run(sut: SeqVariableSUT): Result = {
      sut.remove(pos)
    }

    override def nextState(state: SeqVariableState): SeqVariableState = {
      val newRefList: List[Int] = state.refList.take(pos) ::: state.refList.drop(pos + 1)
      new SeqVariableState(
        state.checkpointLevel,
        newRefList,
        newOperationsValue(state),
        state.previousState
      )
    }
  }

  case class MoveIntSeq(fromPos: Int, toPos: Int, afterPos: Int, flip: Boolean)
      extends SeqVariableOperations {

    override def preCondition(state: SeqVariableState): Boolean = {
      super.preCondition(
        state
      ) && state.length > fromPos && state.length > toPos && state.length > afterPos
    }

    override def run(sut: SeqVariableSUT): Result = {
      sut.move(fromPos, toPos, afterPos, flip)
    }

    override def nextState(state: SeqVariableState): SeqVariableState = {
      val movedSubList: List[Int] =
        if (flip) state.refList.slice(fromPos, toPos + 1).reverse
        else state.refList.slice(fromPos, toPos + 1)
      val refListWithoutSubList: List[Int] =
        state.refList.take(fromPos) ::: state.refList.drop(toPos + 1)
      val newRefList: List[Int] =
        if (afterPos < fromPos)
          refListWithoutSubList.take(afterPos + 1) ::: movedSubList :::
            refListWithoutSubList.drop(afterPos + 1)
        else
          refListWithoutSubList.take(afterPos + 1 - movedSubList.size) ::: movedSubList :::
            refListWithoutSubList.drop(afterPos + 1 - movedSubList.size)

      new SeqVariableState(
        state.checkpointLevel,
        newRefList,
        newOperationsValue(state),
        state.previousState
      )
    }
  }

  case class FlipIntSeq(fromPos: Int, toPos: Int) extends SeqVariableOperations {

    override def preCondition(state: SeqVariableState): Boolean = {
      super.preCondition(state) && state.length > fromPos && state.length > toPos
    }

    override def run(sut: SeqVariableSUT): Result = {
      sut.flip(fromPos, toPos)
    }

    override def nextState(state: SeqVariableState): SeqVariableState = {
      val movedSubList: List[Int] = state.refList.slice(fromPos, toPos + 1).reverse
      val newRefList: List[Int] =
        state.refList.take(fromPos) ::: movedSubList ::: state.refList.drop(toPos + 1)

      new SeqVariableState(
        state.checkpointLevel,
        newRefList,
        newOperationsValue(state),
        state.previousState
      )
    }
  }

  case class SwapIntSeq(
    fromPos_1: Int,
    toPos_1: Int,
    flip_1: Boolean,
    fromPos_2: Int,
    toPos_2: Int,
    flip_2: Boolean
  ) extends SeqVariableOperations {

    override def preCondition(state: SeqVariableState): Boolean = {
      super.preCondition(
        state
      ) && state.length > fromPos_1 && state.length > toPos_1 && state.length > fromPos_2 && state.length > toPos_2
    }

    override def run(sut: SeqVariableSUT): Result = {
      sut.swap(fromPos_1, toPos_1, flip_1, fromPos_2, toPos_2, flip_2)
    }

    override def nextState(state: SeqVariableState): SeqVariableState = {
      val movedSubList1: List[Int] =
        if (flip_1) state.refList.slice(fromPos_1, toPos_1 + 1).reverse
        else state.refList.slice(fromPos_1, toPos_1 + 1)
      val movedSubList2: List[Int] =
        if (flip_2) state.refList.slice(fromPos_2, toPos_2 + 1).reverse
        else state.refList.slice(fromPos_2, toPos_2 + 1)
      val newRefList: List[Int] =
        state.refList.take(fromPos_1) ::: movedSubList2 :::
          state.refList.slice(toPos_1 + 1, fromPos_2) ::: movedSubList1 ::: state.refList.drop(
            toPos_2 + 1
          )

      new SeqVariableState(
        state.checkpointLevel,
        newRefList,
        newOperationsValue(state),
        state.previousState
      )
    }
  }

  case class AssignIntSeq(newValues: List[Int]) extends SeqVariableOperations {

    override def preCondition(state: SeqVariableState): Boolean = {
      super.preCondition(
        state
      ) && state.operationsSinceLastCheckpoint == 0 && state.checkpointLevel == -1
    }

    override def run(sut: SeqVariableSUT): Result = {
      sut.assign(newValues)
    }

    override def nextState(state: SeqVariableState): SeqVariableState = {
      new SeqVariableState(-1, newValues, 0, None)
    }
  }

  case class DefineCheckpointIntSeq() extends SeqVariableOperations {

    override def preCondition(state: SeqVariableState): Boolean = {
      super.preCondition(
        state
      ) && (state.checkpointLevel == -1 || state.operationsSinceLastCheckpoint > 0)
    }

    override def run(sut: SeqVariableSUT): Result = {
      sut.defineCheckpoint()
    }

    override def nextState(state: SeqVariableState): SeqVariableState = {
      new SeqVariableState(state.checkpointLevel + 1, state.refList, 0, Some(state))
    }
  }

  case class RollBackToTopCheckpointIntSeq() extends SeqVariableOperations {

    override def preCondition(state: SeqVariableState): Boolean = {
      super.preCondition(state) && state.checkpointLevel >= 0
    }

    override def run(sut: SeqVariableSUT): Result = {
      sut.rollBackToTopCheckpoint()
    }

    override def nextState(state: SeqVariableState): SeqVariableState = {
      new SeqVariableState(
        state.checkpointLevel,
        state.previousState.get.refList,
        0,
        state.previousState
      )
    }
  }

  case class ReleaseTopCheckPointIntSeq() extends SeqVariableOperations {

    override def preCondition(state: SeqVariableState): Boolean = {
      super.preCondition(
        state
      ) && state.checkpointLevel >= 0 && state.operationsSinceLastCheckpoint == 0
    }

    override def run(sut: SeqVariableSUT): Result = {
      sut.releaseTopCheckPoint()
    }

    override def nextState(state: SeqVariableState): SeqVariableState = {
      state.previousState.get
    }
  }

  case class PerformPropagationIntSeq() extends SeqVariableOperations {

    override def run(sut: SeqVariableSUT): Result = {
      sut.performPropagation()
    }

    override def nextState(state: SeqVariableState): SeqVariableState = {
      state
    }
  }

}
