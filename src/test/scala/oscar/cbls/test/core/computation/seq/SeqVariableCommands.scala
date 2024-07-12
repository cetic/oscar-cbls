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
        case 1 => Gen.oneOf(flipIntSeq, moveIntSeq)
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
    n_1 <- Gen.choose(0, maxValue)
    n_2 <- Gen.choose(n_1, maxValue)
  } yield List(n_1, n_2)

  abstract class SeqVariableOperations extends Command {
    type Result = SeqVariableSUT

    override def preCondition(state: State): Boolean = {
      state.length >= 0 && state.checkpointLevel >= -1 && state.operationsSinceLastCheckpoint >= 0
    }

    override def postCondition(state: State, result: Try[Result]): Prop = {
      result match {
        case Failure(_) => false
        case Success(res) =>
          res.compare() && state.length >= 0 &&
          state.checkpointLevel >= -1 && state.operationsSinceLastCheckpoint >= 0
      }
    }
  }

  case class InsertIntSeq(value: Int, afterPos: Int) extends SeqVariableOperations {

    override def run(sut: SeqVariableSUT): SeqVariableSUT = {
      sut.insert(value, afterPos)
      sut
    }

    override def nextState(state: SeqVariableState): SeqVariableState = {
      state.length += 1
      if (state.checkpointLevel >= 0) state.operationsSinceLastCheckpoint += 1
      state
    }
  }

  case class RemoveIntSeq(pos: Int) extends SeqVariableOperations {

    override def preCondition(state: SeqVariableState): Boolean = {
      super.preCondition(state) && state.length >= 1
    }

    override def run(sut: SeqVariableSUT): SeqVariableSUT = {
      sut.remove(pos)
      sut
    }

    override def nextState(state: SeqVariableState): SeqVariableState = {
      state.length -= 1
      if (state.checkpointLevel >= 0) state.operationsSinceLastCheckpoint += 1
      state
    }
  }

  abstract class AbsMoveIntSeq() extends SeqVariableOperations {
    override def nextState(state: SeqVariableState): SeqVariableState = {
      if (state.checkpointLevel >= 0) state.operationsSinceLastCheckpoint += 1
      state
    }
  }

  case class MoveIntSeq(fromPos: Int, toPos: Int, afterPos: Int, flip: Boolean)
      extends AbsMoveIntSeq {

    override def preCondition(state: SeqVariableState): Boolean = {
      super.preCondition(state) && state.length >= 1
    }

    override def run(sut: SeqVariableSUT): SeqVariableSUT = {
      sut.move(fromPos, toPos, afterPos, flip)
      sut
    }
  }

  case class FlipIntSeq(fromPos: Int, toPos: Int) extends AbsMoveIntSeq {

    override def preCondition(state: SeqVariableState): Boolean = {
      super.preCondition(state) && state.length >= 1
    }

    override def run(sut: SeqVariableSUT): SeqVariableSUT = {
      sut.flip(fromPos, toPos)
      sut
    }
  }

  case class SwapIntSeq(
    fromPos_1: Int,
    toPos_1: Int,
    flip_1: Boolean,
    fromPos_2: Int,
    toPos_2: Int,
    flip_2: Boolean
  ) extends AbsMoveIntSeq {

    override def preCondition(state: SeqVariableState): Boolean = {
      super.preCondition(state) && state.length >= 2
    }

    override def run(sut: SeqVariableSUT): SeqVariableSUT = {
      sut.swap(fromPos_1, toPos_1, flip_1, fromPos_2, toPos_2, flip_2)
      sut
    }
  }

  case class AssignIntSeq(newValues: List[Int]) extends SeqVariableOperations {

    override def preCondition(state: SeqVariableState): Boolean = {
      super.preCondition(
        state
      ) && state.operationsSinceLastCheckpoint == 0 && state.checkpointLevel == -1
    }

    override def run(sut: SeqVariableSUT): SeqVariableSUT = {
      sut.assign(newValues)
      sut
    }

    override def nextState(state: SeqVariableState): SeqVariableState = {
      state.length = newValues.length
      state
    }
  }

  case class DefineCheckpointIntSeq() extends SeqVariableOperations {

    override def preCondition(state: SeqVariableState): Boolean = {
      super.preCondition(
        state
      ) && (state.checkpointLevel == -1 || state.operationsSinceLastCheckpoint > 0)
    }

    override def run(sut: SeqVariableSUT): SeqVariableSUT = {
      sut.defineCheckpoint()
      sut
    }

    override def nextState(state: SeqVariableState): SeqVariableState = {
      val newState = new SeqVariableState(state.checkpointLevel + 1, previousState = Some(state))
      newState.operationsSinceLastCheckpoint = 0
      newState.length = state.length
      newState
    }
  }

  case class RollBackToTopCheckpointIntSeq() extends SeqVariableOperations {

    override def preCondition(state: SeqVariableState): Boolean = {
      super.preCondition(state) && state.checkpointLevel >= 0
    }

    override def run(sut: SeqVariableSUT): SeqVariableSUT = {
      sut.rollBackToTopCheckpoint()
      sut
    }

    override def nextState(state: SeqVariableState): SeqVariableState = {
      state.operationsSinceLastCheckpoint = 0
      state.length = state.previousState.get.length
      state
    }
  }

  case class ReleaseTopCheckPointIntSeq() extends SeqVariableOperations {

    override def preCondition(state: SeqVariableState): Boolean = {
      super.preCondition(
        state
      ) && state.checkpointLevel >= 0 && state.operationsSinceLastCheckpoint == 0
    }

    override def run(sut: SeqVariableSUT): SeqVariableSUT = {
      sut.releaseTopCheckPoint()
      sut
    }

    override def nextState(state: SeqVariableState): SeqVariableState = {
      state.previousState.get
    }
  }

  case class PerformPropagationIntSeq() extends SeqVariableOperations {

    override def run(sut: SeqVariableSUT): SeqVariableSUT = {
      sut.performPropagation()
      sut
    }

    override def nextState(state: SeqVariableState): SeqVariableState = {
      state
    }
  }

}
