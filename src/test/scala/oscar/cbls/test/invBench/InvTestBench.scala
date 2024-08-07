package oscar.cbls.test.invBench

import oscar.cbls.core.computation.{Invariant, Variable}
import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop}

object InvTestBench {
  def apply(inv: Invariant, input: Iterable[Variable], randomInit: Boolean = true): InvTestBench = {
    new InvTestBench(inv, input, randomInit)
  }
}

class InvTestBench(inv: Invariant, input: Iterable[Variable], randomInit: Boolean)
    extends Commands {

  val inputVars: Array[TestVariable] = input.toArray.map(TestVariable(_))

  type Sut = Invariant

  type State = Array[VariableMove]

  def test(): Unit = {
    this.property().check(org.scalacheck.Test.Parameters.default.withMinSuccessfulTests(500))
  }

  override def destroySut(sut: Invariant): Unit = ()

  override def genCommand(state: State): Gen[Command] = {
    for {
      testVar <- Gen.oneOf(inputVars.toSeq)
      move    <- testVar.generateMove()
    } yield new InvariantCommand(move)
  }

  override def genInitialState: Gen[State] = {
    Gen.const(inputVars.map(_.generateMove().sample.get))
  }

  override def initialPreCondition(state: State): Boolean = true

  override def newSut(state: State): Sut = {
    if (randomInit)
      state.foreach(_.mkMove())
    inv.model.propagate()
    inv
  }

  override def canCreateNewSut(
    newState: State,
    initSuts: Iterable[State],
    runningSuts: Iterable[Sut]
  ): Boolean = true

  class InvariantCommand(m: VariableMove) extends UnitCommand {

    override def nextState(state: State): State = state

    override def postCondition(state: State, success: Boolean): Prop = success

    override def preCondition(state: State): Boolean = true

    override def run(sut: Invariant): Result = {
      m.mkMove()
    }

    override def toString: String = m.toString

  }

}
