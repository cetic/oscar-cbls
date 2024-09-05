package oscar.cbls.test.invBench

import oscar.cbls.core.computation.{Invariant, Variable}
import org.scalacheck.commands.Commands
import org.scalacheck.rng.Seed
import org.scalacheck.{Gen, Prop}
import oscar.cbls.core.computation.Store

case class TestBenchData(inv: Invariant, input: Array[Variable], output: Array[Variable])

object InvTestBench {
  def apply(
    createTestData: Store => TestBenchData,
    name: String,
    randomInit: Boolean = true,
    optSeed: Option[String] = None
  ): InvTestBench = {
    new InvTestBench(createTestData, name, randomInit, optSeed)
  }
}

class InvTestBench(
  createTestData: Store => TestBenchData,
  name: String,
  randomInit: Boolean,
  optSeed: Option[String]
) extends Commands {

  type Sut = TestBenchData

  type State = Array[VariableState]

  def test(): Unit = {
    val params =
      optSeed match {
        case None =>
          org.scalacheck.Test.Parameters.default.withMinSuccessfulTests(500)
        case Some(seed) =>
          org.scalacheck.Test.Parameters.default
            .withInitialSeed(Seed.fromBase64(seed).get)
            .withMinSuccessfulTests(500)
      }

    this.property().viewSeed(name).check(params)
  }

  override def destroySut(sut: Sut): Unit = ()

  override def genCommand(state: State): Gen[Command] = {
    for {
      stateVar <- Gen.oneOf(state.toIndexedSeq)
      move     <- stateVar.generateMove()
    } yield new InvariantCommand(move)
  }

  override def genInitialState: Gen[State] = {
    val model = new Store()
    val data = createTestData(model)
    Gen.sequence[Array[VariableState],VariableState](Array.tabulate(data.input.length)(i => VariableState(data.input(i),i)))
  }

  override def initialPreCondition(state: State): Boolean = true

  override def newSut(state: State): Sut = {
    val model = new Store(debugLevel = 3)
    val data  = createTestData(model)
    model.close()
    data.inv.model.propagate()
    data
  }

  override def canCreateNewSut(
    newState: State,
    initSuts: Iterable[State],
    runningSuts: Iterable[Sut]
  ): Boolean = true

  class InvariantCommand(m: VariableMove) extends UnitCommand {

    override def nextState(state: State): State = Array.tabulate(state.length)(i =>
      if (i == m.varId) m.updateState(state(m.varId)) else state(m.varId)
    )

    override def postCondition(state: State, success: Boolean): Prop = success

    override def preCondition(state: State): Boolean = state(m.varId).canMake(m)

    override def run(sut: Sut): Result = {
      m.mkMove(sut.input(m.varId))
    }

    override def toString: String = m.toString

  }

}
