package oscar.cbls.test.invBench

import oscar.cbls.core.computation.{Invariant, Variable}
import org.scalacheck.commands.Commands
import org.scalacheck.rng.Seed
import org.scalacheck.{Gen, Prop}
import oscar.cbls.core.computation.Store
import org.scalacheck.Properties
import org.scalacheck.Test
import scala.util.Success
import scala.util.Failure



case class TestBenchData(inv: Invariant, input: Array[Variable], output: Array[Variable])

abstract class InvTestBenchWithConstGen[T](name: String,
  additionnalSeeds: List[String] = List())
    extends Commands {

  def createConstData() : Gen[T]

  def createInvariant(model : Store,
    inputData : T) : TestBenchData

  def typeTToString(elem:T) = elem.toString()

  type Sut = TestBenchData

  case class TestState(varStates : Array[VariableState],
    constState : T) {
    override def toString = s"TestState(${varStates.mkString(";")},${typeTToString(constState)})"
  }
  
  type State = TestState

  class TestBenchProperty(bench:InvTestBenchWithConstGen[T]) extends Properties(name) {

    propertyWithSeed("Random Seed",None) = bench.property()


    def addSeed(s : String) =
      this.propertyWithSeed(s"$s",Some(s)) = bench.property()

    override def overrideParameters(p: Test.Parameters): Test.Parameters = {
      p.withMinSuccessfulTests(500)
    }
  }

  def test(): Unit = {

    val prop = new TestBenchProperty(this)
    additionnalSeeds.distinct.foreach(prop.addSeed(_))
    prop.check()

  }

  override def destroySut(sut: Sut): Unit = ()

  override def genCommand(state: State): Gen[Command] = {
    for {
      stateVar <- Gen.oneOf(state.varStates.toIndexedSeq)
      move     <- stateVar.generateMove()
    } yield new InvariantCommand(move)
  }

  override def genInitialState: Gen[State] = {
    val model = new Store()
    for {
      constData <- createConstData()
      states <- {val data = createInvariant(model,constData)
        Gen.sequence[Array[VariableState],VariableState](Array.tabulate(data.input.length)(i => VariableState(data.input(i),i)))
      }} yield TestState(states,constData)
  }

  override def initialPreCondition(state: State): Boolean = true

  override def newSut(state: State): Sut = {
    val model = new Store(debugLevel = 3)
    val data  = createInvariant(model,state.constState)
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

    override def nextState(state: State): State = TestState(Array.tabulate(state.varStates.length)(i =>
      if (i == m.varId) m.updateState(state.varStates(m.varId)) else state.varStates(m.varId)),
      state.constState)

    override def postCondition(state: State, success: Boolean): Prop = success

    override def preCondition(state: State): Boolean = state.varStates(m.varId).canMake(m)

    override def run(sut: Sut): Result = {
      m.mkMove(sut.input(m.varId))
    }

    override def toString: String = m.toString

  }

}

