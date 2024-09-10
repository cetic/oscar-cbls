package oscar.cbls.test.invBench

import oscar.cbls.core.computation.{Invariant, Variable}
import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop}
import oscar.cbls.core.computation.Store
import org.scalacheck.Properties
import org.scalacheck.Test

case class TestBenchData(inv: Invariant, input: Array[Variable], output: Array[Variable])

/** Creates a test bench for invariants that need constant value.
  *
  * This test bench is able to create constant values for the invariants that need constant
  * parameters (e.g. a distance matrix in routing invariants).
  *
  * The type parameter of the test bench is a type that contains the constant that need to be
  * generated.
  *
  * Using the test bench requires to implement the following methods:
  *   - createConstData: this method gives the generator to generate constant input data of type T
  *   - createTestData: this method creates the test data (the input and output of the invariant and
  *     the invariant itself) using the constant input data and a store
  *
  * One can also implement typeTToString if the type T has no good printing method (e.g. if the type
  * T is an array)
  *
  * @tparam T
  *   The type of the constant input.
  * @param name
  *   The name of the bench (used when printing the results, please be explicit).
  * @param additionalSeeds
  *   A list of explicit seeds that will be tested in addition to a random seed. Use this when you
  *   find a bug on a specific example.
  */
abstract class InvTestBenchWithConstGen[T](name: String, additionalSeeds: List[String] = List())
    extends Commands {

  def createConstData(): Gen[T]

  def createTestData(model: Store, inputData: T): TestBenchData

  def typeTToString(elem: T): String = elem.toString

  type Sut = TestBenchData

  case class TestState(varStates: Array[VariableState], constState: T) {
    override def toString = s"TestState(${varStates.mkString(";")},${typeTToString(constState)})"
  }

  type State = TestState

  class TestBenchProperty(bench: InvTestBenchWithConstGen[T]) extends Properties(name) {

    propertyWithSeed("Random Seed", None) = bench.property()

    def addSeed(s: String): Unit =
      this.propertyWithSeed(s"$s", Some(s)) = bench.property()

    override def overrideParameters(p: Test.Parameters): Test.Parameters = {
      p.withMinSuccessfulTests(500)
    }
  }

  def test(): Unit = {

    val prop = new TestBenchProperty(this)
    additionnalSeeds.distinct.foreach(prop.addSeed)
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
      states <- {
        val data = createTestData(model, constData)
        Gen.sequence[Array[VariableState], VariableState](
          Array.tabulate(data.input.length)(i => VariableState(data.input(i), i))
        )
      }
    } yield TestState(states, constData)
  }

  override def initialPreCondition(state: State): Boolean = true

  override def newSut(state: State): Sut = {
    val model = new Store(debugLevel = 3)
    val data  = createTestData(model, state.constState)
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

    override def nextState(state: State): State = TestState(
      Array.tabulate(state.varStates.length)(i =>
        if (i == m.varId) m.updateState(state.varStates(m.varId)) else state.varStates(m.varId)
      ),
      state.constState
    )

    override def postCondition(state: State, success: Boolean): Prop = success

    override def preCondition(state: State): Boolean = state.varStates(m.varId).canMake(m)

    override def run(sut: Sut): Result = {
      m.mkMove(sut.input(m.varId))
    }

    override def toString: String = m.toString

  }

}
