package oscar.cbls.test.invBench

import oscar.cbls.core.computation.{Invariant, Variable}
import org.scalacheck.commands.Commands
import org.scalacheck.rng.Seed
import org.scalacheck.{Gen, Prop}
import oscar.cbls.core.computation.Store

object InvTestBench {
  def apply(
    createTestData: Store => TestBenchData,
    name: String,
    additionnalSeeds: List[String] = List()
  ): InvTestBench = {
    new InvTestBench(createTestData, name, additionnalSeeds)
  }
}

class InvTestBench(
  createTestData: Store => TestBenchData,
  name: String,
  additionnalSeeds: List[String]
) extends InvTestBenchWithConstGen[Unit](name, additionnalSeeds) {
  override def createConstData() = {
    Gen.const(())
  }

  override def createInvariant(model: Store, inputData: Unit) = createTestData(model)

}
