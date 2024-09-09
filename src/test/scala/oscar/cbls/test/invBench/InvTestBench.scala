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

/** The test bench for invariants that have no constant input.
  *
  * @param createTestData
  *   A function that given a Store creates the TestBenchData (the input and output of the invariant
  *   and the invariant itself)
  * @param name
  *   The name of the bench (used when printing the results, please be explicit).
  * @param additionnalSeeds
  *   A list of explicit seeds that will be tested in addition to a random seed. Use this when you
  *   find a bug on a specific example.
  */

class InvTestBench(
  createTestData: Store => TestBenchData,
  name: String,
  additionnalSeeds: List[String]
) extends InvTestBenchWithConstGen[Unit](name, additionnalSeeds) {
  override def createConstData() = {
    Gen.const(())
  }

  override def createTestData(model: Store, inputData: Unit) = createTestData(model)

}
