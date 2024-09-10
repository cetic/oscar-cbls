package oscar.cbls.test.invBench

import org.scalacheck.Gen
import oscar.cbls.core.computation.Store

object InvTestBench {
  def apply(
    createTestData: Store => TestBenchData,
    name: String,
    additionalSeeds: List[String] = List()
  ): InvTestBench = {
    new InvTestBench(createTestData, name, additionalSeeds)
  }
}

/** The test bench for invariants that have no constant input.
  *
  * @param createTestData
  *   A function that given a Store creates the TestBenchData (the input and output of the invariant
  *   and the invariant itself)
  * @param name
  *   The name of the bench (used when printing the results, please be explicit).
  * @param additionalSeeds
  *   A list of explicit seeds that will be tested in addition to a random seed. Use this when you
  *   find a bug on a specific example.
  */

class InvTestBench(
  createTestData: Store => TestBenchData,
  name: String,
  additionnalSeeds: List[String]
) extends InvTestBenchWithConstGen[Unit](name, additionalSeeds) {
  override def createConstData(): Gen[Unit] = {
    Gen.const(())
  }

  override def createTestData(model: Store, inputData: Unit): TestBenchData = createTestData(model)

}
