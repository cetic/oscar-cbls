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
import oscar.cbls.modeling.Model

/** Companion object of the [[InvTestBench]] class. */
object InvTestBench {

  /** Instantiates an [[InvTestBench]].
    *
    * @param createTestData
    *   A function that given a Store creates the TestBenchSut (the input and output of the
    *   invariant and the invariant itself).
    * @param name
    *   The name of the bench (used when printing the results, please be explicit).
    * @param additionalSeeds
    *   A list of explicit seeds that will be tested in addition to a random seed. Use this when you
    *   find a bug on a specific example.
    */
  def apply(
    createTestData: Model => TestBenchSut,
    name: String,
    additionalSeeds: List[String] = List()
  ): InvTestBench = {
    new InvTestBench(createTestData, name, additionalSeeds)
  }
}

/** The test bench for invariants that have no constant input.
  *
  * @param createTestSut
  *   A function that given a Store creates the TestBenchSut (the input and output of the invariant
  *   and the invariant itself).
  * @param name
  *   The name of the bench (used when printing the results, please be explicit).
  * @param additionalSeeds
  *   A list of explicit seeds that will be tested in addition to a random seed. Use this when you
  *   find a bug on a specific example.
  */
class InvTestBench(
  createTestSut: Model => TestBenchSut,
  name: String,
  additionalSeeds: List[String]
) extends InvTestBenchWithConstGen[Unit](name, additionalSeeds) {

  override def genConst(): Gen[Unit] = {
    Gen.const(())
  }

  override def createTestBenchSut(model: Model, inputData: Unit): TestBenchSut =
    createTestSut(model)
}
