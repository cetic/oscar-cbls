package oscar.cbls.test.core.propagation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls.core.propagation.{
  TestInvariantElement,
  TestPropagationStructure,
  TestVariableElement
}
import org.scalatest.Suites

/** More dynamic tests about the propagation structure. In these tests, a propagation structure is
  * randomly generated and propagations are made on this structure. The tests are parametrized by
  * the debug level. The actual tests are done in [[PropagationTestSuites]]
  *
  * @param debugLevel
  */
class PropagationTestSuite(debugLevel: Int) extends AnyFunSuite with Matchers {

  val seed: Option[Long] = Some(2000)

  val structureGenerator = new PropagationStructureGenerator(seed)

  test("Total Propagation update once the elements that have to be updated") {
    for (_ <- 0 to 10) {

      val struct = structureGenerator.generateStructure(15, 30, 5, 15, 5, 15, 5, 15, 10, debugLevel)

      struct.close

      val variables   = struct.variables.filter(_.isInput)
      val nbVariables = variables.length

      for (_ <- (0 to 10)) {
        val nbVarToUpdate = 1 + structureGenerator.rand.nextInt((nbVariables / 2).max(1))
        val toUpdate      = structureGenerator.rand.shuffle(variables).take(nbVarToUpdate)

        toUpdate.foreach(_.update)

        struct.totalPropagation()
      }

    }

  }

  test(
    "In a partial propagation, only the variable that are required for a target are updated and after the partial propagation, a total propagation updates the proper variables"
  ) {

    for (_ <- 0 to 10) {

      val rand   = structureGenerator.rand
      val struct = structureGenerator.generateStructure(15, 30, 5, 15, 5, 15, 5, 15, 10, debugLevel)

      val input    = struct.variables.filter(_.isInput)
      val nbInput  = input.length
      val others   = struct.variables.filterNot(_.isInput)
      val nbOthers = others.length

      val partialPropagationTargets = rand.shuffle(others).take(nbOthers / 5)

      partialPropagationTargets.foreach(struct.registerForPartialPropagation(_))

      struct.close

      for (j <- 0 to 10) {
        val toUpdate = rand.shuffle(input).take(1 + rand.nextInt((nbInput / 5).max(1)))
        toUpdate.foreach(_.update)

        val target = rand.shuffle(partialPropagationTargets).toList(0)

        struct.partialPropagation(target)
      }
      struct.totalPropagation()
    }

  }

  test(
    "A partial propagation where the target is not registered for partial propagation has the same effect as a total propagation"
  ) {

    for (_ <- 0 to 10) {

      val rand   = structureGenerator.rand
      val struct = structureGenerator.generateStructure(15, 30, 5, 15, 5, 15, 5, 15, 10, debugLevel)

      val input    = struct.variables.filter(_.isInput)
      val nbInput  = input.length
      val others   = struct.variables.filterNot(_.isInput)
      val nbOthers = others.length

      val (ppTargets, nonPPTargets) = rand.shuffle(others).splitAt(nbOthers / 5)

      ppTargets.foreach(struct.registerForPartialPropagation(_))

      struct.close

      for (j <- 0 to 10) {
        val toUpdate = rand.shuffle(input).take(1 + rand.nextInt((nbInput / 5).max(1)))
        toUpdate.foreach(_.update)

        val target = rand.shuffle(nonPPTargets).toList(0)

        struct.partialPropagation(target)
      }

    }

  }

}

class PropagationTestSuites extends Suites(Seq.tabulate(4)(new PropagationTestSuite(_)): _*)
