package oscar.cbls.test.core.propagation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls.core.propagation.{TestVariableElement,TestInvariantElement,TestPropagationStructure}

class PropagationTestSuite extends AnyFunSuite with Matchers {

  val seed : Option[Long] = None

  val structureGenerator = new PropagationStructureGenerator(seed)

  test("An error shall be raised when there is a cycle in the propagation structure") {
    val struct = new TestPropagationStructure

    val var1 : TestVariableElement = new TestVariableElement(struct)
    val var2 : TestVariableElement = new TestVariableElement(struct)
    val var3 : TestVariableElement = new TestVariableElement(struct)
    val inv1 : TestInvariantElement = new TestInvariantElement(struct)
    val inv2 : TestInvariantElement = new TestInvariantElement(struct)

    inv1.registerStaticAndDynamicDependency(var1)
    var2.setDefiningInvariant(inv1)
    inv2.registerStaticAndDynamicDependency(var2)
    var3.setDefiningInvariant(inv2)
    inv1.registerStaticAndDynamicDependency(var3)

    an[java.lang.IllegalArgumentException] should be thrownBy struct.close
  }

  test("The layer computation algorithm is coherent with the layer computed on build") {
    for (_ <- 0 to 10) {
      val struct = structureGenerator.generateStructure(15,30,5,15,5,15,5,15,10)

      struct.close
      struct.validateLayerAssignation

    }
  }

  test("Total Propagation update once the elements that have to be updated") {
    for (_ <- 0 to 10) {

      val struct = structureGenerator.generateStructure(15,30,5,15,5,15,5,15,10)

      struct.close

      val variables = struct.variables.filter(_.isInput)
      val nbVariables = variables.length

      for (_ <- (0 to 10)) {
        val nbVarToUpdate = 1 + structureGenerator.rand.nextInt((nbVariables/2).max(1))
        val toUpdate = structureGenerator.rand.shuffle(variables).take(nbVarToUpdate)

        toUpdate.foreach(_.update)

        struct.totalPropagation()

      }

    }

  }

  test("In a partial propagation, only the variable that are required for a target are updated and after the partial propagation, a total propagation updates the proper variables") {

    for (_ <- 0 to 10) {

      val rand = structureGenerator.rand
      val struct = structureGenerator.generateStructure(15,30,5,15,5,15,5,15,10)

      val input = struct.variables.filter(_.isInput)
      val nbInput = input.length
      val others = struct.variables.filterNot(_.isInput)
      val nbOthers = others.length

      val partialPropagationTargets = rand.shuffle(others).take(nbOthers/5)

      partialPropagationTargets.foreach(struct.registerForPartialPropagation(_))

      struct.close

      for (j <- 0 to 10) {
        val toUpdate = rand.shuffle(input).take(1 + rand.nextInt((nbInput/5).max(1)))
        toUpdate.foreach(_.update)

        val target = rand.shuffle(partialPropagationTargets).toList(0)

        struct.partialPropagation(target)
      }
      struct .totalPropagation()
    }

  }

  test("A partial propagation where the target is not registered for partial propagation has the same effect as a total propagation") {

    for (_ <- 0 to 10) {

      val rand = structureGenerator.rand
      val struct = structureGenerator.generateStructure(15,30,5,15,5,15,5,15,10)

      val input = struct.variables.filter(_.isInput)
      val nbInput = input.length
      val others = struct.variables.filterNot(_.isInput)
      val nbOthers = others.length

      val (ppTargets,nonPPTargets) = rand.shuffle(others).splitAt(nbOthers/5)

      ppTargets.foreach(struct.registerForPartialPropagation(_))

      struct.close

      for (j <- 0 to 10) {
        val toUpdate = rand.shuffle(input).take(1 + rand.nextInt((nbInput/5).max(1)))
        toUpdate.foreach(_.update)

        val target = rand.shuffle(nonPPTargets).toList(0)

        struct.partialPropagation(target)
      }

    }



  }



}
