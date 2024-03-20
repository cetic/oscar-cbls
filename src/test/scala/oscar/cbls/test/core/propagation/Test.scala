package oscar.cbls.test.core.propagation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import oscar.cbls.core.propagation.{TestVariableElement,TestInvariantElement,TestPropagationStructure}

class Test extends AnyFunSuite with Matchers {

  val seed : Option[Long] = Some(1000)

  val structureGenerator = new PropagationStructureGenerator(seed)

  test("test0") {

    val struct = structureGenerator.generateStructure(3,6,1,4,1,4,1,4,5)
    val elements = struct.elements
    new java.io.PrintWriter("Graph.gv") {
      write(struct.myToDot())
      close()
    }
    val variables = struct.variables.filter(_.isInput)
    val nbVariables = variables.length

    struct.close
    for (i <- (0 to 10)) {
      val nbVarToUpdate = 1 + structureGenerator.rand.nextInt((nbVariables/5).max(1))
      val toUpdate = structureGenerator.rand.shuffle(variables).take(nbVarToUpdate) //variables.filter(_.name == "Variable 3")

      toUpdate.foreach(_.update)
        //println(s"update ${toUpdate.map(_.name)}")

      struct.totalPropagation()

    }

  }


  test("test1") {
    for (_ <- 0 to 10) {

      val rand = structureGenerator.rand
      val struct = structureGenerator.generateStructure(3,6,1,4,1,4,1,4,5)

      val input = struct.variables.filter(_.isInput)
      val nbInput = input.length
      val others = struct.variables.filterNot(_.isInput)
      val nbOthers = others.length

      val partialPropagationTargets = rand.shuffle(others).take(nbOthers/5)

      partialPropagationTargets.foreach(struct.registerForPartialPropagation(_))

      struct.close
      // new java.io.PrintWriter("Graph.gv") {
      //   write(struct.myToDot(Some(struct.variables.filter(_.name == "Variable 9")(0))))
      //   close()
      // }


      for (j <- 0 to 10) {
        //println(s"Update $j")
        val toUpdate = rand.shuffle(input).take(1 + rand.nextInt((nbInput/5).max(1)))
        toUpdate.foreach(_.update)
        //println(s"Updated: ${toUpdate.map(_.name)}")

        val target = rand.shuffle(partialPropagationTargets).toList(0)
          //println(s"Target: ${target.name}")

        struct.partialPropagation(target)
      }
    }


  }

}
