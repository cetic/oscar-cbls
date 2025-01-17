package oscar.cbls.test.core.search

import org.scalatest.funsuite.AnyFunSuite
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.objective.{Minimize, Objective}
import oscar.cbls.lib.invariant.numeric.IntInt2Int

import scala.util.Random

class NeighborhoodAPITestSuite extends AnyFunSuite {

  val random: Random = Random
  val seed: Int      = random.nextInt()
  random.setSeed(seed)

  // Give Objective, objValue and a the IntVariable that is modified during the search
  private def getTestProblemBasicData: (Objective, IntVariable, IntVariable) = {
    val store               = new Store()
    val a: IntVariable      = IntVariable(store, 500)
    val b: IntVariable      = IntVariable(store, 600)
    val objValue            = IntVariable(store, 1000000, name = Some("Pythagoras"))
    val objective: Minimize = Minimize(objValue)

    // Basically Pythagoras theorem
    new IntInt2Int(
      store,
      a,
      b,
      objValue,
      (a, b) => Math.sqrt(Math.pow(a.toDouble, 2) + Math.pow(b.toDouble, 2)).toLong
    )
    store.close()
    (objective, a, b)
  }

  ignore(s"One value minimization works as expected. Seed : $seed") {
    val (objective, a, _) = getTestProblemBasicData

    val search = new TestAssignNeighborhood(a, random)
    // Changes this in case of error.
    search.verbosityLevel = 1
    search.doAllMoves(objective)
    search.displayProfiling()
  }

  test(s"Not reverting the move after exploration raise an error. Seed : $seed") {
    val (objective, a, _) = getTestProblemBasicData

    val search = new TestAssignNeighborhood(a, random, doNotRevertMove = true)
    search.verbosityLevel = 1
    val exception = intercept[IllegalArgumentException](search.doAllMoves(objective))
    assert(
      exception.getMessage.contains("Neighborhood did not restore the model after exploration")
    )
  }

  test(s"Changing the move after an exploration raise an error. Seed : $seed") {
    val (objective, a, _) = getTestProblemBasicData

    val search = new TestAssignNeighborhood(a, random, changeTheValueInReturnedMove = true)
    search.verbosityLevel = 1
    val exception = intercept[IllegalArgumentException](search.doAllMoves(objective))
    assert(exception.getMessage.contains("Neighborhood was lying"))
  }

  ignore(
    s"Combining two Neighborhoods with a CombinatorNeighborhood works as expected. Seed : $seed"
  ) {
    val (objective, a, b) = getTestProblemBasicData

    val search = new TestNeighborhoodCombinator(
      new TestAssignNeighborhood(a, random, name = "Test assign A"),
      new TestAssignNeighborhood(b, random, name = "Test assign B")
    )
    search.verbosityLevel = 1
    search.profileSearch()
    search.doAllMoves(objective)
    search.displayProfiling()
  }
}
