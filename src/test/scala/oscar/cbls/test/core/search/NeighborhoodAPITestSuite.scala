package oscar.cbls.test.core.search

import org.scalatest.funsuite.AnyFunSuite
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.objective.{Exploration, Minimize, Objective}
import oscar.cbls.core.search.{Move, MoveFound, Neighborhood, NeighborhoodCombinator, NoMoveFound, SearchResult, SimpleNeighborhood}
import oscar.cbls.lib.invariant.numeric.IntInt2Int
import oscar.cbls.test.core.search

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

  test(s"One value minimization works as expected. Seed : $seed") {
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


  test(s"Combining two Neighborhoods with a CombinatorNeighborhood works as expected. Seed : $seed") {
    val (objective, a, b) = getTestProblemBasicData

    val search = new TestNeighborhoodCombinator(
      new TestAssignNeighborhood(a, random, name = "Test assign A"),
      new TestAssignNeighborhood(b, random, name = "Test assign B"))
    search.verbosityLevel = 1
    search.doAllMoves(objective)
    search.displayProfiling()
  }

}

class TestAssignNeighborhood(
  variable: IntVariable,
  random: Random,
  doNotRevertMove: Boolean = false,
  changeTheValueInReturnedMove: Boolean = false,
  name: String = "TestAssign"
) extends SimpleNeighborhood(name) {

  override def exploreNeighborhood(exploration: Exploration): Unit = {
    val initValue = variable.value()
    var it        = 0L
    while (exploration.toReturn == NoMoveFound && it < 20) {
      val newValue = random.between(100, 300)
      variable := newValue
      _searchProfiler.neighborSelected()
      exploration.checkNeighbor(objValue => {
        if (changeTheValueInReturnedMove)
          TestAssignNeighborhoodMove(Math.pow(newValue, 2).toLong, objValue, this)
        else TestAssignNeighborhoodMove(newValue, objValue, this)
      })
      if (!doNotRevertMove) variable := initValue
      it += 1
    }
  }

  /** Resets the internal state of the neighborhood */
  override def reset(): Unit = {}

  override def doMove(move: Move): Unit = {
    move match {
      case testAssignNeighborhoodMove: TestAssignNeighborhoodMove =>
        variable := testAssignNeighborhoodMove.newValue
      case _ =>
        require(requirement = false, s"Should be a TestAssignNeighborhoodMove but got : $move")
    }
  }
}

case class TestAssignNeighborhoodMove(
  newValue: Long,
  objValueAfter: Long,
  testAssignNeighborhood: TestAssignNeighborhood
) extends Move(objValueAfter, testAssignNeighborhood) {

  /** Commits this move. */
  override def commit(): Unit = {
    testAssignNeighborhood.doMove(this)
  }
}


class TestNeighborhoodCombinator(first: Neighborhood, second: Neighborhood) extends NeighborhoodCombinator("TestNeighborhoodCombinator",List(first,second)){

  _searchProfiler.summedValueProfile("Number of found moves")
  _searchProfiler.minMeanMaxProfile("First neighborhood random")
  _searchProfiler.minMeanMaxProfile("Second neighborhood random")
  _searchProfiler.nbOccurrencePerIterationProfile("No move found/iteration")
  _searchProfiler.percentageEventOccurrenceProfile("No move found/call")

  private var nextIsFirst: Boolean = true
  private var secondNeighborhoodNoMoveFoundBeforeNextIt: Int = 5
  private var iterationRemaining: Int = 5

  override protected[this] def exploreCombinator(objective: Objective): SearchResult = {
    if(secondNeighborhoodNoMoveFoundBeforeNextIt == 0){
      secondNeighborhoodNoMoveFoundBeforeNextIt = 5
      iterationRemaining -= 1
      _searchProfiler.nbOccurrencePerIterationNextIteration("No move found/iteration")
    }
    if(iterationRemaining > 0) {
      if (nextIsFirst) {
        first.getMove(objective) match {
          case mf: MoveFound =>
            _searchProfiler.summedValuePlus("Number of found moves", 1)
            _searchProfiler.minMeanMaxAddValue("First neighborhood random", Random.between(0, 50))
            _searchProfiler.percentageEventOccurrencePushEvent("No move found/call",occurred = false)
            mf
          case NoMoveFound =>
            _searchProfiler.nbOccurrencePerIterationEventOccurred("No move found/iteration")
            _searchProfiler.percentageEventOccurrencePushEvent("No move found/call",occurred = true)
            nextIsFirst = false
            this.exploreCombinator(objective)
        }
      } else {
        second.getMove(objective) match {
          case mf: MoveFound =>
            _searchProfiler.summedValuePlus("Number of found moves", 1)
            _searchProfiler.minMeanMaxAddValue("Second neighborhood random", Random.between(0, 50))
            _searchProfiler.percentageEventOccurrencePushEvent("No move found/call",occurred = false)
            mf
          case NoMoveFound =>
            _searchProfiler.nbOccurrencePerIterationEventOccurred("No move found/iteration")
            _searchProfiler.percentageEventOccurrencePushEvent("No move found/call",occurred = true)
            secondNeighborhoodNoMoveFoundBeforeNextIt -= 1
            nextIsFirst = true
            this.exploreCombinator(objective)
        }
      }
    } else NoMoveFound
  }
}