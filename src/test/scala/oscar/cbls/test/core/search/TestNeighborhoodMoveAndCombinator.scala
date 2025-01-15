package oscar.cbls.test.core.search

import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.objective.{Exploration, Objective}
import oscar.cbls.core.search._

import scala.util.Random

/** An Assign Neighborhood whose purpose is to test the Neighborhood API.
  *
  * It assigns a random value between (100 and 300) to the given IntVariable.
  *
  * @param variable
  *   The modified IntVariable.
  * @param random
  *   The Random seed for this test.
  * @param doNotRevertMove
  *   Voluntary do not undo the movement after evaluation. This should trigger and error.
  * @param changeTheValueInReturnedMove
  *   Voluntary change the objective value in the returned [[TestAssignNeighborhoodMove]] it should
  *   trigger an error.
  * @param name
  *   The name of this Neighborhood
  */
class TestAssignNeighborhood(
  variable: IntVariable,
  random: Random,
  doNotRevertMove: Boolean = false,
  changeTheValueInReturnedMove: Boolean = false,
  name: String = "TestAssign"
) extends SimpleNeighborhood[TestAssignNeighborhoodMove](name) {

  override def exploreNeighborhood(exploration: Exploration[TestAssignNeighborhoodMove]): Unit = {
    val initValue = variable.value()
    var it        = 0L
    while (exploration.toReturn == NoMoveFound && it < 20) {
      val newValue = random.between(100, 300)
      variable := newValue
      searchProfiler().foreach(x => x.neighborSelected())
      exploration.checkNeighborWP(objValue => {
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

  override def doMove(move: TestAssignNeighborhoodMove): Unit = {
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
  override val objValueAfter: Long,
  testAssignNeighborhood: TestAssignNeighborhood
) extends Move(objValueAfter, testAssignNeighborhood.name) {

  /** Commits this move. */
  override def commit(): Unit = {
    testAssignNeighborhood.doMove(this)
  }
}

/** This [[oscar.cbls.core.search.NeighborhoodCombinator]] is meant to test the generic methods
  * within the [[oscar.cbls.core.search.profiling.CombinatorProfiler]].
  *
  * It uses two [[TestAssignNeighborhood]] and profiles specifics values throughout the search :
  *   - Number of found moves (using `summedValue` generic profiler)
  *   - Some statistic about a value assigned to first and second Neighborhood (using `minMeanMax`
  *     generic profiler)
  *   - No move found / call (using `percentageEventOccurrence` generic profiler)
  *   - No move found / iteration (using `nbOccurrencePerIteration` generic profiler)
  *
  * @param first
  *   The first TestAssignNeighborhood
  * @param second
  *   The second TestAssignNeighborhood
  */
class TestNeighborhoodCombinator(first: Neighborhood, second: Neighborhood)
    extends NeighborhoodCombinator("TestNeighborhoodCombinator", List(first, second)) {

  private var nextIsFirst: Boolean                           = true
  private var secondNeighborhoodNoMoveFoundBeforeNextIt: Int = 5
  private var iterationRemaining: Int                        = 5

  override protected[this] def exploreCombinator(objective: Objective): SearchResult = {
    if (secondNeighborhoodNoMoveFoundBeforeNextIt == 0) {
      secondNeighborhoodNoMoveFoundBeforeNextIt = 5
      iterationRemaining -= 1
      searchProfiler().foreach(x =>
        x.nbOccurrencePerIterationNextIteration("No move found/iteration")
      )
    }
    if (iterationRemaining > 0) {
      if (nextIsFirst) {
        first.getMove(objective) match {
          case mf: MoveFound =>
            searchProfiler().foreach(x => x.summedValuePlus("Number of found moves", 1))
            searchProfiler().foreach(x =>
              x.minMeanMaxAddValue("First neighborhood random", Random.between(0, 50))
            )
            searchProfiler().foreach(x =>
              x.positiveEventPercentagePushEvent("No move found/call", isPositive = false)
            )
            mf
          case NoMoveFound =>
            searchProfiler().foreach(x =>
              x.nbOccurrencePerIterationEventOccurred("No move found/iteration")
            )
            searchProfiler().foreach(x =>
              x.positiveEventPercentagePushEvent("No move found/call", isPositive = true)
            )
            nextIsFirst = false
            this.exploreCombinator(objective)
        }
      } else {
        second.getMove(objective) match {
          case mf: MoveFound =>
            searchProfiler().foreach(x => x.summedValuePlus("Number of found moves", 1))
            searchProfiler().foreach(x =>
              x.minMeanMaxAddValue("Second neighborhood random", Random.between(0, 50))
            )
            searchProfiler().foreach(x =>
              x.positiveEventPercentagePushEvent("No move found/call", isPositive = false)
            )
            mf
          case NoMoveFound =>
            searchProfiler().foreach(x =>
              x.nbOccurrencePerIterationEventOccurred("No move found/iteration")
            )
            searchProfiler().foreach(x =>
              x.positiveEventPercentagePushEvent("No move found/call", isPositive = true)
            )
            secondNeighborhoodNoMoveFoundBeforeNextIt -= 1
            nextIsFirst = true
            this.exploreCombinator(objective)
        }
      }
    } else NoMoveFound
  }
}
