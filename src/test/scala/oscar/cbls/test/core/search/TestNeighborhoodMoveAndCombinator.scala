package oscar.cbls.test.core.search

import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.objective.{Exploration, Objective}
import oscar.cbls.core.search._

import scala.util.Random

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
                                       objValueAfter: Long,
                                       testAssignNeighborhood: TestAssignNeighborhood
                                     ) extends Move(objValueAfter, testAssignNeighborhood) {

  /** Commits this move. */
  override def commit(): Unit = {
    testAssignNeighborhood.doMove(this)
  }
}


class TestNeighborhoodCombinator(first: Neighborhood, second: Neighborhood) extends NeighborhoodCombinator("TestNeighborhoodCombinator",List(first,second)){

  private var nextIsFirst: Boolean = true
  private var secondNeighborhoodNoMoveFoundBeforeNextIt: Int = 5
  private var iterationRemaining: Int = 5

  override protected[this] def exploreCombinator(objective: Objective): SearchResult = {
    if(secondNeighborhoodNoMoveFoundBeforeNextIt == 0){
      secondNeighborhoodNoMoveFoundBeforeNextIt = 5
      iterationRemaining -= 1
      searchProfiler().foreach(x => x.nbOccurrencePerIterationNextIteration("No move found/iteration"))
    }
    if(iterationRemaining > 0) {
      if (nextIsFirst) {
        first.getMove(objective) match {
          case mf: MoveFound =>
            searchProfiler().foreach(x => x.summedValuePlus("Number of found moves", 1))
            searchProfiler().foreach(x => x.minMeanMaxAddValue("First neighborhood random", Random.between(0, 50)))
            searchProfiler().foreach(x => x.percentageEventOccurrencePushEvent("No move found/call",occurred = false))
            mf
          case NoMoveFound =>
            searchProfiler().foreach(x => x.nbOccurrencePerIterationEventOccurred("No move found/iteration"))
            searchProfiler().foreach(x => x.percentageEventOccurrencePushEvent("No move found/call",occurred = true))
            nextIsFirst = false
            this.exploreCombinator(objective)
        }
      } else {
        second.getMove(objective) match {
          case mf: MoveFound =>
            searchProfiler().foreach(x => x.summedValuePlus("Number of found moves", 1))
            searchProfiler().foreach(x => x.minMeanMaxAddValue("Second neighborhood random", Random.between(0, 50)))
            searchProfiler().foreach(x => x.percentageEventOccurrencePushEvent("No move found/call",occurred = false))
            mf
          case NoMoveFound =>
            searchProfiler().foreach(x => x.nbOccurrencePerIterationEventOccurred("No move found/iteration"))
            searchProfiler().foreach(x => x.percentageEventOccurrencePushEvent("No move found/call",occurred = true))
            secondNeighborhoodNoMoveFoundBeforeNextIt -= 1
            nextIsFirst = true
            this.exploreCombinator(objective)
        }
      }
    } else NoMoveFound
  }
}