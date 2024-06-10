package oscar.cbls.test.core.computation.objective

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.objective.{Exploration, Minimize, Objective}
import oscar.cbls.core.search._

class ObjectiveTestSuite extends AnyFunSuite {

  test("Simulating DynAndThen behavior works as expected.") {

    // Simulating a DynAndThen process
    // Only the combination of two moves is checked given acceptance criterion
    val store    = new Store()
    val objValue = IntVariable(store, 1000)
    val minimize = Minimize(objValue)
    store.close()

    var objVariations: List[Long] = Nil
    var summedVariations: Long    = 1000L

    // Test Neighborhood that simply add the provided value to the solution value.
    class AdditionNeighborhood extends Neighborhood("AdditionNeighborhood") {
      def getMove(obj: Objective): SearchResult = {
        val expl = obj.newExploration(new DummySimpleNeighborhood)
        objValue :+= objVariations.head
        summedVariations += objVariations.head
        objVariations = objVariations.tail
        val move = new DummyMove(objValue.pendingValue, new DummySimpleNeighborhood)
        expl.checkNeighbor(_ => move)
        expl.toReturn
      }

      /** Resets the internal state of the neighborhood */
      override def reset(): Unit = {}
    }

    // Simulation of a DynAndThen combinator. Basically creating a AcceptFirstEvalComposition and getting a move from left.
    class DynAndThenTest(left: AdditionNeighborhood, right: Move => AdditionNeighborhood)
        extends AdditionNeighborhood {
      override def getMove(baseObj: Objective): SearchResult = {
        val acceptFirstEvalSecond = new AcceptFirstEvalComposition(baseObj, right)
        left.getMove(acceptFirstEvalSecond)
      }
    }

    // An empty composite move just to have a "complete" representation of DynAndThen
    class CompositeMove(firstMove: Move, secondMove: Move, newObjValue: Long)
        extends Move(newObjValue, new DummySimpleNeighborhood) {
      override def commit(): Unit   = {}
      override def objAfter(): Long = newObjValue
    }

    // Custom objective, accepting first evaluating composition
    class AcceptFirstEvalComposition(baseObj: Objective, right: Move => AdditionNeighborhood)
        extends Objective(objValue) {
      // Creates the base objective exploration, used for composition evaluation.
      // (Get the initial obj upon Exploration creation)
      private val baseExploration = baseObj.newExploration(new DummySimpleNeighborhood)

      /** Creates a new Exploration instance. Must be called when starting an exploration. */
      override def newExploration(neighborhood: SimpleNeighborhood): Exploration =
        new Exploration(objValue.value(), neighborhood) {

          /** Checks if the candidate objValue match the acceptance conditions */
          override def checkNeighbor(buildMove: Long => Move): Unit = {
            val leftExploredMove =
              buildMove(0) // Accept and build move return by right neighborhood
            // Returned move is right neighborhood's move applied to left neighborhood's move
            _toReturn = right(leftExploredMove).getMove(new Objective(objValue) {

              override def newExploration(neighborhood: SimpleNeighborhood): Exploration =
                new Exploration(objValue.value(), neighborhood) {

                  /** Checks if the candidate objValue match the acceptance conditions */
                  override def checkNeighbor(buildMove: Long => Move): Unit = {
                    // checkNeighbor uses baseObjective neighbor to evaluate the composition
                    baseExploration.checkNeighbor(buildMove =
                      newObjValue =>
                        new CompositeMove(leftExploredMove, buildMove(newObjValue), newObjValue)
                    )
                    // Needed for Neighborhood's getMove method to return the searchResult
                    _toReturn = baseExploration.toReturn
                  }
                }

              /** Returns the worst value that the objective value could have considering the
                * Objective.
                */
              override val worstValue: Long = baseObj.worstValue

              /** Returns true if newValue is a better value than currentBest.
                *
                * Depending on used Objective this information may vary
                *
                * @param currentBest
                *   The current best value (has to be given by the caller)
                * @param newValue
                *   The considered new value
                * @return
                *   True if newValue is better than currentBest
                */
              override def isValueNewBest(currentBest: Long, newValue: Long): Boolean =
                baseObj.isValueNewBest(currentBest, newValue)
            })

          }
        }

      /** Returns the worst value that the objective value could have considering the Objective. */
      override val worstValue: Long = baseObj.worstValue

      /** Returns true if newValue is a better value than currentBest.
        *
        * Depending on used Objective this information may vary
        *
        * @param currentBest
        *   The current best value (has to be given by the caller)
        * @param newValue
        *   The considered new value
        * @return
        *   True if newValue is better than currentBest
        */
      override def isValueNewBest(currentBest: Long, newValue: Long): Boolean =
        baseObj.isValueNewBest(currentBest, newValue)
    }
    val dd = new DynAndThenTest(new AdditionNeighborhood, _ => new AdditionNeighborhood)

    val dd_3 = new DynAndThenTest(
      new DynAndThenTest(new AdditionNeighborhood, _ => new AdditionNeighborhood),
      _ => new DynAndThenTest(new AdditionNeighborhood, _ => new AdditionNeighborhood)
    )

    // Manual testing ==> generating scenario
    // One DD going from 1000 to 999 (+100,-101) ==> Should be true despite the fact that the first move lead to higher value
    objVariations = List(100, -101)
    var searchResult = dd.getMove(minimize)
    searchResult.isInstanceOf[MoveFound] should be(true)
    searchResult.asInstanceOf[MoveFound].move.isInstanceOf[CompositeMove] should be(true)
    objValue.pendingValue should be(999)
    // One DD going from 999 to 998 (-100,+99) ==> Should be true.
    // Checks that the original obj value doesn't change it's value with the -100
    // If it does, the last comparison would be is 899 greater than 998 ? ==> false
    objVariations = List(-100, 99)
    searchResult = dd.getMove(minimize)
    searchResult.isInstanceOf[MoveFound] should be(true)
    searchResult.asInstanceOf[MoveFound].move.isInstanceOf[CompositeMove] should be(true)
    objValue.pendingValue should be(998)

    // Three nested DD going from 998 to 997 ((+100 andThen -101) andThen (-101 andThen +101))
    objVariations = List(100, -101, -101, 101)
    val searchResult3 = dd_3.getMove(minimize)
    searchResult3.isInstanceOf[MoveFound] should be(true)
    searchResult3.asInstanceOf[MoveFound].move.isInstanceOf[CompositeMove] should be(true)

    objValue.pendingValue should be(997)
    summedVariations should be(objValue.pendingValue)

  }

}

private class DummyMove(_objAfter: Long, simpleNeighborhood: SimpleNeighborhood)
    extends Move(_objAfter, simpleNeighborhood) {

  override def commit(): Unit = {}

  override def objAfter(): Long = _objAfter
}

class DummySimpleNeighborhood() extends SimpleNeighborhood("") {

  override def exploreNeighborhood(exploration: Exploration): Unit = {}

  override def doMove(move: Move): Unit = {}

  /** Resets the internal state of the neighborhood */
  override def reset(): Unit = {}
}

class TestAssignValueNeighborhood(variable: IntVariable, value: Long)
    extends SimpleNeighborhood("TestAssign") {

  override def exploreNeighborhood(exploration: Exploration): Unit = {
    val initValue = variable.value()
    variable := value
    _searchProfiler.neighborSelected()
    exploration.checkNeighbor(objValue => TestAssignValueNeighborhoodMove(value, objValue, this))
    variable := initValue
  }

  /** Resets the internal state of the neighborhood */
  override def reset(): Unit = {}

  override def doMove(move: Move): Unit = {
    move match {
      case testAssignNeighborhoodMove: TestAssignValueNeighborhoodMove =>
        variable := testAssignNeighborhoodMove.newValue
      case _ =>
        require(requirement = false, s"Should be a TestAssignNeighborhoodMove but got : $move")
    }
  }
}

case class TestAssignValueNeighborhoodMove(
  newValue: Long,
  objValueAfter: Long,
  testAssignNeighborhood: TestAssignValueNeighborhood
) extends Move(objValueAfter, testAssignNeighborhood) {

  /** Commits this move. */
  override def commit(): Unit = {
    testAssignNeighborhood.doMove(this)
  }
}
