package oscar.cbls.test.core.computation.objective

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.objective.{Exploration, Minimize, Objective}
import oscar.cbls.core.search.{Move, MoveFound, SearchResult}

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
    class Neighborhood {
      def getMove(obj: Objective): SearchResult = {
        val expl = obj.newExploration
        objValue :+= objVariations.head
        summedVariations += objVariations.head
        objVariations = objVariations.tail
        val move = new DummyMove(objValue.pendingValue)
        expl.checkNeighbor(_ => move)
        expl.toReturn
      }
    }

    // Simulation of a DynAndThen combinator. Basically creating a AcceptFirstEvalComposition and getting a move from left.
    class DynAndThenTest(left: Neighborhood, right: Move => Neighborhood) extends Neighborhood {
      override def getMove(baseObj: Objective): SearchResult = {
        val acceptFirstEvalSecond = new AcceptFirstEvalComposition(baseObj, right)
        left.getMove(acceptFirstEvalSecond)
      }
    }

    // An empty composite move just to have a "complete" representation of DynAndThen
    class CompositeMove(firstMove: Move, secondMove: Move, newObjValue: Long) extends Move {
      override def commit(): Unit   = {}
      override def objAfter(): Long = newObjValue
    }

    // Custom objective, accepting first evaluating composition
    class AcceptFirstEvalComposition(baseObj: Objective, right: Move => Neighborhood)
        extends Objective {
      // Creates the base objective exploration, used for composition evaluation.
      // (Get the initial obj upon Exploration creation)
      private val baseExploration = baseObj.newExploration

      /** Creates a new Exploration instance. Must be called when starting an exploration. */
      override def newExploration: Exploration = new Exploration {

        /** Checks if the candidate objValue match the acceptance conditions */
        override def checkNeighbor(buildMove: Long => Move): Unit = {
          val leftExploredMove = buildMove(0) // Accept and build move return by right neighborhood
          // Returned move is right neighborhood's move applied to left neighborhood's move
          _toReturn = right(leftExploredMove).getMove(new Objective {

            override def newExploration: Exploration = new Exploration {

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
          })

        }
      }
    }
    val dd = new DynAndThenTest(new Neighborhood, _ => new Neighborhood)

    val dd_3 = new DynAndThenTest(
      new DynAndThenTest(new Neighborhood, _ => new Neighborhood),
      _ => new DynAndThenTest(new Neighborhood, _ => new Neighborhood)
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

private class DummyMove(_objAfter: Long) extends Move {

  override def commit(): Unit = {}

  override def objAfter(): Long = _objAfter
}
