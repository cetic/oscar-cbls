package oscar.cbls.test.core.computation.objective

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.objective.Maximize
import oscar.cbls.core.search.{MoveFound, NoMoveFound}
import oscar.cbls.test.core.computation.DummyMove

class MaximizeTestSuite extends AnyFunSuite {

  test("Maximize objective does not accept moves that does not strictly decreases the value") {
    val store    = new Store()
    val objValue = IntVariable(store, 1000)
    val obj      = Maximize(objValue)
    store.close()

    val exploration = obj.newExploration
    objValue := 500
    exploration.checkNeighbor(objAfter => new DummyMove(objAfter))
    exploration.toReturn should be(NoMoveFound)

    objValue := 1000
    exploration.checkNeighbor(objAfter => new DummyMove(objAfter))
    exploration.toReturn should be(NoMoveFound)

    objValue := 1100
    exploration.checkNeighbor(objAfter => new DummyMove(objAfter))
    exploration.toReturn.isInstanceOf[MoveFound] should be(true)
    exploration.toReturn.asInstanceOf[MoveFound].objAfter() should be(1100)
  }

  test(
    "Maximize objective does not accept moves if over approximated objective is define and condition not met"
  ) {
    val store              = new Store()
    val solValue           = IntVariable(store, 1000)
    val overApproxSolValue = IntVariable(store, 1000)
    val obj = Maximize(solValue, overApproximatedSolutionValue = Some(overApproxSolValue))
    store.close()

    // The objValue is voluntarily acceptable, but it shouldn't be check
    // if the over approximated value is not higher
    var exploration = obj.newExploration
    solValue           := 1100
    overApproxSolValue := 200
    exploration.checkNeighbor(objAfter => new DummyMove(objAfter))
    exploration.toReturn should be(NoMoveFound)

    solValue           := 1100
    overApproxSolValue := 200
    exploration.checkNeighbor(objAfter => new DummyMove(objAfter))
    exploration.toReturn should be(NoMoveFound)

    // Under approximated value is higher but the retained value should be objValue
    solValue           := 1100
    overApproxSolValue := 1200
    exploration.checkNeighbor(objAfter => new DummyMove(objAfter))
    exploration.toReturn.isInstanceOf[MoveFound] should be(true)
    exploration.toReturn.asInstanceOf[MoveFound].objAfter() should be(1100)

    // Under approximated value is higher but the objValue is lower, should be rejected
    exploration = obj.newExploration
    solValue           := 1000
    overApproxSolValue := 1200
    exploration.checkNeighbor(objAfter => new DummyMove(objAfter))
    exploration.toReturn should be(NoMoveFound)
  }

  test("Maximize objective does not accept moves if some constraints are not equal to zero") {
    val store       = new Store()
    val objValue    = IntVariable(store, 1000)
    val constraint1 = IntVariable(store, 0)
    val constraint2 = IntVariable(store, 1)
    val obj         = Maximize(objValue, mustBeZero = List(constraint1, constraint2))
    store.close()

    var exploration = obj.newExploration
    objValue := 1100
    exploration.checkNeighbor(objAfter => new DummyMove(objAfter))
    exploration.toReturn should be(NoMoveFound)

    objValue    := 1100
    constraint2 := 0
    exploration.checkNeighbor(objAfter => new DummyMove(objAfter))
    exploration.toReturn.isInstanceOf[MoveFound] should be(true)
    exploration.toReturn.asInstanceOf[MoveFound].objAfter() should be(1100)

    // Constraint is not violated but obj value is higher, should be rejected
    exploration = obj.newExploration
    objValue := 1000
    exploration.checkNeighbor(objAfter => new DummyMove(objAfter))
    exploration.toReturn should be(NoMoveFound)
  }

}
