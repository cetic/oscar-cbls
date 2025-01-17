package oscar.cbls.test.core.computation.objective

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.objective.Maximize
import oscar.cbls.core.search.{MoveFound, NoMoveFound}

class MaximizeTestSuite extends AnyFunSuite {

  test("Maximize objective does not accept moves that does not strictly increase the value") {
    val store    = new Store()
    val objValue = IntVariable(store, 1000)
    val obj      = Maximize(objValue)
    store.close()

    val exploration = obj.newExploration[DummyMove](new DummySimpleNeighborhood().searchProfiler())
    objValue := 500
    exploration.checkNeighborWP(objAfter => DummyMove(objAfter, new DummySimpleNeighborhood))
    exploration.toReturn should be(NoMoveFound)

    objValue := 1000
    exploration.checkNeighborWP(objAfter => DummyMove(objAfter, new DummySimpleNeighborhood))
    exploration.toReturn should be(NoMoveFound)

    objValue := 1100
    exploration.checkNeighborWP(objAfter => DummyMove(objAfter, new DummySimpleNeighborhood))
    exploration.toReturn.isInstanceOf[MoveFound] should be(true)
    exploration.toReturn.asInstanceOf[MoveFound].objAfter() should be(1100)
  }

  test(
    "Maximize objective does not accept moves if over approximated objective is define " +
      "and condition not met"
  ) {
    val store              = new Store()
    val objValue           = IntVariable(store, 1000)
    val overApproxObjValue = IntVariable(store, 1000)
    val obj                = Maximize(objValue, overApproximatedObjValue = Some(overApproxObjValue))
    store.close()

    // The objValue is voluntarily acceptable, but it shouldn't be check
    // if the over approximated value is not higher
    var exploration = obj.newExploration[DummyMove](new DummySimpleNeighborhood().searchProfiler())
    objValue           := 1100
    overApproxObjValue := 200
    exploration.checkNeighborWP(objAfter => DummyMove(objAfter, new DummySimpleNeighborhood))
    exploration.toReturn should be(NoMoveFound)

    objValue           := 1100
    overApproxObjValue := 1000
    exploration.checkNeighborWP(objAfter => DummyMove(objAfter, new DummySimpleNeighborhood))
    exploration.toReturn should be(NoMoveFound)

    // Over approximated value is higher but the retained value should be objValue
    objValue           := 1100
    overApproxObjValue := 1200
    exploration.checkNeighborWP(objAfter => DummyMove(objAfter, new DummySimpleNeighborhood))
    exploration.toReturn.isInstanceOf[MoveFound] should be(true)
    exploration.toReturn.asInstanceOf[MoveFound].objAfter() should be(1100)

    // Over approximated value is higher but the objValue is lower, should be rejected
    exploration = obj.newExploration[DummyMove](new DummySimpleNeighborhood().searchProfiler())
    objValue           := 1000
    overApproxObjValue := 1200
    exploration.checkNeighborWP(objAfter => DummyMove(objAfter, new DummySimpleNeighborhood))
    exploration.toReturn should be(NoMoveFound)
  }

  test("Maximize objective does not accept moves if some constraints are not equal to zero") {
    val store       = new Store()
    val objValue    = IntVariable(store, 1000)
    val constraint1 = IntVariable(store, 0)
    val constraint2 = IntVariable(store, 1)
    val obj         = Maximize(objValue, mustBeZero = List(constraint1, constraint2))
    store.close()

    var exploration = obj.newExploration[DummyMove](new DummySimpleNeighborhood().searchProfiler())
    objValue := 1100
    exploration.checkNeighborWP(objAfter => DummyMove(objAfter, new DummySimpleNeighborhood))
    exploration.toReturn should be(NoMoveFound)

    objValue    := 1100
    constraint2 := 0
    exploration.checkNeighborWP(objAfter => DummyMove(objAfter, new DummySimpleNeighborhood))
    exploration.toReturn.isInstanceOf[MoveFound] should be(true)
    exploration.toReturn.asInstanceOf[MoveFound].objAfter() should be(1100)

    // Constraint is not violated but obj value is higher, should be rejected
    exploration = obj.newExploration[DummyMove](new DummySimpleNeighborhood().searchProfiler())
    objValue := 1000
    exploration.checkNeighborWP(objAfter => DummyMove(objAfter, new DummySimpleNeighborhood))
    exploration.toReturn should be(NoMoveFound)
  }

}
