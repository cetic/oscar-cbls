package oscar.cbls.test.core.computation.objective

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.objective.Minimize
import oscar.cbls.core.search.{MoveFound, NoMoveFound}

class MinimizeTestSuite extends AnyFunSuite {

  test("Minimize objective does not accept moves that does not strictly decreases the value") {
    val store    = new Store()
    val objValue = IntVariable(store, 1000)
    val obj      = Minimize(objValue)
    store.close()

    val exploration = obj.newExploration[DummyMove](new DummySimpleNeighborhood().searchProfiler())
    objValue := 2000
    exploration.checkNeighborWP(objAfter => DummyMove(objAfter, new DummySimpleNeighborhood))
    exploration.toReturn should be(NoMoveFound)

    objValue := 1000
    exploration.checkNeighborWP(objAfter => DummyMove(objAfter, new DummySimpleNeighborhood))
    exploration.toReturn should be(NoMoveFound)

    objValue := 900
    exploration.checkNeighborWP(objAfter => DummyMove(objAfter, new DummySimpleNeighborhood))
    exploration.toReturn.isInstanceOf[MoveFound] should be(true)
    exploration.toReturn.asInstanceOf[MoveFound].objAfter() should be(900)
  }

  test(
    "Minimize objective does not accept moves if under approximated objective is define " +
      "and condition not met"
  ) {
    val store               = new Store()
    val objValue            = IntVariable(store, 1000)
    val underApproxObjValue = IntVariable(store, 1000)
    val obj = Minimize(objValue, underApproximatedObjValue = Some(underApproxObjValue))
    store.close()

    // The objValue is voluntarily acceptable, be it shouldn't be check
    // if the under approximated value is not lower
    var exploration = obj.newExploration[DummyMove](new DummySimpleNeighborhood().searchProfiler())
    objValue            := 900
    underApproxObjValue := 2000
    exploration.checkNeighborWP(objAfter => DummyMove(objAfter, new DummySimpleNeighborhood))
    exploration.toReturn should be(NoMoveFound)

    objValue            := 900
    underApproxObjValue := 1000
    exploration.checkNeighborWP(objAfter => DummyMove(objAfter, new DummySimpleNeighborhood))
    exploration.toReturn should be(NoMoveFound)

    // Under approximated value is lower but the retained value should be objValue
    objValue            := 900
    underApproxObjValue := 850
    exploration.checkNeighborWP(objAfter => DummyMove(objAfter, new DummySimpleNeighborhood))
    exploration.toReturn.isInstanceOf[MoveFound] should be(true)
    exploration.toReturn.asInstanceOf[MoveFound].objAfter() should be(900)

    // Under approximated value is lower but the objValue is higher, should be rejected
    exploration = obj.newExploration[DummyMove](new DummySimpleNeighborhood().searchProfiler())
    objValue            := 1000
    underApproxObjValue := 850
    exploration.checkNeighborWP(objAfter => DummyMove(objAfter, new DummySimpleNeighborhood))
    exploration.toReturn should be(NoMoveFound)
  }

  test("Minimize objective does not accept moves if some constraints are not equal to zero") {
    val store       = new Store()
    val objValue    = IntVariable(store, 1000)
    val constraint1 = IntVariable(store, 0)
    val constraint2 = IntVariable(store, 1)
    val obj         = Minimize(objValue, mustBeZero = List(constraint1, constraint2))
    store.close()

    var exploration = obj.newExploration[DummyMove](new DummySimpleNeighborhood().searchProfiler())
    objValue := 900
    exploration.checkNeighborWP(objAfter => DummyMove(objAfter, new DummySimpleNeighborhood))
    exploration.toReturn should be(NoMoveFound)

    objValue    := 900
    constraint2 := 0
    exploration.checkNeighborWP(objAfter => DummyMove(objAfter, new DummySimpleNeighborhood))
    exploration.toReturn.isInstanceOf[MoveFound] should be(true)
    exploration.toReturn.asInstanceOf[MoveFound].objAfter() should be(900)

    // Constraint is not violated but obj value is higher, should be rejected
    exploration = obj.newExploration[DummyMove](new DummySimpleNeighborhood().searchProfiler())
    objValue := 1000
    exploration.checkNeighborWP(objAfter => DummyMove(objAfter, new DummySimpleNeighborhood))
    exploration.toReturn should be(NoMoveFound)
  }

}
