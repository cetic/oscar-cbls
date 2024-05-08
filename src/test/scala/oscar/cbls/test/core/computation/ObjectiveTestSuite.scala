package oscar.cbls.test.core.computation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.objective.{AcceptAll, Exploration, Minimize, Objective}
import oscar.cbls.core.search.{Move, MoveFound, NoMoveFound, SearchResult}

import scala.util.Random

class ObjectiveTestSuite extends AnyFunSuite {

  test("Can't create an Objective with a constant Variable") {
    val store = new Store()
    val exception1 =
      intercept[IllegalArgumentException](Minimize(IntVariable(store, 10, isConstant = true)))

    assert(exception1.getMessage.contains("The objective value can not be constant"))
  }

  test("Can't create an Objective with a constant constraint") {
    val store = new Store()
    val exception1 =
      intercept[IllegalArgumentException](
        Minimize(
          IntVariable(store, 10),
          List(
            IntVariable(store, 10, isConstant = true),
            IntVariable(store, 10),
            IntVariable(store, 10)
          )
        )
      )

    assert(exception1.getMessage.contains("A constraint value can not be constant"))
  }

  test("Can't create an Objective with a constant approximated objective value") {
    val store = new Store()
    val exception1 =
      intercept[IllegalArgumentException](
        Minimize(
          IntVariable(store, 10),
          underApproximatedObjective = Some(IntVariable(store, 10, isConstant = true))
        )
      )

    assert(exception1.getMessage.contains("The approximated objective value can not be constant"))
  }

  test("Minimize objective does not accept moves that does not strictly decreases the value"){
    val store = new Store()
    val objValue = IntVariable(store,1000)
    val obj = Minimize(objValue)
    store.close()

    val exploration = obj.newExploration
    objValue := 2000
    exploration.checkNeighbor(objAfter => new DummyMove(objAfter))
    exploration.toReturn should be(NoMoveFound)

    objValue := 1000
    exploration.checkNeighbor(objAfter => new DummyMove(objAfter))
    exploration.toReturn should be(NoMoveFound)

    objValue := 900
    exploration.checkNeighbor(objAfter => new DummyMove(objAfter))
    exploration.toReturn.isInstanceOf[MoveFound] should be(true)
    exploration.toReturn.asInstanceOf[MoveFound].objAfter() should be(900)
  }

  test("Minimize objective does not accept moves if under approximated objective is define and condition not met"){
    val store = new Store()
    val objValue = IntVariable(store,1000)
    val underApproxObjValue = IntVariable(store,1000)
    val obj = Minimize(objValue, underApproximatedObjective = Some(underApproxObjValue))
    store.close()

    // The objValue is voluntarily acceptable, be it shouldn't be check
    // if the under approximated value is not lower
    val exploration = obj.newExploration
    objValue := 900
    underApproxObjValue := 2000
    exploration.checkNeighbor(objAfter => new DummyMove(objAfter))
    exploration.toReturn should be(NoMoveFound)

    objValue := 900
    underApproxObjValue := 1000
    exploration.checkNeighbor(objAfter => new DummyMove(objAfter))
    exploration.toReturn should be(NoMoveFound)

    // Under approximated value is lower but the retained value should be objValue
    objValue := 900
    underApproxObjValue := 950
    exploration.checkNeighbor(objAfter => new DummyMove(objAfter))
    exploration.toReturn.isInstanceOf[MoveFound] should be(true)
    exploration.toReturn.asInstanceOf[MoveFound].objAfter() should be(900)
  }

  test("Minimize objective does not accept moves if some constraints are not equal to zero"){
    val store = new Store()
    val objValue = IntVariable(store,1000)
    val constraint1 = IntVariable(store,0)
    val constraint2 = IntVariable(store,1)
    val obj = Minimize(objValue, mustBeZero = List(constraint1,constraint2))
    store.close()

    val exploration = obj.newExploration
    objValue := 900
    exploration.checkNeighbor(objAfter => new DummyMove(objAfter))
    exploration.toReturn should be(NoMoveFound)

    objValue := 900
    constraint2 := 0
    exploration.checkNeighbor(objAfter => new DummyMove(objAfter))
    exploration.toReturn.isInstanceOf[MoveFound] should be(true)
    exploration.toReturn.asInstanceOf[MoveFound].objAfter() should be(900)
  }

  test("Simulating DynAndThen behavior works as expected"){

    // Simulating a DynAndThen process
    // Only the combination of two moves is checked given acceptance criterion
    val store = new Store()
    val objValue = IntVariable(store,1000)
    val minimize = Minimize(objValue)
    store.close()

    var objVariations: List[Long] = List(-800,500,-800,500,-800,500)
    var summedVariations: Long = 0L


    def getMove(exploration: Exploration, obj: IntVariable): SearchResult = {
      obj :+= objVariations.head
      summedVariations += objVariations.head
      objVariations = objVariations.tail
      exploration.checkNeighbor(objAfter => new DummyMove(objAfter))
      exploration.toReturn
    }

    def getDynAndThenMove(exploration: Exploration, obj: IntVariable): SearchResult ={
      getMove(AcceptAll(obj).newExploration, obj) match {
        case mf: MoveFound =>
          mf.objAfter() should be(1000 + summedVariations)
          getMove(exploration,obj) match {
            case mf: MoveFound =>
              mf.objAfter() should be(1000 + summedVariations)
              mf
            case NoMoveFound =>
              require(false, "A move should have been found")
              NoMoveFound
          }
        case NoMoveFound =>
          require(false, "A move should have been found")
          NoMoveFound
      }
    }

    def startExploration(objective: Objective, skipDynAndThen: Boolean = false): SearchResult ={
      val expl = objective.newExploration
      if(skipDynAndThen) getMove(expl, objValue)
      else getDynAndThenMove(expl, objValue)
    }

    for(_ <- 0 until 2) {
      val move = startExploration(minimize)
      move match {
        case mf: MoveFound =>
          mf.objAfter() should be(1000 + summedVariations)
        case NoMoveFound =>
          require(false, "A resulting move should have been found")
      }
    }

    // Now if we commit between the two moves it should fail
    startExploration(AcceptAll(objValue),skipDynAndThen = true).asInstanceOf[MoveFound].commit()
    startExploration(minimize,skipDynAndThen = true) should be(NoMoveFound)
  }

  test("Simulating alternative meta heuristic as a combinator works as expected"){
    def startExploration(objective: Objective): Unit = {

    }

    def getMetaHeuristicMove(explorer: Exploration, obj: IntVariable): Unit = {

    }

    class SpecificExploration(obj: IntVariable) extends Exploration {

      override val oldObj: Long = obj.value()
      override var toReturn: SearchResult = NoMoveFound

      override def checkNeighbor(buildMove: Long => Move): Unit = {
        val newValue = obj.value()
        if(newValue%2 == 0) toReturn = MoveFound(new DummyMove(newValue))
        else NoMoveFound
      }
    }
  }

}

private class DummyMove(_objAfter: Long) extends Move{

  override def commit(): Unit = {}

  override def objAfter(): Long = _objAfter
}
