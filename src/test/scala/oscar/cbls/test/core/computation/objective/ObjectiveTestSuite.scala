package oscar.cbls.test.core.computation.objective

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.objective.{AcceptAll, Exploration, Minimize, Objective}
import oscar.cbls.core.search.{Move, MoveFound, NoMoveFound, SearchResult}

class ObjectiveTestSuite extends AnyFunSuite {

  test("Simulating DynAndThen behavior works as expected"){

    // Simulating a DynAndThen process
    // Only the combination of two moves is checked given acceptance criterion
    val store = new Store()
    val solValue = IntVariable(store,1000)
    val minimize = Minimize(solValue)
    store.close()

    var solVariations: List[Long] = List(-800,500,-800,500,-800,500)
    var summedVariations: Long = 0L


    def getMove(exploration: Exploration, solution: IntVariable): SearchResult = {
      solution :+= solVariations.head
      summedVariations += solVariations.head
      solVariations = solVariations.tail
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
      if(skipDynAndThen) getMove(expl, solValue)
      else getDynAndThenMove(expl, solValue)
    }

    // Two dynAndThen moves (going from 1000 to 700 (-800 + 500) and from 700 to 400 (-800 + 500)
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
    startExploration(AcceptAll(solValue),skipDynAndThen = true).asInstanceOf[MoveFound].commit()
    // obj value is now -400
    // -400 +500 > -400 ==> FAIL
    startExploration(minimize,skipDynAndThen = true) should be(NoMoveFound)
  }

}

private class DummyMove(_objAfter: Long) extends Move{

  override def commit(): Unit = {}

  override def objAfter(): Long = _objAfter
}
