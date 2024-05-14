package oscar.cbls.test.core.computation.objective

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.objective.AcceptAll
import oscar.cbls.core.search.MoveFound
import oscar.cbls.test.core.computation.DummyMove

import scala.util.Random

class AcceptAllTestSuite extends AnyFunSuite {

  private val random = Random
  private val seed = random.nextLong()
  random.setSeed(seed)

  test(s"No matter the new value, the solution is accepted.(seed = $seed)"){
    val model = new Store()
    val solutionValue = IntVariable(model, 1000)
    model.close()

    val acceptAll = AcceptAll(solutionValue)
    var exploration = acceptAll.newExploration
    for(_ <- 0 until 100) {
      val newValue = random.nextLong()
      solutionValue := newValue
      exploration.checkNeighbor(objAfter => new DummyMove(objAfter))
      exploration.toReturn.isInstanceOf[MoveFound] should be(true)
      exploration.toReturn.asInstanceOf[MoveFound].objAfter() should be(newValue)
      exploration = acceptAll.newExploration
    }
  }

}
