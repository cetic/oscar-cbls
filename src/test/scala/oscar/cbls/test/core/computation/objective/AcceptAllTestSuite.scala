package oscar.cbls.test.core.computation.objective

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.objective.AcceptAll
import oscar.cbls.core.search.MoveFound
import oscar.cbls.lib.invariant.minmax.Min2
import oscar.cbls.lib.invariant.numeric.Int2Int
import oscar.cbls.lib.neighborhoods.AssignNeighborhood

import scala.util.Random

class AcceptAllTestSuite extends AnyFunSuite with Matchers {

  private val random = Random
  private val seed   = random.nextLong()
  random.setSeed(seed)

  test(s"No matter the new value, the new objValue is accepted.(seed = $seed)") {
    val model    = new Store()
    val objValue = IntVariable(model, 1000)
    model.close()

    val acceptAll = AcceptAll(objValue)
    var exploration =
      acceptAll.newExploration[DummyMove](new DummySimpleNeighborhood().searchProfiler())
    for (_ <- 0 until 100) {
      val newValue = random.nextLong()
      objValue := newValue
      exploration.checkNeighborWP(objAfter => DummyMove(objAfter, new DummySimpleNeighborhood()))
      exploration.toReturn.isInstanceOf[MoveFound] should be(true)
      exploration.toReturn.asInstanceOf[MoveFound].objAfter() should be(newValue)
      exploration = acceptAll.newExploration(new DummySimpleNeighborhood().searchProfiler())
    }
  }

  test(s"AcceptAll respects constraints") {
    val model    = new Store()
    val objValue = IntVariable(model, 0)
    val lesserThanFortyConstraint = IntVariable(model, 0)
    new Int2Int(model, objValue, lesserThanFortyConstraint, x => Math.max(x - 40,0))
    model.close()

    val assign = AssignNeighborhood(Array(objValue), (_, _) => List(42, 24))

    val acceptAll = AcceptAll(objValue, List(lesserThanFortyConstraint))
    assign.doAllMoves(acceptAll, shouldStop = _ >= 1)
    objValue.value() must be(24)
  }

  test(s"AcceptAll accepts moves that violate constraint") {
    val model    = new Store()
    val objValue = IntVariable(model, 0)
    val lesserThanFortyConstraint = IntVariable(model, 0)
    new Int2Int(model, objValue, lesserThanFortyConstraint, x => Math.max(x - 40,0))
    model.close()

    val assign = AssignNeighborhood(Array(objValue), (_, _) => List(42, 24))

    val acceptAll =
      AcceptAll(objValue, List(lesserThanFortyConstraint), allowsConstrainViolation = true)
    assign.doAllMoves(acceptAll, shouldStop = _ >= 1)
    objValue.value() must be(42)
  }

}
