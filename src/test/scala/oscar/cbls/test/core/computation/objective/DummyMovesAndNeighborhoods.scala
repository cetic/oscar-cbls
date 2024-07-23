package oscar.cbls.test.core.computation.objective

import oscar.cbls.core.computation.objective.Exploration
import oscar.cbls.core.search._

class DummyMove(_objAfter: Long, simpleNeighborhood: DummySimpleNeighborhood)
    extends Move(_objAfter, simpleNeighborhood) {

  override def commit(): Unit = {}

  override def objAfter(): Long = _objAfter
}

class DummySimpleNeighborhood extends SimpleNeighborhood[DummyMove]("") {

  override def exploreNeighborhood(exploration: Exploration[DummyMove]): Unit = {}

  override def doMove(move: DummyMove): Unit = {}

  /** Resets the internal state of the neighborhood */
  override def reset(): Unit = {}
}
