package oscar.cbls.core.computation.objective

import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.search.{Move, MoveFound, NoMoveFound, SearchResult}

object AcceptAll{
  def apply(objective: IntVariable): AcceptAll ={
    new AcceptAll(objective)
  }
}

class AcceptAll(objective: IntVariable) extends Objective {
  override def newExploration: Exploration = new Exploration {
    override var toReturn: SearchResult = NoMoveFound
    override val oldObj: Long = objective.value()

    override def checkNeighbor(buildMove: Long => Move): Unit = {
      val newValue = objective.value()
      toReturn = MoveFound(buildMove(newValue))
    }
  }
}
