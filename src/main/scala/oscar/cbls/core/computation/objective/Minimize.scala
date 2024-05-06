package oscar.cbls.core.computation.objective

import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.search.{Move, MoveFound, NoMoveFound, SearchResult}

object Minimize {
  def apply(objectiveVariable: IntVariable): Minimize ={
    new Minimize(objectiveVariable)
  }
}

class Minimize(objectiveVariable: IntVariable) extends Objective {

  override def newExploration: Exploration = new Exploration {
    var toReturn: SearchResult = NoMoveFound

    private val oldObj = objectiveVariable.value()

    override def checkNeighbor(buildMove: Long => Move): Unit = {
      val newObj = objectiveVariable.value()
      toReturn match {
        case NoMoveFound if newObj < oldObj =>
          toReturn = MoveFound(buildMove(newObj))
        case mf: MoveFound if newObj < mf.objAfter =>
          toReturn = MoveFound(buildMove(newObj))
        case _ => ;
      }
    }
  }
}
