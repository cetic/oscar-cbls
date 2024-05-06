package oscar.cbls.core.computation.objective

import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.search.{Move, MoveFound, NoMoveFound, SearchResult}


object MinimizeWithSubApproximation{
  def apply(mainObjective: IntVariable, underApproximationObjective: IntVariable): MinimizeWithSubApproximation ={
    mainObjective.model.registerForPartialPropagation(mainObjective)
    underApproximationObjective.model.registerForPartialPropagation(underApproximationObjective)
    new MinimizeWithSubApproximation(mainObjective, underApproximationObjective)
  }
}

class MinimizeWithSubApproximation(obj: IntVariable, underApproximationObjective: IntVariable) extends Objective {

  override def newExploration: Exploration = new Exploration {
    var toReturn: SearchResult = NoMoveFound
    val oldObj: Long = obj.value()

    private def checkNeighborOnRealObj(buildMove: Long => Move): Unit = {
      val newObj = obj.value()
      toReturn match {
        case NoMoveFound if newObj < oldObj => toReturn = MoveFound(buildMove(newObj))
        case mf: MoveFound if newObj < mf.objAfter => toReturn = MoveFound(buildMove(newObj))
        case _ => ;
      }
    }

    def checkNeighbor(buildMove: Long => Move): Unit = {
      val newApproxObj = underApproximationObjective.value()
      toReturn match {
        case NoMoveFound if newApproxObj < oldObj =>
          checkNeighborOnRealObj(buildMove)
        case mf: MoveFound if newApproxObj < mf.objAfter =>
          checkNeighborOnRealObj(buildMove)
        case _ => ;
      }
    }
  }
}
