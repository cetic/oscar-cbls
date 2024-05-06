package oscar.cbls.core.computation.objective

import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.search.{Move, MoveFound, NoMoveFound, SearchResult}

object Maximize {
  def apply(objective: IntVariable,
            mustBeZero: List[IntVariable] = List.empty,
            approximatedObjective: Option[IntVariable] = None): Maximize = {
    new Maximize(objective, mustBeZero,approximatedObjective)
  }
}

class Maximize(
  objective: IntVariable,
  mustBeZero: List[IntVariable],
  approximatedObjective: Option[IntVariable]
) extends Objective {

  override def newExploration: Exploration = new Exploration {
    var toReturn: SearchResult = NoMoveFound
    private val oldObj = objective.value()

    private def checkNeighborOnApproximatedObjective(buildMove: Long => Move): Unit = {
      val newApproxObj = approximatedObjective.get.value()
      toReturn match {
        case NoMoveFound if newApproxObj > oldObj =>
          checkNeighborOnRealObjective(buildMove)
        case m: MoveFound if newApproxObj > m.objAfter =>
          checkNeighborOnRealObjective(buildMove)
        case _ => ;
      }
    }

    private def checkNeighborOnRealObjective(buildMove: Long => Move): Unit = {
      val newObj = objective.value()
      toReturn match {
        case NoMoveFound if newObj > oldObj      => toReturn = MoveFound(buildMove(newObj))
        case m: MoveFound if newObj > m.objAfter => toReturn = MoveFound(buildMove(newObj))
        case _                                   => ;
      }
    }

    override def checkNeighbor(buildMove: Long => Move): Unit = {
      if(!mustBeZero.exists(_.value() > 0)){
        if (approximatedObjective.isDefined) checkNeighborOnApproximatedObjective(buildMove)
        else checkNeighborOnRealObjective(buildMove)
      }
    }
  }
}
