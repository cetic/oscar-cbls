package oscar.cbls.core.computation.objective

import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.search.{Move, MoveFound, NoMoveFound, SearchResult}

object Minimize {
  def apply(
    objective: IntVariable,
    mustBeZero: List[IntVariable] = List.empty,
    underApproximatedObjective: Option[IntVariable] = None
  ): Minimize = {
    require(!objective.isConstant, "The objective value can not be constant")
    require(!mustBeZero.exists(_.isConstant), "A constraint value can not be constant")
    require(
      underApproximatedObjective.isEmpty || !underApproximatedObjective.get.isConstant,
      "The approximated objective value can not be constant"
    )
    objective.model.registerForPartialPropagation(objective)
    mustBeZero.foreach(mbz => mbz.model.registerForPartialPropagation(mbz))
    underApproximatedObjective.foreach(ao => ao.model.registerForPartialPropagation(ao))
    new Minimize(objective, mustBeZero, underApproximatedObjective)
  }
}

class Minimize(
  objective: IntVariable,
  mustBeZero: List[IntVariable],
  underApproximatedObjective: Option[IntVariable]
) extends Objective {

  override def newExploration: Exploration = new Exploration {
    var toReturn: SearchResult = NoMoveFound
    val oldObj: Long = objective.value()

    private def checkNeighborOnApproximatedObjective(buildMove: Long => Move): Unit = {
      val newApproxObj = underApproximatedObjective.get.value()
      toReturn match {
        case NoMoveFound if newApproxObj < oldObj =>
          checkNeighborOnRealObjective(buildMove)
        case m: MoveFound if newApproxObj < m.objAfter =>
          checkNeighborOnRealObjective(buildMove)
        case _ => ;
      }
    }

    private def checkNeighborOnRealObjective(buildMove: Long => Move): Unit = {
      val newObj = objective.value()
      toReturn match {
        case NoMoveFound if newObj < oldObj      => toReturn = MoveFound(buildMove(newObj))
        case m: MoveFound if newObj < m.objAfter => toReturn = MoveFound(buildMove(newObj))
        case _                                   => ;
      }
    }

    override def checkNeighbor(buildMove: Long => Move): Unit = {
      if (!mustBeZero.exists(_.value() > 0)) {
        if (underApproximatedObjective.isDefined) checkNeighborOnApproximatedObjective(buildMove)
        else checkNeighborOnRealObjective(buildMove)
      }
    }
  }
}
