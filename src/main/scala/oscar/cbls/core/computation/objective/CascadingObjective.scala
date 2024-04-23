package oscar.cbls.core.computation.objective

object CascadingObjective {
  def apply(objectives: List[Objective]): CascadingObjective ={
    require(objectives.nonEmpty, "Building CascadingObjective with an empty list of Objectives")
    require(objectives.size >= 2, "Building CascadingObjective with less than 2 Objectives")

    def buildCascading(objectives: List[Objective]):Objective = {
      objectives match {
        case head :: Nil => head
        case head :: tail => CascadingObjective(head, buildCascading(tail))
      }
    }

    CascadingObjective(objectives.head,buildCascading(objectives.tail))
  }

  def apply(mustBeZeroObjective: Objective, secondObjective: Objective): CascadingObjective = {
    new CascadingObjective(mustBeZeroObjective,secondObjective)
  }
}

class CascadingObjective(mustBeZeroObjective: Objective, secondObjective: Objective) extends Objective {
  override def value(): Long = {
    if(mustBeZeroObjective.value() != 0) Long.MaxValue
    else secondObjective.value()
  }
}
