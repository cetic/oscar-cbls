package oscar.cbls.business.scheduling.neighborhood

import oscar.cbls.core.search.{Move, Neighborhood}

abstract class SchedulingMove(val neighborhood: Neighborhood,
                              override val neighborhoodName: String = "",
                              override val objAfter: Long)
  extends Move(objAfter, neighborhoodName) {

  def impactedActivities: Iterable[Int]
}
