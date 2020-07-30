package oscar.cbls.business.scheduling.invariants

import oscar.cbls.business.scheduling.ActivityId
import oscar.cbls.business.scheduling.model.{Resource, ResourceState}

case class StartTimesState(resourceStates: Map[Resource, ResourceState],
                           makeSpanValue: Int,
                           activityId: ActivityId,
                           activityStartTime: Int)
