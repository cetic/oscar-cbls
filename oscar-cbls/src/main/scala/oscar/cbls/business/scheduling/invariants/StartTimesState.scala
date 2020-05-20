package oscar.cbls.business.scheduling.invariants

import oscar.cbls.business.scheduling.ActivityId
import oscar.cbls.business.scheduling.model.{Resource, ResourceState}

import scala.collection.mutable

case class StartTimesState(resourceStates: mutable.Map[Resource, ResourceState],
                           makeSpanValue: Int,
                           activityId: ActivityId,
                           activityStartTime: Int)
