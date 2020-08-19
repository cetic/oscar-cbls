package oscar.cbls.business.scheduling.modeling

import oscar.cbls.business.scheduling.ActivityId
import oscar.cbls.business.scheduling.invariants.StartTimes
import oscar.cbls.business.scheduling.model.{Precedences, Resource}
import oscar.cbls.core.computation.{CBLSIntVar, ChangingSeqValue}

trait SchedulingInvariants {
  def startTimes(actPriorityList: ChangingSeqValue,
                 actDurations: Map[ActivityId, Int],
                 actPrecedences: Precedences,
                 actMinStartTimes: Map[ActivityId, Int],
                 resourceConstraints: List[Resource]): (CBLSIntVar, Map[ActivityId, CBLSIntVar]) = {
    StartTimes(actPriorityList, actDurations, actPrecedences, actMinStartTimes, resourceConstraints)
  }
}
