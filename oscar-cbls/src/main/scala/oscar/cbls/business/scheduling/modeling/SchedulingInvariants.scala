package oscar.cbls.business.scheduling.modeling

import oscar.cbls.CBLSIntVar
import oscar.cbls.business.scheduling.Activity
import oscar.cbls.business.scheduling.invariants.StartTimes
import oscar.cbls.business.scheduling.model.{PrecedencesData, Resource}
import oscar.cbls.core.ChangingSeqValue

trait SchedulingInvariants {
  def startTimes(actPriorityList: ChangingSeqValue,
                 actDurations: Map[Activity, Long],
                 actPrecedences: PrecedencesData,
                 actMinStartTimes: Map[Activity, Long],
                 resourceConstraints: List[Resource]): (CBLSIntVar, Map[Activity, CBLSIntVar]) = {
    StartTimes(actPriorityList, actDurations, actPrecedences, actMinStartTimes, resourceConstraints)
  }
}
