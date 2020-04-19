package oscar.cbls.business.scheduling.invariants

import oscar.cbls.business.scheduling.ActivityId
import oscar.cbls.business.scheduling.model.{Resource, ResourceState}

import scala.annotation.tailrec

case class StartTimesData(resourceStates: Array[ResourceState],
                          makeSpanValue: Long,
                          startTimesVals: Map[ActivityId, Int]){
  @tailrec
  final def initResourceStates(i: Int, resLs: List[Resource]): Unit = resLs match {
    case Nil => ()
    case r::rs =>
      resourceStates(i) = r.initialState
      initResourceStates(i+1, rs)
  }
}

object StartTimesData {
  def initial(nbMaxActivities: Int): StartTimesData = {
    StartTimesData(new Array[ResourceState](nbMaxActivities), 0L, Map())
  }
}