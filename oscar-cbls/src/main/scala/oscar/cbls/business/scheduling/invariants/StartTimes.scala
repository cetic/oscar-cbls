package oscar.cbls.business.scheduling.invariants

import oscar.cbls.CBLSIntVar
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.scheduling.Activity
import oscar.cbls.business.scheduling.model.{PrecedencesData, Resource, ResourceState}
import oscar.cbls.core.computation.SeqUpdate
import oscar.cbls.core.propagation.Checker
import oscar.cbls.core.{ChangingSeqValue, Invariant, SeqNotificationTarget}

import scala.collection.BitSet

class StartTimes(actPriorityList: ChangingSeqValue,
                 actDurations: Map[Activity, Long],
                 actPrecedences: PrecedencesData,
                 actMinStartTimes: Map[Activity, Long],
                 resources: List[Resource],
                 makeSpan: CBLSIntVar,
                 startTimes: Map[Activity, CBLSIntVar])
  extends Invariant with SeqNotificationTarget {
  // Invariant initialization
  registerStaticAndDynamicDependency(actPriorityList)
  finishInitialization()
  // Set defining invariant for output variables
  makeSpan.setDefiningInvariant(this)
  for {st <- startTimes.values} st.setDefiningInvariant(this)
  // Compute resources used by tasks
  var activityUsedResourceIndices: Map[Activity, Set[Int]] = Map()
  for {rcInd <- resources.indices} {
    resources(rcInd).usingActivities.foreach { act =>
      val actUsedResInd = activityUsedResourceIndices.getOrElse(act, Set())
      activityUsedResourceIndices += (act -> (actUsedResInd + rcInd))
    }
  }
  // Compute first start times
  computeStartTimes(actPriorityList.value)

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    //TODO Incremental computation
    scheduleForPropagation()
  }

  //TODO recompute when another dimension changes
  override def performInvariantPropagation(): Unit = {
    computeStartTimes(actPriorityList.value)
  }

  /** To override whenever possible to spot errors in invariants.
    * this will be called for each invariant after propagation is performed.
    * It requires that the Model is instantiated with the variable debug set to true.
    */
  override def checkInternals(c: Checker): Unit = {
    //TODO Implement this
  }

  // Compute the start times
  def computeStartTimes(actPriorityList: IntSequence): Unit = {
    val resourceStates: Array[ResourceState] = resources.map(_.initialState).toArray
    var makeSpanValue = 0L
    var startTimesVals: Map[Activity, Long] = Map()
    for {actInd <- actPriorityList} {
      val actIndI = actInd.toInt
      // Compute maximum ending time for preceding activities
      val maxEndTimePrecs = actPrecedences
        .predMap.getOrElse(actIndI, BitSet.empty)
        .filter(actPriorityList.contains(_))
        .foldLeft(0L) { (acc, precInd) =>
          acc max (startTimesVals(precInd) + actDurations(precInd))
        }
      // Compute maximum of earliest release time for all needed resources
      val maxReleaseResources = activityUsedResourceIndices
        .getOrElse(actIndI, Set())
        .foldLeft(0L) { (acc, resInd) =>
          acc max resourceStates(resInd).earliestStartTime(actIndI, 0L)
        }
      // Getting the minimum start time for this task
      val minStartTime = actMinStartTimes.getOrElse(actIndI, 0L)
      val earliestStartTime = maxEndTimePrecs max maxReleaseResources max minStartTime
      // Update resource states
      activityUsedResourceIndices
        .getOrElse(actIndI, Set())
        .foreach { resInd =>
          resourceStates(resInd) = resourceStates(resInd).nextState(actIndI,
            actDurations(actIndI),
            earliestStartTime)
        }
      val actEndTime = earliestStartTime + actDurations(actIndI)
      if (actEndTime > makeSpanValue) {
        makeSpanValue = actEndTime
      }
      startTimesVals += (actIndI -> earliestStartTime)
      startTimes(actIndI) := startTimesVals(actIndI)
    }
    makeSpan := makeSpanValue
  }
}

object StartTimes {
  def apply(actPriorityList: ChangingSeqValue,
            actDurations: Map[Activity, Long],
            actPrecedences: PrecedencesData,
            actMinStartTimes: Map[Activity, Long] = Map(),
            resources: List[Resource]): (CBLSIntVar, Map[Activity, CBLSIntVar]) = {
    val model = actPriorityList.model
    val makeSpan = CBLSIntVar(model, 0L, name="Schedule Makespan")
    val startTimes: Map[Activity, CBLSIntVar] = actDurations.map { mapActDur =>
      val act = mapActDur._1
      act -> CBLSIntVar(model, 0L, name=s"Start Time of Activity($act)")
    }
    new StartTimes(actPriorityList, actDurations, actPrecedences, actMinStartTimes,
      resources, makeSpan, startTimes)
    (makeSpan, startTimes)
  }
}
