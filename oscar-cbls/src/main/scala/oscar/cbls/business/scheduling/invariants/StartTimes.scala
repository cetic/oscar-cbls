package oscar.cbls.business.scheduling.invariants

import oscar.cbls.CBLSIntVar
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.scheduling.ActivityId
import oscar.cbls.business.scheduling.model.{Precedences, Resource, ResourceState}
import oscar.cbls.core.computation.{SeqUpdate, SeqUpdateDefineCheckpoint, SeqUpdateInsert, SeqUpdateLastNotified, SeqUpdateMove, SeqUpdateRemove, SeqUpdateRollBackToCheckpoint}
import oscar.cbls.core.propagation.Checker
import oscar.cbls.core.{ChangingSeqValue, Invariant, SeqNotificationTarget}

import scala.annotation.tailrec
import scala.collection.BitSet

class StartTimes(actPriorityList: ChangingSeqValue,
                 actDurations: Map[ActivityId, Int],
                 actPrecedences: Precedences,
                 actMinStartTimes: Map[ActivityId, Int],
                 resources: List[Resource],
                 makeSpan: CBLSIntVar,
                 startTimes: Map[ActivityId, CBLSIntVar])
  extends Invariant with SeqNotificationTarget {
  // Invariant initialization
  registerStaticAndDynamicDependency(actPriorityList)
  finishInitialization()
  // Set defining invariant for output variables
  makeSpan.setDefiningInvariant(this)
  for {st <- startTimes.values} st.setDefiningInvariant(this)
  // Compute resources used by tasks
  var activityUsedResourceIndices: Map[ActivityId, Set[Int]] = Map()
  for {rcInd <- resources.indices} {
    resources(rcInd).usingActivities.foreach { act =>
      val actUsedResInd = activityUsedResourceIndices.getOrElse(act, Set())
      activityUsedResourceIndices += (act -> (actUsedResInd + rcInd))
    }
  }
  val resourceStates: Array[ResourceState] = new Array[ResourceState](resources.size)
  // Checkpoint data
  protected var checkpoint: IntSequence = actPriorityList.value
  var startTimeStates: Array[StartTimesData] = Array.tabulate(actDurations.size+1)(_ => StartTimesData.initial(resourceStates.length))
  startTimeStates(0).initResourceStates(0, resources)
  protected var startTimeStatesAtCheck: Array[StartTimesData] = _
  protected var currentPosition: Int = 0
  protected var positionAtCheck: Int = 0
  // Compute first start times
  computeStartTimesFrom(actPriorityList.value, 0)

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    if (!digestUpdates(changes)) {
      computeStartTimesFrom(v.value, 0)
    }
    scheduleForPropagation()
  }

  private def digestUpdates(changes: SeqUpdate): Boolean = {
    //TODO Incremental computation
    changes match {
      case SeqUpdateDefineCheckpoint(prev, _, checkpointLevel) =>
        // We only consider level 0; other are not managed.
        if (checkpointLevel == 0) {
          if (!digestUpdates(prev)) {
            computeStartTimesFrom(changes.newValue, 0)
          }
          checkpoint = changes.newValue
          positionAtCheck = currentPosition
          startTimeStatesAtCheck = startTimeStates.clone()
          true
        } else {
          digestUpdates(prev)
        }
      case SeqUpdateRollBackToCheckpoint(sequence, i) =>
        //require(sequence quickEquals this.checkpoint)
        startTimeStates = startTimeStatesAtCheck
        computeStartTimesFrom(sequence, positionAtCheck)
        true
      case SeqUpdateInsert(_, pos, prev) =>
        if (!digestUpdates(prev))
          false
        else {
          computeStartTimesFrom(changes.newValue, pos)
          true
        }
      case SeqUpdateMove(fromIncluded, toIncluded, after, _, prev) =>
        if (!digestUpdates(prev))
          false
        else {
          val pos = (after + 1) min fromIncluded min toIncluded
          computeStartTimesFrom(changes.newValue, pos)
          true
        }
      case SeqUpdateRemove(pos, prev) =>
        if (!digestUpdates(prev))
          false
        else {
          computeStartTimesFrom(changes.newValue, pos)
          true
        }
      case SeqUpdateLastNotified(value) =>
        false
      case _ =>
        false
    }
  }

  //TODO recompute when another dimension changes
  override def performInvariantPropagation(): Unit = {
    computeStartTimesFrom(actPriorityList.value, 0)
  }

  /** To override whenever possible to spot errors in invariants.
    * this will be called for each invariant after propagation is performed.
    * It requires that the Model is instantiated with the variable debug set to true.
    */
  override def checkInternals(c: Checker): Unit = {
    //TODO Implement this
  }

  // Compute the start times from a given position
  def computeStartTimesFrom(actPriorityList: IntSequence, pos: Int): Unit = {
    currentPosition = pos
    // Initialize explorer
    var seqExplorerOpt = actPriorityList.explorerAtPosition(pos)
    var makeSpanValue = 0L
    while (seqExplorerOpt.isDefined) {
      val seqExplorer = seqExplorerOpt.get
      val position = seqExplorer.position
      val actInd = seqExplorer.value
      val startTimesStateAtPos = startTimeStates(position)
      val startTimesValsAtPos = startTimesStateAtPos.startTimesVals
      val resourceStatesAtPos = startTimesStateAtPos.resourceStates
      val makeSpanValueAtPos = startTimesStateAtPos.makeSpanValue
      // Compute maximum ending time for preceding activities
      val maxEndTimePrecs = actPrecedences
        .predMap.getOrElse(actInd, BitSet.empty)
        .foldLeft(0) { (acc, precInd) =>
          if (actPriorityList.contains(precInd)) {
            acc max (startTimesValsAtPos(precInd) + actDurations(precInd))
          } else {
            acc
          }
        }
      // Compute maximum of earliest release time for all needed resources
      val maxReleaseResources = activityUsedResourceIndices
        .getOrElse(actInd, Set())
        .foldLeft(0) { (acc, resInd) =>
          acc max resourceStatesAtPos(resInd).earliestStartTime(actInd, 0)
        }
      // Getting the minimum start time for this task
      val minStartTime = actMinStartTimes.getOrElse(actInd, 0)
      val earliestStartTime = maxEndTimePrecs max maxReleaseResources max minStartTime
      val actEndTime = earliestStartTime + actDurations(actInd)
      makeSpanValue = if (actEndTime > makeSpanValueAtPos) {
        actEndTime
      } else {
        makeSpanValueAtPos
      }
      // Update resource states for next position (if it exists)
      seqExplorerOpt = seqExplorer.next
      if (seqExplorerOpt.isDefined) {
        val nextPos = seqExplorerOpt.get.position
        val startTimesStateAtNext = startTimeStates(nextPos)
        val resourceStatesAtNext = startTimesStateAtNext.resourceStates
        val actUsedResIndices = activityUsedResourceIndices.getOrElse(actInd, Set())
        // Update resource states for next position
        for {i <- resourceStatesAtPos.indices} {
          if (actUsedResIndices.contains(i)) {
            resourceStatesAtNext(i) = resourceStatesAtPos(i).nextState(actInd,
              actDurations(actInd),
              earliestStartTime)
          } else {
            resourceStatesAtNext(i) = resourceStatesAtPos(i)
          }
        }
        // Update start times for next position
        val startTimesValsAtNext= startTimesValsAtPos + (actInd -> earliestStartTime)
        // Update start time state for next position
        startTimeStates(nextPos) = StartTimesData(resourceStatesAtNext, makeSpanValue, startTimesValsAtNext)
      }
      // Update CBLS variable
      startTimes(actInd) := earliestStartTime
    }
    makeSpan := makeSpanValue
  }

  //TODO delete this when incrementality is stable
  // Compute the start times from scratch
  def computeStartTimes(actPriorityList: IntSequence): Unit = {
    @tailrec
    def initResourceStates(i: Int, resLs: List[Resource]): Unit = resLs match {
      case Nil => ()
      case r::rs =>
        resourceStates(i) = r.initialState
        initResourceStates(i+1, rs)
    }
    /////
    initResourceStates(0, resources)
    var makeSpanValue = 0L
    var startTimesVals: Map[ActivityId, Int] = Map()
    for {actInd <- actPriorityList} {
      val actIndI = actInd.toInt
      // Compute maximum ending time for preceding activities
      val maxEndTimePrecs = actPrecedences
        .predMap.getOrElse(actIndI, BitSet.empty)
        .foldLeft(0) { (acc, precInd) =>
          if (actPriorityList.contains(precInd)) {
            acc max (startTimesVals(precInd) + actDurations(precInd))
          } else {
            acc
          }
        }
      // Compute maximum of earliest release time for all needed resources
      val maxReleaseResources = activityUsedResourceIndices
        .getOrElse(actIndI, Set())
        .foldLeft(0) { (acc, resInd) =>
          acc max resourceStates(resInd).earliestStartTime(actIndI, 0)
        }
      // Getting the minimum start time for this task
      val minStartTime = actMinStartTimes.getOrElse(actIndI, 0)
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
            actDurations: Map[ActivityId, Int],
            actPrecedences: Precedences,
            actMinStartTimes: Map[ActivityId, Int] = Map(),
            resources: List[Resource]): (CBLSIntVar, Map[ActivityId, CBLSIntVar]) = {
    val model = actPriorityList.model
    val makeSpan = CBLSIntVar(model, 0L, name="Schedule Makespan")
    val startTimes: Map[ActivityId, CBLSIntVar] = actDurations.map { mapActDur =>
      val act = mapActDur._1
      act -> CBLSIntVar(model, 0L, name=s"Start Time of Activity($act)")
    }
    new StartTimes(actPriorityList, actDurations, actPrecedences, actMinStartTimes,
      resources, makeSpan, startTimes)
    (makeSpan, startTimes)
  }
}
