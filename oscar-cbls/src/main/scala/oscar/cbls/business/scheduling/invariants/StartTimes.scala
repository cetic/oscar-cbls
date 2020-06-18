package oscar.cbls.business.scheduling.invariants

import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.scheduling.ActivityId
import oscar.cbls.business.scheduling.model.{Precedences, Resource, ResourceState}
import oscar.cbls.core.computation.{CBLSIntVar, ChangingSeqValue, Invariant, SeqNotificationTarget, SeqUpdate, SeqUpdateAssign, SeqUpdateDefineCheckpoint, SeqUpdateInsert, SeqUpdateLastNotified, SeqUpdateMove, SeqUpdateRemove, SeqUpdateRollBackToCheckpoint}
import oscar.cbls.core.propagation.Checker

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
  val (activityUsedResources, initialResourceStates): (Map[ActivityId, Set[Resource]], Map[Resource, ResourceState]) =
    resources.foldLeft(Map(): Map[ActivityId, Set[Resource]], Map(): Map[Resource, ResourceState]) { (accTup, res) =>
      val accAUR = accTup._1
      val accIRS = accTup._2
      val newAccAUR = res.usingActivities.foldLeft(accAUR) { (accAUR1, act) =>
        val actUsedRes = accAUR1.getOrElse(act, Set())
        accAUR1 + (act -> (actUsedRes + res))
      }
      val newAccIRS = accIRS + (res -> res.initialState)
      (newAccAUR, newAccIRS)
    }
  // Checkpoint data
  val numActivities: Int = actDurations.size
  val initialSTState: StartTimesState = StartTimesState(initialResourceStates, 0, StartTimes.NO_ACTIVITY, 0)
  val forwardStatesSequence: Array[(String, Array[StartTimesState])] = new Array[(String, Array[StartTimesState])](numActivities)
  var nextIndex: Int = 0
  // Compute first start times
  computeStartTimesFrom(actPriorityList.value, 0)

  private def findIndex(index: String): Option[(String, Array[StartTimesState])] = {
    var notFound = true
    var i = 0
    var res: Option[(String, Array[StartTimesState])] = None
    while (notFound && (i < numActivities)) {
      val seqI = forwardStatesSequence(i)
      if ((seqI != null) && (seqI._1 == index)) {
        notFound = false
        res = Some(seqI)
      }
      i += 1
    }
    res
  }

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    val boundsOpt = digestUpdates(changes)
    if (boundsOpt.isDefined) {
      val bounds = boundsOpt.get
      val prevSeq = bounds._3
      computeStartTimesFrom(prevSeq, bounds._1)
    }
  }

  private def digestUpdates(changes: SeqUpdate): Option[(Int, Int, IntSequence, Option[IntSequence])] = {
    //TODO Test and improve
    val changesValue = changes.newValue
    val lastValuePos = changesValue.size - 1
    changes match {
      case SeqUpdateDefineCheckpoint(_, _, _) =>
        Some(0, lastValuePos, changesValue, None)
      case r@SeqUpdateRollBackToCheckpoint(_, _) =>
        val rbkValue = r.howToRollBack.newValue // checkpoint value
        Some(0, rbkValue.size-1, rbkValue, None)
      case SeqUpdateInsert(_, pos, prev) =>
        val boundsPrevOpt = digestUpdates(prev)
        if (boundsPrevOpt.isEmpty)
          Some(0, lastValuePos, changesValue, None)
        else {
          val prevSeq = prev.newValue
          Some(pos, pos + 1, changesValue, Some(prevSeq))
        }
      case SeqUpdateMove(fromIncluded, toIncluded, after, _, prev) =>
        val boundsPrevOpt = digestUpdates(prev)
        if (boundsPrevOpt.isEmpty)
          Some(0, lastValuePos, changesValue, None)
        else {
          val prevSeq = prev.newValue
          val posMin = (after + 1) min fromIncluded min toIncluded
          val posMax = (after + 1) max fromIncluded max toIncluded
          Some(posMin, posMax, changesValue, Some(prevSeq))
        }
      case SeqUpdateRemove(pos, prev) =>
        val boundsPrevOpt = digestUpdates(prev)
        if (boundsPrevOpt.isEmpty)
          Some(0, lastValuePos, changesValue, None)
        else {
          val prevSeq = prev.newValue
          Some(pos, pos + 1, changesValue, Some(prevSeq))
        }
      case SeqUpdateLastNotified(value) =>
        Some(0, value.size-1, value, None)
      case SeqUpdateAssign(value) =>
        Some(0, value.size-1, value, None)
      case _ =>
        None
    }
  }

  /** To override whenever possible to spot errors in invariants.
    * this will be called for each invariant after propagation is performed.
    * It requires that the Model is instantiated with the variable debug set to true.
    */
  override def checkInternals(c: Checker): Unit = {
    //TODO Implement this
  }

  // Compute the start times from a given position
  def computeStartTimesFrom(actPriorityList: IntSequence,
                            pos: Int,
                            prevList: Option[IntSequence] = None): Unit = {
    val keyMap = actPriorityList.mkString(",")
    val optSeq = findIndex(keyMap)
    if (optSeq.isDefined) {
      // If there already exists the value for this sequence
      val arrStates = optSeq.get._2
      for {i <- arrStates.indices} {
        val stateI = arrStates(i)
        startTimes(stateI.activityId) := stateI.activityStartTime
      }
      makeSpan := arrStates(arrStates.length-1).makeSpanValue
    } else {
      var makeSpanValue = 0
      var startTimesVals: Map[ActivityId, Int] = Map()
      val nextArrayStates: Array[StartTimesState] = new Array[StartTimesState](actPriorityList.size)
      val startPos = if (prevList.isDefined) {
        val keyMapPrev = prevList.get.mkString(",")
        val optSeqPrev = findIndex(keyMapPrev)
        if (optSeqPrev.isDefined) {
          val prevArrayStates: Array[StartTimesState] = optSeqPrev.get._2
          // Put the values for CBLS variables in prefix from previous sequence
          for {i <- 0 until pos} {
            val stateI = prevArrayStates(i)
            nextArrayStates(i) = stateI
            startTimesVals += stateI.activityId -> stateI.activityStartTime
            startTimes(stateI.activityId) := stateI.activityStartTime
            makeSpanValue = stateI.makeSpanValue
          }
          pos
        } else 0
      } else 0
      // Initialize explorer from startPos
      var seqExplorerOpt = actPriorityList.explorerAtPosition(startPos)
      while (seqExplorerOpt.isDefined) {
        val seqExplorer = seqExplorerOpt.get
        val position = seqExplorer.position
        val actInd = seqExplorer.value
        val stStateBeforePos = if (position == 0) initialSTState else nextArrayStates(position-1)
        // Compute maximum ending time for preceding activities
        val maxEndTimePrecs = actPrecedences
          .predMap.getOrElse(actInd, BitSet.empty)
          .foldLeft(0) { (acc, precInd) =>
            if (actPriorityList.contains(precInd)) {
              acc max (startTimesVals(precInd) + actDurations(precInd))
            } else {
              acc
            }
          }
        // Compute maximum of earliest release time for all needed resources
        val actUsedResources = activityUsedResources.getOrElse(actInd, Set())
        val maxReleaseResources = actUsedResources.foldLeft(0) { (acc, res) =>
            acc max stStateBeforePos.resourceStates(res).earliestStartTime(actInd, 0)
          }
        // Getting the minimum start time for this task
        val minStartTime = actMinStartTimes.getOrElse(actInd, 0)
        val earliestStartTime = maxEndTimePrecs max maxReleaseResources max minStartTime
        val actEndTime = earliestStartTime + actDurations(actInd)
        makeSpanValue = if (actEndTime > stStateBeforePos.makeSpanValue) {
          actEndTime
        } else {
          stStateBeforePos.makeSpanValue
        }
        // Update resource states
        val nextResState = actUsedResources
          .foldLeft(stStateBeforePos.resourceStates) { (accM, res) =>
          accM + (res ->
            stStateBeforePos.resourceStates(res).nextState(actInd, actDurations(actInd), earliestStartTime))
        }
        val stStateAtPos = StartTimesState(nextResState, makeSpanValue, actInd, earliestStartTime)
        nextArrayStates(position) = stStateAtPos
        // Update CBLS variable
        startTimesVals += actInd -> earliestStartTime
        startTimes(actInd) := earliestStartTime
        seqExplorerOpt = seqExplorer.next
      }
      // Update makeSpan variable
      makeSpan := makeSpanValue
      // Update map
      forwardStatesSequence(nextIndex) = (keyMap, nextArrayStates)
      nextIndex = (nextIndex+1)%numActivities
    }
  }
}

object StartTimes {
  final val NO_ACTIVITY = -1

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
