package oscar.cbls.business.scheduling.model

import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.scheduling.ActivityId
import oscar.cbls.business.scheduling.invariants.StartTimes
import oscar.cbls.{CBLSSeqVar, Store}

import scala.collection.BitSet

class Schedule(model: Store,
               val activities: List[ActivityId],
               val initialActivities: List[ActivityId],
               durations: Map[ActivityId, Int],
               minStartTimes: Map[ActivityId, Int],
               precedencePairs: List[(ActivityId, ActivityId)],
               resources: List[Resource]) {

  // Precedences
  val precedencesData = new Precedences(precedencePairs)
  // Initial priority list
  val initialPriorityList: List[Int] = precedencesData.getPriorityList(initialActivities)

  println(s"Initial Activities: $initialActivities")
  println(s"Initial Priority List: $initialPriorityList")

  // CBLS variable: the priority list of activities to be scheduled
  val activityPriorityList = new CBLSSeqVar(model,
                                           IntSequence(initialPriorityList),
                                           n = "Activities' Priority List")
  // CBLS invariant: start times
  val (makeSpan, startTimes) = StartTimes(activityPriorityList, durations,
    precedencesData, minStartTimes, resources)

  /**
    * Given an activity index (indAct), obtain the sequence of indices that
    * can be swapped with indAct in the priority list
    *
    * @param indAct the index of the swapping activities
    * @return a sequence of activity indices
    */
  def swappableIndices(indAct: Int): Iterable[Int] = {
    val prioritySequence = activityPriorityList.value
    val currentActivity = prioritySequence.valueAtPosition(indAct).get
    val predActIndices = precedencesData.predMap.getOrElse(currentActivity, BitSet.empty)
    val succActIndices = precedencesData.succMap.getOrElse(currentActivity, BitSet.empty)
    var swappableIndices: List[Int] = List()
    // Determine the bounds of the swappable zone in the priority sequence
    // First loop: backward exploration to find last predecessor
    var optExplorer = prioritySequence.explorerAtPosition(indAct)
    var inLoop = true
    var exploredActs: List[Int] = List()
    do {
      optExplorer = optExplorer.get.prev
      if (optExplorer.isDefined) {
        val actExplorer = optExplorer.get
        val pos = actExplorer.position
        val activityAtPos = actExplorer.value
        exploredActs ::= activityAtPos
        if (predActIndices.contains(activityAtPos)) {
          inLoop = false
        } else {
          // Check whether the already explored activities
          // are not successors of the current explored activity
          val successorsOfActAtPos = precedencesData
            .succMap
            .getOrElse(activityAtPos, BitSet.empty)
          if (!exploredActs.exists(successorsOfActAtPos.contains)) {
            swappableIndices ::= pos
          }
        }
      } else {
        inLoop = false
      }
    } while (inLoop)
    // Second loop : forward exploration to find first successor
    optExplorer = prioritySequence.explorerAtPosition(indAct)
    inLoop = true
    exploredActs = List()
    do {
      optExplorer = optExplorer.get.next
      if (optExplorer.isDefined) {
        val actExplorer = optExplorer.get
        val pos = actExplorer.position
        val activityAtPos = actExplorer.value.toInt
        exploredActs ::= activityAtPos
        if (succActIndices.contains(activityAtPos)) {
          inLoop = false
        } else {
          // Check whether the already explored activities
          // are not predecessors of the current explored activity
          val predecessorsOfActAtPos = precedencesData
            .predMap
            .getOrElse(activityAtPos, BitSet.empty)
          if (!exploredActs.exists(predecessorsOfActAtPos.contains)) {
            swappableIndices ::= pos
          }
        }
      } else {
        inLoop = false
      }
    } while (inLoop)
    // The swappable indices are those between the bounds (excluded the bounds and
    // the index of the activity)
    swappableIndices
  }

  /**
    * Given an activity index (indAct), obtain the sequence of indices where
    * indAct can be reinserted in the priority list
    *
    * @param indAct the index of the swapping activities
    * @return a sequence of activity indices
    */
  def reinsertableIndices(indAct: Int): Iterable[Int] = {
    val prioritySequence = activityPriorityList.value
    val currentActivity = prioritySequence.valueAtPosition(indAct).get.toInt
    val predActIndices = precedencesData.predMap.getOrElse(currentActivity, BitSet.empty)
    val succActIndices = precedencesData.succMap.getOrElse(currentActivity, BitSet.empty)
    var reinsertableIndices: List[Int] = List()
    // Determine the bounds of the reinsertable zone in the priority sequence
    // First loop: backward exploration to find last predecessor
    var optExplorer = prioritySequence.explorerAtPosition(indAct)
    var inLoop = true
    do {
      optExplorer = optExplorer.get.prev
      if (optExplorer.isDefined) {
        val actExplorer = optExplorer.get
        val pos = actExplorer.position
        val activityAtPos = actExplorer.value.toInt
        if (predActIndices.contains(activityAtPos)) {
          inLoop = false
        } else {
          reinsertableIndices ::= pos
        }
      } else {
        inLoop = false
      }
    } while (inLoop)
    // Second loop : forward exploration to find first successor
    optExplorer = prioritySequence.explorerAtPosition(indAct)
    inLoop = true
    do {
      optExplorer = optExplorer.get.next
      if (optExplorer.isDefined) {
        val actExplorer = optExplorer.get
        val pos = actExplorer.position
        val activityAtPos = actExplorer.value.toInt
        if (succActIndices.contains(activityAtPos)) {
          inLoop = false
        } else {
          reinsertableIndices ::= pos
        }
      } else {
        inLoop = false
      }
    } while (inLoop)
    // The reinsertable indices are those between the bounds (excluded the bounds and
    // the index of the activity)
    reinsertableIndices
  }

  def insertableIndices(actValue: Long): Iterable[Int] = {
    val prioritySequence = activityPriorityList.value
    if (prioritySequence.isEmpty) {
      // The sequence is empty: the only insertable index is 0
      List(0)
    } else {
      val predActIndices = precedencesData.predMap.getOrElse(actValue.toInt, BitSet.empty)
      val succActIndices = precedencesData.succMap.getOrElse(actValue.toInt, BitSet.empty)
      // First loop: backward exploration to find last predecessor
      var lastPred = -1
      var firstSucc = prioritySequence.size
      var optExplorer = prioritySequence.explorerAtPosition(firstSucc - 1)
      var inLoop = true
      while (inLoop && optExplorer.isDefined) {
        val actExplorer = optExplorer.get
        val pos = actExplorer.position
        val activityAtPos = actExplorer.value.toInt
        if (predActIndices.contains(activityAtPos)) {
          inLoop = false
          lastPred = pos
        }
        optExplorer = actExplorer.prev
      }
      // Second loop: forward exploration to find first successor
      optExplorer = prioritySequence.explorerAtPosition(0)
      inLoop = true
      while (inLoop && optExplorer.isDefined) {
        val actExplorer = optExplorer.get
        val pos = actExplorer.position
        val activityAtPos = actExplorer.value.toInt
        if (succActIndices.contains(activityAtPos)) {
          inLoop = false
          firstSucc = pos
        }
        optExplorer = actExplorer.next
      }
      // The insertable indices are between the bounds
      (lastPred+1) until firstSucc
    }
  }
}
