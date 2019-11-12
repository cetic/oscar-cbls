package oscar.cbls.business.scheduling.model

import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.scheduling.Activity
import oscar.cbls.business.scheduling.invariants.StartTimes
import oscar.cbls.{CBLSSeqVar, Store}

import scala.collection.BitSet

class Schedule(model: Store,
               activities: List[ActivityData],
               precedences: List[(Activity, Activity)],
               resources: List[Resource]) {
  var mandatoryActivities: List[Activity] = Nil
  var optionalActivities: List[Activity] = Nil
  var flexibleActivities: List[Activity] = Nil
  var activityDurations: Map[Activity, Long] = Map()
  var activityMinStartTimes: Map[Activity, Long] = Map()
  var activityTypes: Map[Activity, ActivityType] = Map()
  // Take activities data
  activities.foreach(actData => {
    actData.`type` match {
      case Mandatory =>
        mandatoryActivities ::= actData.activity
      case Optional =>
        optionalActivities ::= actData.activity
      case Flexible =>
        flexibleActivities ::= actData.activity
    }
    activityDurations += (actData.activity -> actData.duration)
    activityMinStartTimes += (actData.activity -> actData.minStartTime)
    activityTypes += (actData.activity -> actData.`type`)
  })
  // Precedences
  val precedencesData = new PrecedencesData(precedences)
  // Maps of clusters of flexibles
  var mapClusterFlexibles: Map[Activity, (BitSet, BitSet)] = Map()
  var mapFlexibleClusters: Map[(BitSet, BitSet), Set[Activity]] = Map()
  flexibleActivities.foreach(act => {
    val predAct = precedencesData.predMap.getOrElse(act, BitSet.empty)
    val succAct = precedencesData.succMap.getOrElse(act, BitSet.empty)
    mapClusterFlexibles += (act -> (predAct,succAct))
    val flexClusters = mapFlexibleClusters.getOrElse((predAct,succAct), Set())
    mapFlexibleClusters += ((predAct,succAct) -> (flexClusters + act))
  })
  // List of initial activities: it is composed of:
  // * The mandatory activities
  // * One activity of each flexible cluster
  val flexibleInits: List[Activity] = mapFlexibleClusters.foldLeft(Nil: List[Activity])(
    (acc, flexPair) => {
      val flexCluster = flexPair._2
      val flexAct = flexCluster.head
      flexAct::acc
    }
  )
  val initialActivities: List[Activity] = flexibleInits ::: mandatoryActivities
  val initialPriorityList: List[Long] = precedencesData.getPriorityList(initialActivities)

  println(s"Initial Activities: $initialActivities")
  println(s"Initial Priority List: $initialPriorityList")

  // CBLS variable: the priority list of activities to be scheduled
  val activityPriorityList = new CBLSSeqVar(model,
                                           IntSequence(initialPriorityList),
                                           n = "Activities' Priority List")
  // CBLS invariant: start times
  val (makeSpan, startTimes) = StartTimes(activityPriorityList,
    activityDurations,
    precedencesData,
    activityMinStartTimes,
    resources)

  /**
    * Given an activity index (indAct), obtain the sequence of indices that
    * can be swapped with indAct in the priority list
    *
    * @param indAct the index of the swapping activities
    * @return a sequence of activity indices
    */
  def swappableIndices(indAct: Int): Iterable[Int] = {
    val prioritySequence = activityPriorityList.value
    val currentActivity = prioritySequence.valueAtPosition(indAct).get.toInt
    val predActIndices = precedencesData.predMap.getOrElse(currentActivity, BitSet.empty)
    val hasPredecessors = predActIndices.exists(p => prioritySequence.contains(p))
    val succActIndices = precedencesData.succMap.getOrElse(currentActivity, BitSet.empty)
    val hasSuccessors = succActIndices.exists(s => prioritySequence.contains(s))
    // Determine the bounds of the swappable zone in the priority sequence
    var lastPrecSeqIndex = -1
    var firstSuccSeqIndex = prioritySequence.size
    var seqIndexIndAct = -1
    var inLoop = true
    var optExplorer = prioritySequence.explorerAtPosition(0)
    while (inLoop && optExplorer.isDefined) {
      val actExplorer = optExplorer.get
      val i = actExplorer.position
      val activityAtI = actExplorer.value.toInt
      // is i the index for the activity indAct ?
      if (i == indAct) {
        seqIndexIndAct = i
        inLoop = hasSuccessors
      } else if (seqIndexIndAct == -1 && hasPredecessors) {
        // we check if this index is a predecessor
        if (predActIndices.contains(activityAtI)) {
          lastPrecSeqIndex = i
        }
      } else if (seqIndexIndAct != -1) {
        // notice that this implies hasSuccessors == true, otherwise the loop should
        // have stopped
        if (succActIndices.contains(activityAtI)) {
          firstSuccSeqIndex = i
          // we can stop loop because we found the first successor
          inLoop = false
        }
      }
      optExplorer = actExplorer.next
    }
    // The swappable indices are those between the bounds (excluded the bounds and
    // the index of the activity)
    var swappableIndices: List[Int] = List()
    for { i <- lastPrecSeqIndex+1 until firstSuccSeqIndex if i != seqIndexIndAct } {
      optExplorer = prioritySequence.explorerAtPosition(i)
      val activityAtI = optExplorer.get.value.toInt
      if (i > seqIndexIndAct) {
        // if index i is after indAct, check that there is no predecessor of i
        // between indAct and i, in that case i, indAct are not swappable
        val predecessorsOfI = precedencesData
          .predMap
          .getOrElse(activityAtI, BitSet.empty)
          .filter(p => prioritySequence.contains(p))
        var noPrecBetweenIndAct_I = true
        for { j <- indAct+1 until i if noPrecBetweenIndAct_I} {
          optExplorer = prioritySequence.explorerAtPosition(j)
          val activityAtJ = optExplorer.get.value.toInt
          noPrecBetweenIndAct_I = !predecessorsOfI.contains(activityAtJ)
        }
        if (noPrecBetweenIndAct_I) {
          swappableIndices :+= i
        }
      }
      else {
        // if index i is before indAct, check that there is no successor of i
        // between i and indAct, in that case i, indAct are not swappable
        val successorsOfI = precedencesData
          .succMap
          .getOrElse(activityAtI, BitSet.empty)
          .filter(s => prioritySequence.contains(s))
        var noSuccBetweenI_IndAct = true
        for { j <- i+1 until seqIndexIndAct if noSuccBetweenI_IndAct} {
          optExplorer = prioritySequence.explorerAtPosition(j)
          val activityAtJ = optExplorer.get.value.toInt
          noSuccBetweenI_IndAct = !successorsOfI.contains(activityAtJ)
        }
        if (noSuccBetweenI_IndAct) {
          swappableIndices :+= i
        }
      }
    }
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
    val hasPredecessors = predActIndices.exists(p => prioritySequence.contains(p))
    val succActIndices = precedencesData.succMap.getOrElse(currentActivity, BitSet.empty)
    val hasSuccessors = succActIndices.exists(s => prioritySequence.contains(s))
    // Determine the bounds of the swappable zone in the priority sequence
    var lastPrecSeqIndex = -1
    var firstSuccSeqIndex = prioritySequence.size
    var seqIndexIndAct = -1
    var inLoop = true
    var optExplorer = prioritySequence.explorerAtPosition(0)
    while (inLoop && optExplorer.isDefined) {
      val actExplorer = optExplorer.get
      val i = actExplorer.position
      val activityAtI = actExplorer.value.toInt
      // is i the index for the activity indAct ?
      if (i == indAct) {
        seqIndexIndAct = i
        inLoop = hasSuccessors
      } else if (seqIndexIndAct == -1 && hasPredecessors) {
        // we check if this index is a predecessor
        if (predActIndices.contains(activityAtI)) {
          lastPrecSeqIndex = i
        }
      } else if (seqIndexIndAct != -1) {
        // notice that this implies hasSuccessors == true, otherwise the loop should
        // have stopped
        if (succActIndices.contains(activityAtI)) {
          firstSuccSeqIndex = i
          // we can stop loop because we found the first successor
          inLoop = false
        }
      }
      optExplorer = actExplorer.next
    }
    // The reinsertable indices are those between the bounds (excluded the bounds and
    // the index of the activity)
    var reinsertableIndices: List[Int] = List()
    for { i <- lastPrecSeqIndex+1 until firstSuccSeqIndex if i != seqIndexIndAct } {
      reinsertableIndices :+= i
    }
    reinsertableIndices
  }

  def insertableIndices(actValue: Long): Iterable[Int] = {
    val prioritySequence = activityPriorityList.value
    if (prioritySequence.isEmpty) {
      // The sequence is empty: the only insertable index is 0
      List(0)
    } else {
      val predActIndices = precedencesData.predMap.getOrElse(actValue.toInt, BitSet.empty)
      val hasPredecessors = predActIndices.exists(p => prioritySequence.contains(p))
      val succActIndices = precedencesData.succMap.getOrElse(actValue.toInt, BitSet.empty)
      val hasSuccessors = succActIndices.exists(s => prioritySequence.contains(s))
      // Determine the bounds of the insertable zone in the priority sequence
      var lastPrecSeqIndex = -1
      var firstSuccSeqIndex = prioritySequence.size
      var inLoop = true
      var optExplorer = prioritySequence.explorerAtPosition(0)
      while (inLoop && optExplorer.isDefined) {
        val actExplorer = optExplorer.get
        val i = actExplorer.position
        val activityAtI = actExplorer.value.toInt
        // is i the index for the activity actInd ?
        if (lastPrecSeqIndex == -1 && hasPredecessors) {
          // we check if this index is a predecessor
          if (predActIndices.contains(activityAtI)) {
            lastPrecSeqIndex = i
          }
        } else if (hasSuccessors) {
          // we check if this index is a successor
          if (succActIndices.contains(activityAtI)) {
            firstSuccSeqIndex = i
            // we can stop loop because we found the first successor
            inLoop = false
          }
        }
        optExplorer = actExplorer.next
      }
      // The insertable indices are those between the bounds
      var insertableIndices: List[Int] = List()
      for { i <- lastPrecSeqIndex+1 until firstSuccSeqIndex } {
        insertableIndices :+= i
      }
      insertableIndices
    }
  }
}
