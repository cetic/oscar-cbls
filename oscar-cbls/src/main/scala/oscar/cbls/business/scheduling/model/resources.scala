package oscar.cbls.business.scheduling.model

import oscar.cbls.business.scheduling.{ActivityId, Mode}

import scala.annotation.tailrec

// Resource constraints
// Model the resources

abstract class Resource {
  def usingActivities: Iterable[ActivityId]
  def initialState: ResourceState
}

class DisjunctiveResource(val usingActivities: Iterable[ActivityId]) extends Resource {
  override def initialState: ResourceState = new DisjunctiveResourceState(this, 0)
}

class DisjunctiveResourceWithSetupTimes(val usingActivities: Iterable[ActivityId],
                                        setupTimes: SetupTimes) extends Resource {
  override def initialState: ResourceState =
    new DisjunctiveResourceWithSetupTimesState(this, 0, setupTimes, setupTimes.initialMode)
}

class CumulativeResource(capacity: Long,
                         val activitiesConsumption: Map[ActivityId, Long])
  extends Resource {
  override def usingActivities: Iterable[ActivityId] = activitiesConsumption.keys

  override def initialState: ResourceState = {
    new CumulativeResourceState(this, Map(0 -> capacity))
  }
}

class CumulativeResourceWithSetupTimes(val capacity: Long,
                                       val activitiesConsumption: Map[ActivityId, Long],
                                       setupTimes: SetupTimes)
  extends Resource {
  override def usingActivities: Iterable[ActivityId] = activitiesConsumption.keys

  override def initialState: ResourceState = {
    new CumulativeResourceWithSetupTimesState(this, Map(0 -> capacity), setupTimes, setupTimes.initialMode)
  }
}

class CumulativeResourceWithSetupTimesMultiMode(val capacity: Long,
                                                val activitiesConsumption: Map[ActivityId, Long],
                                                setupTimes: SetupTimes)
  extends Resource {
  override def usingActivities: Iterable[ActivityId] = activitiesConsumption.keys

  override def initialState: ResourceState = {
    new CumulativeResourceWithSetupTimesMultiModeState(this,
                                                   Map(0 -> Set((setupTimes.initialMode, capacity))),
                                                   setupTimes,
                                                   setupTimes.initialMode)
  }
}

class ConsumableResource(capacity: Long,
                         val activitiesConsumption: Map[ActivityId, Long]) extends Resource {
  override def usingActivities: Iterable[ActivityId] = activitiesConsumption.keys

  override def initialState: ResourceState =
    new ConsumableResourceState(this, capacity)
}

class ConsumableResourceWithSetupTimes(capacity: Long,
                                       val activitiesConsumption: Map[ActivityId, Long],
                                       setupTimes: SetupTimes)
  extends Resource {
  override def usingActivities: Iterable[ActivityId] = activitiesConsumption.keys

  override def initialState: ResourceState =
    new ConsumableResourceWithSetupTimesState(this, 0, capacity, setupTimes, setupTimes.initialMode)
}

// Resource States
// These classes model the evolution of a resource through the schedule

abstract class ResourceState {
  def earliestStartTime(activity: ActivityId, earliestStart: Int): Int
  def nextState(activity: ActivityId, taskDuration: Int, startTime: Int): ResourceState
}

class DisjunctiveResourceState(base: DisjunctiveResource,
                               availableTime: Int)
  extends ResourceState {
  override def earliestStartTime(activity: ActivityId, earliestStart: Int): Int = {
    assert(base.usingActivities.exists(_ == activity))
    //////////
    availableTime max earliestStart
  }

  override def nextState(activity: ActivityId, taskDuration: Int, startTime: Int): ResourceState = {
    assert(base.usingActivities.exists(_ == activity))
    require(startTime >= availableTime)
    //////////
    new DisjunctiveResourceState(base, startTime + taskDuration)
  }
}

class DisjunctiveResourceWithSetupTimesState(base: DisjunctiveResourceWithSetupTimes,
                                             availableTime: Int,
                                             setupTimes: SetupTimes,
                                             currentMode: Int)
  extends ResourceState {
  override def earliestStartTime(activity: ActivityId, earliestStart: Int): Int = {
    assert(base.usingActivities.exists(_ == activity))
    //////////
    // Check whether there is a mode change
    val actMode = setupTimes.activityModes.getOrElse(activity, currentMode)
    if (actMode == currentMode) {
      availableTime max earliestStart
    } else {
      val setupTime = setupTimes.setupTimes.getOrElse((currentMode, actMode), 0)
      (availableTime + setupTime) max earliestStart
    }
  }

  override def nextState(activity: ActivityId, taskDuration: Int, startTime: Int): ResourceState = {
    assert(base.usingActivities.exists(_ == activity))
    require(startTime >= availableTime)
    //////////
    val actMode = setupTimes.activityModes.getOrElse(activity, currentMode)
    new DisjunctiveResourceWithSetupTimesState(base, startTime + taskDuration, setupTimes, actMode)
  }
}

class CumulativeResourceState(base: CumulativeResource,
                              releaseTimes: Map[Int, Long])
  extends ResourceState {
  override def earliestStartTime(activity: ActivityId, earliestStart: Int): Int = {
    @tailrec
    def releaseTimeFrom(relTime: Int,
                        qtyToConsume: Long,
                        relTimes: List[Int]): Int = relTimes match {
      case Nil => relTime
      case t::ts =>
        val releasedAtT = releaseTimes(t)
        if (qtyToConsume > releasedAtT) releaseTimeFrom(t, qtyToConsume-releasedAtT, ts)
        else t
    }
    //////////
    val qtyResourceToConsume = base.activitiesConsumption(activity)
    // Explore the releaseTimes map for obtaining the release time for enough resources
    val releaseTimesSorted = releaseTimes.keys.toList.sorted
    val releaseTime = releaseTimeFrom(0, qtyResourceToConsume, releaseTimesSorted)
    releaseTime max earliestStart
  }

  override def nextState(activity: ActivityId, taskDuration: Int, startTime: Int): ResourceState = {
    @tailrec
    def relTimesMapFrom(qtyToConsume: Long,
                        relTimes: List[Int],
                        alreadyConsumed: Boolean,
                        relTimesMapAcc: Map[Int, Long]): Map[Int, Long] = relTimes match {
      case Nil => relTimesMapAcc
      case t::ts =>
        val releasedAtT = releaseTimes(t)
        if (alreadyConsumed)
          relTimesMapFrom(qtyToConsume, ts, alreadyConsumed, relTimesMapAcc + (t -> releasedAtT))
        else {
          if (qtyToConsume > releasedAtT)
            relTimesMapFrom(qtyToConsume-releasedAtT, ts, alreadyConsumed, relTimesMapAcc)
          else if (releasedAtT == qtyToConsume)
            relTimesMapFrom(0L, ts, alreadyConsumed = true, relTimesMapAcc)
          else
            relTimesMapFrom(0L, ts, alreadyConsumed = true, relTimesMapAcc + (t -> (releasedAtT-qtyToConsume)))
        }
    }
    //////////
    val qtyResourceConsumed = base.activitiesConsumption(activity)
    val releaseTimesSorted = releaseTimes.keys.toList.sorted
    val releaseTimeAct = startTime + taskDuration
    val updatedReleaseTimes0 = relTimesMapFrom(qtyResourceConsumed, releaseTimesSorted,
      alreadyConsumed = false, Map())
    val qtyReleased = updatedReleaseTimes0.getOrElse(releaseTimeAct, 0L)
    val updatedReleaseTimes = updatedReleaseTimes0 + (releaseTimeAct -> (qtyResourceConsumed+qtyReleased))
    // Return the new state
    new CumulativeResourceState(base, updatedReleaseTimes)
  }
}

class CumulativeResourceWithSetupTimesState(base: CumulativeResourceWithSetupTimes,
                                            releaseTimes: Map[Int, Long],
                                            setupTimes: SetupTimes,
                                            currentMode: Mode)
  extends ResourceState {
  override def earliestStartTime(activity: ActivityId, earliestStart: Int): Int = {
    @tailrec
    def releaseTimeFrom(relTime: Int,
                        qtyToConsume: Long,
                        relTimes: List[Int]): Int = relTimes match {
      case Nil => relTime
      case t::ts =>
        val releasedAtT = releaseTimes(t)
        if (qtyToConsume > releasedAtT) releaseTimeFrom(t, qtyToConsume-releasedAtT, ts)
        else t
    }
    //////////
    val qtyResourceToConsume = base.activitiesConsumption(activity)
    // Explore the releaseTimes map for obtaining the release time for enough resources
    val releaseTimesSorted = releaseTimes.keys.toList.sorted
    // Check whether there is a mode change
    val actMode = setupTimes.activityModes.getOrElse(activity, currentMode)
    if (actMode == currentMode) {
      val releaseTime = releaseTimeFrom(0, qtyResourceToConsume, releaseTimesSorted)
      releaseTime max earliestStart
    } else {
      // Mode has changed : we need to release all the resources before changing mode
      val lastTime = releaseTimesSorted.last
      val setupTime = setupTimes.setupTimes.getOrElse((currentMode, actMode), 0)
      (lastTime + setupTime) max earliestStart
    }
  }

  override def nextState(activity: ActivityId, taskDuration: Int, startTime: Int): ResourceState = {
    @tailrec
    def relTimesMapFrom(qtyToConsume: Long,
                        relTimes: List[Int],
                        alreadyConsumed: Boolean,
                        relTimesMapAcc: Map[Int, Long]): Map[Int, Long] = relTimes match {
      case Nil => relTimesMapAcc
      case t::ts =>
        val releasedAtT = releaseTimes(t)
        if (alreadyConsumed)
          relTimesMapFrom(qtyToConsume, ts, alreadyConsumed, relTimesMapAcc + (t -> releasedAtT))
        else {
          if (qtyToConsume > releasedAtT)
            relTimesMapFrom(qtyToConsume-releasedAtT, ts, alreadyConsumed, relTimesMapAcc)
          else if (releasedAtT == qtyToConsume)
            relTimesMapFrom(0L, ts, alreadyConsumed = true, relTimesMapAcc)
          else
            relTimesMapFrom(0L, ts, alreadyConsumed = true, relTimesMapAcc + (t -> (releasedAtT-qtyToConsume)))
        }
    }
    //////////
    val qtyResourceConsumed = base.activitiesConsumption(activity)
    // Check whether there is a mode change
    val actMode = setupTimes.activityModes.getOrElse(activity, currentMode)
    if (actMode == currentMode) {
      // No mode change, behaves as cumulative resource without state
      val releaseTimesSorted = releaseTimes.keys.toList.sorted
      val releaseTimeAct = startTime + taskDuration
      val updatedReleaseTimes0 = relTimesMapFrom(qtyResourceConsumed, releaseTimesSorted,
        alreadyConsumed = false, Map())
      val qtyReleased = updatedReleaseTimes0.getOrElse(releaseTimeAct, 0L)
      val updatedReleaseTimes = updatedReleaseTimes0 + (releaseTimeAct -> (qtyResourceConsumed+qtyReleased))
      // Return the new state
      new CumulativeResourceWithSetupTimesState(base, updatedReleaseTimes, setupTimes, actMode)
    } else {
      // Mode has changed
      val availableQtyAfterModeChanged = base.capacity - qtyResourceConsumed
      val newReleaseTimes = Map(
        startTime -> availableQtyAfterModeChanged,
        (startTime+taskDuration) -> qtyResourceConsumed
      )
      // Return the new state
      new CumulativeResourceWithSetupTimesState(base, newReleaseTimes, setupTimes, actMode)
    }
  }
}

class CumulativeResourceWithSetupTimesMultiModeState(base: CumulativeResourceWithSetupTimesMultiMode,
                                                     releaseTimes: Map[Int, Set[(Mode, Long)]],
                                                     setupTimes: SetupTimes,
                                                     currentMode: Mode)
  extends ResourceState {
  override def earliestStartTime(activity: ActivityId, earliestStart: Int): Int = {
    @tailrec
    def releaseTimeFrom(relTime: Int,
                        qtyToConsume: Long,
                        releaseTimesWithST: Map[Int, Long],
                        relTimes: List[Int]): Int = relTimes match {
      case Nil => relTime
      case t::ts =>
        val releasedAtT = releaseTimesWithST(t)
        if (qtyToConsume > releasedAtT)
          releaseTimeFrom(t, qtyToConsume-releasedAtT, releaseTimesWithST, ts)
        else t
    }
    //////////
    val qtyResourceToConsume = base.activitiesConsumption(activity)
    val actMode = setupTimes.activityModes.getOrElse(activity, currentMode)
    val releaseTimesWithST = releaseTimes.foldLeft(Map(): Map[Int, Long]) { (mapAcc, tup) =>
      val t = tup._1
      val setModeQty = tup._2
      setModeQty.foldLeft(mapAcc) { (mapAcc1, tup1) =>
        val m = tup1._1
        val qty = tup1._2
        val st = setupTimes.setupTimes.getOrElse((m, actMode), 0)
        val rt = t + st
        val qty0 = mapAcc1.getOrElse(rt, 0L)
        mapAcc1 + (rt -> (qty0+qty))
      }
    }
    val releaseTimesSorted = releaseTimesWithST.keys.toList.sorted
    val releaseTime = releaseTimeFrom(0, qtyResourceToConsume, releaseTimesWithST, releaseTimesSorted)
    releaseTime max earliestStart
  }

  override def nextState(activity: ActivityId, taskDuration: Int, startTime: Int): ResourceState = {
    @tailrec
    def relTimesMapFrom(qtyToConsume: Long,
                        relTimes: List[Int],
                        alreadyConsumed: Boolean,
                        releaseTimesWithST: Map[Int, Set[(Int, Mode, Long)]],
                        relTimesMapAcc: Map[Int, Set[(Mode, Long)]]): Map[Int, Set[(Mode, Long)]] = {
      @tailrec
      def takeQuantity(qtyRemaining: Long,
                       rTimesST: List[(Int, Mode, Long)],
                       rTMapAcc:Map[Int, Set[(Mode, Long)]]): Map[Int, Set[(Mode, Long)]] = rTimesST match {
        case Nil => rTMapAcc
        case (originalT,m,qty)::rts =>
          if (qtyRemaining == 0) {
            val setModeQtyAtT = rTMapAcc.getOrElse(originalT, Set())
            takeQuantity(0, rts, rTMapAcc + (originalT -> (setModeQtyAtT + ((m, qty)))))
          } else if (qty <= qtyRemaining) {
            takeQuantity(qtyRemaining-qty, rts, rTMapAcc)
          } else {
            val setModeQtyAtT = rTMapAcc.getOrElse(originalT, Set())
            takeQuantity(0, rts, rTMapAcc + (originalT -> (setModeQtyAtT + ((m, qty-qtyRemaining)))))
          }
      }
      //////////
      relTimes match {
        case Nil => relTimesMapAcc
        case t :: ts =>
          val relTimeST = releaseTimesWithST(t)
          if (alreadyConsumed) {
            val newRelTimesMapAcc = relTimeST.foldLeft(relTimesMapAcc) { (mapAcc, tup) =>
              val originalT = tup._1
              val m = tup._2
              val qty = tup._3
              val setModeQtyAtT = mapAcc.getOrElse(originalT, Set())
              mapAcc + (originalT -> (setModeQtyAtT + ((m, qty))))
            }
            relTimesMapFrom(qtyToConsume, ts, alreadyConsumed, releaseTimesWithST, newRelTimesMapAcc)
          }
          else {
            val releasedAtT = relTimeST.foldLeft(0L) { (acc, tup) => acc + tup._3 }
            if (qtyToConsume > releasedAtT)
              relTimesMapFrom(qtyToConsume - releasedAtT, ts, alreadyConsumed, releaseTimesWithST, relTimesMapAcc)
            else if (releasedAtT == qtyToConsume)
              relTimesMapFrom(0L, ts, alreadyConsumed = true, releaseTimesWithST, relTimesMapAcc)
            else {
              val rTimesST = relTimeST.toList.sortBy(_._3)
              val newRelTimesMapAcc = takeQuantity(qtyToConsume, rTimesST, relTimesMapAcc)
              relTimesMapFrom(0L, ts, alreadyConsumed = true, releaseTimesWithST,
                newRelTimesMapAcc)
            }
          }
      }
    }
    //////////
    val qtyResourceConsumed = base.activitiesConsumption(activity)
    val actMode = setupTimes.activityModes.getOrElse(activity, currentMode)
    val releaseTimesWithST = releaseTimes
      .foldLeft(Map(): Map[Int, Set[(Int, Mode, Long)]]) { (mapAcc, tup) =>
        val t = tup._1
        val setModeQty = tup._2
        setModeQty.foldLeft(mapAcc) { (mapAcc1, tup1) =>
          val m = tup1._1
          val qty = tup1._2
          val st = setupTimes.setupTimes.getOrElse((m, actMode), 0)
          val rt = t + st
          val setModeQty0 = mapAcc1.getOrElse(rt, Set())
          mapAcc1 + (rt -> (setModeQty0 + ((t, m, qty))))
        }
      }
    val releaseTimesSorted = releaseTimesWithST.keys.toList.sorted
    val updatedReleaseTimes0 = relTimesMapFrom(qtyResourceConsumed, releaseTimesSorted, alreadyConsumed = false,
      releaseTimesWithST, Map())
    val releaseTime = startTime + taskDuration
    val setTMQ = updatedReleaseTimes0.getOrElse(releaseTime, Set())
    val updatedReleaseTimes = updatedReleaseTimes0 + (releaseTime -> (setTMQ + ((actMode,qtyResourceConsumed))))
    // Return the new state
    new CumulativeResourceWithSetupTimesMultiModeState(base, updatedReleaseTimes, setupTimes, actMode)
  }
}

class ConsumableResourceState(base: ConsumableResource,
                              availableCapacity: Long) extends ResourceState {

  override def earliestStartTime(activity: ActivityId, earliestStart: Int): Int = {
    assert(base.usingActivities.exists(_ == activity))
    //////////
    val qtyResourceToConsume = base.activitiesConsumption(activity)
    if (qtyResourceToConsume > availableCapacity) {
      // Not available resource, activity will never start
      Int.MaxValue
    } else {
      earliestStart
    }
  }

  override def nextState(activity: ActivityId, taskDuration: Int, startTime: Int): ResourceState = {
    assert(base.usingActivities.exists(_ == activity))
    //////////
    val qtyResourceToConsume = base.activitiesConsumption(activity)
    if (qtyResourceToConsume > availableCapacity)
      ErrorResourceState
    else {
      new ConsumableResourceState(base, availableCapacity - qtyResourceToConsume)
    }
  }
}

class ConsumableResourceWithSetupTimesState(base: ConsumableResourceWithSetupTimes,
                                            availableTime: Int,
                                            availableCapacity: Long,
                                            setupTimes: SetupTimes,
                                            initialMode: Mode) extends ResourceState {

  override def earliestStartTime(activity: ActivityId, earliestStart: Int): Int = {
    assert(base.usingActivities.exists(_ == activity))
    //////////
    val qtyResourceToConsume = base.activitiesConsumption(activity)
    if (qtyResourceToConsume > availableCapacity) {
      // Not available resource, activity will never start
      Int.MaxValue
    } else {
      // Check whether there is a mode change
      val actMode = setupTimes.activityModes.getOrElse(activity, initialMode)
      if (actMode == initialMode) {
        availableTime max earliestStart
      } else {
        val setupTime = setupTimes.setupTimes.getOrElse((initialMode, actMode), 0)
        (availableTime + setupTime) max earliestStart
      }
    }
  }

  override def nextState(activity: ActivityId, taskDuration: Int, startTime: Int): ResourceState = {
    assert(base.usingActivities.exists(_ == activity))
    require(startTime >= availableTime)
    //////////
    val qtyResourceToConsume = base.activitiesConsumption(activity)
    if (qtyResourceToConsume > availableCapacity)
      ErrorResourceState
    else {
      new ConsumableResourceWithSetupTimesState(base,
        startTime,
        availableCapacity - qtyResourceToConsume,
        setupTimes,
        initialMode)
    }
  }
}

object ErrorResourceState extends ResourceState {
  override def earliestStartTime(activity: ActivityId, earliestStart: Int): Int = Int.MaxValue

  override def nextState(activity: ActivityId, taskDuration: Int, startTime: Int): ResourceState = this
}

// Case class that models Setup Times
case class SetupTimes(initialMode: Mode,
                      activityModes: Map[ActivityId, Mode],
                      setupTimes: Map[(Mode, Mode), Int])
