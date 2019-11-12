package oscar.cbls.test.scheduling

import oscar.cbls._
import oscar.cbls.Store
import oscar.cbls.business.scheduling.model.{ActivityData, Optional, Schedule}
import oscar.cbls.business.scheduling.neighborhood.{AddActivity, ReplaceActivity}
import oscar.cbls.core.objective.Objective
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Profile}

object TestReplace {
  // Model
  // Activities
  val (a, b, c, d, e, f) = (0, 10, 20, 30, 40, 50)
  val activities = List(
    ActivityData(a, 2L, 0L, Optional),
    ActivityData(b, 1L, 0L, Optional),
    ActivityData(c, 8L, 0L, Optional),
    ActivityData(d, 4L, 0L, Optional),
    ActivityData(e, 7L, 0L, Optional),
    ActivityData(f, 6L, 0L, Optional)
  )
  val precPairs = List((a,b), (c,d), (e,f))
  val penaltyForUnscheduled = 10000L

  def main(args: Array[String]): Unit = {
    val m = new Store()
    val nActsToSchedule = activities.length
    val schedule = new Schedule(m, activities, precPairs, Nil)
    val objFunc = Objective(schedule.makeSpan + (penaltyForUnscheduled * (nActsToSchedule - length(schedule.activityPriorityList))))
    m.close()
    println("Model closed.")
    // Neighborhood
    val replaceNH = Profile(new ReplaceActivity(schedule, "Replace"))
    val addNH = Profile(new AddActivity(schedule, "Add"))
    val combinedNH = BestSlopeFirst(List(replaceNH, addNH))
    // This is the search strategy
    combinedNH.doAllMoves(obj = objFunc)
    // And here, the results
    println(combinedNH.profilingStatistics)
    println(s"*************** RESULTS ***********************************")
    println(s"Schedule makespan = ${schedule.makeSpan.value}")
    println(s"Scheduling sequence = ${schedule.activityPriorityList.value}")
    println("Scheduling start times = [  ")
    schedule.startTimes.foreach(v => println(s"    $v"))
    println("]")
  }
}
