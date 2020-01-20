package oscar.cbls.test.scheduling

import oscar.cbls._
import oscar.cbls.Store
import oscar.cbls.business.scheduling.model.Schedule
import oscar.cbls.business.scheduling.neighborhood.{AddActivity, ReplaceActivity}
import oscar.cbls.core.objective.Objective
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Profile}

object TestReplace {
  // Model
  // Activities
  val (a, b, c, d, e, f) = (0, 10, 20, 30, 40, 50)
  val activities = List(a, b, c, d, e, f)
  val initialActivities = List(f, c, e)
  val actDurations = Map(
    a -> 2L,
    b -> 1L,
    c -> 8L,
    d -> 4L,
    e -> 7L,
    f -> 6L
  )
  val actMinStartTimes = Map(
    a -> 0L,
    b -> 0L,
    c -> 0L,
    d -> 0L,
    e -> 0L,
    f -> 0L
  )
  val precPairs = List((a,b), (c,d), (e,f))
  val penaltyForUnscheduled = 10000L

  def main(args: Array[String]): Unit = {
    val m = new Store()
    val nActsToSchedule = activities.length
    val schedule = new Schedule(m, activities, initialActivities, actDurations, actMinStartTimes, precPairs, Nil)
    val objFunc = Objective(schedule.makeSpan + (penaltyForUnscheduled * (nActsToSchedule - length(schedule.activityPriorityList))))
    m.close()
    println("Model closed.")
    // Neighborhood
    val replaceNH = Profile(new ReplaceActivity(schedule, "Replace"))
    //val addNH = Profile(new AddActivity(schedule, "Add"))
    //val combinedNH = BestSlopeFirst(List(replaceNH, addNH))
    // This is the search strategy
    replaceNH.verbose = 1
    replaceNH.doAllMoves(obj = objFunc)
    // And here, the results
    println(replaceNH.profilingStatistics)
    println(s"*************** RESULTS ***********************************")
    println(s"Schedule makespan = ${schedule.makeSpan.value}")
    println(s"Scheduling sequence = ${schedule.activityPriorityList.value}")
    println("Scheduling start times = [  ")
    schedule.startTimes.foreach(v => println(s"    $v"))
    println("]")
  }
}
