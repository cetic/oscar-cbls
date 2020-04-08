package oscar.cbls.test.scheduling

import oscar.cbls.Store
import oscar.cbls.business.scheduling.model.Schedule
import oscar.cbls.business.scheduling.neighborhood.{ReinsertActivity, SwapActivity}
import oscar.cbls.core.objective.Objective
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Profile}

object SwDevelopment {
  // Model
  val (analysis, design, coding, testing, qc, pm) = (5, 15, 25, 35, 45, 55)
  val activities = List(analysis, design, coding, testing, qc, pm)
  val initials = List(pm, qc, testing, coding, design, analysis)
  val durations = Map(
    analysis -> 10,
    design -> 10,
    coding -> 15,
    testing -> 25,
    qc -> 50,
    pm -> 60
  )
  val precPairs = List((analysis, design), (analysis, qc), (design, coding), (coding, testing))

  def main(args: Array[String]): Unit = {
    val m = new Store()
    val schedule = new Schedule(m, activities, initials, durations, Map(), precPairs, Nil)
    val objFunc = Objective(schedule.makeSpan)
    m.close()
    println("Model closed.")
    // Neighborhoods
    val swapNH = new SwapActivity(schedule, "Swap")
    val reinsertNH = new ReinsertActivity(schedule, "Reinsert")
    val combinedNH = BestSlopeFirst(List(Profile(reinsertNH), Profile(swapNH)))
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
