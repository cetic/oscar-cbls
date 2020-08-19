package oscar.cbls.test.scheduling

import oscar.cbls.Store
import oscar.cbls.business.scheduling.model._
import oscar.cbls.business.scheduling.neighborhood.{ReinsertActivity, SwapActivity}
import oscar.cbls.core.objective.Objective
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Profile}

object SwDevResources {
  // Model
  // Activities
  val (analysis, design, coding, testing, qc, pm) = (50, 40, 30, 20, 10, 0)
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
  // Resources
  val (analyst_mode, qapm) = (20, 21)
  val (dev, test) = (30, 31)
  val analyst_st = SetupTimes(analyst_mode,
    Map(analysis->analyst_mode, design->analyst_mode, qc->qapm, pm->qapm),
    Map((analyst_mode, qapm)->1, (qapm, analyst_mode)->1))
  val analyst = new CumulativeResourceWithSetupTimesMultiMode(5,
    Map(analysis->2L, design->1L, qc->1L, pm->2L),
    analyst_st)
  val senior_dev_test_st = SetupTimes(dev,
    Map(coding->dev, testing->test),
    Map((dev, test)->2, (test, dev)->2))
  val senior_dev_test = new CumulativeResourceWithSetupTimesMultiMode(2,
    Map(coding->2L, testing->1L),
    senior_dev_test_st)

  def main(args: Array[String]): Unit = {
    val m = new Store()
    val schedule = new Schedule(m, activities, initials, durations, Map(), precPairs, List(analyst, senior_dev_test))
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
