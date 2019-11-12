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
  val activities = List(
    ActivityData(analysis, 10L, 0L, Mandatory),
    ActivityData(design, 10L, 0L, Mandatory),
    ActivityData(coding, 15L, 0L, Mandatory),
    ActivityData(testing, 25L, 0L, Mandatory),
    ActivityData(qc, 50L, 0L, Mandatory),
    ActivityData(pm, 60L, 0L, Mandatory)
  )
  val precPairs = List((analysis, design), (analysis, qc), (design, coding), (coding, testing))
  // Resources
  val (analyst_mode, qapm) = (20, 21)
  val (dev, test) = (30, 31)
  val analyst_st = SetupTimes(analyst_mode,
    Map(analysis->analyst_mode, design->analyst_mode, qc->qapm, pm->qapm),
    Map((analyst_mode, qapm)->1L, (qapm, analyst_mode)->1L))
  val analyst = new CumulativeMultiResourceWithSetupTimes(5,
    Map(analysis->2L, design->1L, qc->1L, pm->2L),
    analyst_st)
  val senior_dev_test_st = SetupTimes(dev,
    Map(coding->dev, testing->test),
    Map((dev, test)->2L, (test, dev)->2L))
  val senior_dev_test = new CumulativeMultiResourceWithSetupTimes(2,
    Map(coding->2L, testing->1L),
    senior_dev_test_st)

  def main(args: Array[String]): Unit = {
    val m = new Store()
    val schedule = new Schedule(m, activities, precPairs, List(analyst, senior_dev_test))
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
