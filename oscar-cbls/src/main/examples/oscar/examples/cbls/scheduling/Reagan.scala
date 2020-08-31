package oscar.examples.cbls.scheduling

import oscar.cbls.Store
import oscar.cbls.business.scheduling.model.{CumulativeResource, Schedule}
import oscar.cbls.business.scheduling.neighborhood.{ReinsertActivity, SwapActivity}
import oscar.cbls.core.objective.Objective
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Profile}

object Reagan {
  // Reagan model
  val (eat, sleep, think, chew, speak, drink) = (0, 1, 2, 3, 4, 5)

  val durations = Map(
    eat -> 2,
    sleep -> 8,
    think -> 12,
    chew -> 3,
    speak -> 3,
    drink -> 3
  )

  val precPairs = List((think, drink), (eat, sleep), (chew, speak))
  val reagan = new CumulativeResource(3L,
    Map(eat -> 2L, sleep -> 1L, think -> 1L, chew -> 3L, speak -> 3L, drink -> 3L))

  def main(args: Array[String]): Unit = {
    val m = new Store()
    val schedule = new Schedule(m, List(eat, sleep, think, chew, speak, drink),
      List(eat, sleep, think, chew, speak, drink), durations, Map(),
      precPairs, List(reagan))
    val objFunc = Objective(schedule.makeSpan)
    m.close()
    // Neighborhoods
    val swapNH = new SwapActivity(schedule, "Swap")
    val reinsertNH = new ReinsertActivity(schedule, "Reinsert")
    val combinedNH = BestSlopeFirst(List(Profile(reinsertNH), Profile(swapNH)))
    // This is the search strategy
    combinedNH.doAllMoves(obj=objFunc)
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
