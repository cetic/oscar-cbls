package oscar.cbls.test.scheduling

import oscar.cbls.{Objective, Store}
import oscar.cbls.business.scheduling.model.{DisjunctiveResource, Schedule}
import oscar.cbls.business.scheduling.neighborhood.{ReinsertActivity, SwapActivity}
import oscar.cbls.lib.invariant.numeric.Sum
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Profile}

object SwDevTardiness {
  // Model
  val (a1, a2, a3, a4, a5) = (10, 11, 12, 13, 14)
  val activities = List(a1, a2, a3, a4, a5)
  val durations = Map(
    a1 -> 10,
    a2 -> 10,
    a3 -> 10,
    a4 -> 10,
    a5 -> 10
  )
  // Resource
  val analyst = new DisjunctiveResource(List(a1, a2, a3, a4, a5))

  def main(args: Array[String]): Unit = {
    // CBLS Store
    val m = new Store()
    val schedule = new Schedule(m, activities, activities, durations, Map(), Nil, List(analyst))
    // Tardiness variables
    val tardinessPenalty = 100L
    val numAct = activities.length
    val activitiesTardiness = Array.tabulate(numAct) { i =>
      val act = activities(i)
      schedule.startTimes(act) * tardinessPenalty * (numAct-i)
    }
    val globalTardiness: Objective = Sum(activitiesTardiness)
    // Model closed
    m.close()
    // Neighborhoods
    val swapNH = new SwapActivity(schedule, "Swap")
    val reinsertNH = new ReinsertActivity(schedule, "Reinsert")
    val combinedNH = BestSlopeFirst(List(Profile(reinsertNH), Profile(swapNH)))
    // This is the search strategy
    combinedNH.doAllMoves(obj = globalTardiness)
    // And here, the results
    println(combinedNH.profilingStatistics)
    println(s"*************** RESULTS ***********************************")
    println(s"Schedule makespan = ${schedule.makeSpan.value}")
    println(s"Scheduling sequence = ${schedule.activityPriorityList.value}")
    println("Scheduling start times = [  ")
    schedule.startTimes.foreach(v => println(s"    $v"))
    println("]")
    println(s"Global tardiness = ${globalTardiness.value}")
  }

}
