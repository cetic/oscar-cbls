package oscar.cbls.test.scheduling

import oscar.cbls.Store
import oscar.cbls.business.scheduling.model._
import oscar.cbls.business.scheduling.neighborhood.{ReinsertActivity, SwapActivity}
import oscar.cbls.core.objective.Objective
import oscar.cbls.core.propagation.ErrorChecker
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Profile}

/**
  * This is the scheduling example in Cédric Pralet's paper
  */
object Pralet {
  // Model
  // Activities
  val (a, b, c, d, e) = (0, 10, 20, 30, 40)
  val names = Map(
    a -> "a",
    b -> "b",
    c -> "c",
    d -> "d",
    e -> "e"
  )
  val activities = List(a, b, c, d, e)
  val initialActs = List(e, d, c, b, a)
  val (m0, m1) = (0, 1)
  val durations = Map(
    a -> 4,
    b -> 1,
    c -> 3,
    d -> 4,
    e -> 2
  )
  val precPairs = List((a, c), (c, e))
  // Resources
  val setupTimesR = SetupTimes(m1, Map(a->m1, b->m1, c->m1, d->m1, e->m0), Map((m0,m1)->1, (m1,m0)->1))
  val r = new CumulativeResourceWithSetupTimes(3L, Map(a->2L, b->1L, c->1L, d->2L, e->2L), setupTimesR)
  val setupTimesR1 = SetupTimes(m0, Map(b->m0, c->m1, e->m0), Map((m0,m1)-> 1, (m1,m0)->4))
  val r1 = new DisjunctiveResourceWithSetupTimes(List(b, c, e), setupTimesR1)

  def main(args: Array[String]): Unit = {
    val m = new Store(checker = Some(ErrorChecker()))
    val schedule = new Schedule(m, activities, initialActs, durations, Map(), precPairs, List(r, r1))
    val objFunc = Objective(schedule.makeSpan)
    m.close()
    println("Model closed.")
    // Neighborhoods
    val swapNH = new SwapActivity(schedule, "Swap")
    val reinsertNH = new ReinsertActivity(schedule, "Reinsert")
    val combinedNH = BestSlopeFirst(List(Profile(reinsertNH), Profile(swapNH)))
    // This is the search strategy
    combinedNH.verbose = 1
    combinedNH.doAllMoves(obj = objFunc)
    // And here, the results
    println(combinedNH.profilingStatistics)
    println(s"*************** RESULTS ***********************************")
    println(s"Schedule makespan = ${schedule.makeSpan.value}")
    println(s"Scheduling sequence = ${schedule.activityPriorityList.value.toList.map(names(_))}")
    println("Scheduling start times = [  ")
    schedule.startTimes.foreach(v => println(s"    Starting time of ${names(v._1)} = ${v._2.value}"))
    println("]")
  }
}
