package oscar.cbls.test.scheduling
/*
TODO Redo this example

import oscar.cbls.Store
import oscar.cbls.business.scheduling.model.{Activity, Flexible, Mandatory, Schedule}
import oscar.cbls.business.scheduling.neighborhood.{ReinsertActivity, ReplaceActivity, SwapActivity}
import oscar.cbls.core.objective.Objective
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Profile}

object FlexibleJobShop {
  // Model
  // Activities
  val a = 0
  val b = 100
  val c1 = 201
  val c2 = 202
  val d1 = 301
  val d2 = 302
  val d3 = 303
  val d4 = 304
  val e = 400
  val f = 500
  val g = 600
  val h = 700
  val i = 800
  val j = 900
  val k = 1000
  val activities = List(
    Activity(a, 5L, 0L, Mandatory),
    Activity(b, 8L, 0L, Mandatory),
    Activity(c1, 3L, 0L, Flexible),
    Activity(c2, 4L, 0L, Flexible),
    Activity(d1, 9L, 0L, Flexible),
    Activity(d2, 7L, 0L, Flexible),
    Activity(d3, 4L, 0L, Flexible),
    Activity(d4, 6L, 0L, Flexible),
    Activity(e, 10L, 0L, Mandatory),
    Activity(f, 12L, 0L, Mandatory),
    Activity(g, 9L, 0L, Mandatory),
    Activity(h, 11L, 0L, Mandatory),
    Activity(i, 8L, 0L, Mandatory),
    Activity(j, 4L, 0L, Mandatory),
    Activity(k, 13L, 0L, Mandatory)
  )
  val precedencePairs = List(
    (a, b),
    (a, c1),
    (a, c2),
    (c1, d1),
    (c1, d2),
    (c1, d3),
    (c1, d4),
    (c2, d1),
    (c2, d2),
    (c2, d3),
    (c2, d4),
    (d1, e),
    (d2, e),
    (d3, e),
    (d4, e),
    (d1, f),
    (d2, f),
    (d3, f),
    (d4, f),
    (d1, g),
    (d2, g),
    (d3, g),
    (d4, g),
    (j, k)
  )
  val resources = Nil

  def main(args: Array[String]): Unit = {
    val m = new Store()
    val schedule = new Schedule(m, activities, precedencePairs, resources)
    val objFunc = Objective(schedule.makeSpan)
    m.close()
    println("Model closed.")
    // Neighborhood
    val swapNH = new SwapActivity(schedule, "Swap")
    val reinsertNH = new ReinsertActivity(schedule, "Reinsert")
    val replaceNH = new ReplaceActivity(schedule, "Replace")
    val combinedNH = BestSlopeFirst(List(Profile(reinsertNH), Profile(swapNH), Profile(replaceNH)))
    // Search Procedure
    combinedNH.doAllMoves(obj = objFunc)
    // Output
    println(combinedNH.profilingStatistics)
    println(s"*************** RESULTS ***********************************")
    println(s"Schedule makespan = ${schedule.makeSpan.value}")
    println(s"Scheduling sequence = ${schedule.activityPriorityList.value}")
    println("Scheduling start times = [  ")
    schedule.startTimes.foreach(v => println(s"    $v"))
    println("]")
  }
}


 */