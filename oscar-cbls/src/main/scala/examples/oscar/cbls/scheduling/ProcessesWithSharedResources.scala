package examples.oscar.cbls.scheduling

import oscar.cbls.{Objective, Store}
import oscar.cbls.business.scheduling.{ActivityId, Mode, Schedule}
import oscar.cbls.business.scheduling.model.{DisjunctiveResourceWithSetupTimes, Resource, SetupTimes}
import oscar.cbls.business.scheduling.neighborhood.{ReinsertActivity, SwapActivity}
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Profile}

import scala.util.Random

object ProcessesWithSharedResources {
  final val NB_PROCESSES = 200
  final val NB_RESOURCES = 20
  final val MAX_DURATION = 20
  final val MAX_TRANSITION = 10
  final val DENSITY_PREC = 10

  // Random Generator
  val randomGen = new Random()
  val identityModeMap: Map[ActivityId, Mode] = (for {i <- 1 to NB_PROCESSES} yield i -> i).toMap

  val activities:  List[ActivityId] = Random.shuffle((1 to NB_PROCESSES).toList)
  val durations: Map[ActivityId, Int] = (for {i <- 1 to NB_PROCESSES} yield i -> randomInterval(1, MAX_DURATION)).toMap
  val resources: List[Resource] = (for {_ <- 1 to NB_RESOURCES}
    yield new DisjunctiveResourceWithSetupTimes(activities, randomSetupTimes)).toList
  val minStartTimes: Map[ActivityId, Int] = Map()
  val precedences: List[(ActivityId, ActivityId)] = (for {i <- 1 to NB_PROCESSES
                                                          j <- 1 until i
                                                          if randomBoolean(DENSITY_PREC)} yield (i, j)).toList

  def randomBoolean(density: Int): Boolean = {
    val rdVal = randomGen.nextInt(100)
    rdVal < density
  }

  def randomInterval(inf: Int, sup: Int): Int = {
    require(inf <= sup)
    val offset = if (inf == sup) 0 else randomGen.nextInt(sup - inf)
    inf + offset
  }

  def randomSetupTimes: SetupTimes = {
    val initMode = randomInterval(1, NB_PROCESSES)
    // Mode change map
    var transitionModeMap: Map[(Mode, Mode), Int] = Map()
    for {i <- 1 to NB_PROCESSES} {
      for {j <- 1 to NB_PROCESSES} {
        transitionModeMap += (i, j) -> randomInterval(0, MAX_TRANSITION)
      }
    }
    // Result
    SetupTimes(initMode, identityModeMap, transitionModeMap)
  }

  def main(args: Array[String]): Unit = {
    // The CBLS store
    val model = Store(checker = None, noCycle=false)
    val schedulingProblem = new Schedule(
      model,
      activities,
      activities,
      durations,
      minStartTimes,
      precedences,
      resources)
    val objectiveFunction = Objective(schedulingProblem.makeSpan)
    model.close()
    // Neighborhoods
    val swapNH = new SwapActivity(schedulingProblem, "Swap")
    val reinsertNH = new ReinsertActivity(schedulingProblem, "Reinsert")
    val combinedNH = BestSlopeFirst(List(Profile(reinsertNH), Profile(swapNH)))
    // This is the search strategy
    println("Computing solution...")
    val t0 = System.nanoTime()
    combinedNH.verbose = 1
    combinedNH.doAllMoves(obj = objectiveFunction)
    val t1 = System.nanoTime()
    println(combinedNH.profilingStatistics)
    // And here, the results
    val actPriorList = schedulingProblem.activityPriorityList.value.toList
    println(s"*************** RESULTS ***********************************")
    println(s"Elapsed time : ${t1-t0} ns")
    println(s"Schedule makespan = ${schedulingProblem.makeSpan.value}")
    println(s"Scheduling sequence = ${schedulingProblem.activityPriorityList.value.toList}")
    println("Scheduling start times = [  ")
    for {a <- actPriorList} {
      val startTimeA = schedulingProblem.startTimes(a.toInt).value
      val durationA = durations(a)
      println(s"    Activity $a : Start Time = $startTimeA : Duration : $durationA")
    }
    println("]")
  }
}
