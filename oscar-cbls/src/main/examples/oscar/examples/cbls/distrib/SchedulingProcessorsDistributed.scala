package oscar.examples.cbls.distrib

import oscar.cbls.Objective
import oscar.cbls.business.scheduling.model.{DisjunctiveResourceWithSetupTimes, Resource, SetupTimes}
import oscar.cbls.business.scheduling.neighborhood.{ReinsertActivity, SwapActivity}
import oscar.cbls.business.scheduling.{ActivityId, Mode, Schedule}
import oscar.cbls.core.computation.Store
import oscar.cbls.core.distrib
import oscar.cbls.core.search.Neighborhood
import oscar.cbls.lib.search.combinators.BestSlopeFirst
import oscar.cbls.lib.search.combinators.distributed.{DistributedBest, DistributedFirst}

import scala.collection.parallel.immutable.ParRange
import scala.util.Random

object SchedulingProcessorsDistributed {
  sealed trait NBType

  case object Sequential extends NBType

  case object DistFirst extends NBType

  case object DistBest extends NBType

  final val NB_WORKERS = 6
  final val NB_PROCESSES = 200
  final val NB_RESOURCES = 20
  final val MAX_DURATION = 20
  final val MAX_TRANSITION = 10
  final val DENSITY_PREC = 10
  final val FIRST_NB = false

  // Random Generator
  val randomGen = new Random()

  val identityModeMap: Map[ActivityId, Mode] = (for {i <- 1 to NB_PROCESSES} yield i -> i).toMap

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

  def createRandomProblem(): (List[ActivityId], List[ActivityId], Map[ActivityId, Int], Map[ActivityId, Int], List[(ActivityId, ActivityId)], List[Resource]) = {
    val activities: List[ActivityId] = Random.shuffle((1 to NB_PROCESSES).toList)
    val durations: Map[ActivityId, Int] = (for {i <- 1 to NB_PROCESSES} yield i -> randomInterval(1, MAX_DURATION)).toMap
    val resources: List[Resource] = (for {_ <- 1 to NB_RESOURCES}
      yield new DisjunctiveResourceWithSetupTimes(activities, randomSetupTimes)).toList
    val minStartTimes: Map[ActivityId, Int] = Map()
    val precedences: List[(ActivityId, ActivityId)] = (for {i <- 1 to NB_PROCESSES
                                                            j <- 1 until i
                                                            if randomBoolean(DENSITY_PREC)} yield (i, j)).toList
    (activities, activities, durations, minStartTimes, precedences, resources)
  }

  def createCBLSProblem(activities: List[ActivityId],
                        initialActivities: List[ActivityId],
                        durations: Map[ActivityId, Int],
                        minStartTimes: Map[ActivityId, Int],
                        precedencePairs: List[(ActivityId, ActivityId)],
                        resources: List[Resource],
                        typeNb: NBType): (Store, Objective, Neighborhood, () => Unit) = {
    // The CBLS store
    val model = Store(checker = None, noCycle = false)
    val scProblem = new Schedule(model, activities, initialActivities, durations, minStartTimes, precedencePairs, resources)
    val objFunc = Objective(scProblem.makeSpan)
    model.close()
    val arrNH: Array[Neighborhood] = if (typeNb == Sequential) {
      val swapNH = new SwapActivity(scProblem, "Swap")
      val reinsertNH = new ReinsertActivity(scProblem, "Reinsert")
      Array(swapNH, reinsertNH)
    } else {
      val actIndices = initialActivities.indices
      val actIndices0 = actIndices.filter(_ % 3 == 0)
      val actIndices1 = actIndices.filter(_ % 3 == 1)
      val actIndices2 = actIndices.filter(_ % 3 == 2)
      val swapNH0 = new SwapActivity(scProblem, "Swap0", searchIndices = Some(() => actIndices0))
      val swapNH1 = new SwapActivity(scProblem, "Swap1", searchIndices = Some(() => actIndices1))
      val swapNH2 = new SwapActivity(scProblem, "Swap2", searchIndices = Some(() => actIndices2))
      val reinsertNH0 = new ReinsertActivity(scProblem, "Reinsert0", searchIndices = Some(() => actIndices0))
      val reinsertNH1 = new ReinsertActivity(scProblem, "Reinsert1", searchIndices = Some(() => actIndices1))
      val reinsertNH2 = new ReinsertActivity(scProblem, "Reinsert2", searchIndices = Some(() => actIndices2))
      Array(swapNH0, swapNH1, swapNH2, reinsertNH0, reinsertNH1, reinsertNH2)
    }
    val neighborhood = typeNb match {
      case Sequential => BestSlopeFirst(arrNH.toList)
      case DistFirst => new DistributedFirst(arrNH)
      case DistBest => new DistributedBest(arrNH)
    }

    // Final Print
    def finalPrint(): Unit = {
      val actPriorList = scProblem.activityPriorityList.value.toList
      println(s"*************** RESULTS ***********************************")
      println(s"Schedule makespan = ${scProblem.makeSpan.value}")
      println(s"Scheduling sequence = ${scProblem.activityPriorityList.value.toList}")
      println("Scheduling start times = [  ")
      for {a <- actPriorList} {
        val startTimeA = scProblem.startTimes(a.toInt).value
        val durationA = durations(a)
        println(s"    Activity $a : Start Time = $startTimeA : Duration : $durationA")
      }
      println("]")
    }

    (model, objFunc, neighborhood, () => finalPrint())
  }

  def executeProblemSeq(activities: List[ActivityId],
                        initialActivities: List[ActivityId],
                        durations: Map[ActivityId, Int],
                        minStartTimes: Map[ActivityId, Int],
                        precedencePairs: List[(ActivityId, ActivityId)],
                        resources: List[Resource],
                        typeNb: NBType): Unit = {
    val (_, obj, search, finalPrint) = createCBLSProblem(activities, initialActivities, durations, minStartTimes, precedencePairs, resources, typeNb)
    search.verbose = 1
    search.doAllMoves(obj = obj)
    println(search.profilingStatistics)
    finalPrint()
  }

  def executeProblemPar(activities: List[ActivityId],
                        initialActivities: List[ActivityId],
                        durations: Map[ActivityId, Int],
                        minStartTimes: Map[ActivityId, Int],
                        precedencePairs: List[(ActivityId, ActivityId)],
                        resources: List[Resource],
                        typeNb: NBType): Unit = {
    //supervisor side
    val (store, obj, search, finalPrint) = createCBLSProblem(activities, initialActivities, durations, minStartTimes, precedencePairs, resources, typeNb)
    print("Creating Actor System...")
    val supervisor = distrib.startSupervisorAndActorSystem(search)
    println("Done")
    //creating all the workers; here we only create local workers
    print(s"Creating $NB_WORKERS workers...")
    for (i <- ParRange(0, NB_WORKERS, 1, inclusive = false)) {
      val (store2, _, search2, _) = createCBLSProblem(activities, initialActivities, durations, minStartTimes, precedencePairs, resources, typeNb)
      supervisor.createLocalWorker(store2, search2)
      search2.verbose = 2
    }
    println("Done.")
    search.verbose = 1
    search.doAllMoves(obj = obj)
    println(search.profilingStatistics)
    supervisor.shutdown()
    finalPrint()
  }

  def main(args: Array[String]): Unit = {
    print("Creating Problem...")
    val (acts, initialActs, durations, minStartTimes, precPairs, resources) = createRandomProblem()
    println("******************** Sequential ********************")
    executeProblemSeq(acts, initialActs, durations, minStartTimes, precPairs, resources, Sequential)
    println("******************** DistributedFirst ********************")
    executeProblemPar(acts, initialActs, durations, minStartTimes, precPairs, resources, DistFirst)
    println("******************** DistributedBest  ********************")
    executeProblemPar(acts, initialActs, durations, minStartTimes, precPairs, resources, DistBest)
    println("****************************************")
  }
}
