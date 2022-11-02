package examples.oscar.cbls.distrib

import oscar.cbls.business.scheduling.ActivityId
import oscar.cbls.business.scheduling.model._
import oscar.cbls.business.scheduling.neighborhood.{ReinsertActivity, ReplaceActivity, SwapActivity}
import oscar.cbls.core.distrib
import oscar.cbls.core.search.Neighborhood
import oscar.cbls.lib.search.combinators.BestSlopeFirst
import oscar.cbls.lib.search.combinators.distributed.{DistributedBest, DistributedFirst}
import oscar.cbls.{Objective, Store}

import scala.collection.parallel.immutable.ParRange
import scala.util.Random

object SchedulingBigExampleDistributed {
  sealed trait NBType

  case object Sequential extends NBType

  case object DistFirst extends NBType

  case object DistBest extends NBType

  val nbWorkers = 6
  val nbAct = 200
  val nbRes = 20
  val minDuration = 1
  val maxDuration = 25
  val minStartTime = 0
  val maxStartTime = 10
  val minCapacity = 1L
  val maxCapacity = 25L
  val minRMRes = 0
  val maxRMRes = 10
  val densityUsageRes = 75
  val minSetupTimeRM = 0
  val maxSetupTimeRM = 25
  val densityPrecedencies = 50
  val densityMultiResources = 25
  val densityInitialActs = 75
  // Random Generator
  val randomGen = new Random()

  def randomBoolean(density: Int): Boolean = {
    val rdVal = randomGen.nextInt(100)
    rdVal < density
  }

  def randomInterval(inf: Int, sup: Int): Int = {
    require(inf <= sup)
    val offset = if (inf == sup) 0 else randomGen.nextInt(sup - inf)
    inf + offset
  }

  def randomInterval(inf: Long, sup: Long): Long = {
    require(inf <= sup)
    val rdVal = if (inf == sup) 0L else Math.abs(randomGen.nextLong()) % (sup - inf)
    inf + rdVal
  }

  def createRandomProblem(): (List[ActivityId], List[ActivityId], Map[ActivityId, Int], Map[ActivityId, Int], List[(ActivityId, ActivityId)], List[Resource]) = {
    // Activities
    //val nAct = randomInterval(1, nbAct)
    val nAct = nbAct
    val activities = (0 until nAct).toList
    // Durations
    var durations: Map[ActivityId, Int] = Map()
    var minStartTimes: Map[ActivityId, Int] = Map()
    var initials: List[ActivityId] = List()
    for {i <- 0 until nAct} {
      durations += (i -> randomInterval(minDuration, maxDuration))
      minStartTimes += (i -> randomInterval(minStartTime, maxStartTime))
      if (randomBoolean(densityInitialActs)) {
        initials ::= i
      }
    }
    // Precedencies
    val seqPairs = for {i <- 0 until nAct
                        j <- 0 until i
                        if randomBoolean(densityPrecedencies)} yield (i, j)
    val precPairs = seqPairs.toList
    // Resources
    //val nRes = randomInterval(0, nbRes)
    val nRes = nbRes
    val resources: Array[Resource] = Array.tabulate(nRes) { _ =>
      // Capacity for resource i
      val resCap = randomInterval(minCapacity, maxCapacity)
      // Setup Times
      //val nRMs = randomInterval(minRMRes, maxRMRes)
      val nRMs = maxRMRes
      val initialMode = randomInterval(0, nRMs - 1)
      // Activity usages
      val usageRes = for {i <- 0 until nAct
                          if randomBoolean(densityUsageRes)} yield (i, (randomInterval(1, resCap), randomInterval(0, nRMs - 1)))
      val tupMaps = usageRes.map { tuple => ((tuple._1, tuple._2._1), (tuple._1, tuple._2._2)) }.unzip
      val mapUsedCaps = tupMaps._1.toMap
      val mapRMs = tupMaps._2.toMap
      val stSeq = for {i <- 0 until nRMs
                       j <- 0 until nRMs
                       if i != j} yield ((i, j), randomInterval(minSetupTimeRM, maxSetupTimeRM))
      val mapSetupTimes = stSeq.toMap
      val setupTimes = SetupTimes(initialMode, mapRMs, mapSetupTimes)
      if (resCap == 1L) {
        new DisjunctiveResourceWithSetupTimes(mapUsedCaps.keys, setupTimes)
      } else if (randomBoolean(densityMultiResources)) {
        new CumulativeResourceWithSetupTimesMultiMode(resCap, mapUsedCaps, setupTimes)
      } else {
        new CumulativeResourceWithSetupTimes(resCap, mapUsedCaps, setupTimes)
      }
    }
    (activities, initials, durations, minStartTimes, precPairs, resources.toList)
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
    val actIndices = initialActivities.indices

    val arrNH: Array[Neighborhood] = if (typeNb == Sequential) {
      val swapNH = new SwapActivity(scProblem, "Swap")
      val reinsertNH = new ReinsertActivity(scProblem, "Reinsert")
      val replaceNH = new ReplaceActivity(scProblem, "Replace")
      Array(swapNH, reinsertNH, replaceNH)
    } else {
      val swapNH0 = new SwapActivity(scProblem, "Swap0", searchIndices = Some(() => actIndices.filter(_ % 2 == 0)))
      val swapNH1 = new SwapActivity(scProblem, "Swap1", searchIndices = Some(() => actIndices.filter(_ % 2 == 1)))
      val reinsertNH0 = new ReinsertActivity(scProblem, "Reinsert", searchIndices = Some(() => actIndices.filter(_ % 2 == 0)))
      val reinsertNH1 = new ReinsertActivity(scProblem, "Reinsert", searchIndices = Some(() => actIndices.filter(_ % 2 == 1)))
      val replaceNH0 = new ReplaceActivity(scProblem, "Replace0", searchValues = Some(() => actIndices.filter(_ % 2 == 0)))
      val replaceNH1 = new ReplaceActivity(scProblem, "Replace1", searchValues = Some(() => actIndices.filter(_ % 2 == 1)))
      Array(swapNH0, swapNH1, reinsertNH0, reinsertNH1, replaceNH0, replaceNH1)
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
        val startTimeA = scProblem.startTimes(a).value
        val durationA = durations(a)
        println(s"    Activity $a : Start Time = $startTimeA : Duration : $durationA")
      }
      println("]")
    }
    // Results
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
    print(s"Creating $nbWorkers workers...")
    for (_ <- ParRange(0, nbWorkers, 1, inclusive = false)) {
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
