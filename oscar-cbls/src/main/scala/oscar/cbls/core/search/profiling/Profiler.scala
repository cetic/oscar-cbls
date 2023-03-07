package oscar.cbls.core.search.profiling

import oscar.cbls.core.search.{Neighborhood, NeighborhoodCombinator}
import oscar.cbls.util.Properties

import scala.collection.mutable


object Profiler {
  def selectedStatisticInfo(profilers:Iterable[Profiler]):String = {
    Properties.justifyRightArray(profilers.toList.flatMap(p => List(p.collectThisProfileHeader,p.collectThisProfileData))).mkString("\n")
  }
}


/**
 * This class purpose is to profile a Neighborhood during the search.
 * By using the explorationStarted and explorationEnded method you'll get various information as :
 * - The total gain
 * - Total call
 * - The time spent
 * _ ...
 *
 * Be aware that the explorationStarted and explorationEnded methods are called within the
 * Neighborhood.profiledGetMove() method so you don't have to call them manually if you use this method instead of getMove()
 *
 * @param neighborhood :  the profiled Neighborhood
 */
class Profiler(val neighborhood:Neighborhood){

  protected var startExplorationAt = 0L
  private var currentExplorationTimeSpent = 0L
  private var explorationPausedAt = 0L
  private var explorationResumedAt = 0L

  def subProfilers:List[Profiler] = List.empty

  def explorationStarted(): Unit = {
    commonProfilingData.callInc()
    startExplorationAt = System.nanoTime()
    explorationPausedAt = 0L
    explorationResumedAt = 0L
    currentExplorationTimeSpent = 0L
  }

  def explorationPaused(): Unit = {
    explorationPausedAt = System.nanoTime()
    currentExplorationTimeSpent += explorationPausedAt-Math.max(startExplorationAt,explorationResumedAt)
  }
  def explorationResumed(): Unit = explorationResumedAt = System.nanoTime()

  def explorationEnded(gain: Option[Long]): Unit = {
    val timeSpent = currentExplorationTimeSpent + System.nanoTime() - Math.max(startExplorationAt,explorationResumedAt)
    if (gain.nonEmpty) {
      commonProfilingData.foundInc()
      commonProfilingData.gainPlus(gain.get)
      commonProfilingData.timeSpentMoveFoundPlus(timeSpent)
    } else {
      commonProfilingData.timeSpentNoMoveFoundPlus(timeSpent)
    }
  }

  // The object that holds the common profiling data (meaning nbCalls,AvgTimeMove,nbFound...)
  lazy val commonProfilingData: CommonProfilingData = new CommonProfilingData()

  private def gainPerCall:String = if(commonProfilingData.nbCalls ==0L) "NA" else s"${commonProfilingData.gain / commonProfilingData.nbCalls}"
  private def callDuration:String = if(commonProfilingData.nbCalls == 0L ) "NA" else s"${commonProfilingData.timeSpentMillis / commonProfilingData.nbCalls}"
  private def slope:String = if(commonProfilingData.timeSpentMillis == 0L) "NA" else s"${1000 * (commonProfilingData.gain.toDouble / commonProfilingData.timeSpentMillis.toDouble).toLong}"
  private def avgTimeSpendNoMove:String = if(commonProfilingData.nbCalls - commonProfilingData.nbFound == 0L) "NA" else s"${commonProfilingData.timeSpentNoMoveFoundMillis / (commonProfilingData.nbCalls - commonProfilingData.nbFound)}"
  private def avgTimeSpendMove:String = if(commonProfilingData.nbFound == 0L) "NA" else s"${commonProfilingData.timeSpentMoveFoundMillis / commonProfilingData.nbFound}"
  protected def nbExplored: String = "NA"
  protected def avgTimeExplore: String = "NA"
  protected def avgTimeFirstNeighborSelection: String = "NA"
  protected def avgTimeNotFirstNeighborSelection: String = "NA"

  final def collectThisProfileStatistics:List[Array[String]] = List(collectThisProfileHeader, collectThisProfileData)
  final def collectThisProfileHeader: Array[String] = Array("Neighborhood","calls", "found", "explored", "sumGain", "sumTime(ms)", "avgGain",
    "avgTime(ms)", "slope(-/s)", "avgTimeNoMove", "avgTimeMove", "wastedTime", "avgTimeExplored(ms)",
    "avgFirstSelectionTime(ms)", "avgOtherSelectionTime(ms)")
  final def collectThisProfileData:Array[String]= {
    Array[String](s"$neighborhood",
      s"${commonProfilingData.nbCalls}", s"${commonProfilingData.nbFound}", nbExplored,
      s"${commonProfilingData.gain}", s"${commonProfilingData.timeSpentMillis}", s"$gainPerCall", s"$callDuration", s"$slope",
      s"$avgTimeSpendNoMove", s"$avgTimeSpendMove", s"${commonProfilingData.timeSpentNoMoveFoundMillis}",
      avgTimeExplore, avgTimeFirstNeighborSelection, avgTimeNotFirstNeighborSelection)
  }

  def goodValueIndicator(): Array[Option[String]] = {
    // Neighborhood, calls, founds, explored, sumGain, sumTime, avgGain
    Array(None, None, Some("Max"), None, Some("Max"), Some("Min"), Some("Max"),
      // avgTime, slope, avgTimeNoMove, avgTimeMove, wastedTime, avtTimeExplored
      Some("Min"), Some("Max"), Some("Min"), Some("Min"), Some("Min"), Some("Min"),
      // avgFirstNeighborSelectionTime, avgNotFirstNeighborSelectionTime
      Some("Min"), Some("Min"))
  }

  // Use this method if you need to reset some statistics (but keeping the total statistics)
  // For instance when using BestSlopeFirst
  def resetThisStatistics(): Unit = commonProfilingData.resetStatisticsForSelection()
  // This method is used when you need to merge to IDENTICAL provider.
  // For instance with the DynAndThen generating dynamic neighborhood
  def merge(profiler: Profiler): Unit = {
    commonProfilingData.merge(profiler.commonProfilingData)
  }

  override def toString: String = s"Profile(${neighborhood.toString})\n$commonProfilingData"
  // Get the detailedRecursiveName, the idea is to be able to distinguish two generated neighborhood no matter the depth
  def detailedRecursiveName: String = s"${neighborhood.toString}"
}

/**
 * Base profiler for EasyNeighborhoods
 * It also tracks the exploration timing, the nb of exploration, the selection duration...
 *
 * @param neighborhood the profiled neighborhood
 */
class NeighborhoodProfiler(override val neighborhood: Neighborhood) extends Profiler(neighborhood) {

  override lazy val commonProfilingData: NeighborhoodProfilingData = NeighborhoodProfilingData()

  private var lastNeighborExploredAt: Long = 0L
  private var firstNeighborSelection: Boolean = true

  def neighborSelected(): Unit = {
    val neighborSelectionAt = System.nanoTime()
    if(firstNeighborSelection) {
      val selectionDuration = neighborSelectionAt - startExplorationAt
      commonProfilingData.firstNeighborSelectionCounterInc()
      commonProfilingData.firstNeighborSelectionDurationPlus(selectionDuration)
      firstNeighborSelection = false
    } else {
      val selectionDuration = neighborSelectionAt - lastNeighborExploredAt
      commonProfilingData.notFirstNeighborSelectionCounterInc()
      commonProfilingData.notFirstNeighborSelectionDurationPlus(selectionDuration)
    }
  }

  override def avgTimeFirstNeighborSelection: String = commonProfilingData.avgTimeFirstNeighborSelectionMillis()
  override def avgTimeNotFirstNeighborSelection: String = commonProfilingData.avgTimeNotFirstNeighborSelectionMillis()
  override def nbExplored: String = commonProfilingData.nbExplored.toString
  override def avgTimeExplore: String = s"${(commonProfilingData.timeSpentMillis.toDouble/commonProfilingData.nbExplored*1000).round/1000.0}"

  override def explorationStarted(): Unit ={
    firstNeighborSelection = true
    super.explorationStarted()
  }

  def neighborExplored(): Unit ={
    commonProfilingData.exploreInc()
    lastNeighborExploredAt = System.nanoTime()
  }

}

/**
 * Base class for combinator profiler.
 * It allows you to profile specifics data following one of the four CombinatorProfilingData defined in ProfilingData file.
 * @param combinator The profiled combinator
 */
class CombinatorProfiler(val combinator: NeighborhoodCombinator) extends Profiler(combinator) {

  override def subProfilers: List[Profiler] = combinator.subNeighborhoods.map(_.profiler)

  // Merge this profiler data and the sub-profiler data (recursively)
  override def merge(profiler: Profiler): Unit ={
    val combinatorProfiler = profiler.asInstanceOf[CombinatorProfiler]
    commonProfilingData.merge(profiler.commonProfilingData)
    mergeSpecificStatistics(combinatorProfiler)
    combinator.subNeighborhoods.zip(combinatorProfiler.combinator.subNeighborhoods).
      foreach(sn => sn._1.profiler.merge(sn._2.profiler))
  }

  override def detailedRecursiveName: String = s"${neighborhood.toString}(${subProfilers.map(_.detailedRecursiveName).mkString(",")})"

  // MinMeanMax : See ProfilingData.MinMeanMaxData
  /////////////
  private val minMeanMaxProfiledData: mutable.HashMap[String, MinMeanMaxData] = mutable.HashMap.empty
  // Profile a new value
  def minMeanMaxProfile(name: String): Unit = minMeanMaxProfiledData.addOne(name,MinMeanMaxData())
  def minMeanMaxAddValue(name: String,value: Long): Unit = minMeanMaxProfiledData(name).add(value)

  // occurrence per iteration : See ProfilingData.NbOccurrencesPerIteration
  ///////////////////////////
  private val nbOccurrencesPerIterationData: mutable.HashMap[String,NbOccurrencesPerIteration] = mutable.HashMap.empty
  // Profile a new value
  def nbOccurrencePerIterationProfile(name: String, initFirstIteration: Boolean = false): Unit =
    nbOccurrencesPerIterationData.addOne(name,NbOccurrencesPerIteration(initFirstIteration))
  def nbOccurrencePerIterationNextIteration(name: String): Unit =
    nbOccurrencesPerIterationData(name).nextIteration()
  def nbOccurrencePerIterationEventOccurred(name: String): Unit =
    nbOccurrencesPerIterationData(name).eventOccurred()

  // percentage occurrence : See ProfilingData.PercentageEventOccurrence
  ////////////////////////
  private val percentageEventOccurrenceData: mutable.HashMap[String,PercentageEventOccurrence] = mutable.HashMap.empty
  // Profile a new value
  def percentageEventOccurrenceProfile(name: String): Unit =
    percentageEventOccurrenceData.addOne(name, PercentageEventOccurrence())
  def percentageEventOccurrencePushEvent(name: String,occurred: Boolean): Unit =
    percentageEventOccurrenceData(name).pushEvent(occurred)

  // summed value : See ProfilingData.SummedValue
  ///////////////
  private val summedValueProfiledData: mutable.HashMap[String,SummedValue] = mutable.HashMap.empty
  // Profile a new value
  def summedValueProfile(name: String): Unit = summedValueProfiledData.addOne(name, SummedValue())
  def summedValuePlus(name: String, value: Long): Unit = summedValueProfiledData(name).plus(value)

  private def collectSpecificStatistic(data: mutable.HashMap[String,CombinatorProfilingData]): List[List[String]] =
    List(
      if (data.isEmpty) List.empty[String]
      else
        Properties.justifyRightArray(List(Array("Profiled var") ++ data.values.head.collectStatisticsHeaders()) ++
        data.keys.map(key => Array(key) ++ data(key).collectStatisticsData())))

  protected def mergeSpecificStatistics(other: CombinatorProfiler): Unit ={
    minMeanMaxProfiledData.keys.foreach(key => minMeanMaxProfiledData(key).merge(other.minMeanMaxProfiledData(key)))
    nbOccurrencesPerIterationData.keys.foreach(key => nbOccurrencesPerIterationData(key).merge(other.nbOccurrencesPerIterationData(key)))
    percentageEventOccurrenceData.keys.foreach(key => percentageEventOccurrenceData(key).merge(other.percentageEventOccurrenceData(key)))
    summedValueProfiledData.keys.foreach(key => summedValueProfiledData(key).merge(other.summedValueProfiledData(key)))
  }

  def collectCombinatorSpecificStatistics: List[List[String]] = {
    List(List(combinator.getClass.getSimpleName)) :::
      collectSpecificStatistic(minMeanMaxProfiledData.asInstanceOf[mutable.HashMap[String,CombinatorProfilingData]]) :::
      collectSpecificStatistic(nbOccurrencesPerIterationData.asInstanceOf[mutable.HashMap[String,CombinatorProfilingData]]) :::
      collectSpecificStatistic(percentageEventOccurrenceData.asInstanceOf[mutable.HashMap[String,CombinatorProfilingData]]) :::
      collectSpecificStatistic(summedValueProfiledData.asInstanceOf[mutable.HashMap[String,CombinatorProfilingData]])
  }
}

/**
 * Base profiler for Selection type combinator.
 * It add a comparison dimension, allowing the user to see which neighborhood has been used and in what proportion...
 * @param combinator The profiled combinator
 * @param neighborhoods The list of supervised neighborhood
 */
class SelectionProfiler(combinator: NeighborhoodCombinator, val neighborhoods: List[Neighborhood]) extends CombinatorProfiler(combinator) {

  override def collectCombinatorSpecificStatistics: List[List[String]] =
    super.collectCombinatorSpecificStatistics ++ List(Properties.justifyRightArray(List(Array("Name","Usage", "Success")) ++
      profilers.indices.map(pi => Array(s"${profilers(pi).neighborhood}", s"${neighborhoodUsage(pi)}%", s"${neighborhoodSuccess(pi)}%"))))

  ///////////////////////////////////////
  // Selection-Neighborhood management //
  ///////////////////////////////////////

  val profilers: Array[Profiler] = neighborhoods.map(_.profiler).toArray

  protected def totalTimeSpentSubN(i: Int): Long = profilers(i).commonProfilingData.timeSpentMillisForSelection

  private def neighborhoodUsage(neighborhoodId: Int): Double =
    ((profilers(neighborhoodId).commonProfilingData.nbFound.toDouble/profilers.map(_.commonProfilingData.nbFound).sum)*10000).round/100.0

  private def neighborhoodSuccess(neighborhoodId: Int): Double =
    ((profilers(neighborhoodId).commonProfilingData.nbFound.toDouble/profilers(neighborhoodId).commonProfilingData.nbCalls)*10000).round/100.0

  override def detailedRecursiveName: String = s"${neighborhood.toString}(${profilers.map(_.detailedRecursiveName).mkString(",")})"
}

/**
 * A selection profiler specialized for BestFirstProfiler
 * @param combinator The profiled combinator
 * @param neighborhoods The list of supervised neighborhood
 */
case class BestFirstProfiler(override val combinator: NeighborhoodCombinator,
                             override val neighborhoods: List[Neighborhood]) extends SelectionProfiler(combinator,neighborhoods){

  nbOccurrencePerIterationProfile("NbFirstFailedPerReset")

  def slopeForCombinators(neighborhoodId: Int, defaultIfNoCall:Long = Long.MaxValue):Long =
    if(totalTimeSpentSubN(neighborhoodId) == 0L) defaultIfNoCall
    else ((1000L * totalGainSubN(neighborhoodId)) / totalTimeSpentSubN(neighborhoodId)).toInt

  def nbFoundSubN(i: Int): Long = profilers(i).commonProfilingData.nbFoundForSelection
  private def totalGainSubN(i: Int): Long = profilers(i).commonProfilingData.gainForSelection
  def totalTimeSpentMoveFoundSubN(i: Int): Long = profilers(i).commonProfilingData.timeSpentMoveFoundMillisForSelection

  def firstFailed(): Unit = nbOccurrencePerIterationEventOccurred("NbFirstFailedPerReset")
  def resetSelectionNeighborhoodStatistics(): Unit ={
    nbOccurrencePerIterationNextIteration("NbFirstFailedPerReset")
    profilers.foreach(p => p.resetThisStatistics())
  }
}

/**
 * A Profiler specialized for Composition type combinator.
 * It add the possibility to handle dynamically generated neighborhood by merging their data (if compatible)
 * @param combinator The profiled combinator
 * @param left [optional] the first Neighborhood (not dynamically generated)
 */
case class CompositionProfiler(override val combinator: NeighborhoodCombinator, left: Option[Neighborhood] = None) extends CombinatorProfiler(combinator){

  override def subProfilers: List[Profiler] = (if(left.isDefined) List(left.get.profiler) else List.empty) ++ dynNeighborhoodProfiler

  private var dynNeighborhoodProfiler: List[Profiler] = List.empty

  /*
    1° merge common profiling data
    2° merge combinator specific statistics
    3° merge left neighborhood and dynamically generated one
   */
  override def merge(profiler: Profiler): Unit = {
    val compositionProfiler = profiler.asInstanceOf[CompositionProfiler]
    commonProfilingData.merge(profiler.commonProfilingData)
    mergeSpecificStatistics(compositionProfiler)
    if(left.isDefined)left.get.profiler.merge(compositionProfiler.left.get.profiler)
    compositionProfiler.dynNeighborhoodProfiler.foreach(mergeDynProfiler)
  }

  /*
   * Within Composition combinator, neighborhood are generated during the search each time we use it,
   * meaning that a new profiler is created each time with initial value.
   * To ensure that we have proper value, we must merge the old profiler with the new one.
   * That's the purpose of this method.
   *
   * We first determine if we already encountered this type of neighborhood.
   * To do that we use the detailedRecursiveName of the neighborhood (basically getting all the structure of the neighborhood)
   *
   * We DON'T WANT TO MERGE when :
   *  - The B neighborhood isn't generic (when using andThen for instance) hence the eq check
   */
  def mergeDynProfiler(profiler: Profiler): Unit ={
    if (!dynNeighborhoodProfiler.exists(x => x eq profiler)) {
      val matchingProfilerOpt = dynNeighborhoodProfiler.find(x => x.detailedRecursiveName == profiler.detailedRecursiveName)
      if (matchingProfilerOpt.isDefined) matchingProfilerOpt.get.merge(profiler)
      else dynNeighborhoodProfiler = dynNeighborhoodProfiler :+ profiler
    }
  }
}