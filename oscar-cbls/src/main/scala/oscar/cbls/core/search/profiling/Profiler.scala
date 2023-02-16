package oscar.cbls.core.search.profiling

import oscar.cbls.core.search.{Neighborhood, NeighborhoodCombinator}
import oscar.cbls.util.Properties


object Profiler {
  def selectedStatisticInfo(profilers:Iterable[Profiler]):String = {
    Properties.justifyRightArray(profilers.toList.flatMap(p => List(p.collectThisProfileHeader,p.collectThisProfileData))).mkString("\n")
  }

//  def nodeSelectionProfiling(nodeSelectionFunction: () => Int => Iterable[Int])(profiler: Profiler): () => Int => Iterable[Int] ={
//    nodeSelectionFunction()
//  }
}



class Profiler(val neighborhood:Neighborhood){

  def subProfilers:List[Profiler] = List.empty

  lazy val bpd: ProfilingData = ProfilingData()
  lazy val totalBpd: ProfilingData = ProfilingData()

  def gainPerCall:String = if(totalBpd.nbCalls ==0L) "NA" else s"${totalBpd.gain / totalBpd.nbCalls}"
  def callDuration:String = if(totalBpd.nbCalls == 0L ) "NA" else s"${totalBpd.timeSpent / totalBpd.nbCalls}"
  def slope:String = if(totalBpd.timeSpent == 0L) "NA" else s"${1000 * (totalBpd.gain.toDouble / totalBpd.timeSpent.toDouble).toLong}"
  def avgTimeSpendNoMove:String = if(totalBpd.nbCalls - totalBpd.nbFound == 0L) "NA" else s"${totalBpd.timeSpentNoMoveFound / (totalBpd.nbCalls - totalBpd.nbFound)}"
  def avgTimeSpendMove:String = if(totalBpd.nbFound == 0L) "NA" else s"${totalBpd.timeSpentMoveFound / totalBpd.nbFound}"
  def waistedTime:String = if(totalBpd.nbCalls - totalBpd.nbFound == 0L) "NA" else s"${totalBpd.timeSpentNoMoveFound / (totalBpd.nbCalls - totalBpd.nbFound)}"
  def nbExplored: String = "NA"
  def avgTimeExplore: String = "NA"
  def avgTimeFirstNeighborSelection: String = "NA"
  def avgTimeNotFirstNeighborSelection: String = "NA"

  final def collectThisProfileStatistics:List[Array[String]] = List(collectThisProfileHeader, collectThisProfileData)
  final def collectThisProfileHeader: Array[String] = Array("Neighborhood","calls", "found", "explored", "sumGain", "sumTime(ms)", "avgGain",
    "avgTime(ms)", "slope(-/s)", "avgTimeNoMove", "avgTimeMove", "wastedTime", "avgTimeExplored(ms)",
    "avgFirstSelectionTime(ms)", "avgOtherSelectionTime(ms)")
  final def collectThisProfileData:Array[String]= {
    Array[String](s"${neighborhood}",
      s"${totalBpd.nbCalls}", s"${totalBpd.nbFound}", nbExplored,
      s"${totalBpd.gain}", s"${totalBpd.timeSpent}", s"$gainPerCall", s"$callDuration", s"$slope",
      s"$avgTimeSpendNoMove", s"$avgTimeSpendMove", s"${totalBpd.timeSpentNoMoveFound}",
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

  def resetThisStatistics(): Unit = bpd.resetAll()
  // TODO : Should return a Boolean that tells if the operation is a success
  //        We need it because sometimes we wont have the same search structure.
  //        Especially when dealing with MU with no fixed depth.
  def merge(profiler: Profiler): Unit = {
    bpd.merge(profiler.bpd)
    totalBpd.merge(profiler.totalBpd)
  }

  override def toString: String = s"Profile(${neighborhood.toString})\nTOTAL : $totalBpd\nCURRENT : $bpd"
  def profiledNeighborhood: String = neighborhood.toString
  def detailedRecursiveName: String = s"${neighborhood.toString}"
}

/**
 * Base profiler for EasyNeighborhoods
 * @param neighborhood
 */
class NeighborhoodProfiler(override val neighborhood: Neighborhood) extends Profiler(neighborhood) {

  override lazy val bpd: NeighborhoodProfilingData = NeighborhoodProfilingData()
  override lazy val totalBpd: NeighborhoodProfilingData = NeighborhoodProfilingData()

  private var startExplorationAtMillis: Long = 0L
  private var lastNeighborExploredAtMillis: Long = 0L
  private var firstNeighborSelection: Boolean = true
  private var firstNeighborSelectionCounter: Long = 0L
  private var firstNeighborSelectionDuration: Long = 0L
  private var notFirstNeighborSelectionCounter: Long = 0L
  private var notFirstNeighborSelectionsDuration: Long = 0L

  def neighborSelected(): Unit = {
    val neighborSelectionAtMillis = System.currentTimeMillis()
    if(firstNeighborSelection) {
      firstNeighborSelectionCounter += 1
      firstNeighborSelectionDuration += neighborSelectionAtMillis - startExplorationAtMillis
      firstNeighborSelection = false
    } else {
      notFirstNeighborSelectionCounter += 1
      notFirstNeighborSelectionsDuration += neighborSelectionAtMillis - lastNeighborExploredAtMillis
    }
  }

  override def avgTimeFirstNeighborSelection: String = if(firstNeighborSelectionCounter == 0) "NA" else s"${firstNeighborSelectionDuration.toDouble/firstNeighborSelectionCounter}"
  override def avgTimeNotFirstNeighborSelection: String = if(notFirstNeighborSelectionCounter == 0) "NA" else s"${notFirstNeighborSelectionsDuration.toDouble/notFirstNeighborSelectionCounter}"

  override def subProfilers: List[Profiler] = List.empty
  override def nbExplored: String = totalBpd.nbExplored.toString
  override def avgTimeExplore: String = s"${(totalBpd.timeSpent.toDouble/totalBpd.nbExplored*1000).round/1000.0}"

  override def detailedRecursiveName: String = neighborhood.toString

  /**
   * Use this method to set up some profiling variables.
   * Like when did we start the neighborhood exploration, reset some variable...
   */
  def explorationStarted(): Unit ={
    startExplorationAtMillis = System.currentTimeMillis()
    firstNeighborSelection = true
    bpd.callIncr(); totalBpd.callIncr()
  }

  /**
   * Use this method to update some profiling variables.
   * Is this explored neighbor valid ? Did it violate a strong constraint ?
   * Did it made the obj function worse ? How much time ....
   */
  def neighborExplored(): Unit ={
    bpd.exploreIncr(); totalBpd.exploreIncr()
    lastNeighborExploredAtMillis = System.currentTimeMillis()
  }

  /**
   * Use this method to update some profiling variables.
   * Move found or not, total time searching ...
   */
  def explorationEnded(gain: Option[Long]): Unit ={
    val timeSpent = System.currentTimeMillis()-startExplorationAtMillis
    if(gain.nonEmpty) {
      bpd.timeSpentMoveFoundPlus(timeSpent); totalBpd.timeSpentMoveFoundPlus(timeSpent)
      bpd.foundIncr(); totalBpd.foundIncr()
      bpd.gainPlus(gain.get); totalBpd.gainPlus(gain.get)
    }
    else {
      bpd.timeSpentNoMoveFoundPlus(timeSpent); totalBpd.timeSpentNoMoveFoundPlus(timeSpent)
    }
  }

}


class CombinatorProfiler(val combinator: NeighborhoodCombinator) extends Profiler(combinator) {

  var explorationStartAt: Long = 0L

  override def subProfilers: List[Profiler] = combinator.subNeighborhoods.map(_.profiler)
  def collectSubProfilersInheritedStatisticsHeaders: Array[Array[String]] = Array(Array("Name"))
  def collectSubProfilersInheritedStatisticsData: Array[Array[String]] = Array(Array(combinator.getClass.getSimpleName))

  override def merge(profiler: Profiler): Unit ={
    val combinatorProfiler = profiler.asInstanceOf[CombinatorProfiler]
    bpd.merge(profiler.bpd)
    totalBpd.merge(profiler.totalBpd)
    combinator.subNeighborhoods.zip(combinatorProfiler.combinator.subNeighborhoods).
      foreach(sn => sn._1.profiler.merge(sn._2.profiler))
  }

  override def detailedRecursiveName: String = s"${neighborhood.toString}(${subProfilers.map(_.detailedRecursiveName).mkString(",")})"

  def explorationStarted(): Unit = {
    bpd.callIncr(); totalBpd.callIncr()
    explorationStartAt = System.currentTimeMillis()
  }
  def explorationEnded(gain: Option[Long]): Unit = {
    val timeSpent = System.currentTimeMillis()-explorationStartAt
    if(gain.nonEmpty) {
      bpd.foundIncr();totalBpd.foundIncr()
      bpd.gainPlus(gain.get);totalBpd.gainPlus(gain.get)
      bpd.timeSpentMoveFoundPlus(timeSpent);totalBpd.timeSpentMoveFoundPlus(timeSpent)
    } else {
      bpd.timeSpentNoMoveFoundPlus(timeSpent);totalBpd.timeSpentNoMoveFoundPlus(timeSpent)
    }
  }
}

class TransparentCombinatorProfiler(combinator: NeighborhoodCombinator) extends CombinatorProfiler(combinator) {
  // Lazy : the other profiler may not be already initiated
  override lazy val bpd: ProfilingData = if(combinator.subNeighborhoods.nonEmpty)combinator.subNeighborhoods.head.profiler.bpd else ProfilingData()
  override lazy val totalBpd: ProfilingData = if(combinator.subNeighborhoods.nonEmpty)combinator.subNeighborhoods.head.profiler.totalBpd else ProfilingData()

  override def merge(profiler: Profiler): Unit = {
    val dummyProfiler = profiler.asInstanceOf[TransparentCombinatorProfiler]
    combinator.subNeighborhoods.map(_.profiler).zip(dummyProfiler.combinator.subNeighborhoods.map(_.profiler)).foreach(x => x._1.merge(x._2))
  }
}

class SelectionProfiler(combinator: NeighborhoodCombinator, val neighborhoods: List[Neighborhood]) extends CombinatorProfiler(combinator) {

  override final def collectSubProfilersInheritedStatisticsHeaders: Array[Array[String]] =
    Array(Array("Name") ++ collectExtraProfileStatisticsHeader, Array("Name","Usage", "Success"))

  override final def collectSubProfilersInheritedStatisticsData: Array[Array[String]] = {
    collectExtraProfileStatisticsData.map(x =>Array(combinator.getClass.getSimpleName) ++ x) ++
      profilers.indices.map(pi => Array(s"${profilers(pi).neighborhood}",s"${neighborhoodUsage(pi)}%",s"${neighborhoodSuccess(pi)}%"))
  }

  ///////////////////////////////////////
  // Selection-Neighborhood management //
  ///////////////////////////////////////

  val profilers: Array[Profiler] = neighborhoods.map(_.profiler).toArray

  def totalTimeSpentSubN(i: Int): Long = profilers(i).totalBpd.timeSpent

  def neighborhoodUsage(neighborhoodId: Int): Double =
    ((profilers(neighborhoodId).totalBpd.nbFound.toDouble/profilers.map(_.totalBpd.nbFound).sum)*10000).round/100.0

  def neighborhoodSuccess(neighborhoodId: Int): Double =
    ((profilers(neighborhoodId).totalBpd.nbFound.toDouble/profilers(neighborhoodId).totalBpd.nbCalls)*10000).round/100.0

  def collectExtraProfileStatisticsHeader: Array[String] = Array.empty
  def collectExtraProfileStatisticsData: Array[Array[String]] = Array.empty
  override def detailedRecursiveName: String = s"${neighborhood.toString}(${profilers.map(_.detailedRecursiveName).mkString(",")})"
}

case class BestFirstProfiler(override val combinator: NeighborhoodCombinator,
                             override val neighborhoods: List[Neighborhood]) extends SelectionProfiler(combinator,neighborhoods){

  var nbReset: Int = 0
  var nbFirstFailed: Int = 0

  def slopeForCombinators(neighborhoodId: Int, defaultIfNoCall:Long = Long.MaxValue):Long =
    if(totalTimeSpentSubN(neighborhoodId) == 0L) defaultIfNoCall
    else ((1000L * totalGainSubN(neighborhoodId)) / totalTimeSpentSubN(neighborhoodId)).toInt

  def nbFoundSubN(i: Int): Long = profilers(i).totalBpd.nbFound
  def totalGainSubN(i: Int): Long = profilers(i).totalBpd.gain
  def totalTimeSpentMoveFoundSubN(i: Int): Long = profilers(i).totalBpd.timeSpentMoveFound

  def firstFailed(): Unit = nbFirstFailed += 1
  def resetSelectionNeighborhoodStatistics(): Unit ={
    nbReset += 1
    profilers.foreach(p => p.resetThisStatistics())
  }

  override def collectExtraProfileStatisticsHeader: Array[String] = Array(s"NbResets",s"NbFirstFailed")
  override def collectExtraProfileStatisticsData: Array[Array[String]] = Array(Array(s"$nbReset",s"$nbFirstFailed"))

  override def merge(profiler: Profiler): Unit = {
    val bestFirstProfiler = profiler.asInstanceOf[BestFirstProfiler]
    super.merge(profiler)
    nbReset += bestFirstProfiler.nbReset
    nbFirstFailed += bestFirstProfiler.nbFirstFailed
  }
}

case class CompositionProfiler(override val combinator: NeighborhoodCombinator, left: Neighborhood, right: () => Neighborhood) extends CombinatorProfiler(combinator){

  override def subProfilers: List[Profiler] = List(left.profiler) ++ dynNeighborhoodProfiler
  override final def collectSubProfilersInheritedStatisticsHeaders: Array[Array[String]] =
    Array(Array("Name"),Array("Name","Success (%)"))

  override final def collectSubProfilersInheritedStatisticsData: Array[Array[String]] = {
    Array(Array(combinator.getClass.getSimpleName) ++
      Array(s"${left.profiler.neighborhood}",s"${percFound(left.profiler.totalBpd)}")) ++
      dynNeighborhoodProfiler.toArray.map(dynP => Array(s"${dynP.neighborhood}",s"${percFound(dynP.totalBpd)}"))
  }

  var dynNeighborhoodProfiler: List[Profiler] = List.empty

  private def percFound(bdp: ProfilingData): Double ={
    ((bdp.nbFound.toDouble/bdp.nbCalls)*10000).toInt/100.0
  }

  override def merge(profiler: Profiler): Unit = {
    bpd.merge(profiler.bpd)
    totalBpd.merge(profiler.totalBpd)
    val compositionProfiler = profiler.asInstanceOf[CompositionProfiler]
    left.profiler.merge(compositionProfiler.left.profiler)
    compositionProfiler.dynNeighborhoodProfiler.foreach(mergeDynProfiler)
  }

  def mergeDynProfiler(profiler: Profiler): Unit ={
    val matchingProfilerOpt = dynNeighborhoodProfiler.find(x => x.detailedRecursiveName == profiler.detailedRecursiveName)
    if(matchingProfilerOpt.isDefined) matchingProfilerOpt.get.merge(profiler)
    else dynNeighborhoodProfiler = dynNeighborhoodProfiler :+ profiler
  }
}

case class GenericCombinatorProfiler(override val combinator: NeighborhoodCombinator,
                                     extraStatisticsHeader: List[String],
                                     baseValues: List[Long] = List.empty) extends CombinatorProfiler(combinator) {

  require(extraStatisticsHeader.size == baseValues.size || baseValues.isEmpty, "Either supply a base value for each extra stat or leave it empty")

  val extraStatistics: Array[Long] =
    if(baseValues.isEmpty) Array.fill(extraStatisticsHeader.size)(0L)
    else baseValues.toArray

  def statEqual(statistic: Int, value: Long): Unit =
    extraStatistics(statistic) = value

  def statPlus(statistic: Int, value: Long): Unit =
    extraStatistics(statistic) += value

  def statSub(statistic: Int, value: Long): Unit =
    extraStatistics(statistic) -= value

  def statMult(statistic: Int, value: Long): Unit =
    extraStatistics(statistic) *= value

  def statDiv(statistic: Int, value: Long): Unit =
    extraStatistics(statistic) /= value

  def statMin(statistic: Int, value: Long): Unit =
    extraStatistics(statistic) = Math.min(extraStatistics(statistic),value)

  def statMax(statistic: Int, value: Long): Unit =
    extraStatistics(statistic) = Math.max(extraStatistics(statistic), value)

  override def collectSubProfilersInheritedStatisticsHeaders: Array[Array[String]] =
    Array(Array("Name") ++ extraStatisticsHeader)

  override def collectSubProfilersInheritedStatisticsData: Array[Array[String]] = {
    Array(Array(s"$neighborhood") ++ extraStatistics.map(_.toString))
  }

  override def merge(profiler: Profiler): Unit = {
    super.merge(profiler)
    val basicProfiler = profiler.asInstanceOf[GenericCombinatorProfiler]
    for(i <- extraStatistics.indices)extraStatistics(i)+=basicProfiler.extraStatistics(i)
  }

}