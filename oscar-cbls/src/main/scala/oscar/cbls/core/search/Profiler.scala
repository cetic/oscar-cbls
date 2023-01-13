package oscar.cbls.core.search

import oscar.cbls.util.Properties

object BasicProfilingData {
  def apply(): BasicProfilingData = new BasicProfilingData()
}

class BasicProfilingData(){

  private var _nbCalls: Long = 0L
  private var _nbFound: Long = 0L
  private var _nbExplored: Long = 0L
  private var _gain: Long = 0L
  private var _timeSpentMoveFound: Long = 0L
  private var _timeSpentNoMoveFound: Long = 0L

  def merge(bpd: BasicProfilingData): Unit ={
    _nbCalls+=bpd._nbCalls; _nbFound+=bpd._nbFound
    _nbExplored+=bpd._nbExplored; _gain+=bpd._gain
    _timeSpentMoveFound+=bpd._timeSpentMoveFound; _timeSpentNoMoveFound+=bpd._timeSpentNoMoveFound
  }

  def gainPlus(gain: Long): Unit = this._gain += gain
  def callIncr(): Unit = _nbCalls+=1
  def foundIncr(): Unit = _nbFound+=1
  def exploreIncr(): Unit = _nbExplored+=1
  def explorePlus(value: Long): Unit = _nbExplored+=value
  def timeSpentMoveFoundPlus(time: Long): Unit = this._timeSpentMoveFound+=time
  def timeSpentNoMoveFoundPlus(time: Long): Unit = this._timeSpentNoMoveFound+=time

  def setNbCalls(value: Long): Unit = _nbCalls = value
  def setNbFounds(value: Long): Unit = _nbFound = value
  def setNbExplored(value: Long): Unit = _nbExplored = value
  def setGain(value: Long): Unit = _gain = value
  def setTimeSpentMoveFound(value: Long): Unit = _timeSpentMoveFound = value
  def setTimeSpentNoMoveFound(value: Long): Unit = _timeSpentNoMoveFound = value
  
  def nbCalls: Long = _nbCalls
  def nbFound: Long = _nbFound
  def nbExplored: Long = _nbExplored
  def gain: Long = _gain
  def timeSpentMoveFound: Long = _timeSpentMoveFound
  def timeSpentNoMoveFound: Long = _timeSpentNoMoveFound
  def timeSpent: Long = _timeSpentMoveFound + _timeSpentNoMoveFound

  def resetAll(): Unit = {
    _nbCalls = 0L; _nbFound = 0L
    _nbExplored = 0L; _gain = 0L
    _timeSpentMoveFound = 0L; _timeSpentNoMoveFound = 0L
  }

  override def toString: String = {
    s"Calls :${_nbCalls} | Founds :${_nbFound} | Explored :${_nbExplored} | " +
      s"gain :${_gain} | total time move :${_timeSpentMoveFound} | total time no move :${_timeSpentNoMoveFound}"
  }
}


object Profiler {
  def selectedStatisticInfo(profilers:Iterable[Profiler]):String = {
    Properties.justifyRightArray(profilers.toList.flatMap(p => List(p.collectThisProfileHeader,p.collectThisProfileData))).mkString("\n")
  }

//  def nodeSelectionProfiling(nodeSelectionFunction: () => Int => Iterable[Int])(profiler: Profiler): () => Int => Iterable[Int] ={
//    nodeSelectionFunction()
//  }
}



abstract class Profiler(val neighborhood:Neighborhood){

  lazy val bpd: BasicProfilingData = BasicProfilingData()
  lazy val totalBpd: BasicProfilingData = BasicProfilingData()

  def gainPerCall:String = if(totalBpd.nbCalls ==0L) "NA" else s"${totalBpd.gain / totalBpd.nbCalls}"
  def callDuration:String = if(totalBpd.nbCalls == 0L ) "NA" else s"${totalBpd.timeSpent / totalBpd.nbCalls}"
  //gain in obj/s
  def slope:String = if(totalBpd.timeSpent == 0L) "NA" else s"${1000 * (totalBpd.gain.toDouble / totalBpd.timeSpent.toDouble).toLong}"
  def avgTimeSpendNoMove:String = if(totalBpd.nbCalls - totalBpd.nbFound == 0L) "NA" else s"${totalBpd.timeSpentNoMoveFound / (totalBpd.nbCalls - totalBpd.nbFound)}"
  def avgTimeSpendMove:String = if(totalBpd.nbFound == 0L) "NA" else s"${totalBpd.timeSpentMoveFound / totalBpd.nbFound}"
  def avgTimeExplore: String = s"${(totalBpd.timeSpent.toDouble/totalBpd.nbExplored*1000).round/1000.0}"
  def waistedTime:String = if(totalBpd.nbCalls - totalBpd.nbFound == 0L) "NA" else s"${totalBpd.timeSpentNoMoveFound / (totalBpd.nbCalls - totalBpd.nbFound)}"
  def avgTimeFirstNeighborSelection: String = "NA"
  def avgTimeNotFirstNeighborSelection: String = "NA"

  def subProfilers:List[Profiler]
  def collectThisProfileStatistics:List[Array[String]] = List(collectThisProfileHeader, collectThisProfileData)
  final def collectThisProfileHeader: Array[String] = Array("Neighborhood","calls", "found", "explored", "sumGain", "sumTime(ms)", "avgGain",
    "avgTime(ms)", "slope(-/s)", "avgTimeNoMove", "avgTimeMove", "wastedTime", "avgTimeExplored(ms)",
    "avgFirstSelectionTime(ms)", "avgOtherSelectionTime(ms)")
  final def collectThisProfileData:Array[String]= {
    Array[String](s"${neighborhood}",
      s"${totalBpd.nbCalls}", s"${totalBpd.nbFound}", s"${totalBpd.nbExplored}",
      s"${totalBpd.gain}", s"${totalBpd.timeSpent}", s"$gainPerCall", s"$callDuration", s"$slope",
      s"$avgTimeSpendNoMove", s"$avgTimeSpendMove", s"${totalBpd.timeSpentNoMoveFound}",
      s"$avgTimeExplore", avgTimeFirstNeighborSelection, avgTimeNotFirstNeighborSelection)
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
  def merge(profiler: Profiler): Unit

  override def toString: String = s"Profile(${neighborhood.toString})\nTOTAL : $totalBpd\nCURRENT : $bpd"
  def profiledNeighborhood: String = neighborhood.toString
  def detailedRecursiveName: String
}

class EmptyProfiler(neighborhood: Neighborhood) extends Profiler(neighborhood) {
  override def subProfilers: List[Profiler] = List.empty
  // Nothing to do
  override def resetThisStatistics(): Unit = {}
  override def merge(profiler: Profiler): Unit = {}
  override def detailedRecursiveName: String = neighborhood.toString
}

/**
 * Base profiler for EasyNeighborhoods
 * @param neighborhood
 */
class NeighborhoodProfiler(override val neighborhood: Neighborhood) extends Profiler(neighborhood) {

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



  override def merge(profiler: Profiler): Unit ={
    bpd.merge(profiler.bpd)
    totalBpd.merge(profiler.totalBpd)
  }

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
  def collectThisCombinatorProfileStatisticsHeaders: Array[Array[String]] = Array(Array("Name"))
  def collectThisCombinatorProfileStatisticsData: Array[Array[String]] = Array(Array(combinator.getClass.getSimpleName))

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
    bpd.exploreIncr();totalBpd.exploreIncr()
    if(gain.nonEmpty) {
      bpd.foundIncr();totalBpd.foundIncr()
      bpd.gainPlus(gain.get);totalBpd.gainPlus(gain.get)
      bpd.timeSpentMoveFoundPlus(timeSpent);totalBpd.timeSpentMoveFoundPlus(timeSpent)
    } else {
      bpd.timeSpentNoMoveFoundPlus(timeSpent);totalBpd.timeSpentNoMoveFoundPlus(timeSpent)
    }
  }
}

class DummyCombinatorProfiler(combinator: NeighborhoodCombinator) extends CombinatorProfiler(combinator) {
  // Lazy : the other profiler may not be already initiated
  override lazy val bpd: BasicProfilingData = if(combinator.subNeighborhoods.nonEmpty)combinator.subNeighborhoods.head.profiler.bpd else BasicProfilingData()
  override lazy val totalBpd: BasicProfilingData = if(combinator.subNeighborhoods.nonEmpty)combinator.subNeighborhoods.head.profiler.totalBpd else BasicProfilingData()

  override def merge(profiler: Profiler): Unit = {
    val dummyProfiler = profiler.asInstanceOf[DummyCombinatorProfiler]
    combinator.subNeighborhoods.map(_.profiler).zip(dummyProfiler.combinator.subNeighborhoods.map(_.profiler)).foreach(x => x._1.merge(x._2))
  }
}

class SelectionProfiler(combinator: NeighborhoodCombinator, val neighborhoods: List[Neighborhood]) extends CombinatorProfiler(combinator) {

  override final def collectThisCombinatorProfileStatisticsHeaders: Array[Array[String]] =
    Array(Array("Name") ++ collectExtraProfileStatisticsHeader, Array("Name","Usage", "Success"))

  override final def collectThisCombinatorProfileStatisticsData: Array[Array[String]] = {
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
  override final def collectThisCombinatorProfileStatisticsHeaders: Array[Array[String]] =
    Array(Array("Name"),Array("Name","Success (%)","Perf. (ms/expl.)"))

  override final def collectThisCombinatorProfileStatisticsData: Array[Array[String]] = {
    Array(Array(combinator.getClass.getSimpleName) ++
      Array(s"${left.profiler.neighborhood}",s"${percFound(left.profiler.totalBpd)}", s"${performance(left.profiler)}")) ++
      dynNeighborhoodProfiler.toArray.map(dynP => Array(s"${dynP.neighborhood}",s"${percFound(dynP.totalBpd)}", s"${performance(dynP)}"))
  }

  var dynNeighborhoodProfiler: List[Profiler] = List.empty

  private def percFound(bdp: BasicProfilingData): Double ={
    ((bdp.nbFound.toDouble/bdp.nbCalls)*10000).toInt/100.0
  }

  private def performance(subProfiler: Profiler): Double =
    (subProfiler.totalBpd.timeSpent.toDouble/subProfiler.totalBpd.nbExplored*1000).round.toDouble/1000.0

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

case class BasicProfiler(override val combinator: NeighborhoodCombinator,
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

  def statMin(statistic: Int, value: Long): Unit =
    extraStatistics(statistic) -= value

  def statMult(statistic: Int, value: Long): Unit =
    extraStatistics(statistic) *= value

  def statDiv(statistic: Int, value: Long): Unit =
    extraStatistics(statistic) /= value

  override def collectThisCombinatorProfileStatisticsHeaders: Array[Array[String]] =
    Array(Array("Name") ++ extraStatisticsHeader)

  override def collectThisCombinatorProfileStatisticsData: Array[Array[String]] = {
    Array(Array(s"$neighborhood") ++ extraStatistics.map(_.toString))
  }

  override def merge(profiler: Profiler): Unit = {
    super.merge(profiler)
    val basicProfiler = profiler.asInstanceOf[BasicProfiler]
    for(i <- extraStatistics.indices)extraStatistics(i)+=basicProfiler.extraStatistics(i)
  }

}