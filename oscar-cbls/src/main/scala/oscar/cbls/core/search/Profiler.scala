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

  def subProfilers:List[Profiler]
  def collectThisProfileStatistics:List[Array[String]] = List(collectThisProfileHeader, collectThisProfileData)
  def collectThisProfileHeader:Array[String]
  def collectThisProfileData:Array[String]

  def resetThisStatistics(): Unit = bpd.resetAll()
  // TODO : Should return a Boolean that tells if the operation is a success
  //        We need it because sometimes we wont have the same search structure.
  //        Especially when dealing with MU with no fixed depth.
  def merge(profiler: Profiler): Unit

  override def toString: String = s"Profile(${neighborhood.toString})\nTOTAL : $totalBpd\nCURRENT : $bpd"
  def profiledNeighborhood: String = neighborhood.toString
}

class EmptyProfiler(neighborhood: Neighborhood) extends Profiler(neighborhood) {
  override def subProfilers: List[Profiler] = List.empty
  override def collectThisProfileHeader: Array[String] = Array.empty
  override def collectThisProfileData: Array[String] = Array.empty
  // Nothing to do
  override def resetThisStatistics(): Unit = {}
  override def merge(profiler: Profiler): Unit = {}
}

/**
 * Base profiler for EasyNeighborhoods
 * @param neighborhood
 */
class NeighborhoodProfiler(override val neighborhood: Neighborhood) extends Profiler(neighborhood) {

  private var startExplorationAtMillis: Long = 0L
  private var lastNeighborSelectionAtMillis: Long = 0L
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
      notFirstNeighborSelectionsDuration += neighborSelectionAtMillis - lastNeighborSelectionAtMillis
    }
    lastNeighborSelectionAtMillis = neighborSelectionAtMillis
  }

  def gainPerCall:String = if(totalBpd.nbCalls ==0L) "NA" else s"${totalBpd.gain / totalBpd.nbCalls}"
  def callDuration:String = if(totalBpd.nbCalls == 0L ) "NA" else s"${totalBpd.timeSpent / totalBpd.nbCalls}"
  //gain in obj/s
  def slope:String = if(totalBpd.timeSpent == 0L) "NA" else s"${1000 * (totalBpd.gain.toDouble / totalBpd.timeSpent.toDouble).toLong}"

  def avgTimeSpendNoMove:String = if(totalBpd.nbCalls - totalBpd.nbFound == 0L) "NA" else s"${totalBpd.timeSpentNoMoveFound / (totalBpd.nbCalls - totalBpd.nbFound)}"
  def avgTimeSpendMove:String = if(totalBpd.nbFound == 0L) "NA" else s"${totalBpd.timeSpentMoveFound / totalBpd.nbFound}"
  def avgTimeExplore: String = s"${(totalBpd.timeSpent.toDouble/totalBpd.nbExplored*1000).round/1000.0}"
  def avgTimeFirstNeighborSelection: String = if(firstNeighborSelectionCounter == 0) "NA" else s"${firstNeighborSelectionDuration.toDouble/firstNeighborSelectionCounter}"
  def avgTimeNotFirstNeighborSelection: String = if(notFirstNeighborSelectionCounter == 0) "NA" else s"${notFirstNeighborSelectionsDuration.toDouble/notFirstNeighborSelectionCounter}"
  def waistedTime:String = if(totalBpd.nbCalls - totalBpd.nbFound == 0L) "NA" else s"${totalBpd.timeSpentNoMoveFound / (totalBpd.nbCalls - totalBpd.nbFound)}"

  override def subProfilers: List[Profiler] = List.empty

  def goodValueIndicator(): Array[Option[String]] = {
    // Neighborhood, calls, founds, explored, sumGain, sumTime, avgGain
    Array(None, None, Some("Max"), None, Some("Max"), Some("Min"), Some("Max"),
      // avgTime, slope, avgTimeNoMove, avgTimeMove, wastedTime, avtTimeExplored
      Some("Min"), Some("Max"), Some("Min"), Some("Min"), Some("Min"), Some("Min"),
      // avgFirstNeighborSelectionTime, avgNotFirstNeighborSelectionTime
      Some("Min"), Some("Min"))
  }

  override def collectThisProfileHeader: Array[String] = Array("Neighborhood","calls", "found", "explored", "sumGain", "sumTime(ms)", "avgGain",
    "avgTime(ms)", "slope(-/s)", "avgTimeNoMove", "avgTimeMove", "wastedTime", "avgTimeExplored(ms)",
    "avgFirstSelectionTime(ms)", "avgOtherSelectionTime(ms)")

  override def collectThisProfileData:Array[String] =
    {
      Array[String](s"${neighborhood}",
        s"${totalBpd.nbCalls}", s"${totalBpd.nbFound}", s"${totalBpd.nbExplored}",
        s"${totalBpd.gain}", s"${totalBpd.timeSpent}", s"$gainPerCall", s"$callDuration", s"$slope",
        s"$avgTimeSpendNoMove", s"$avgTimeSpendMove", s"${totalBpd.timeSpentNoMoveFound}",
        s"$avgTimeExplore", avgTimeFirstNeighborSelection, avgTimeNotFirstNeighborSelection)
    }

  override def merge(profiler: Profiler): Unit ={
    bpd.merge(profiler.bpd)
    totalBpd.merge(profiler.totalBpd)
  }

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
  override def collectThisProfileHeader: Array[String] = Array("Combinator", "Total calls", "% Founds", "Total time (ms)")

  override def collectThisProfileData: Array[String] =
    Array(combinator.getClass.getSimpleName, totalBpd.nbCalls.toString,
      (Math.floor(totalBpd.nbFound.toDouble/totalBpd.nbCalls*100000)/1000).toString, totalBpd.timeSpent.toString)

  override def merge(profiler: Profiler): Unit ={
    val combinatorProfiler = profiler.asInstanceOf[CombinatorProfiler]
    bpd.merge(profiler.bpd)
    totalBpd.merge(profiler.totalBpd)
    combinator.subNeighborhoods.zip(combinatorProfiler.combinator.subNeighborhoods).
      foreach(sn => sn._1.profiler.merge(sn._2.profiler))
  }

  def explorationStarted(): Unit = {
    bpd.callIncr(); totalBpd.callIncr()
    explorationStartAt = System.currentTimeMillis()
  }
  def explorationEnded(found: Boolean): Unit = {
    val timeSpent = System.currentTimeMillis()-explorationStartAt
    if(found) {
      bpd.foundIncr();totalBpd.foundIncr()
      bpd.timeSpentMoveFoundPlus(timeSpent);totalBpd.timeSpentMoveFoundPlus(timeSpent)
    } else {
      bpd.timeSpentNoMoveFoundPlus(timeSpent);totalBpd.timeSpentNoMoveFoundPlus(timeSpent)
    }
  }
}

class DummyCombinatorProfiler(combinator: NeighborhoodCombinator) extends CombinatorProfiler(combinator) {
  // Lazy : the other profiler may not be already initiated
  override lazy val bpd: BasicProfilingData = combinator.subNeighborhoods.head.profiler.bpd
  override lazy val totalBpd: BasicProfilingData = combinator.subNeighborhoods.head.profiler.totalBpd

  override def merge(profiler: Profiler): Unit = {
    val dummyProfiler = profiler.asInstanceOf[DummyCombinatorProfiler]
    combinator.subNeighborhoods.map(_.profiler).zip(dummyProfiler.combinator.subNeighborhoods.map(_.profiler)).foreach(x => x._1.merge(x._2))
  }

  override def collectThisProfileHeader: Array[String] = Array.empty
  override def collectThisProfileData: Array[String] = Array.empty
}

class SelectionProfiler(combinator: NeighborhoodCombinator, val neighborhoods: List[Neighborhood]) extends CombinatorProfiler(combinator) {

  override final def collectThisProfileHeader: Array[String] =
    super.collectThisProfileHeader ++
      collectExtraProfileHeader ++
      profilers.flatMap(p => Array("|", " ", "Usage (%)", "Success (%)"))
  override final def collectThisProfileData: Array[String] =
    super.collectThisProfileData ++
      collectExtraProfileData ++
      profilers.indices.flatMap(pi =>
        Array("|", profilers(pi).neighborhood.toString,
          s"${neighborhoodUsage(pi)}",
          s"${neighborhoodSuccess(pi)}")
        )

  ///////////////////////////////////////
  // Selection-Neighborhood management //
  ///////////////////////////////////////

  val profilers: Array[Profiler] = neighborhoods.map(_.profiler).toArray

  def totalTimeSpentSubN(i: Int): Long = profilers(i).totalBpd.timeSpent

  def neighborhoodUsage(neighborhoodId: Int): Double =
    ((profilers(neighborhoodId).totalBpd.nbFound.toDouble/profilers.map(_.totalBpd.nbFound).sum)*10000).round/100.0

  def neighborhoodSuccess(neighborhoodId: Int): Double =
    ((profilers(neighborhoodId).totalBpd.nbFound.toDouble/profilers(neighborhoodId).totalBpd.nbCalls)*10000).round/100.0

  def collectExtraProfileHeader: Array[String] = Array.empty
  def collectExtraProfileData: Array[String] = Array.empty
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

  override def collectExtraProfileHeader: Array[String] = Array("NbReset", "NbFirstFailed")
  override def collectExtraProfileData: Array[String] = Array(s"$nbReset",s"$nbFirstFailed")

  override def merge(profiler: Profiler): Unit = {
    val bestFirstProfiler = profiler.asInstanceOf[BestFirstProfiler]
    super.merge(profiler)
    nbReset += bestFirstProfiler.nbReset
    nbFirstFailed += bestFirstProfiler.nbFirstFailed
  }
}

case class CompositionProfiler(override val combinator: NeighborhoodCombinator, left: Neighborhood, right: () => Neighborhood) extends CombinatorProfiler(combinator){

  override def subProfilers: List[Profiler] = List(left.profiler) ++ dynNeighborhoodProfiler
  override final def collectThisProfileHeader: Array[String] =
    super.collectThisProfileHeader ++
      Array.fill(2)(Array("|", "side", "calls", "founds", "explored", "total time", "perf. (ms/ex)")).flatten
  override final def collectThisProfileData: Array[String] = {
    super.collectThisProfileData ++
      Array(
        Array("|", s"left", s"${left.profiler.totalBpd.nbCalls}", s"${left.profiler.totalBpd.nbFound}",
          s"${left.profiler.totalBpd.nbExplored}", s"${left.profiler.totalBpd.timeSpent}", s"${performance(left.profiler)}") ++
          dynNeighborhoodProfiler.toArray.flatMap(dynP => Array("|", s"right", s"${dynP.totalBpd.nbCalls}", s"${dynP.totalBpd.nbFound}",
            s"${dynP.totalBpd.nbExplored}", s"${dynP.totalBpd.timeSpent}", s"${performance(dynP)}"))
      ).flatten
  }

  var dynNeighborhoodProfiler: List[Profiler] = List.empty

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
    val matchingProfilerOpt = dynNeighborhoodProfiler.find(_.neighborhood.toString == profiler.neighborhood.toString)
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

  override def collectThisProfileHeader: Array[String] =
    super.collectThisProfileHeader ++ extraStatisticsHeader.toArray

  override def collectThisProfileData: Array[String] =
    super.collectThisProfileData ++ extraStatistics.map(_.toString)

  override def merge(profiler: Profiler): Unit = {
    super.merge(profiler)
    val basicProfiler = profiler.asInstanceOf[BasicProfiler]
    for(i <- extraStatistics.indices)extraStatistics(i)+=basicProfiler.extraStatistics(i)
  }

}