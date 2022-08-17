package oscar.cbls.core.search

import oscar.cbls.util.Properties

object Profiler {
  def selectedStatisticInfo(profilers:Iterable[Profiler]):String = {
    Properties.justifyRightArray(profilers.toList.flatMap(p => List(p.collectThisProfileHeader,p.collectThisProfileData))).mkString("\n")
  }
}

abstract class Profiler(val neighborhood:Neighborhood){
  def collectThisProfileStatistics:List[Array[String]] = List(collectThisProfileHeader, collectThisProfileData)
  def collectThisProfileHeader:Array[String]
  def collectThisProfileData:Array[String]

  def resetThisStatistics(): Unit

  override def toString: String = s"Profile(${neighborhood.toString})"
  def profiledNeighborhood: String = neighborhood.getClass.getSimpleName
}

class EmptyProfiler(neighborhood: Neighborhood) extends Profiler(neighborhood) {
  override def collectThisProfileHeader: Array[String] = Array.empty
  override def collectThisProfileData: Array[String] = Array.empty
  // Nothing to do
  override def resetThisStatistics(): Unit = {}
}

/**
 * Base profiler for EasyNeighborhoods
 * @param neighborhood
 */
class NeighborhoodProfiler(neighborhood: Neighborhood) extends Profiler(neighborhood) {

  var nbCalls:Long = 0L
  var nbFound:Long = 0L
  var nbExplored: Long = 0L
  var totalExplored: Long = 0L
  var totalGain:Long = 0L
  var totalTimeSpentMoveFound:Long = 0L
  var totalTimeSpentNoMoveFound:Long = 0L

  var startExplorationAtMillis: Long = 0L
  var lastExploredNeighborAtMillis: Long = 0L

  def totalTimeSpent:Long = totalTimeSpentMoveFound + totalTimeSpentNoMoveFound


  def gainPerCall:String = if(nbCalls ==0L) "NA" else s"${totalGain / nbCalls}"
  def callDuration:String = if(nbCalls == 0L ) "NA" else s"${totalTimeSpent / nbCalls}"
  //gain in obj/s
  def slope:String = if(totalTimeSpent == 0L) "NA" else s"${1000 * (totalGain.toDouble / totalTimeSpent.toDouble).toLong}"

  def avgTimeSpendNoMove:String = if(nbCalls - nbFound == 0L) "NA" else s"${totalTimeSpentNoMoveFound / (nbCalls - nbFound)}"
  def avgTimeSpendMove:String = if(nbFound == 0L) "NA" else s"${totalTimeSpentMoveFound / nbFound}"
  def avgTimeExplore: String = s"${(totalTimeSpent.toDouble/totalExplored*1000).round/1000.0}"
  def waistedTime:String = if(nbCalls - nbFound == 0L) "NA" else s"${totalTimeSpentNoMoveFound / (nbCalls - nbFound)}"

  override def collectThisProfileHeader: Array[String] = Array("Neighborhood","calls", "found", "explored", "sumGain", "sumTime(ms)", "avgGain",
    "avgTime(ms)", "slope(-/s)", "avgTimeNoMove", "avgTimeMove", "wastedTime", "avgTimeExplored(ms)")

  override def collectThisProfileData:Array[String] =
    {
      Array[String](s"${neighborhood}",
        s"$nbCalls", s"$nbFound", s"$totalExplored",
        s"$totalGain", s"$totalTimeSpent", s"$gainPerCall", s"$callDuration", s"$slope",
        s"$avgTimeSpendNoMove", s"$avgTimeSpendMove", s"$totalTimeSpentNoMoveFound",
        s"$avgTimeExplore")
    }

  def slopeOrZero:Long = if(totalTimeSpent == 0L) 0L else ((100L * totalGain) / totalTimeSpent).toInt

  override def resetThisStatistics(): Unit ={
    nbCalls = 0
    nbFound = 0
    totalGain = 0
    totalTimeSpentMoveFound = 0
    totalTimeSpentNoMoveFound = 0
  }

  /**
   * Use this method to set up some profiling variables.
   * Like when did we start the neighborhood exploration, reset some variable...
   */
  def explorationStarted(): Unit ={
    startExplorationAtMillis = System.currentTimeMillis()
    nbCalls += 1
    nbExplored = 0
  }

  /**
   * Use this method to update some profiling variables.
   * Is this explored neighbor valid ? Did it violate a strong constraint ?
   * Did it made the obj function worse ? How much time ....
   */
  def neighborExplored(): Unit ={
    nbExplored += 1
  }

  /**
   * Use this method to update some profiling variables.
   * Move found or not, total time searching ...
   */
  def explorationEnded(gain: Option[Long]): Unit ={
    if(gain.nonEmpty) {
      totalTimeSpentMoveFound += System.currentTimeMillis()-startExplorationAtMillis
      nbFound += 1
      totalGain += gain.get
    }
    else totalTimeSpentNoMoveFound += System.currentTimeMillis()-startExplorationAtMillis

    totalExplored += nbExplored
  }


}


class CombinatorProfiler(combinator: NeighborhoodCombinator) extends Profiler(combinator) {

  protected var nbCalls: Long = 0L
  protected var nbFounds: Long = 0L
  protected var totalTime: Long = 0L
  protected var explorationStartAt: Long = 0L

  override def collectThisProfileHeader: Array[String] = Array("Combinator", "Total calls", "% Founds", "Total time (ms)")

  override def collectThisProfileData: Array[String] = Array(combinator.toString, nbCalls.toString, (Math.floor(nbFounds.toDouble/nbCalls*100000)/1000).toString, totalTime.toString)

  override def resetThisStatistics(): Unit = {
    nbCalls = 0L
    nbFounds = 0L
    totalTime = 0L
  }

  def explorationStarted(): Unit = {
    nbCalls += 1
    explorationStartAt = System.currentTimeMillis()
  }
  def explorationEnded(found: Boolean): Unit = {
    if(found) nbFounds += 1
    totalTime += System.currentTimeMillis()-explorationStartAt
  }
}

class SelectionProfiler(combinator: NeighborhoodCombinator, nbNeighborhoods: List[Neighborhood]) extends CombinatorProfiler(combinator) {

  val nbMoveFoundPerNeighbor: Array[Int] = Array.fill(nbNeighborhoods.size)(0)

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

  val profilers: Array[NeighborhoodProfiler] = nbNeighborhoods.map(n => new NeighborhoodProfiler(n)).toArray

  def totalTimeSpentSubN(i: Int): Long = profilers(i)totalTimeSpent

  def subExplorationStarted(neighborhoodId: Int): Unit = profilers(neighborhoodId).explorationStarted()
  def subExplorationEnded(neighborhoodId: Int, gain: Option[Long]): Unit = {
    profilers(neighborhoodId).explorationEnded(gain)
    if(gain.nonEmpty)nbMoveFoundPerNeighbor(neighborhoodId)+=1
  }

  def neighborhoodUsage(neighborhoodId: Int): Double =
    ((nbMoveFoundPerNeighbor(neighborhoodId).toDouble/nbMoveFoundPerNeighbor.sum)*10000).round/100.0

  def neighborhoodSuccess(neighborhoodId: Int): Double =
    ((profilers(neighborhoodId).nbFound.toDouble/profilers(neighborhoodId).nbCalls)*10000).round/100.0

  def collectExtraProfileHeader: Array[String] = Array.empty
  def collectExtraProfileData: Array[String] = Array.empty
}

case class RoundRobinProfiler(combinator: NeighborhoodCombinator,
                              nbNeighborhoods: List[Neighborhood]) extends SelectionProfiler(combinator,nbNeighborhoods){

}

case class BestFirstProfiler(combinator: NeighborhoodCombinator,
                             nbNeighborhoods: List[Neighborhood]) extends SelectionProfiler(combinator,nbNeighborhoods){

  var nbReset: Int = 0
  var nbFirstFailed: Int = 0
  var neighborhoodsProfilersSummedStatistics: Array[Array[Long]] = nbNeighborhoods.map(_ => Array(0L,0L,0L)).toArray

  def slopeForCombinators(neighborhoodId: Int, defaultIfNoCall:Long = Long.MaxValue):Long =
    if(totalTimeSpentSubN(neighborhoodId) == 0L) defaultIfNoCall
    else ((1000L * totalGainSubN(neighborhoodId)) / totalTimeSpentSubN(neighborhoodId)).toInt

  def nbFoundSubN(i: Int): Long = profilers(i).nbFound
  def totalGainSubN(i: Int): Long = profilers(i).totalGain
  def totalTimeSpentMoveFoundSubN(i: Int): Long = profilers(i).totalTimeSpentMoveFound

  def firstFailed(): Unit = nbFirstFailed += 1
  def resetSelectionNeighborhoodStatistics(): Unit ={
    nbReset += 1
    saveSelectionNeighborhoodStatistics()
    profilers.foreach(p => p.resetThisStatistics())
  }
  private def saveSelectionNeighborhoodStatistics(): Unit = {
    profilers.indices.foreach(i => {
      neighborhoodsProfilersSummedStatistics(i)(0) += profilers(i).nbCalls
      neighborhoodsProfilersSummedStatistics(i)(1) += profilers(i).nbFound
      neighborhoodsProfilersSummedStatistics(i)(2) += profilers(i).totalGain
    })
  }

  override def collectThisProfileStatistics: List[Array[String]] = {
    saveSelectionNeighborhoodStatistics() // Save last stat data
    super.collectThisProfileStatistics
  }
  override def collectExtraProfileHeader: Array[String] = Array("NbReset", "NbFirstFailed")
  override def collectExtraProfileData: Array[String] = Array(s"$nbReset",s"$nbFirstFailed")
  override def neighborhoodUsage(neighborhoodId: Int): Double =
    ((neighborhoodsProfilersSummedStatistics(neighborhoodId)(1).toDouble/nbMoveFoundPerNeighbor.sum)*10000).round/100.0
  override def neighborhoodSuccess(neighborhoodId: Int): Double =
    ((neighborhoodsProfilersSummedStatistics(neighborhoodId)(1).toDouble/neighborhoodsProfilersSummedStatistics(neighborhoodId)(0))*10000).round/100.0
}
