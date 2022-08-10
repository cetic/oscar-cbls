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
  override def collectThisProfileHeader: Array[String] = Array("Neighborhood", "Undefined")
  override def collectThisProfileData: Array[String] = Array(neighborhood.toString,"N/A")
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


case class BestNeighborhoodFirstProfiler(combinator: Neighborhood, nbNeighborhoods: List[Neighborhood]) extends Profiler(combinator) {

  val profilers: Array[NeighborhoodProfiler] = nbNeighborhoods.map(n => new NeighborhoodProfiler(n)).toArray

  def slopeForCombinators(neighborhoodId: Int, defaultIfNoCall:Long = Long.MaxValue):Long =
    if(totalTimeSpent(neighborhoodId) == 0L) defaultIfNoCall
    else ((1000L * totalGain(neighborhoodId)) / totalTimeSpent(neighborhoodId)).toInt

  //def nbCalls(i: Int): Long = profilers(i).nbCalls
  def nbFound(i: Int): Long = profilers(i).nbFound
  //def nbExplored(i: Int): Long = profilers(i).nbExplored
  //def totalExplored(i: Int): Long = profilers(i).totalExplored
  def totalGain(i: Int): Long = profilers(i).totalGain
  def totalTimeSpentMoveFound(i: Int): Long = profilers(i).totalTimeSpentMoveFound
  //def totalTimeSpentNoMoveFound(i: Int): Long = profilers(i).totalTimeSpentNoMoveFound
  def totalTimeSpent(i: Int): Long = profilers(i)totalTimeSpent

  var nbReset: Int = 0
  var nbFirstFailed: Int = 0
  val nbMoveFoundPerNeighbor: Array[Int] = Array.fill(nbNeighborhoods.size)(0)

  override def collectThisProfileHeader: Array[String] = Array("Neighborhood", "NbReset", "NbFirstFailed") ++ profilers.map(p => s"${p.neighborhood.toString} - Usage (%)")
  override def collectThisProfileData: Array[String] =
    Array(s"${neighborhood.toString}", s"$nbReset",s"$nbFirstFailed") ++ nbMoveFoundPerNeighbor.map(m => s"${((m.toDouble/nbMoveFoundPerNeighbor.sum)*100).round}")

  override def resetThisStatistics(): Unit = {
    nbReset += 1
    profilers.foreach(p => p.resetThisStatistics())
  }

  def firstFailed(): Unit = nbFirstFailed += 1
  def explorationStarted(neighborhoodId: Int): Unit = profilers(neighborhoodId).explorationStarted()
  def explorationEnded(neighborhoodId: Int, gain: Option[Long]): Unit = {
    profilers(neighborhoodId).explorationEnded(gain)
    if(gain.nonEmpty)nbMoveFoundPerNeighbor(neighborhoodId)+=1
  }
}
