package oscar.cbls.core.search

import oscar.cbls.util.Properties

object Profiler {
  def statisticsHeader: Array[String] = Array("Neighborhood","calls", "found", "sumGain", "sumTime(ms)", "avgGain",
    "avgTime(ms)", "slope(-/s)", "avgTimeNoMove", "avgTimeMove", "wastedTime")

  def selectedStatisticInfo(i:Iterable[Profiler]):String = {
    Properties.justifyRightArray(Profiler.statisticsHeader :: i.toList.map(_.collectThisProfileStatistics)).mkString("\n")
  }
}

class Profiler(neighborhoodName: String) {

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
  def waistedTime:String = if(nbCalls - nbFound == 0L) "NA" else s"${totalTimeSpentNoMoveFound / (nbCalls - nbFound)}"

  def collectThisProfileStatistics:Array[String] =
    {
      Array[String](s"${neighborhoodName}", s"$nbCalls", s"$nbFound", s"$totalGain",
        s"$totalTimeSpent", s"$gainPerCall", s"$callDuration", s"$slope",
        s"$avgTimeSpendNoMove", s"$avgTimeSpendMove", s"$totalTimeSpentNoMoveFound", s"$totalExplored")
    }
  //TODO: (${if(nbCalls == 0) "NA" else (100*nbFound.toFloat/nbCalls.toFloat).toInt}%)

  //  override def toString: String = "Statistics(" + a + " nbCalls:" + nbCalls + " nbFound:" + nbFound + " totalGain:" + totalGain + " totalTimeSpent " + totalTimeSpent + " ms timeSpendWithMove:" + totalTimeSpentMoveFound + " ms totalTimeSpentNoMoveFound " + totalTimeSpentNoMoveFound + " ms)"
  override def toString: String = s"Profile($neighborhoodName)"

  def slopeOrZero:Long = if(totalTimeSpent == 0L) 0L else ((100L * totalGain) / totalTimeSpent).toInt

  def resetThisStatistics(): Unit ={
    println(s"$neighborhoodName reset")
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


case class BestNeighborhoodFirstProfiler(combinatorName: String, nbNeighborhoods: Int) extends Profiler(combinatorName) {

  val profilers: Array[Profiler] = Array.tabulate(nbNeighborhoods)(i => new Profiler(s"$combinatorName - neighborhood $i"))

  def slopeForCombinators(neighborhoodId: Int, defaultIfNoCall:Long = Long.MaxValue):Long =
    if(totalTimeSpent(neighborhoodId) == 0L) defaultIfNoCall
    else ((1000L * totalGain(neighborhoodId)) / totalTimeSpent(neighborhoodId)).toInt

  def nbCalls(i: Int): Long = profilers(i).nbCalls
  def nbFound(i: Int): Long = profilers(i).nbFound
  def nbExplored(i: Int): Long = profilers(i).nbExplored
  def totalExplored(i: Int): Long = profilers(i).totalExplored
  def totalGain(i: Int): Long = profilers(i).totalGain
  def totalTimeSpentMoveFound(i: Int): Long = profilers(i).totalTimeSpentMoveFound
  def totalTimeSpentNoMoveFound(i: Int): Long = profilers(i).totalTimeSpentNoMoveFound
  def totalTimeSpent(i: Int): Long = profilers(i)totalTimeSpent

}
