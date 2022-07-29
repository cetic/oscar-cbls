package oscar.cbls.core.search

class Profiler(neighborhoodName: String) {

  private var nbCalls:Long = 0L
  private var nbFound:Long = 0L
  private var nbExplored: Long = 0L
  private var totalExplored: Long = 0L
  private var totalGain:Long = 0L
  private var totalTimeSpentMoveFound:Long = 0L
  private var totalTimeSpentNoMoveFound:Long = 0L

  private var startExplorationAtMillis: Long = 0L
  private var lastExploredNeighborAtMillis: Long = 0L

  private def totalTimeSpent:Long = totalTimeSpentMoveFound + totalTimeSpentNoMoveFound


  private def gainPerCall:String = if(nbCalls ==0L) "NA" else s"${totalGain / nbCalls}"
  private def callDuration:String = if(nbCalls == 0L ) "NA" else s"${totalTimeSpent / nbCalls}"
  //gain in obj/s
  private def slope:String = if(totalTimeSpent == 0L) "NA" else s"${1000 * (totalGain.toDouble / totalTimeSpent.toDouble).toLong}"

  private def avgTimeSpendNoMove:String = if(nbCalls - nbFound == 0L) "NA" else s"${totalTimeSpentNoMoveFound / (nbCalls - nbFound)}"
  private def avgTimeSpendMove:String = if(nbFound == 0L) "NA" else s"${totalTimeSpentMoveFound / nbFound}"
  private def waistedTime:String = if(nbCalls - nbFound == 0L) "NA" else s"${totalTimeSpentNoMoveFound / (nbCalls - nbFound)}"

  def collectThisProfileStatistics:Array[String] =
    {
      println(s"Collecting - $neighborhoodName")
      Array[String](s"${neighborhoodName}", s"$nbCalls", s"$nbFound", s"$totalGain",
        s"$totalTimeSpent", s"$gainPerCall", s"$callDuration", s"$slope",
        s"$avgTimeSpendNoMove", s"$avgTimeSpendMove", s"$totalTimeSpentNoMoveFound", s"$totalExplored")
    }
  //TODO: (${if(nbCalls == 0) "NA" else (100*nbFound.toFloat/nbCalls.toFloat).toInt}%)

  //  override def toString: String = "Statistics(" + a + " nbCalls:" + nbCalls + " nbFound:" + nbFound + " totalGain:" + totalGain + " totalTimeSpent " + totalTimeSpent + " ms timeSpendWithMove:" + totalTimeSpentMoveFound + " ms totalTimeSpentNoMoveFound " + totalTimeSpentNoMoveFound + " ms)"
  override def toString: String = s"Profile($neighborhoodName)"

  private def slopeOrZero:Long = if(totalTimeSpent == 0L) 0L else ((100L * totalGain) / totalTimeSpent).toInt

  private def slopeForCombinators(defaultIfNoCall:Long = Long.MaxValue):Long =  if(totalTimeSpent == 0L) defaultIfNoCall else ((1000L * totalGain) / totalTimeSpent).toInt

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
  def explorationEnded(moveFound: Boolean, gain: Long): Unit ={
    if(moveFound) {
      totalTimeSpentMoveFound += System.currentTimeMillis()-startExplorationAtMillis
      nbFound += 1
      totalGain += gain
    }
    else totalTimeSpentNoMoveFound += System.currentTimeMillis()-startExplorationAtMillis

    totalExplored += nbExplored
  }


}
