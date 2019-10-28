package oscar.cbls.lib.search.combinators


import oscar.cbls._
import oscar.cbls.core.search._
import oscar.cbls.visual.SingleFrameWindow
import oscar.cbls.visual.obj.ObjectiveFunctionDisplay

trait UtilityCombinators{
  /**
    * collects statistics about the run time and progress achieved by neighborhood a
    * they can be obtained by querying this object with method toString
    * or globally on the whole neighborhood using the method statistics
    * WARNING: do not use this inside an AndThen,
    *          since the objective function is instrumented by this combinator, so the statistics will be counter-intuitive
    *
    * @param a
    * @param ignoreInitialObj
    */
  def profile(a:Neighborhood,ignoreInitialObj:Boolean = false) = Profile(a,ignoreInitialObj)
}


/**
 * This combinator create a frame that draw the evolution curve of the objective function.
 * The drawn curve possess a scrollbar on the right that allow the user to decrease or
 * increase the number of value displayed.
 *
 * @param a a neighborhood
 * @param obj the objective function
 * @author fabian.germeau@student.vinci.be
 */
class ShowObjectiveFunction(a: Neighborhood, obj: () => Long, title: String = "Objective function vs. time[s]") extends NeighborhoodCombinator(a){
  //objGraphic is an internal frame that contains the curve itself and visualFrame is a basic frame that contains objGraphic
  val objGraphic = ObjectiveFunctionDisplay(title)
  SingleFrameWindow.show(objGraphic,title)

  override def getMove(obj: Objective, initialObj:Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult ={
    a.getMove(obj, initialObj, acceptanceCriteria) match {
      case m: MoveFound =>
        InstrumentedMove(m.m, null, () => notifyNewObjValue(m.m))
      case x => x
    }
  }

  /*
    After each move we send the new value and time to objGraphic who will register the value
    and then we write the curve
   */
  def notifyNewObjValue(m:Move): Unit ={
    objGraphic.drawFunction(obj())
  }
}


/**
 * the purpose of this combinator is to change the name of the neighborhood it is given as parameter.
 * it will add a prefix to all moves sent back by this combinator
 * the only purposes are documentation and debug
 *
 * @param a
 * @param name
 */
class Name(a: Neighborhood, val name: String) extends NeighborhoodCombinator(a) {
  /**
   * @param acceptanceCriterion oldObj,newObj => should the move to the newObj be kept (default is oldObj > newObj)
   *                            beware that a changing criteria might interact unexpectedly with stateful neighborhood combinators
   * @return an improving move
   */
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult = {
    a.getMove(obj, initialObj:Long, acceptanceCriterion) match {
      case NoMoveFound => NoMoveFound
      case MoveFound(m) => NamedMove(m, name)
    }
  }

  override def toString: String = name
}


/**
 * the purpose of this combinator is to change the name of the neighborhood it is given as parameter.
 * it will add a prefix to all moves sent back by this combinator
 * the only purposes are documentation and debug
 *
 * @param a
 * @param name
 */
class ChainableName[MoveType <: Move](a: Neighborhood with SupportForAndThenChaining[MoveType], val name: String)
  extends NeighborhoodCombinator(a) with SupportForAndThenChaining[MoveType]{
  /**
   * @param acceptanceCriterion oldObj,newObj => should the move to the newObj be kept (default is oldObj > newObj)
   *                            beware that a changing criteria might interact unexpectedly with stateful neighborhood combinators
   * @return an improving move
   */
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult = {
    a.getMove(obj, initialObj:Long, acceptanceCriterion) match {
      case NoMoveFound => NoMoveFound
      case MoveFound(m) => NamedMove(m, name)
    }
  }

  override def toString: String = name

  override def instantiateCurrentMove(newObj : Long) : MoveType = a.instantiateCurrentMove(newObj)
}

/**
 * tis combinator overrides the acceptance criterion given to the whole neighborhood
 * this can be necessary if you have a neighborhood with some phases only including simulated annealing
 * notice that the actual acceptance criteria is the one that you give,
 * with a slight modification: it will reject moves that lead to MaxInt, except if we are already at MaxInt.
 * Since MaxInt is used to represent that a strong constraint is violated, we cannot tolerate such moves at all.
 *
 * @param a the neighborhood
 * @param overridingAcceptanceCriterion the acceptance criterion that is used instead of the one given to the overall search
 *                                      with the addition that moves leading to MaxInt will be rejected anyway, except if we are already at MaxInt
 *
 */
class WithAcceptanceCriterion(a: Neighborhood, overridingAcceptanceCriterion: (Long, Long) => Boolean) extends NeighborhoodCombinator(a) {
  /**
   * @param acceptanceCriterion this criterion is not considered by this combinator.
   * @return an improving move
   */
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult
  = a.getMove(obj,initialObj:Long,
    (a, b) => (a == Long.MaxValue || b != Long.MaxValue) && overridingAcceptanceCriterion(a, b))
}


/**
 * Forces the use of a given objective function.
 * this overrides the one that you might pass in the higher level
 *
 * @param a the combined neighborhood
 * @param overridingObjective the objective to use instead of the given one
 */
class OverrideObjective(a: Neighborhood, overridingObjective: Objective) extends NeighborhoodCombinator(a) {
  /**
   * the method that returns a move from the neighborhood.
   * The returned move should typically be accepted by the acceptance criterion over the objective function.
   * Some neighborhoods are actually jumps, so that they might violate this basic rule however.
   *
   * @param obj the objective function. notice that it is actually a function. if you have an [[oscar.cbls.core.objective.Objective]] there is an implicit conversion available
   * @param acceptanceCriterion
   * @return
   */
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult =
    getMove(overridingObjective, overridingObjective.value,acceptanceCriterion)
}


/**
 * collects statistics about the run time and progress achieved by neighborhood a
 * they can be obtained by querying this object with method toString
 * or globally on the whole neighborhood using the method statistics
 * WARNING: do not use this inside an AndThen,
 *          since the objective function is instrumented by this combinator, so the statistics will be counter-intuitive
 *
 * @param a
 * @param ignoreInitialObj
 */
case class Profile(a:Neighborhood,ignoreInitialObj:Boolean = false) extends NeighborhoodCombinator(a){

  var nbCalls = 0L
  var nbFound = 0L
  var totalGain:Double = 0L
  var totalTimeSpentMoveFound:Long = 0L
  var totalTimeSpentNoMoveFound:Long = 0L

  def totalTimeSpent = totalTimeSpentMoveFound + totalTimeSpentNoMoveFound

  override def resetStatistics(){
    resetThisStatistics()
    super.resetStatistics()
  }

  def resetThisStatistics() {
    nbCalls = 0L
    nbFound = 0L
    totalGain = 0L
    totalTimeSpentMoveFound = 0L
    totalTimeSpentNoMoveFound = 0L
  }

  /**
   * the method that returns a move from the neighborhood.
   * The returned move should typically be accepted by the acceptance criterion over the objective function.
   * Some neighborhoods are actually jumps, so that they might violate this basic rule however.
   *
   * @param obj the objective function. notice that it is actually a function. if you have an [[oscar.cbls.core.objective.Objective]] there is an implicit conversion available
   * @param acceptanceCriterion
   * @return
   */
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult = {

    nbCalls += 1L
    val startTime = System.nanoTime()

    a.getMove(obj, initialObj:Long, acceptanceCriterion) match {
      case NoMoveFound =>
        totalTimeSpentNoMoveFound += (System.nanoTime() - startTime) / 1000000L
        NoMoveFound
      case m: MoveFound =>
        totalTimeSpentMoveFound += (System.nanoTime() - startTime) / 1000000L
        nbFound += 1L
        if (!ignoreInitialObj || nbCalls > 1L) totalGain += initialObj - m.objAfter
        m
    }
  }

  def gainPerCall:String = if(nbCalls ==0L) "NA" else "" + (totalGain / nbCalls).toInt
  def callDuration:String = if(nbCalls == 0L ) "NA" else "" + (totalTimeSpent / nbCalls).toInt
  //gain in obj/s
  def slope:String = if(totalTimeSpent == 0L) "NA" else "" + (1000L * totalGain.toDouble / totalTimeSpent.toDouble).toInt

  def avgTimeSpendNoMove:String = if(nbCalls - nbFound == 0L) "NA" else "" + (totalTimeSpentNoMoveFound / (nbCalls - nbFound))
  def avgTimeSpendMove:String = if(nbFound == 0L) "NA" else "" + (totalTimeSpentMoveFound / nbFound)
  def waistedTime:String = if(nbCalls - nbFound == 0L) "NA" else "" + (totalTimeSpentNoMoveFound / (nbCalls - nbFound))

  override def collectProfilingStatistics: List[String] =
    collectThisProfileStatistics :: super.collectProfilingStatistics

  def collectThisProfileStatistics:String =
    padToLength("" + a,31L) + " " +
      padToLength("" + nbCalls,6L) + " " +
      padToLength("" + nbFound,6L) + " " +
      padToLength("" + totalGain.toInt,8L) + " " +
      padToLength("" + totalTimeSpent,12L) + " " +
      padToLength("" + gainPerCall,8L) + " " +
      padToLength("" + callDuration,12L)+ " " +
      padToLength("" + slope,11L)+ " " +
      padToLength("" + avgTimeSpendNoMove,13L)+ " " +
      padToLength("" + avgTimeSpendMove,12L)+ " " +
      totalTimeSpentNoMoveFound

  private def padToLength(s: String, l: Long) = {
    val extended = s + nStrings(l+1L, " ")
    val nextchar = extended.substring(l+1L, l+1L)
    if(nextchar equals " "){
      extended.substring(0L, l-1L) + "§"
    }else{
      extended.substring(0L, l)
    }
  }
  private def nStrings(n: Long, s: String): String = if (n <= 0L) "" else s + nStrings(n - 1L, s)

  //  override def toString: String = "Statistics(" + a + " nbCalls:" + nbCalls + " nbFound:" + nbFound + " totalGain:" + totalGain + " totalTimeSpent " + totalTimeSpent + " ms timeSpendWithMove:" + totalTimeSpentMoveFound + " ms totalTimeSpentNoMoveFound " + totalTimeSpentNoMoveFound + " ms)"
  override def toString: String = "Profile(" + a + ")"

  def slopeOrZero:Long = if(totalTimeSpent == 0L) 0L else ((100L * totalGain) / totalTimeSpent).toInt

  def slopeForCombinators(defaultIfNoCall:Long = Long.MaxValue):Long =  if(totalTimeSpent == 0L) defaultIfNoCall else ((1000L * totalGain) / totalTimeSpent).toInt
}

object Profile{
  private def padToLength(s: String, l: Long) = (s + nStrings(l, " ")).substring(0L, l)
  private def nStrings(n: Long, s: String): String = if (n <= 0L) "" else s + nStrings(n - 1L, s)
  def statisticsHeader: String = padToLength("Neighborhood",30L) + "  calls  found  sumGain  sumTime(ms)  avgGain  avgTime(ms)  slope(-/s)  avgTimeNoMove avgTimeMove  wastedTime"
  def selectedStatisticInfo(i:Iterable[Profile]) = {
    (statisticsHeader :: i.toList.map(_.collectThisProfileStatistics)).mkString("\n")
  }
}


case class NoReset(a: Neighborhood) extends NeighborhoodCombinator(a) {
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriteria: (Long, Long) => Boolean) =
    a.getMove(obj, initialObj:Long, acceptanceCriteria)

  //this resets the internal state of the move combinators
  override def reset() {}
}


/**
 * @author renaud.delandtsheer@cetic.be
 */
class ResetOnExhausted(a: Neighborhood) extends NeighborhoodCombinator(a) {
  override def getMove(obj: Objective, initialObj:Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {
    a.getMove(obj, initialObj:Long, acceptanceCriteria) match {
      case NoMoveFound =>
        a.reset()
        a.getMove(obj, initialObj:Long, acceptanceCriteria)
      case m: MoveFound => m
    }
  }
}


/**
  * sets a timeout for a search procedure.
  * notice that hte timeout itself is a bit lax, because the combinator has no possibility to interrupt a neighborhood during its exploration.
  * this combinator will therefore just prevent any new exploration past the end of the timeout.
  * @param a a neighborhood
  * @param maxDuration the maximal duration, in milliseconds
  */
class Timeout(a:Neighborhood, maxDuration:Long) extends NeighborhoodCombinator(a) {
  private var deadline: Long = -1

  override def getMove(obj: Objective, initialObj: Long, acceptanceCriteria: (Long, Long) => Boolean): SearchResult = {
    if (deadline == -1) {
      deadline = System.currentTimeMillis() + maxDuration
    }

    if (System.currentTimeMillis() >= deadline) {
      if(printExploredNeighborhoods) println("Timeout reached")
      NoMoveFound
    } else {
      a.getMove(obj, initialObj: Long, acceptanceCriteria)
    }
  }
}

/**
  * This combinator will interrupt the search when it becomes too flat.
  * use it to cut the tail of long, undesired searches
  * it works by time period.
  * at the end of every time period, as set by timePeriodInMilliSecond,
  * it will compute the relative improvement of obj of this latest time period over hte best so far
  * if the relative improvement is smaller than minRelativeImprovementByCut, it is considered too flat, and search is stopped
  *
  * NOTICE that if your base neighborhood has a search time that is bigger then the time period,
  * it will not be interrupted during its exploration.
  * this combinator only decides if a new neighborhood exploration is to be started
  *
  * @param a the base neighborhood
  * @param timePeriodInMilliSecond defines teh time period for the cut
  * @param minRelativeImprovementByCut the relative improvement over obj
  */
class CutTail(a:Neighborhood, timePeriodInMilliSecond:Long,minRelativeImprovementByCut:Double,minTimeBeforeFirstCutInMilliSecond:Long)
  extends NeighborhoodCombinator(a){

  var bestSoFarAtPreviousCut:Long = -1
  var bestSoFar:Long = Long.MaxValue
  var nextCutTime:Long = -1

  var stopped:Boolean = false

  override def reset(): Unit = {
    bestSoFarAtPreviousCut = -1
    bestSoFar = Long.MaxValue
    nextCutTime = -1
    stopped = false

    super.reset()
  }

  override def getMove(obj: Objective, initialObj: Long, acceptanceCriterion: (Long, Long) => Boolean): SearchResult = {

    if(stopped) return NoMoveFound

    val currentTime = System.currentTimeMillis()

    if(nextCutTime == -1){
      //the combinator has just been reset, so we just reinitialize it.
      nextCutTime = currentTime + (timePeriodInMilliSecond max minTimeBeforeFirstCutInMilliSecond)
      bestSoFar = initialObj
      bestSoFarAtPreviousCut = initialObj
      //println("initialize cut")
    }else if(nextCutTime < currentTime){
      //need to check for a cut
      val relativeImprovementSincePreviousCut = (bestSoFarAtPreviousCut - bestSoFar).toDouble / bestSoFar.toDouble

      if(relativeImprovementSincePreviousCut < minRelativeImprovementByCut){
        //we have to stop it
        println("check for cut, cut")
        stopped = true
        return NoMoveFound
      }else{
        println("check for cut, no cut")
        //we can carry on
        nextCutTime = currentTime + timePeriodInMilliSecond
        bestSoFar = bestSoFar min bestSoFarAtPreviousCut
        bestSoFarAtPreviousCut = bestSoFar
      }
    }

    a.getMove(obj,initialObj,acceptanceCriterion) match{
      case NoMoveFound => NoMoveFound
      case f:MoveFound =>
//        println("update best in cut")
        bestSoFar = bestSoFar min f.objAfter
        f
    }
  }
}



