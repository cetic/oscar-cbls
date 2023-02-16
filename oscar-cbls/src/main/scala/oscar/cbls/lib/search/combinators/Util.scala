package oscar.cbls.lib.search.combinators

import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search._
import oscar.cbls.core.search.profiling.TransparentCombinatorProfiler
import oscar.cbls.util.Properties
import oscar.cbls.visual.SingleFrameWindow
import oscar.cbls.visual.obj.ObjectiveFunctionDisplay

import java.awt.event.ActionEvent
import java.awt.{BorderLayout, FlowLayout}
import javax.swing.JLabel
import scala.concurrent.duration.{Duration, DurationInt}

/**
 * This combinator create a frame that draw the evolution curve of the objective function.
 * You can also display other information on the curve, but the main curve will always be the obj function.
 *
 * @param a a neighborhood
 * @param obj the objective function
 * @param title The title of the frame
 * @param minCap The minimum displayed value
 * @param maxCap The maximum displayed value
 * @param percentile The percentile (1 to 100) of the best displayed value
 * @param otherValues An array of other value you want to be displayed (as a tuple (String, () => Long))
 * @author fabian.germeau@cetic.be
 */
class ShowObjectiveFunction(a: Neighborhood,
                            obj: () => Long,
                            title: String = "Objective function vs. time[s]",
                            minCap: Long = 0L,
                            maxCap:Long = Long.MaxValue,
                            percentile:Int = 100,
                            otherValues: Array[(String, () => Long)] = Array.empty,
                            logScale:Boolean = true) extends NeighborhoodCombinator(a){
  //objGraphic is an internal frame that contains the curve itself and visualFrame is a basic frame that contains objGraphic
  private val objGraphic = ObjectiveFunctionDisplay(title, minCap, maxCap, percentile, otherValues.map(_._1).toList,logScale)
  private val otherValuesFunctions = otherValues.map(_._2)
  val window = SingleFrameWindow.show(objGraphic,title)

  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriteria: AcceptanceCriterion): SearchResult = {
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
    objGraphic.drawFunction(obj(), otherValuesFunctions)
  }
}


/**
 * the purpose of this combinator is to change the name of the neighborhood it is given as parameter.
 * it will add a prefix to all moves sent back by this combinator
 * the only purposes are documentation and debug
 *
 * @param a the base neighborhood
 * @param name the name
 */
class Name(a: Neighborhood, val name: String) extends NeighborhoodCombinator(a) {
  override val profiler: TransparentCombinatorProfiler = new TransparentCombinatorProfiler(this)
  /**
   * @param acceptanceCriterion oldObj,newObj => should the move to the newObj be kept (default is oldObj > newObj)
   *                            beware that a changing criteria might interact unexpectedly with stateful neighborhood combinators
   * @return an improving move
   */
  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriteria: AcceptanceCriterion): SearchResult = {
    if (printExploredNeighborhoods) println(name + ": start exploration")
    a.getMove(obj, initialObj, acceptanceCriteria) match {
      case NoMoveFound =>
        if (printExploredNeighborhoods) println(name + ": NoMoveFound")
        NoMoveFound
      case MoveFound(m) =>
        if (printExploredNeighborhoods) println(name + ": MoveFound:" + m)
        NamedMove(m, name)
    }
  }

  override def toString: String = name
}


/**
 * the purpose of this combinator is to change the name of the neighborhood it is given as parameter.
 * it will add a prefix to all moves sent back by this combinator
 * the only purposes are documentation and debug
 *
 * @param a The base neighborhood
 * @param name The combinator's name
 */
class ChainableName[MoveType <: Move](a: Neighborhood with SupportForAndThenChaining[MoveType], val name: String)
  extends NeighborhoodCombinator(a) with SupportForAndThenChaining[MoveType]{
  override val profiler: TransparentCombinatorProfiler = new TransparentCombinatorProfiler(this)
  /**
   * @param acceptanceCriterion oldObj,newObj => should the move to the newObj be kept (default is oldObj > newObj)
   *                            beware that a changing criteria might interact unexpectedly with stateful neighborhood combinators
   * @return an improving move
   */
  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriteria: AcceptanceCriterion): SearchResult = {
    if(printExploredNeighborhoods) println(name + ": start exploration")
    a.getMove(obj, initialObj, acceptanceCriteria) match {
      case NoMoveFound =>
        if(printExploredNeighborhoods) println(name + ": NoMoveFound")
        NoMoveFound
      case MoveFound(m) =>
        if(printExploredNeighborhoods) println(name + ": MoveFound:" + m)
        NamedMove(m, name)
    }
  }

  override def toString: String = name

  override def instantiateCurrentMove(newObj : Long) : MoveType = a.instantiateCurrentMove(newObj)
}

/**
 * this combinator overrides the acceptance criterion given to the whole neighborhood
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
class WithAcceptanceCriterion(a: Neighborhood,
                              overridingAcceptanceCriterion: AcceptanceCriterion) extends NeighborhoodCombinator(a) {
  /**
   * @param acceptanceCriterion this criterion is not considered by this combinator.
   * @return an improving move
   */
  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriterion: AcceptanceCriterion): SearchResult
  = a.getMove(obj, initialObj, OverrideCriterion(overridingAcceptanceCriterion))
}

class StrictlyImproveOverBestKnown(a: Neighborhood, bestKnown : () => Long) extends NeighborhoodCombinator(a) {

  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriteria: AcceptanceCriterion): SearchResult = {
    val bestKnownObj = bestKnown()
    a.getMove(obj, initialObj, StrictlyBetterThan(bestKnownObj))
  }
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
   * @param acceptanceCriterion a function to decide the acceptation of a move
   * @return
   */
  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriterion: AcceptanceCriterion): SearchResult =
    getMove(overridingObjective, overridingObjective.value, acceptanceCriterion)
}

case class NoReset(a: Neighborhood) extends NeighborhoodCombinator(a) {
  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriteria: AcceptanceCriterion): SearchResult =
    a.getMove(obj, initialObj, acceptanceCriteria)

  //this resets the internal state of the move combinators
  override def reset(): Unit = {}
}

/**
 * @author renaud.delandtsheer@cetic.be
 */
class ResetOnExhausted(a: Neighborhood) extends NeighborhoodCombinator(a) {
  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriteria: AcceptanceCriterion): SearchResult = {
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
 * @param timeOut the maximal duration, in milliseconds
 */
class WeakTimeout(a:Neighborhood, timeOut:Duration = 1.minutes) extends NeighborhoodCombinator(a) {
  private var deadline: Long = -1

  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriteria: AcceptanceCriterion): SearchResult = {
    if (deadline == -1) {
      deadline = System.currentTimeMillis() + timeOut.toMillis
    }

    if (System.currentTimeMillis() >= deadline) {
      println(s"Timeout of $timeOut")
      NoMoveFound
    } else {
      a.getMove(obj, initialObj: Long, acceptanceCriteria)
    }
  }

  override def reset(): Unit = {
    deadline = -1
    a.reset()
  }
}


/**
 * @warning this is experimental.
 * sets a hard timeout for a search procedure; interrupts ongoing neighborhood exploration if necessary,
 * and restores the state before the last exploration.
 * Do not use it on the right of cartesian products.
 * @param a a neighborhood
 * @param timeOut the maximal duration
 *
 */
class HardTimeout(a:Neighborhood, timeOut:Duration = 1.minutes) extends NeighborhoodCombinator(a) {
  private var deadline: Long = -1

  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriteria: AcceptanceCriterion): SearchResult = {
    if (deadline == -1) {
      deadline = System.currentTimeMillis() + timeOut.toMillis
    }

    if (System.currentTimeMillis() >= deadline) {
      println(s"Timeout of $timeOut")
      NoMoveFound
    } else {
      val shouldAbort = () => System.currentTimeMillis() >= deadline
      a.getMoveAbortable(obj, initialObj, acceptanceCriteria, shouldAbort)
    }
  }

  override def reset(): Unit = {
    deadline = -1
    a.reset()
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

  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriterion: AcceptanceCriterion): SearchResult = {

    if (stopped) return NoMoveFound

    val currentTime = System.currentTimeMillis()

    if (nextCutTime == -1) {
      //the combinator has just been reset, so we just reinitialize it.
      nextCutTime = currentTime + (timePeriodInMilliSecond max minTimeBeforeFirstCutInMilliSecond)
      bestSoFar = initialObj
      bestSoFarAtPreviousCut = initialObj
      //println("initialize cut")
    } else if (nextCutTime < currentTime){
      //need to check for a cut
      val relativeImprovementSincePreviousCut = (bestSoFarAtPreviousCut - bestSoFar).toDouble / bestSoFar.toDouble

      if(relativeImprovementSincePreviousCut < minRelativeImprovementByCut){
        //we have to stop it
        println(s"tail cut; relativeImprovement:$relativeImprovementSincePreviousCut periodDurationMilliSecond:$timePeriodInMilliSecond")
        stopped = true
        return NoMoveFound
      } else {
        //we can carry on
        nextCutTime = currentTime + timePeriodInMilliSecond
        bestSoFar = bestSoFar min bestSoFarAtPreviousCut
        bestSoFarAtPreviousCut = bestSoFar
      }
    }

    a.getMove(obj, initialObj, acceptanceCriterion) match {
      case NoMoveFound => NoMoveFound
      case f:MoveFound =>
        bestSoFar = bestSoFar min f.objAfter
        f
    }
  }
}

/**
 * This combinator will prevent a neighborhood from taking way more time than usual
 * it first calibrates to know the time that the neighborhood needs to find a move, "maxTimeToFind"
 * then it will set a hard timeout on the neighborhood.
 * This hard timeout will abort the exploration after "maxTimeToFind * cutMultiplier".
 * In case of abort, this combinator will return "noMoveFound"
 *
 * @param a the base neighborhood
 * @param calibrationRuns the number of moves that the neighborhood will find in order to calibrate the watchdog
 * @param cutMultiplier the max multiplier on the time
 * @param reevaluate after a set of neighborhood exploration, calibration wil be performed again
 */
case class WatchDog(a:Neighborhood, calibrationRuns:Int = 5, cutMultiplier:Double = 2, reevaluate:Int = 1000)
  extends NeighborhoodCombinator(a) {

  var maxTimeFoundMS:Int = 0
  var nbCalibrationRunsFound:Int = 0
  var nbSearch:Int = 0

  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriterion: AcceptanceCriterion): SearchResult = {
    nbSearch += 1
    if (nbSearch > reevaluate) {
      maxTimeFoundMS = 0
      nbCalibrationRunsFound = 0
      nbSearch = 0
    }

    if (nbCalibrationRunsFound > calibrationRuns) {
      //watchdog is active
      val cutDuration = (maxTimeFoundMS * cutMultiplier).toInt
      val startTimeMs = System.currentTimeMillis()
      new HardTimeout(a,cutDuration.millisecond).getMove(obj,initialObj,acceptanceCriterion) match{
        case m:MoveFound =>
          nbCalibrationRunsFound += 1
          val duration = (System.currentTimeMillis() - startTimeMs).toInt
          maxTimeFoundMS = maxTimeFoundMS max duration
          m
        case x => x
        //not found
      }
    } else {
      //still calibrating
      val startTimeMs = System.currentTimeMillis()
      a.getMove(obj,initialObj,acceptanceCriterion) match{
        case m:MoveFound =>
          nbCalibrationRunsFound += 1
          val duration = (System.currentTimeMillis() - startTimeMs).toInt
          maxTimeFoundMS = maxTimeFoundMS max duration
          m
        case x => x
        //not found
      }
    }
  }
}


/**
 * Displays a graphical window to interrupt or kill a search
 * @param n the base neighborhood
 * @param hardStop true for a hard stop, false for a soft one
 *                 hard  interrupts ongoing neighborhoods,
 *                 soft waits for current neighborhood to finish)
 * @param message a message for the title of the window
 */
class GraphicalInterrupt(n:Neighborhood, message:String = "Stop search", hardStop:Boolean) extends NeighborhoodCombinator(n){

  var stopped = false

  import javax.swing.{JFrame, JPanel, JButton}

  class StopWindow() extends JFrame {
    setTitle(message)
    setSize(600, 200)

    val panel = new JPanel(new BorderLayout())
    // Add button to JPanel

    val label = new JLabel("<HTML>\"kill\" will exit the application immediately <BR> \"stop\" will mark the search as complete</HTML>")
    import java.awt.Font
    label.setFont(new Font("Serif", Font.PLAIN, 32))
    panel.add(label,"North")

    val panel2 = new JPanel(new FlowLayout(FlowLayout.CENTER))
    panel2.setSize(600, 50)
    panel.add(panel2,"South")

    // Create JButton and JPanel
    val stopButton = new JButton("stop")
    stopButton.addActionListener((_: ActionEvent) => {
      stopped = true
      this.setVisible(false)
    })
    stopButton.setFont(new Font("Serif", Font.PLAIN, 32))
    panel2.add(stopButton,"West")

    val killButton = new JButton("kill")
    killButton.addActionListener((_: ActionEvent) => {
      System.exit(0)
    })
    killButton.setFont(new Font("Serif", Font.PLAIN, 32))
    panel2.add(killButton,"East")

    //TODO: add pause button

    // And JPanel needs to be added to the JFrame itself!
    this.getContentPane.add(panel)
    setLocationRelativeTo(null)
    setResizable(false)
    setVisible(true)
  }

  val myStop = new StopWindow()
  //SingleFrameWindow.showFrame(myStop,title = message)

  override def getMove(obj: Objective,
                       initialObj: Long,
                       acceptanceCriterion: AcceptanceCriterion): SearchResult = {
    if(stopped) NoMoveFound
    else n.getMove(obj, initialObj, acceptanceCriterion) match{
      case NoMoveFound =>
        myStop.setVisible(false)
        NoMoveFound
      case x => x
    }
  }
}
