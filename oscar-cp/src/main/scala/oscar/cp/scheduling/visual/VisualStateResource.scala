package oscar.cp.scheduling.visual

import oscar.visual._
import oscar.cp.core.variables.CPIntVar
import java.awt.Color
import oscar.visual.shapes.VisualRoundRectangle

/**
 * Created on 08/12/14.
 * @author Cyrille Dejemeppe (cyrille.dejemeppe@gmail.com)
 *
 * Gantt chart representing the state a which a state resource is at each time.
 */
class VisualStateResource(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], stateNeeded: Array[Int], needed: Array[Boolean], colors: (Int) => Color = i => Color.WHITE) extends VisualDrawing(false, false) {
  var rectangles = List[VisualRoundRectangle]()

  private val max = stateNeeded.max

  def update(xScale: Int, yScale: Int): Unit = {
    rectangles = List[VisualRoundRectangle]()
    clear()

    val makespan = ends.map(_.max).max

    val sortedActivitiesNeedingStateIndex = (0 until starts.length).filter(i => needed(i)).sortWith((i1, i2) => starts(i1).min < starts(i2).min)

    var startTimeState = starts(sortedActivitiesNeedingStateIndex(0)).min
    var curState = stateNeeded(sortedActivitiesNeedingStateIndex(0))
    for (i <- 1 until sortedActivitiesNeedingStateIndex.length) {
      if (stateNeeded(sortedActivitiesNeedingStateIndex(i)) != curState) {
        val rect = VisualRoundRectangle(this, 0, 0, (starts(sortedActivitiesNeedingStateIndex(i)).min - startTimeState) * xScale, yScale, 3, 3)
        rect.innerCol = colors(curState)
        rect.move(startTimeState * xScale, curState * yScale)
        rectangles ::= rect
        curState = stateNeeded(sortedActivitiesNeedingStateIndex(i))
        startTimeState = starts(sortedActivitiesNeedingStateIndex(i)).min
      }
    }

    val rect = VisualRoundRectangle(this, 0, 0, (ends(sortedActivitiesNeedingStateIndex(sortedActivitiesNeedingStateIndex.length - 1)).min - starts(sortedActivitiesNeedingStateIndex(sortedActivitiesNeedingStateIndex.length - 1)).min) * xScale, yScale, 3, 3)
    rect.innerCol = colors(stateNeeded(sortedActivitiesNeedingStateIndex(sortedActivitiesNeedingStateIndex.length - 1)))
    rect.move(starts(sortedActivitiesNeedingStateIndex(sortedActivitiesNeedingStateIndex.length - 1)).min * xScale, stateNeeded(sortedActivitiesNeedingStateIndex(sortedActivitiesNeedingStateIndex.length - 1)) * yScale)
    rectangles ::= rect

    repaint()
  }
}
