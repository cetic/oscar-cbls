// OscaR is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 2.1 of the License, or
// (at your option) any later version.
//
// OscaR is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License  for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with OscaR.
// If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html

package oscar.cbls.visual.additionalStages

import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.{Solution, Variable}
import oscar.cbls.visual.OscaRStage
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.scene.chart.{LineChart, NumberAxis, XYChart}
import scalafx.scene.control.Tooltip
import scalafx.scene.{Node, Scene}

object ObjectiveFunctionDisplay {

  /** Implements a ScalaFX chart window to display the evolution of the objective value over time
    *
    * @param objectiveFunctionVariable
    *   The IntVariable containing the objective function value.
    * @param startTime
    *   The time (in nanosecond) at which the search started.
    * @param titleWindow
    *   the window title
    */
  def apply(
    objectiveFunctionVariable: IntVariable,
    startTime: Long,
    titleWindow: String = "Objective Function"
  ): ObjectiveFunctionDisplay = {
    new ObjectiveFunctionDisplay(objectiveFunctionVariable, startTime, titleWindow)
  }
}

/** Implements a ScalaFX chart window to display the evolution of the objective value over time
  *
  * @param objectiveFunctionVariable
  *   The IntVariable containing the objective function value.
  * @param startTime
  *   The time (in nanosecond) at which the search started.
  * @param titleWindow
  *   the window title
  */
class ObjectiveFunctionDisplay(
  objectiveFunctionVariable: IntVariable,
  startTime: Long,
  titleWindow: String
) extends OscaRStage {
  private lazy val objFSeries: XYChart.Series[Number, Number] = new XYChart.Series[Number, Number] {
    name = titleWindow
    data = Seq(XYChart.Data[Number, Number](0, 0))
  }

  /** Updates the observable values of time and objective value. */
  private def updateObj(time: Double, obj: Long): Unit = {
    val dataPoint = XYChart.Data[Number, Number](time, obj)
    objFSeries.data() += dataPoint
    val pointNode: Node = dataPoint.node()
    val tooltip         = new Tooltip(f"time:${time % .3f}, obj:$obj")
    Tooltip.install(pointNode, tooltip)
  }

  override def variablesForSolutionExtraction(): List[Variable] = List(objectiveFunctionVariable)

  override def init(): Unit = {
    // Setting the title and the scene of this Stage
    title = titleWindow
    scene = new Scene {
      root = {
        new LineChart[Number, Number](NumberAxis("Time (s)"), NumberAxis("Objective value")) {
          title = "Objective value over time"
          data() += objFSeries
        }
      }
    }
    updateObj(0.0, objectiveFunctionVariable.pendingValue)
  }

  override def redraw(solution: Solution): Unit = {
    val time = (System.nanoTime() - startTime) * 1e-9
    Platform.runLater {
      updateObj(time, solution.valueOfVariable(objectiveFunctionVariable).get)
    }
  }
}
