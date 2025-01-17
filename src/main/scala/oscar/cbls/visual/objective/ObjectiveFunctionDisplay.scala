package oscar.cbls.visual.objective

import scalafx.application.Platform
import scalafx.beans.property.{DoubleProperty, LongProperty}
import scalafx.Includes._
import scalafx.scene.{Node, Scene}
import scalafx.scene.chart.{LineChart, NumberAxis, XYChart}
import scalafx.scene.control.Tooltip
import scalafx.stage.Stage

/**
 * The class implements a ScalaFX chart window to display the evolution of the
 * objective value over time
 * @param titleWindow the window title
 */
class ObjectiveFunctionDisplay(titleWindow: String = "Objective Function") {
  private val time = DoubleProperty(0)
  private val objP = LongProperty(0)
  private val objFSeries: XYChart.Series[Number, Number] = new XYChart.Series[Number, Number] {
    name = "Objective Function"
    data = Seq(XYChart.Data[Number, Number](0, 0))
  }

  // Change interface objects when the objective function observable changes
  objP.onChange {
    Platform.runLater {
      // Update Objective Function window
      val dataPoint = XYChart.Data[Number, Number](time.value, objP.value)
      objFSeries.data() += dataPoint
      val pointNode: Node = dataPoint.node()
      val tooltip = new Tooltip(f"time:${time.value}%.3f, obj:${objP.value}")
      Tooltip.install(pointNode, tooltip)
    }
  }

  /**
   * updates the observable values of time and objective value
   * @param newTime time
   * @param newObjVal objective value
   */
  def updateObj(newTime: Double, newObjVal: Long): Unit = {
    time.update(newTime)
    objP.update(newObjVal)
  }

  /**
   * The stage is the window object, associated to a ScalaFX interface
   */
  val stage: Stage = new Stage {
    title = titleWindow
    scene = new Scene {
      root = {
        new LineChart[Number, Number](NumberAxis("Time (s)"), NumberAxis("Objective value")) {
          title = "Objective value over time"
          data() += objFSeries
        }
      }
    }
  }
}
