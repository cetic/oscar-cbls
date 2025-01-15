package oscar.cbls.visual.logging

import scalafx.beans.property.StringProperty
import scalafx.collections.ObservableBuffer
import scalafx.scene.Scene
import scalafx.scene.control.{TableColumn, TableView}
import scalafx.stage.Stage

/**
 * The class implements a ScalaFX logging window
 * @param titleWindow the window title
 */
class LoggerDisplay(titleWindow: String = "Logs") {
  /**
   * Internal class for logs registers
   * @param time_ execution time
   * @param msg_ log message
   * @param obj_ objective value
   */
  private class TableLog(time_ : String, msg_ : String, obj_ : String) {
    val time = new StringProperty(this, "Time", time_)
    val msg = new StringProperty(this, "Message", msg_)
    val obj = new StringProperty(this, "Objective Value", obj_)
  }

  private val logs = ObservableBuffer[TableLog]()

  /**
   * Add a log to the observable buffer
   * @param time execution time
   * @param msg log message
   * @param obj objective value
   */
  def addLog(time : String, msg : String, obj : String): Unit = {
    logs.add(new TableLog(time, msg, obj))
  }

  /**
   * The stage is the window object, associated to a ScalaFX interface
   */
  val stage: Stage = new Stage {
    title = titleWindow
    scene = new Scene {
      root = new TableView[TableLog](logs){
        columns ++= Seq(
          new TableColumn[TableLog, String] {
            text = "Time"
            cellValueFactory = _.value.time
            sortable = false
          },
          new TableColumn[TableLog, String] {
            text = "Message"
            cellValueFactory = _.value.msg
            sortable = false
          },
          new TableColumn[TableLog, String] {
            text = "Objective Value"
            cellValueFactory = _.value.obj
            sortable = false
          }
        )
        columnResizePolicy = TableView.ConstrainedResizePolicy
      }
    }
  }
}
