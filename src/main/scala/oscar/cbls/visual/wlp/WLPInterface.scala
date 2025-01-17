package oscar.cbls.visual.wlp

import oscar.cbls.visual.logging.LoggerDisplay
import oscar.cbls.visual.objective.ObjectiveFunctionDisplay
import scalafx.application.{JFXApp3, Platform}
import scalafx.beans.property.IntegerProperty
import scalafx.collections.ObservableBuffer
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control.{ListView, ScrollPane, Tooltip}
import scalafx.scene.layout.{HBox, Pane}
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Circle, Line, Rectangle, Shape}

import scala.collection.SortedSet
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.math.{pow, sqrt}

/**
 * This class sets up a ScalaFX interface for a Warehouse Locatop Problem (WLP)
 * @param nbWarehouses number of warehouses
 * @param nbDelivery number of delivery points
 * @param costsForOpeningWarehouses costs for opening warehouses
 * @param warehousesPositions points for warehouses
 * @param deliveryPositions points for delivery stops
 * @param distancesDeliveryWarehouses distance matrix between delivery points and warehouses
 * @param solverProcedure a procedure that computes a WLP solution
 */
class WLPInterface(nbWarehouses: Int,
                   nbDelivery: Int,
                   costsForOpeningWarehouses: Array[Long],
                   warehousesPositions: Array[(Long, Long)],
                   deliveryPositions: Array[(Long, Long)],
                   distancesDeliveryWarehouses: Array[Array[Long]],
                   solverProcedure: => Unit) extends JFXApp3 {
  // Result of a Move
  sealed trait MoveResult
  case class SortedSetResult(ss: SortedSet[Int], objVal: Long) extends MoveResult
  // What is obtained when searching for a move
  sealed trait MoveOperation
  case class Move(res: MoveResult) extends MoveOperation
  case object NoMove extends MoveOperation
  // Warehouse Location Map

  /**
   * Warehouse Location Map
   * @param deliveryCoordinates points for delivery stops
   * @param warehouseCoordinates points for warehouses
   * @param distanceCostD2W distance matrix between delivery points and warehouses
   * @param warehouseOpeningCosts costs for opening warehouses
   */
  private class WarehouseLocationMap(deliveryCoordinates: Array[(Long, Long)],
                                     warehouseCoordinates: Array[(Long, Long)],
                                     distanceCostD2W: Array[Array[Long]],
                                     warehouseOpeningCosts: Array[Long]) {
    val maxX: Long = deliveryCoordinates.map(_._1).max max warehouseCoordinates.map(_._1).max
    val maxY: Long = deliveryCoordinates.map(_._2).max max warehouseCoordinates.map(_._2).max
    var openWarehouses: SortedSet[Int] = SortedSet.empty
    /**
     * @param d the point from where distance is computed
     * @return -1L is no open warehouse near to d
     */
    def nearestOpenWarehouse(d: Int): Int = {
      var closestW = -1
      var minDistance = Long.MaxValue
      for (w <- openWarehouses) {
        val distance = distanceCostD2W(d)(w)
        if (distance < minDistance) {
          closestW = w
          minDistance = distance
        }
      }
      closestW
    }
  }
  // Decision variables
  private val wlpMap = new WarehouseLocationMap(
    deliveryPositions,
    warehousesPositions,
    distancesDeliveryWarehouses,
    costsForOpeningWarehouses
  )
  private val remaining = IntegerProperty(5)
  private val pointData = ObservableBuffer[String]()
  private var objDisplay: ObjectiveFunctionDisplay = _
  private var logDisplay: LoggerDisplay = _

  /**
   * Display of the WLP window
   * @param openWS the open warehouses
   * @return the ScalaFX scene which displays the warehouses and delivery points
   */
  private def sceneWLP(openWS: SortedSet[Int]): Scene = new Scene {
    // Tracé de la bordure
    val border: Rectangle = Rectangle((wlpMap.maxX + 10).toDouble, (wlpMap.maxY + 10).toDouble, Color.White)
    border.setStroke(Color.Black)
    // Tracé des lignes
    val lignes: Array[Line] = deliveryPositions
      .indices
      .foldLeft(Array[Line]()) { (acc, i) =>
        val wi = wlpMap.nearestOpenWarehouse(i)
        if (wi > 0) {
          val line = new Line {
            startX = warehousesPositions(wi)._1.toDouble
            startY = warehousesPositions(wi)._2.toDouble
            endX = deliveryPositions(i)._1.toDouble
            endY = deliveryPositions(i)._2.toDouble
            strokeDashArray = Seq(1D, 2.5D)
          }
          line +: acc
        } else {
          acc
        }
      }
    // Tracé des rectangles (Warehouses)
    val rectangles: Array[Rectangle] = Array.tabulate(nbWarehouses) { i =>
      val whp = warehousesPositions(i)
      val rxy = Rectangle(whp._1.toDouble - 4D, whp._2.toDouble - 4D, 8D, 8D)
      rxy.setStroke(Color.Black)
      if (openWS.contains(i)) {
        rxy.fill = Color.GreenYellow
        val tooltip = new Tooltip(s"Open warehouse at (${whp._1}, ${whp._2})")
        Tooltip.install(rxy, tooltip)
      } else {
        val tooltip = new Tooltip(s"Closed warehouse at (${whp._1}, ${whp._2})")
        Tooltip.install(rxy, tooltip)
        rxy.fill = Color.Pink
      }
      rxy.onMouseClicked = { _ =>
        pointData.clear()
        pointData.add("Warehouse")
        pointData.add(s"X = ${whp._1}")
        pointData.add(s"Y = ${whp._2}")
        pointData.add(s"Status : ${if (openWS.contains(i)) "Open" else "Closed"}")
      }
      rxy
    }
    val circles: Array[Circle] = Array.tabulate(nbDelivery) { i =>
      val dlv = deliveryPositions(i)
      val (whp, distance) = if (wlpMap.nearestOpenWarehouse(i) == -1) {
        (
          (-1, -1),
          Double.PositiveInfinity
        )

      } else {
        val wp = warehousesPositions(wlpMap.nearestOpenWarehouse(i))
        (
          wp,
          sqrt(pow((dlv._1-wp._1).toDouble, 2d) + pow((dlv._2-wp._2).toDouble, 2d))
        )
      }
      val cxy = new Circle {
        centerX = dlv._1.toDouble
        centerY = dlv._2.toDouble
        radius = 2
        fill = Color.Black
      }
      cxy.onMouseClicked = { _ =>
        pointData.clear()
        pointData.add("Delivery point")
        pointData.add(s"X = ${dlv._1}")
        pointData.add(s"Y = ${dlv._2}")
        pointData.add(s"Served by warehouse at (${whp._1}, ${whp._2})")
        pointData.add(f"Distance = $distance%.5f")
      }
      val tooltip = new Tooltip(s"Delivery point at(${dlv._1}, ${dlv._2})")
      Tooltip.install(cxy, tooltip)
      cxy
    }
    val shapes: Array[Shape] = Array(border) ++ lignes ++ rectangles ++ circles
    val mainPane: ScrollPane = new ScrollPane {
      maxWidth = 1024
      maxHeight = 768
      content = {
        new Pane {
          children = shapes
        }
      }
    }
    val infoList: ListView[String] = new ListView[String] {
      maxWidth = 256
      maxHeight = 768
      items = pointData
    }
    root = {
      new HBox {
        padding = Insets(10)
        spacing = 5
        alignment = Pos.Center
        children = List(mainPane, infoList)
      }
    }
  }

  /**
   * Updates observable values after a move
   * @param move the move results (found or not)
   * @param newTime execution time
   * @param newRemainingRetrys remaining retrys
   */
  def update(move: MoveOperation, newTime: Double, newRemainingRetrys: Int): Unit = {
    remaining.update(newRemainingRetrys)
    move match {
      case Move(res) =>
        res match {
          case SortedSetResult(openWs, objVal) =>
            wlpMap.openWarehouses = openWs
            println(s"$newTime s: Found move : Open warehouses = ${openWs.size}; Objective Value = $objVal")
            objDisplay.updateObj(newTime, objVal)
            Platform.runLater {
              logDisplay.addLog(s"$newTime s", "Found Move", s"Open warehouses = ${openWs.size}; Objective Value = $objVal")
              stage.scene = sceneWLP(openWs)
            }
          case _ => throw new IllegalArgumentException("Content of move is not good")
        }
      case NoMove =>
        if (remaining.value > 0) {
          println("Restarting...")
          Platform.runLater {
            logDisplay.addLog(s"$newTime s", "Restarting...", "")
          }
        } else {
          println("Solver ended.")
          Platform.runLater {
            logDisplay.addLog(s"$newTime s", "Solver Ended.", "")
          }
        }
    }
  }

  /**
   * Main thread of the ScalaFX routing interface
   */
  override def start(): Unit = {
    objDisplay = new ObjectiveFunctionDisplay(titleWindow = "WLP Drawing - Objective Function")
    logDisplay = new LoggerDisplay(titleWindow = "WLP Drawing - Logs")
    stage = new JFXApp3.PrimaryStage {
      title = "WLP Drawing"
      scene = new Scene { }
    }
    val objFuncStage = objDisplay.stage
    val logsStage = logDisplay.stage
    objFuncStage.show()
    logsStage.show()
    Future(solverProcedure)
  }
}
