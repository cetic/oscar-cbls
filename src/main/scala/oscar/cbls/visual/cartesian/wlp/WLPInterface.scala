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

package oscar.cbls.visual.cartesian.wlp

import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.core.computation.{Solution, Variable}
import oscar.cbls.visual.additionalStages.ObjectiveFunctionDisplay
import oscar.cbls.visual.cartesian.{CartesianLayer, CartesianNode}
import oscar.cbls.visual.cartesian.wlp.layers.{WLPLinkCartesianLayer, WLPNodeCartesianLayer}
import oscar.cbls.visual.{OscaRDisplay, OscaRPrimaryStage}
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control.ListView
import scalafx.scene.layout.{HBox, Pane}
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Rectangle, Shape}

object WLPInterface {

  /** This class sets up a ScalaFX interface for a Warehouse Location Problem (WLP).
    *
    * @param nbWarehouses
    *   The number of warehouses.
    * @param nbDeliveries
    *   The number of delivery points.
    * @param nodesCoordinates
    *   The coordinates of each warehouse and delivery point.
    * @param openFacilities
    *   The SetVariable maintaining the open warehouses.
    * @param obj
    *   The IntVariable maintaining the value of the objective function.
    * @param width
    *   The desired width of the window (default = 1200).
    * @param height
    *   The desired height of the window (default = 700).
    * @return
    *   The [[oscar.cbls.visual.OscaRDisplay]] enclosing this Stage and all other Stages.
    */
  def apply(
    nbWarehouses: Int,
    nbDeliveries: Int,
    nodesCoordinates: Array[(Long, Long)],
    openFacilities: SetVariable,
    obj: IntVariable,
    width: Int = 1200,
    height: Int = 700
  ): OscaRDisplay = {
    require(
      !nodesCoordinates.exists(x => x._1 < 0 || x._2 < 0),
      "This routing display tool accepts only positive coordinates."
    )
    val cartesianNodes = CartesianNode(nodesCoordinates)
    val additionalStages =
      List(() => ObjectiveFunctionDisplay(obj, System.nanoTime()))
    OscaRDisplay.apply(
      obj.model,
      () =>
        new WLPInterface(nbWarehouses, nbDeliveries, cartesianNodes, openFacilities, width, height),
      additionalStages = additionalStages
    )
  }
}

/** This class sets up a ScalaFX interface for a Warehouse Location Problem (WLP).
  *
  * @param nbWarehouses
  *   The number of warehouses.
  * @param nbDeliveries
  *   The number of delivery points.
  * @param nodesCoordinates
  *   The coordinates of each warehouse and delivery point.
  * @param openFacilities
  *   The SetVariable maintaining the open warehouses.
  * @param myWidth
  *   The desired width of the window.
  * @param myHeight
  *   The desired height of the window.
  */
class WLPInterface(
  nbWarehouses: Int,
  nbDeliveries: Int,
  nodesCoordinates: Array[CartesianNode],
  openFacilities: SetVariable,
  myWidth: Int,
  myHeight: Int
) extends OscaRPrimaryStage {
  private lazy val pointData = ObservableBuffer[String]()
  private lazy val closestSortedWarehouses: Array[List[Int]] = {
    def distanceBetween(a: (Long, Long), b: (Long, Long)): Double = {
      Math.sqrt(
        Math.pow(a._1.toDouble - b._1.toDouble, 2) + Math.pow(a._2.toDouble - b._2.toDouble, 2)
      )
    }
    val allDeliveriesCoordinate = nodesCoordinates.drop(nbWarehouses)
    val allWarehousesWithIndex  = nodesCoordinates.take(nbWarehouses).zipWithIndex

    val allWarehousesSortedForEachDelivery =
      allDeliveriesCoordinate.map(d =>
        allWarehousesWithIndex
          .sortBy(w => distanceBetween(d.realCoordinates, w._1.realCoordinates))
      )

    allWarehousesSortedForEachDelivery.map(_.map(_._2).toList)
  }
  private val WLPDisplayShapes: List[CartesianLayer] =
    List(
      WLPLinkCartesianLayer(
        nbWarehouses,
        nbDeliveries,
        nodesCoordinates,
        closestSortedWarehouses,
        openFacilities
      ),
      WLPNodeCartesianLayer(
        nbWarehouses,
        nbDeliveries,
        nodesCoordinates,
        closestSortedWarehouses,
        openFacilities,
        pointData
      )
    )

  // Simple box over the WLP display to separate it from infoList, which displays data of selected component.
  private val border: Rectangle = Rectangle(myWidth.toDouble, myHeight.toDouble, Color.White)
  border.setStroke(Color.Black)

  // A ListView displaying information. In this case, those are related to selected component.
  private val infoList: ListView[String] = new ListView[String] {
    maxWidth = 256
    maxHeight = myHeight
    items = pointData
  }

  /** Returns a new Scene containing the Shape that need to be displayed. */
  private def sceneWLP(): Scene = new Scene {
    val shapes: Array[Shape] = Array(border) ++ WLPDisplayShapes.flatMap(_.listOfShapes)

    def refresh(): Unit = {
      root = {
        new HBox {
          padding = Insets(10)
          spacing = 5
          alignment = Pos.Center
          children = List(new Pane { children = shapes }, infoList)
        }
      }
    }
    refresh()
  }

  override def variablesForSolutionExtraction(): List[Variable] = List(openFacilities)

  override def init(): Unit = {
    nodesCoordinates.foreach(_.resize(myWidth, myHeight))
    WLPDisplayShapes.foreach(_.initLayer())
    title = "WLP Drawing"
    scene = sceneWLP()
  }

  override protected[visual] def redraw(solution: Solution): Unit = {
    Platform.runLater {
      WLPDisplayShapes.foreach(_.redraw(solution))
      scene = sceneWLP()
    }
  }
}
