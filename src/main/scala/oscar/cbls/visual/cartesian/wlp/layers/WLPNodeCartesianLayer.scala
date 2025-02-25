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

package oscar.cbls.visual.cartesian.wlp.layers

import oscar.cbls.core.computation.Solution
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.visual.cartesian.{CartesianLayer, CartesianNode}
import scalafx.collections.ObservableBuffer
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Circle, Rectangle, Shape}

object WLPNodeCartesianLayer {

  /** Returns a layer displaying the delivery points and the warehouses of the WLP.
    *
    * It provides some information to the
    * [[https://javadoc.io/doc/org.scalafx/scalafx_2.12/22.0.0-R33/scalafx/scene/control/ListView.html scalafx.scene.control.ListView]]
    * in [[oscar.cbls.visual.cartesian.wlp.WLPInterface]].
    *
    * @param nbWarehouses
    *   The number of warehouses of this WLP.
    * @param nbDeliveries
    *   The number of deliveries of this WLP.
    * @param nodesCoordinates
    *   The coordinates of all nodes of this WLP (warehouses followed by deliveries).
    * @param closestOpenedWarehouse
    *   An array containing, for each delivery, a function mapping the open warehouse to the closest
    *   one.
    * @param openWarehouses
    *   The SetVariable maintaining the open warehouses set.
    * @param pointData
    *   The buffer containing the data that will be displayed in the
    *   [[https://javadoc.io/doc/org.scalafx/scalafx_2.12/22.0.0-R33/scalafx/scene/control/ListView.html scalafx.scene.control.ListView]]
    */
  def apply(
    nbWarehouses: Int,
    nbDeliveries: Int,
    nodesCoordinates: Array[CartesianNode],
    closestOpenedWarehouse: Array[List[Int] => Int],
    openWarehouses: SetVariable,
    pointData: ObservableBuffer[String]
  ): WLPNodeCartesianLayer = {
    new WLPNodeCartesianLayer(
      nbWarehouses,
      nbDeliveries,
      nodesCoordinates,
      closestOpenedWarehouse,
      openWarehouses,
      pointData
    )
  }
}

/** This layer displays the delivery points and the warehouses of the WLP.
  *
  * It provides some information to the
  * [[https://javadoc.io/doc/org.scalafx/scalafx_2.12/22.0.0-R33/scalafx/scene/control/ListView.html scalafx.scene.control.ListView]]
  * in [[oscar.cbls.visual.cartesian.wlp.WLPInterface]]
  *
  * @param nbWarehouses
  *   The number of warehouses of this WLP.
  * @param nbDeliveries
  *   The number of deliveries of this WLP.
  * @param nodesCoordinates
  *   The coordinates of all nodes of this WLP (warehouses followed by deliveries).
  * @param closestOpenedWarehouse
  *   An array containing, for each delivery, a function mapping the open warehouse to the closest
  *   one.
  * @param openWarehouses
  *   The SetVariable maintaining the open warehouses set.
  * @param pointData
  *   The buffer containing the data that will be displayed in the
  *   [[scalafx.scene.control.ListView]].
  */
class WLPNodeCartesianLayer(
  nbWarehouses: Int,
  nbDeliveries: Int,
  nodesCoordinates: Array[CartesianNode],
  closestOpenedWarehouse: Array[List[Int] => Int],
  openWarehouses: SetVariable,
  pointData: ObservableBuffer[String]
) extends CartesianLayer {
  private var _openWarehouses: Set[Int] = Set.empty

  private lazy val warehouses: Array[Rectangle] = Array.tabulate(nbWarehouses) { w =>
    val wCoordinates = nodesCoordinates(w)
    val resized      = wCoordinates.resizedCoordinates
    val square       = Rectangle(resized._1 - 4d, resized._2 - 4d, 8d, 8d)
    square.setStroke(Color.Black)
    square.fill = Color.Pink
    square.onMouseClicked = { _ =>
      pointData.clear()
      getWarehouseDataOnClick(w).foreach(pointData.add)
    }

    square
  }
  private lazy val deliveries: Array[Circle] = Array.tabulate(nbDeliveries) { d =>
    val dCoordinates = nodesCoordinates(nbWarehouses + d)
    val resized      = dCoordinates.resizedCoordinates
    val circle = new Circle {
      centerX = resized._1
      centerY = resized._2
      radius = 2
      fill = Color.Black
    }
    circle.onMouseClicked = { _ =>
      pointData.clear()
      getDeliveryDataOnClick(d).foreach(pointData.add)
    }

    circle
  }

  /** Returns some information about this warehouse. */
  private def getWarehouseDataOnClick(w: Int): List[String] = {
    val real = nodesCoordinates(w).realCoordinates
    List(
      s"Warehouse $w",
      s"X = ${real._1}",
      s"Y = ${real._2}",
      s"Status : ${if (_openWarehouses.contains(w)) "Open" else "Closed"}"
    )
  }

  /** Returns some information about this delivery point. */
  private def getDeliveryDataOnClick(d: Int): List[String] = {
    val real = nodesCoordinates(d + nbWarehouses).realCoordinates
    List(s"Delivery point $d", s"X = ${real._1}", s"Y = ${real._2}") :::
      (if (_openWarehouses.nonEmpty) {
         val deliveredBy = closestOpenedWarehouse(d)(_openWarehouses.toList)
         val wReal       = nodesCoordinates(deliveredBy).realCoordinates
         List(s"Served by warehouse $deliveredBy at (${wReal._1}, ${wReal._2})")
       } else List.empty)
  }

  /** Based on the new value of open warehouses, update warehouse icon and tooltips.
    *
    * @param openWarehouses
    *   The new value of open warehouses.
    */
  private def updateWarehouses(openWarehouses: Set[Int]): Unit = {
    warehouses.indices.foreach(w => {
      warehouses(w).setFill(Color.Pink)
    })
    openWarehouses.foreach(ow => {
      warehouses(ow).setFill(Color.GreenYellow)
    })
  }

  override private[cartesian] var listOfShapes: List[Shape] = List.empty

  override def redraw(solution: Solution): Unit = {
    val openWarehousesNow = solution.valueOfVariable(openWarehouses).get
    _openWarehouses = openWarehousesNow
    updateWarehouses(openWarehousesNow)
    listOfShapes = warehouses.toList ::: deliveries.toList
  }

  override def initLayer(): Unit = {}
}
