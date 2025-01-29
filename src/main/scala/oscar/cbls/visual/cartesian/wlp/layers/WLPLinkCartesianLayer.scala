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
import scalafx.scene.shape.{Line, Shape}

object WLPLinkCartesianLayer {

  /** Returns a layer displaying the link between a delivery point and the warehouse that serves it.
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
    */
  def apply(
    nbWarehouses: Int,
    nbDeliveries: Int,
    nodesCoordinates: Array[CartesianNode],
    closestOpenedWarehouse: Array[List[Int] => Int],
    openWarehouses: SetVariable
  ): WLPLinkCartesianLayer = {
    new WLPLinkCartesianLayer(
      nbWarehouses,
      nbDeliveries,
      nodesCoordinates,
      closestOpenedWarehouse,
      openWarehouses
    )
  }
}

/** This layer displays the link between a delivery point and the warehouse that serves it.
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
  */
class WLPLinkCartesianLayer(
  nbWarehouses: Int,
  nbDeliveries: Int,
  nodesCoordinates: Array[CartesianNode],
  closestOpenedWarehouse: Array[List[Int] => Int],
  openWarehouses: SetVariable
) extends CartesianLayer() {

  private val links: Array[Line] = Array.fill(nbDeliveries)(null)

  override def redraw(solution: Solution): Unit = {
    val openWarehousesNow = solution.valueOfVariable(openWarehouses).get
    updateLinks(openWarehousesNow.toList)
    listOfShapes = links.toList
  }

  private def updateLinks(openWarehouses: List[Int]): Unit = {
    links.indices.foreach(l => {
      val linkedW  = closestOpenedWarehouse(l)(openWarehouses)
      val resizedW = nodesCoordinates(linkedW).resizedCoordinates
      if (links(l) == null) { // Link creation
        val resizedD = nodesCoordinates(nbWarehouses + l).resizedCoordinates
        links(l) = new Line {
          startX = resizedD._1
          startY = resizedD._2
          endX = resizedW._1
          endY = resizedW._2
          strokeDashArray = Seq(1d, 2.5d)
        }
      } else { // Just moving the end of the link to the new warehouse
        links(l).endX = resizedW._1
        links(l).endY = resizedW._2
      }
    })
  }

  override private[cartesian] var listOfShapes: List[Shape] = List.empty

  override def initLayer(): Unit = {}
}
