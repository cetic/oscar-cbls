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

package oscar.cbls.visual.cartesian.routing.layers

import oscar.cbls.core.computation.Solution
import oscar.cbls.visual.cartesian.{CartesianLayer, CartesianNode}
import scalafx.application.Platform
import scalafx.beans.property.ObjectProperty
import scalafx.scene.control.Tooltip
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Circle, Shape}

object RoutingNodeCartesianLayer {

  /** This layer displays all the nodes that are part of this VRS.
    *
    * Every node-related information should be in this class.
    * @param nbVehicles
    *   The number of vehicles of this VRS.
    * @param nodesCoordinates
    *   The coordinates of all nodes of this VRS.
    */
  def apply(nbVehicles: Int, nodesCoordinates: Array[CartesianNode]): RoutingNodeCartesianLayer = {
    new RoutingNodeCartesianLayer(nbVehicles, nodesCoordinates)
  }
}

/** This layer displays all the nodes that are part of this VRS.
  *
  * Every node-related information should be in this class.
  * @param nbVehicles
  *   The number of vehicles of this VRS.
  * @param nodesCoordinates
  *   The coordinates of all nodes of this VRS.
  */
class RoutingNodeCartesianLayer(nbVehicles: Int, nodesCoordinates: Array[CartesianNode])
    extends CartesianLayer {

  private val allNodesCoordinates: ObjectProperty[Array[CartesianNode]] = ObjectProperty(
    nodesCoordinates
  )

  allNodesCoordinates.onChange {
    Platform.runLater(generateNodes())
  }

  private def generateNodes(): Unit = {
    val newValue = allNodesCoordinates.value
    listOfShapes = List.tabulate(newValue.length)(i => {
      val (x, y) = newValue(i).resizedCoordinates
      val rc     = newValue(i).realCoordinates
      val icon =
        if (i < nbVehicles) Circle(x, y, 4, Color.OrangeRed) else Circle(x, y, 4, Color.Blue)
      val tooltip = new Tooltip(
        s"Node $i: ${if (i < nbVehicles) "Depot point" else "Passage point"} at (${rc._1}, ${rc._1})"
      )
      Tooltip.install(icon, tooltip)
      icon
    })
  }

  override private[cartesian] var listOfShapes: List[Shape] = List.empty

  override def redraw(solution: Solution): Unit = {}

  override def initLayer(): Unit = {
    generateNodes()
  }
}
