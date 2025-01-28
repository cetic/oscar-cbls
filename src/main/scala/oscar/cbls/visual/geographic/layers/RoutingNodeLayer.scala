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

package oscar.cbls.visual.geographic.layers

import com.gluonhq.maps.MapPoint
import oscar.cbls.core.computation.Solution
import scalafx.application.Platform
import scalafx.beans.property.ObjectProperty
import scalafx.scene.control.Tooltip
import scalafx.scene.paint.Color
import scalafx.scene.shape.Circle

object RoutingNodeLayer {

  /** This layer displays all the nodes that are part of this VRP.
    *
    * Every node-related information should be in this class.
    * @param nbVehicles
    *   The number of vehicles for this problem instance.
    * @param nodesCoordinates
    *   The coordinates of all nodes of this problem instance.
    */
  def apply(nbVehicles: Int, nodesCoordinates: Array[(Double, Double)]): RoutingNodeLayer = {
    new RoutingNodeLayer(nbVehicles, nodesCoordinates)
  }
}

/** This layer displays all the nodes that are part of this VRP.
  *
  * Every node-related information should be in this class.
  * @param nbVehicles
  *   The number of vehicles for this problem instance.
  * @param nodesCoordinates
  *   The coordinates of all nodes of this problem instance.
  */
class RoutingNodeLayer(nbVehicles: Int, nodesCoordinates: Array[(Double, Double)])
    extends RoutingDisplayLayer {

  private val allNodesCoordinates: ObjectProperty[Array[(Double, Double)]] = ObjectProperty(
    nodesCoordinates
  )
  private var pointsAndNodes: Array[(MapPoint, Circle)] = Array.empty

  allNodesCoordinates.onChange {
    Platform.runLater(generateNodes())
  }

  private def generateNodes(): Unit = {
    val newValue = allNodesCoordinates.value
    for (node <- pointsAndNodes) this.getChildren.remove(node._2)
    pointsAndNodes = Array.tabulate(newValue.length)(i => {
      val (lat, lon) = newValue(i)
      val point      = new MapPoint(lat, lon)
      val icon       = if (i < nbVehicles) Circle(4, Color.OrangeRed) else Circle(4, Color.Blue)
      val tooltip = new Tooltip(s"Node $i: ${if (i < nbVehicles) "Depot point"
        else "Passage point"} at (${point.getLatitude}, ${point.getLongitude})")
      Tooltip.install(icon, tooltip)

      this.getChildren.add(icon)
      (point, icon)
    })
    this.markDirty()
  }

  override def redraw(solution: Solution): Unit = {}

  override def init(): Unit = {
    generateNodes()
  }

  override def layoutLayer(): Unit = {
    for { pointAndNode <- pointsAndNodes } {
      val point    = pointAndNode._1
      val node     = pointAndNode._2
      val mapPoint = getMapPoint(point.getLatitude, point.getLongitude)
      node.setVisible(true)
      node.setTranslateX(mapPoint.getX)
      node.setTranslateY(mapPoint.getY)
    }
  }

}
