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

import oscar.cbls.core.computation.Solution
import oscar.cbls.core.computation.seq.SeqVariable
import oscar.cbls.visual.generator.{ArrowGenerator, ColorGenerator}
import scalafx.beans.property.ObjectProperty
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Line, Polygon, Shape}

object RoutingRouteLayer {

  /** This RoutingDisplayLayer's purpose is to draw the route of each vehicle.
    *
    * @param nbVehicles
    *   The number of vehicles for this problem instance.
    * @param nodesCoordinates
    *   The coordinates of all nodes of this problem instance.
    * @param routesAsVariable
    *   * The variable representing the route in the problem.
    */
  def apply(
    nbVehicles: Int,
    nodesCoordinates: Array[(Double, Double)],
    routesAsVariable: SeqVariable
  ): RoutingRouteLayer = {
    new RoutingRouteLayer(nbVehicles, nodesCoordinates, routesAsVariable)
  }
}

/** This RoutingDisplayLayer's purpose is to draw the route of each vehicle.
  *
  * @param nbVehicles
  *   The number of vehicles for this problem instance.
  * @param nodesCoordinates
  *   The coordinates of all nodes of this problem instance.
  * @param routesAsVariable
  *   * The variable representing the route in the problem.
  */
class RoutingRouteLayer(
  nbVehicles: Int,
  nodesCoordinates: Array[(Double, Double)],
  routesAsVariable: SeqVariable
) extends RoutingDisplayLayer {

  // Radius, in pixels, of a node's circle icon (see RoutingNodeLayer). Arrowheads stop short of
  // the destination node by this amount so the tip touches the circle's boundary.
  private final val NODE_RADIUS: Double = 4.0

  private val colors: Array[Color] =
    ColorGenerator.generateContrastingColors(nbVehicles, saturation = 0.95, brightness = 0.65)
  private val routesSequence: ObjectProperty[List[Int]] = ObjectProperty[List[Int]](List.empty)

  // The shapes drawn for the previous layout, so they can be removed before rebuilding.
  private var edgeShapes: List[Shape] = List.empty

  routesSequence.onChange {
    markDirty()
  }

  override def redraw(solution: Solution): Unit = {
    val newRouteValue = solution.valueOfVariable(routesAsVariable)
    require(newRouteValue.nonEmpty, "Can not retrieve route's new value.")
    routesSequence.value = newRouteValue.get.toList
  }

  override def layoutLayer(): Unit = {
    edgeShapes.foreach(this.getChildren.remove(_))
    edgeShapes = List.empty

    if (routesSequence.value.nonEmpty) {
      var shapes: List[Shape] = List.empty

      // Draws the directed edge of vehicle vehicleId's route from node fromId to node toId, as a
      // Line shaft plus a triangular arrowhead Polygon. Skipped when fromId == toId, which happens
      // when a vehicle's route is empty (its depot's closing edge would otherwise loop on itself).
      def drawEdge(vehicleId: Int, fromId: Int, toId: Int): Unit = {
        if (fromId != toId) {
          val p1    = getMapPoint(nodesCoordinates(fromId)._1, nodesCoordinates(fromId)._2)
          val p2    = getMapPoint(nodesCoordinates(toId)._1, nodesCoordinates(toId)._2)
          val arrow = ArrowGenerator.computeArrow(p1.getX, p1.getY, p2.getX, p2.getY, NODE_RADIUS)

          val shaft = new Line {
            startX = p1.getX
            startY = p1.getY
            endX = arrow.shaftEndX
            endY = arrow.shaftEndY
            stroke = colors(vehicleId)
            strokeWidth = 2
          }
          val arrowhead = new Polygon {
            fill = colors(vehicleId)
          }
          arrowhead.points.addAll(arrow.headPoints.map(d => Double.box(d)): _*)

          shapes = shapes :+ shaft :+ arrowhead
        }
      }

      var currentVehicleId = -1
      var previousPointId  = -1
      for { pointId <- routesSequence.value } {
        if (pointId < nbVehicles) {
          if (currentVehicleId >= 0) drawEdge(currentVehicleId, previousPointId, currentVehicleId)
          currentVehicleId += 1
        } else {
          drawEdge(currentVehicleId, previousPointId, pointId)
        }
        previousPointId = pointId
      }
      drawEdge(currentVehicleId, previousPointId, currentVehicleId)

      edgeShapes = shapes
      edgeShapes.foreach(this.getChildren.add(_))
    }
  }

  override def init(): Unit = {}

}
