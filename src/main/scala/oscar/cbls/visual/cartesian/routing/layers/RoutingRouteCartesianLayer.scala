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
import oscar.cbls.core.computation.seq.SeqVariable
import oscar.cbls.visual.cartesian.{CartesianLayer, CartesianNode}
import oscar.cbls.visual.generator.{ArrowGenerator, ColorGenerator}
import scalafx.application.Platform
import scalafx.beans.property.ObjectProperty
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Line, Polygon, Shape}

object RoutingRouteCartesianLayer {

  /** Returns a layer displaying the route of each vehicle.
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
    nodesCoordinates: Array[CartesianNode],
    routesAsVariable: SeqVariable
  ): RoutingRouteCartesianLayer = {
    new RoutingRouteCartesianLayer(nbVehicles, nodesCoordinates, routesAsVariable)
  }
}

/** This layer displays the route of each vehicle.
  *
  * @param nbVehicles
  *   The number of vehicles for this problem instance.
  * @param nodesCoordinates
  *   The coordinates of all nodes of this problem instance.
  * @param routesAsVariable
  *   * The variable representing the route in the problem.
  */
class RoutingRouteCartesianLayer(
  nbVehicles: Int,
  nodesCoordinates: Array[CartesianNode],
  routesAsVariable: SeqVariable
) extends CartesianLayer {

  // Radius, in pixels, of a node's circle icon (see RoutingNodeCartesianLayer). Arrowheads stop
  // short of the destination node by this amount so the tip touches the circle's boundary.
  private final val NODE_RADIUS: Double = 4.0

  private val colors: Array[Color] = ColorGenerator.generateContrastingColors(nbVehicles)
  private val routesSequence: ObjectProperty[List[Int]] = ObjectProperty[List[Int]](List.empty)

  routesSequence.onChange {
    Platform.runLater(drawRoutes())
  }

  override private[cartesian] var listOfShapes: List[Shape] = List.empty

  override def redraw(solution: Solution): Unit = {
    val newRouteValue = solution.valueOfVariable(routesAsVariable)
    require(newRouteValue.nonEmpty, "Can not retrieve route's new value.")
    routesSequence.value = newRouteValue.get.toList
  }

  private def drawRoutes(): Unit = {
    if (routesSequence.value.nonEmpty) {
      var shapes: List[Shape] = List.empty

      // Draws the directed edge of vehicle vehicleId's route from node fromId to node toId, as a
      // Line shaft plus a triangular arrowhead Polygon. Skipped when fromId == toId, which happens
      // when a vehicle's route is empty (its depot's closing edge would otherwise loop on itself).
      def drawEdge(vehicleId: Int, fromId: Int, toId: Int): Unit = {
        if (fromId != toId) {
          val (x1, y1) = nodesCoordinates(fromId).resizedCoordinates
          val (x2, y2) = nodesCoordinates(toId).resizedCoordinates
          val arrow    = ArrowGenerator.computeArrow(x1, y1, x2, y2, NODE_RADIUS)

          val shaft = new Line {
            startX = x1
            startY = y1
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

      listOfShapes = shapes
    }
  }

  override def initLayer(): Unit = {}
}
