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
import oscar.cbls.visual.generator.ColorGenerator
import scalafx.application.Platform
import scalafx.beans.property.ObjectProperty
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Polyline, Shape}

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

  private val colors: Array[Color]    = ColorGenerator.generateRandomColors(nbVehicles)
  private var routes: Array[Polyline] = Array.empty
  private val routesSequence: ObjectProperty[List[Int]] = ObjectProperty[List[Int]](List.empty)

  routesSequence.onChange {
    Platform.runLater(drawRoutes())
  }

  private def generateRoutes(): Unit = {
    routes = Array.tabulate(nbVehicles)(v => {
      val route = new Polyline()
      route.setStroke(colors(v))
      route.setFill(Color.Transparent)
      route.setStrokeWidth(2)
      listOfShapes = listOfShapes :+ route
      route
    })
  }

  override private[cartesian] var listOfShapes: List[Shape] = List.empty

  override def redraw(solution: Solution): Unit = {
    val newRouteValue = solution.valueOfVariable(routesAsVariable)
    require(newRouteValue.nonEmpty, "Can not retrieve route's new value.")
    routesSequence.value = newRouteValue.get.toList
  }

  private def drawRoutes(): Unit = {
    if (routesSequence.value.nonEmpty) {
      routes.foreach(_.getPoints.clear())
      var currentVehicleId = -1
      for { pointId <- routesSequence.value } {
        if (pointId < nbVehicles) {
          if (currentVehicleId >= 0) addPointToTravel(currentVehicleId, currentVehicleId)
          currentVehicleId += 1
        }
        addPointToTravel(currentVehicleId, pointId)
      }
      addPointToTravel(currentVehicleId, currentVehicleId)

      def addPointToTravel(vehicleId: Int, pointId: Int): Unit = {
        val coordinate = nodesCoordinates(pointId).resizedCoordinates
        routes(vehicleId).getPoints.addAll(coordinate._1, coordinate._2)
      }
    }
  }

  override def initLayer(): Unit = {
    generateRoutes()
  }
}
