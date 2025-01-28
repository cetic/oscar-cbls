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
import oscar.cbls.visual.generator.ColorGenerator
import scalafx.beans.property.ObjectProperty
import scalafx.scene.paint.Color
import scalafx.scene.shape.Polyline

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

  private val colors: Array[Color]    = ColorGenerator.generateRandomColors(nbVehicles)
  private var routes: Array[Polyline] = Array.empty
  private val routesSequence: ObjectProperty[List[Int]] = ObjectProperty[List[Int]](List.empty)

  routesSequence.onChange {
    markDirty()
  }

  private def generateRoutes(): Unit = {
    routes = Array.tabulate(nbVehicles)(v => {
      val route = new Polyline()
      route.setStroke(colors(v))
      route.setFill(Color.Transparent)
      route.setStrokeWidth(2)
      this.getChildren.add(route)
      route
    })
  }

  override def redraw(solution: Solution): Unit = {
    val newRouteValue = solution.valueOfVariable(routesAsVariable)
    require(newRouteValue.nonEmpty, "Can not retrieve route's new value.")
    routesSequence.value = newRouteValue.get.toList
  }

  override def layoutLayer(): Unit = {
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
        val coordinate = nodesCoordinates(pointId)
        val mapPoint   = getMapPoint(coordinate._1, coordinate._2)
        routes(vehicleId).getPoints.addAll(mapPoint.getX, mapPoint.getY)
      }
    }
  }

  override def init(): Unit = {
    generateRoutes()
  }

}
