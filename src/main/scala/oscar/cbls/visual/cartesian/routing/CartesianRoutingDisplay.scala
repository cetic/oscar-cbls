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

package oscar.cbls.visual.cartesian.routing

import oscar.cbls.IntVariable
import oscar.cbls.core.computation.{Solution, Variable}
import oscar.cbls.modeling.routing.VRS
import oscar.cbls.visual.additionalStages.ObjectiveFunctionDisplay
import oscar.cbls.visual.cartesian.{CartesianLayer, CartesianNode}
import oscar.cbls.visual.cartesian.routing.layers.{
  RoutingNodeCartesianLayer,
  RoutingRouteCartesianLayer
}
import oscar.cbls.visual.{OscaRDisplay, OscaRPrimaryStage}
import scalafx.scene.Scene
import scalafx.scene.paint.Color.White

object CartesianRoutingDisplay {

  /** Generates a CartesianRoutingDisplay.
    *
    * @param vrs
    *   The vehicle routing structure
    * @param nodesCoordinates
    *   The cartesian coordinates of all nodes of the problem.
    * @param additionalPanes
    *   Some additional panes to display on top of the basic ones.
    * @param width
    *   The width of the window
    * @param height
    *   The height of the window
    * @return
    *   The OscaRDisplay object containing the routing display.
    */
  def apply(
    obj: IntVariable,
    vrs: VRS,
    nodesCoordinates: Array[(Long, Long)],
    additionalPanes: List[CartesianLayer] = List.empty,
    width: Int = 1200,
    height: Int = 700
  ): OscaRDisplay = {
    require(
      !nodesCoordinates.exists(x => x._1 < 0 || x._2 < 0),
      "This routing display tool accepts only positive coordinates."
    )
    val cartesianNodes   = CartesianNode(nodesCoordinates)
    val routingNodePane  = RoutingNodeCartesianLayer(vrs.v, cartesianNodes)
    val routingRoutePane = RoutingRouteCartesianLayer(vrs.v, cartesianNodes, vrs.routes)
    val panes            = additionalPanes ::: List(routingRoutePane, routingNodePane)
    val additionalStages = List(() => ObjectiveFunctionDisplay(obj, System.nanoTime()))
    OscaRDisplay.apply(
      vrs.store,
      () => new CartesianRoutingDisplay(cartesianNodes, panes, width, height),
      additionalStages = additionalStages
    )
  }
}

/** Displays the resolution of a routing problem on a cartesian plan.
  *
  * @param nodesCoordinates
  *   The geographical coordinates of all nodes of the problem.
  * @param panes
  *   All the panes that must be displayed.
  * @param myWidth
  *   The width of the window
  * @param myHeight
  *   The height of the window
  */
class CartesianRoutingDisplay(
  nodesCoordinates: Array[CartesianNode],
  panes: List[CartesianLayer],
  myWidth: Int,
  myHeight: Int
) extends OscaRPrimaryStage {

  this.resizable = false

  /** Redraws all graphical items that needs to be redrawn. */
  override def redraw(solution: Solution): Unit = {
    panes.foreach(_.redraw(solution))
  }

  override def init(): Unit = {
    nodesCoordinates.foreach(_.resize(myWidth, myHeight))
    panes.foreach(_.initLayer())
    title = "Routing display"
    scene = new Scene(myWidth.toDouble, myHeight.toDouble) {
      fill = White
      content = panes.flatMap(_.listOfShapes)
    }
  }

  override def variablesForSolutionExtraction(): List[Variable] = {
    List.empty
  }
}
