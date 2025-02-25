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

package oscar.cbls.visual.geographic

import com.gluonhq.maps.MapView
import oscar.cbls.core.computation.{Solution, Variable}
import oscar.cbls.modeling.routing.VRS
import oscar.cbls.visual.geographic.layers.{
  RoutingDisplayLayer,
  RoutingNodeLayer,
  RoutingRouteLayer
}
import oscar.cbls.visual.{OscaRDisplay, OscaRPrimaryStage, OscaRStage}
import scalafx.scene.Scene
import scalafx.scene.layout.Region

object GeographicalRoutingDisplay {

  /** Displays the resolution of a VRS on an OSM map.
    *
    * It can be extended by using [[oscar.cbls.visual.geographic.layers.RoutingDisplayLayer]]
    * exposing information related to other constraints for instance.
    *
    * @param vrs
    *   The vehicle routing structure
    * @param nodesCoordinates
    *   The geographical coordinates of all nodes of the problem.
    * @param additionalLayers
    *   Some additional layers to display on top of the basic ones.
    * @param additionalStages
    *   Some additional stages to be displayed on separated windows.
    * @param width
    *   The width of the window.
    * @param height
    *   The height of the window.
    * @return
    *   The [[OscaRDisplay]] enclosing all Stages.
    */
  def apply(
    vrs: VRS,
    nodesCoordinates: Array[(Double, Double)],
    additionalLayers: List[RoutingDisplayLayer] = List.empty,
    additionalStages: List[() => OscaRStage] = List.empty,
    width: Int = 1200,
    height: Int = 700
  ): OscaRDisplay = {
    val nodesLayer = RoutingNodeLayer(vrs.v, nodesCoordinates)
    val routeLayer = RoutingRouteLayer(vrs.v, nodesCoordinates.map(x => (x._1, x._2)), vrs.routes)
    val layers     = additionalLayers ::: List(routeLayer, nodesLayer)
    OscaRDisplay.apply(
      vrs.store,
      () => new GeographicalRoutingDisplay(nodesCoordinates, layers, width, height),
      additionalStages
    )
  }
}

/** Displays the resolution of a VRS on an OSM map.
  *
  * It can be extended by using [[oscar.cbls.visual.geographic.layers.RoutingDisplayLayer]] exposing
  * information related to other constraints for instance.
  *
  * @param nodesCoordinates
  *   The geographical coordinates of all nodes of the problem.
  * @param layers
  *   All the layers that will be displayed on top of the OSM map.
  * @param myWidth
  *   The width of the window.
  * @param myHeight
  *   The height of the window.
  */
class GeographicalRoutingDisplay(
  nodesCoordinates: Array[(Double, Double)],
  layers: List[RoutingDisplayLayer],
  myWidth: Int,
  myHeight: Int
) extends OscaRPrimaryStage {

  private val mapView: MapView = new MapView
  private var minZoom: Int     = 0
  private val center: (Double, Double) =
    (
      nodesCoordinates.map(_._1).sum / nodesCoordinates.length,
      nodesCoordinates.map(_._2).sum / nodesCoordinates.length
    )

  private def centerAndAdjustZoom(): Unit = {
    val maxWidthInLon  = nodesCoordinates.map(_._2).max - nodesCoordinates.map(_._2).min
    val maxHeightInLat = nodesCoordinates.map(_._1).max - nodesCoordinates.map(_._1).min
    var zoom           = 0
    while (
      myWidth / Math.pow(2, zoom + 1) > maxWidthInLon &&
      myHeight / Math.pow(2, zoom + 1) > maxHeightInLat
    ) zoom += 1
    minZoom = zoom
    mapView.setCenter(center._1, center._2)
    mapView.setZoom(zoom)
  }

  override def redraw(solution: Solution): Unit = layers.foreach(_.redraw(solution))

  override def init(): Unit = {
    for (layer <- layers) {
      mapView.addLayer(layer)
      layer.init()
    }
    centerAndAdjustZoom()

    title = "Routing display"
    scene = new Scene(myWidth.toDouble, myHeight.toDouble) {
      root = new Region(mapView)
    }
  }

  override def variablesForSolutionExtraction(): List[Variable] = {
    layers.flatMap(_.variablesForSolutionExtraction())
  }
}
