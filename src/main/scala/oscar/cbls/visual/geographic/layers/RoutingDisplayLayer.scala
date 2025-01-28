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

import com.gluonhq.maps.{MapLayer, MapPoint}
import oscar.cbls.core.computation.{Solution, Variable}
import oscar.cbls.visual.geographic.GeographicalRoutingDisplay
import scalafx.scene.Node
import scalafx.scene.shape.Polygon
import scalafx.scene.text.Text
import scalafx.scene.text.TextAlignment.Center

/** Base class containing all the methods that need to be overridden to properly interact with
  * [[GeographicalRoutingDisplay]].
  *
  * @param additionalVariableForExtraction
  *   The additional [[oscar.cbls.core.computation.Variable]] needed to update the data displayed on
  *   the layer.
  */
abstract class RoutingDisplayLayer(additionalVariableForExtraction: List[Variable] = List.empty)
    extends MapLayer {

  /** Returns a list variable that should be extract from the solution for display purposes. */
  def variablesForSolutionExtraction(): List[Variable] = additionalVariableForExtraction

  /** Redraws some graphical data if needed.
    *
    * @param solution
    *   The current solution of the problem. It should contain all needed value.
    */
  def redraw(solution: Solution): Unit

  /** Initializes the RoutingDisplayLayer if needed. */
  def init(): Unit

  /** Bypasses the protected annotation. */
  override def markDirty(): Unit = super.markDirty()

  /** Generates a [[MapLayerPolygon]] based on Polygon input.
    *
    * Basically generates all the necessary element to handle a polygon on a
    * [[https://javadoc.io/doc/com.gluonhq/maps/latest/com.gluonhq.maps/com/gluonhq/maps/MapLayer.html com.gluonhq.maps.MapLayer]].
    *
    * @param polygon
    *   The Polygon shape.
    * @param edgeCoordinates
    *   The coordinates of the polygon, used to recompute the position of the shape on the MapLayer
    *   when zooming or moving the map.
    * @param edgeIconGen
    *   The icon of the edge.
    */
  protected def genMapLayerPolygon(
    polygon: Polygon,
    edgeCoordinates: List[(Double, Double)],
    edgeIconGen: () => Node,
    text: Option[Text] = None
  ): MapLayerPolygon = {
    var mapPoints: List[MapPoint] = List.empty
    var icons: List[Node]         = List.empty
    for (p <- edgeCoordinates) {
      val icon     = edgeIconGen()
      val mapPoint = new MapPoint(p._1, p._2)
      mapPoints :+= mapPoint
      icons :+= icon
      this.getChildren.add(icon)
      this.markDirty()
    }
    MapLayerPolygon(polygon, icons, mapPoints, text)
  }

  /** Translates a Polygon shape when needed.
    *
    * @param mapLayerPolygon
    *   The MapLayerPolygon containing all necessary data.
    */
  protected def translatePolygons(mapLayerPolygon: MapLayerPolygon): Unit = {
    mapLayerPolygon.polygon.getPoints.clear()
    for { (point, icon) <- mapLayerPolygon.edges zip mapLayerPolygon.edgeIcon } {
      val mapPoint = getMapPoint(point.getLatitude, point.getLongitude)
      icon.setVisible(true)
      icon.setTranslateX(mapPoint.getX)
      icon.setTranslateY(mapPoint.getY)
      mapLayerPolygon.polygon.getPoints.addAll(mapPoint.getX, mapPoint.getY)
    }
    mapLayerPolygon.text.foreach(t => {
      val points: Array[Double] = mapLayerPolygon.polygon.points.toArray.map(_.doubleValue())
      val longitudes: List[Double] =
        List.tabulate(points.length / 2 - 1)(x => points(x * 2 + 1)).sorted
      val latitudes: List[Double] = List.tabulate(points.length / 2 - 1)(x => points(x * 2)).sorted

      t.setTranslateX(latitudes.sum / latitudes.size)
      t.setTranslateY(longitudes.sum / longitudes.size)
      t.setTranslateX(t.getTranslateX - t.getLayoutBounds.getWidth / 2)
      t.setTranslateY(t.getTranslateY + t.getLayoutBounds.getHeight / 4)
      t.textAlignment = Center
    })
  }
}
