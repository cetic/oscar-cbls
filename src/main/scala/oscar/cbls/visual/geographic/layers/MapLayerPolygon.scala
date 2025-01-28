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
import scalafx.scene.Node
import scalafx.scene.paint.Color
import scalafx.scene.shape.Polygon
import scalafx.scene.text.Text

/** A polygon on a [[com.gluonhq.maps.MapLayer]] with all it's artefact.
  *
  * @param polygon
  *   The Polygon object.
  * @param edgeIcon
  *   The icon of each edge.
  * @param edges
  *   The MapPoint at each edge.
  * @param text
  *   The text of the polygon.
  */
case class MapLayerPolygon(
  polygon: Polygon,
  edgeIcon: List[Node],
  edges: List[MapPoint],
  text: Option[Text]
) {
  def changeColor(newColor: Color): Unit = {
    polygon.setStroke(newColor)
    polygon.setFill(newColor)
  }
}
