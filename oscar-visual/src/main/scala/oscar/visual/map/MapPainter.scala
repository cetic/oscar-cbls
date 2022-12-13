/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
package oscar.visual.map

import java.awt.BasicStroke
import java.awt.Color
import java.awt.Graphics2D
import java.awt.RenderingHints
import org.jdesktop.swingx.JXMapViewer
import org.jdesktop.swingx.mapviewer.DefaultWaypointRenderer
import org.jdesktop.swingx.mapviewer.GeoPosition
import org.jdesktop.swingx.mapviewer.Waypoint
import org.jdesktop.swingx.painter.Painter
import java.awt.Font

/**
 * @author Pierre Schaus
 */
class MapPainter(map : VisualMap, lineStyle: String = "line") extends Painter[JXMapViewer] {
  var mymap: VisualMap = map

  private val renderer = new DefaultWaypointRenderer()

  def paint(gin: Graphics2D,
    map: JXMapViewer,
    w: Int,
    h: Int): Unit = {
    val g =  gin.create().asInstanceOf[Graphics2D]

    //convert from viewport to world bitmap
    val rect = mymap.viewer.getViewportBounds
    g.translate(-rect.x, -rect.y)

    /*
     * draw : 
     */
    //lines
    g.setColor(Color.RED)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.setStroke(new BasicStroke(2))

    for ( l <- mymap.lines) {
      //convert geo to world bitmap pixel
      val pt1 = mymap.viewer.getTileFactory.geoToPixel(new GeoPosition(l.orig._1, l.orig._2), mymap.viewer.getZoom)
      val pt2 = mymap.viewer.getTileFactory.geoToPixel(new GeoPosition(l.dest._1, l.dest._2), mymap.viewer.getZoom)
      g.setColor(l.color)

      lineStyle.toLowerCase match {
        case "arrow" => drawArrow(g, pt1.getX.toInt, pt1.getY.toInt, pt2.getX.toInt, pt2.getY.toInt)
        case "midarrow" => drawMidArrow(g, pt1.getX.toInt, pt1.getY.toInt, pt2.getX.toInt, pt2.getY.toInt)
        case _ => g.drawLine( pt1.getX.toInt, pt1.getY.toInt, pt2.getX.toInt, pt2.getY.toInt)
      }
    }
    //paths
    g.setColor(Color.BLACK)
    for ( l <- mymap.paths.flatMap(_.lines)) {
      //convert geo to world bitmap pixel
      val pt1 = mymap.viewer.getTileFactory.geoToPixel(new GeoPosition(l.orig._1, l.orig._2), mymap.viewer.getZoom)
      val pt2 = mymap.viewer.getTileFactory.geoToPixel(new GeoPosition(l.dest._1, l.dest._2), mymap.viewer.getZoom)
      g.setColor(l.color)
      g.drawLine(pt1.getX.toInt, pt1.getY.toInt, pt2.getX.toInt, pt2.getY.toInt)
    }
    //waypoints
    g.setColor(Color.BLUE)
    for (wp <- mymap.waypoints) {
      val pt1 = map.getTileFactory.geoToPixel(new GeoPosition(wp.lat, wp.long), map.getZoom)
      val x =  pt1.getX.toInt
      val y = pt1.getY.toInt
      g.setColor(wp.color)
      g.setStroke(new BasicStroke(2f))
      g.drawOval(x-10,y-10,20,20)
      g.setStroke(new BasicStroke(1f))
      g.drawLine(x-10,y-0,x+10,y+0)
      g.drawLine(x-0,y-10,x+0,y+10)
      if (wp.label != null) {
	g.setFont(new Font("Arial", Font.BOLD, 16))
	g.drawString(wp.label, x+15 , y)
      }
    }

    for (poly <- mymap.polygons) {
      val pointsPxl = poly.coords.map(c => mymap.viewer.getTileFactory.geoToPixel(new GeoPosition(c._1,c._2),mymap.viewer.getZoom))
      val ptX = pointsPxl.map(_.getX.toInt)
      val ptY = pointsPxl.map(_.getY.toInt)
      val nPoints = poly.coords.length
      g.setColor(poly.color)
      g.fillPolygon(ptX,ptY,nPoints)

    }
    g.dispose()
  }

  protected def paintWaypoint(w: Waypoint, g:  Graphics2D): Unit = {
    renderer.paintWaypoint(g, mymap.viewer, w)
  }

  // Adapted from https://stackoverflow.com/a/27461352 :
  /** Draw an arrow line between two points.
    *
    * @param g the graphics component.
    * @param x1 x -position of first point.
    * @param y1 y -position of first point.
    * @param x2 x -position of second point.
    * @param y2 y -position of second point.
    * @param h the height of the arrow.
    * @param w the width of the arrow.
    */
  protected def drawArrow(g: Graphics2D, x1: Int, y1: Int, x2: Int, y2: Int,
                          h: Int = 10, w: Int = 2): Unit = {

    val mid = ((x1 + x2) / 2, (y1 + y2) / 2)
    val (dx, dy) = (x2 - x1, y2 - y1)
    val l = Math.sqrt(dx * dx + dy * dy)
    var xm: Double = l - h
    var xn: Double = xm
    var ym: Double = w
    var yn: Double = -w
    val sin: Double = dy / l
    val cos: Double = dx / l

    var x: Double = xm * cos - ym * sin + x1
    ym = xm * sin + ym * cos + y1
    xm = x
    x = xn * cos - yn * sin + x1
    yn = xn * sin + yn * cos + y1
    xn = x

//    val xs = Array(x2, xm.asInstanceOf[Int], xn.asInstanceOf[Int])
//    val ys = Array(y2, ym.asInstanceOf[Int], yn.asInstanceOf[Int])
    g.drawLine(x1, y1, x2, y2)
    g.drawLine(x2, y2, xm.toInt, ym.toInt)
    g.drawLine(x2, y2, xn.toInt, yn.toInt)
//    g.fillPolygon(xs, ys, 3)
  }

  /** Draw a mid-arrow line between two points.
    *
    * @param g  the graphics component.
    * @param x1 x -position of first point.
    * @param y1 y -position of first point.
    * @param x2 x -position of second point.
    * @param y2 y -position of second point.
    * @param h  the height of the arrow.
    * @param w  the width of the arrow.
    */
  protected def drawMidArrow(g: Graphics2D, x1: Int, y1: Int, x2: Int, y2: Int,
                          h: Int = 10, w: Int = 4): Unit = {

    val (x3, y3) = ((x1 + x2) / 2, (y1 + y2) / 2)
    val (dx, dy) = (x3 - x1, y3 - y1)
    val l = Math.sqrt(dx * dx + dy * dy)
    var xm: Double = l - h
    var xn: Double = xm
    var ym: Double = w
    var yn: Double = -w
    val sin: Double = dy / l
    val cos: Double = dx / l

    var x: Double = xm * cos - ym * sin + x1
    ym = xm * sin + ym * cos + y1
    xm = x
    x = xn * cos - yn * sin + x1
    yn = xn * sin + yn * cos + y1
    xn = x

    val xs = Array(x3, xm.round.toInt, xn.round.toInt)
    val ys = Array(y3, ym.round.toInt, yn.round.toInt)
    g.drawLine(x1, y1, x2, y2)
//    g.drawLine(x3, y3, xm.round.toInt, ym.round.toInt)
//    g.drawLine(x3, y3, xn.round.toInt, yn.round.toInt)
    g.fillPolygon(xs, ys, 3)
  }
}
