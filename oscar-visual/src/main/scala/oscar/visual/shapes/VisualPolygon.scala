/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.visual.shapes

import java.awt.Polygon
import oscar.visual.VisualDrawing

class VisualPolygon(d: VisualDrawing, s: Polygon) extends VisualShape(d) {

  type S = Polygon
  protected val shape = s

  def reset(): Unit = shape.reset

  def addVertices(vertices: Iterable[(Int, Int)]): Unit = {
    for (v <- vertices) shape.addPoint(v._1, v._2)
    if (autoRepaint) repaint()
  }

  def addVertices(vertices: (Int, Int)*): Unit = {
    for (v <- vertices) shape.addPoint(v._1, v._2)
    if (autoRepaint) repaint()
  }

  def update(vertices: Iterable[(Int, Int)]): Unit = {
    reset()
    addVertices(vertices)
  }

  // Warning, we take the first vertex as point of reference
  def moveAt(x: Double, y: Double): Unit = {
    val refX = shape.xpoints.head
    val refY = shape.ypoints.head
    val deltaX = x-refX
    val deltaY = y-refY
    shape.translate(deltaX.toInt,deltaY.toInt)
  }

  def translate(x: Double, y: Double): Unit = shape.translate(x.toInt, y.toInt)
}

object VisualPolygon {

  def apply(drawing: VisualDrawing): VisualPolygon = {
    new VisualPolygon(drawing, new Polygon)
  }

  def apply(drawing: VisualDrawing, vertices: Iterable[(Int, Int)]): VisualPolygon = {
    val polygon = new VisualPolygon(drawing, new Polygon)
    polygon.addVertices(vertices)
    polygon
  }
}
