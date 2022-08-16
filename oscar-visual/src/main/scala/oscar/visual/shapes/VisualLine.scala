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

import java.awt.geom.Line2D
import oscar.visual.VisualDrawing
import java.awt.Graphics2D
import java.awt.BasicStroke

/**
 *
 * @author Pierre Schaus
 *
 */
class VisualLine(d: VisualDrawing, s: Line2D.Double) extends VisualShape(d) {
  
  type S = Line2D.Double
  protected val shape = s
  
  // No need to fill the line
  fill = false
  
  def orig = (shape.getX1(), shape.getY1())
  def dest = (shape.getX2(), shape.getY2())

  /**
   * Move the destination point
   * @param x
   * @param y
   */
  def dest_=(dest: (Double, Double)): Unit = { 
    shape.setLine(shape.getX1(), shape.getY1(), dest._1, dest._2)
    if (autoRepaint) repaint()
  }

  /**
   * Move the origin point
   * @param x
   * @param y
   */
  def orig_=(orig: (Double, Double)): Unit = {
    shape.setLine(orig._1, orig._2, shape.getX2(), shape.getY2())
    if (autoRepaint) repaint()
  }
  
  def move(x: Double, y: Double): Unit = {
    val (xOrig, yOrig) = orig
    val (xDest, yDest) = dest
    orig = (xOrig + x, yOrig + y)
    dest = (xDest + x, yDest + y)
    if (autoRepaint) repaint()
  }
}

object VisualLine {
  
  def apply(drawing: VisualDrawing, xOrig: Double, yOrig: Double, xDest: Double, yDest: Double): VisualLine = {
    new VisualLine(drawing, new Line2D.Double(xOrig, yOrig, xDest, yDest))
  }
}
