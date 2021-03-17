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
package oscar.visual.calendar

import java.awt.Color
import java.awt.geom.RoundRectangle2D
import oscar.visual.VisualDrawing
import oscar.visual.VisualFrame
import oscar.visual.shapes.VisualLabelledRoundRectangle

/**
 *
 * @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
 *
 */
class VisualCalendarTile(d: VisualDrawing, s: RoundRectangle2D.Double, dayLabel: String, _marginWidth: Double = 0) extends VisualLabelledRoundRectangle(d, s, dayLabel, _marginWidth) {
  this.marginWidth = _marginWidth
  
  
  def this(d: VisualDrawing, x: Double, y: Double, size: Int, label: String, arcw: Double, arch: Double) = {
    this(d, 
         new RoundRectangle2D.Double(x,y,size,size,arcw,arch),
         label,
         (size - d.getFontMetrics(d.getFont).stringWidth(label)) / 2)
  }

  def this(d: VisualDrawing, x: Double, y: Double, size: Int, label: String) = this(d,x,y,size,label,7.0,7.0)
    
  /**
   * Y coordinates of bottom left corner
   * @return
   */
  override def yText: Int = (y + d.getFontMetrics(d.getFont).getHeight).toInt
}

object VisualCalendarTile {

  def main(args: Array[String]): Unit = {
    val f = VisualFrame("toto")
    val d = VisualDrawing(flipped = false)
    val inf = f.createFrame("Drawing")

    val rect = new VisualCalendarTile(d, 50, 50, 100, "Rectangle")
    rect.toolTip = "Hello"

    inf.add(d)
    f.pack()

    Thread.sleep(1000)
    rect.innerCol = Color.red
    Thread.sleep(1000)
    rect.move(100, 20)
    for (_ <- 0 until 20) {
      Thread.sleep(50)
      rect.move(rect.x + 5, rect.y)
    }
  }
}
