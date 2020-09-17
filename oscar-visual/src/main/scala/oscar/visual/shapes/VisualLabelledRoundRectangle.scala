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

import java.awt.Color
import java.awt.geom.RoundRectangle2D
import oscar.visual.VisualDrawing
import oscar.visual.VisualFrame

/**
 *
 * @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
 *
 */
class VisualLabelledRoundRectangle(d: VisualDrawing,
                                   s: RoundRectangle2D.Double,
                                   label: String,
                                   _marginWidth: Double = 0) extends VisualRoundRectangle(d, s) {

  def rect: RoundRectangle2D.Double = shape
  var marginWidth: Double = _marginWidth
  val textDraw = new VisualText(d, (x + marginWidth).toInt, (y + marginWidth + d.getFontMetrics(d.getFont).getHeight).toInt, label)
  shape.height = (textDraw.nLines * d.getFontMetrics(d.getFont).getHeight) + marginWidth * 2
  shape.width = getWidth(label)
  
  textDraw.move(xText, yText)

  def this(d: VisualDrawing, x: Double, y: Double, label: String, marginWidth: Double, arcw: Double, arch: Double) = {
    this(d, new RoundRectangle2D.Double(x,
        y,
        d.getFontMetrics(d.getFont).stringWidth(label) + marginWidth * 2,
        d.getFontMetrics(d.getFont).getHeight + marginWidth * 2,
        arcw,
        arch),
      label,
      marginWidth)
  }
  
  def this(d: VisualDrawing, x: Double, y: Double, label: String, marginWidth: Double) = this(d,x,y,label,marginWidth,7,7)
  
  def this(d: VisualDrawing, x: Double, y: Double, label: String) = this(d,x,y,label,5,7,7)

  /**
   * X coordinates of bottom left corner
   * @return
   */
  def xText: Int = (x + marginWidth).toInt

  /**
   * Y coordinates of bottom left corner
   * @return
   */
  def yText: Int = (y + marginWidth + d.getFontMetrics(d.getFont).getHeight).toInt

  /**
   * Move the specified left corner
   * @param x X coordinate
   * @param y Y coordinate
   */
  override def move(x: Double, y: Double): Unit = {
    super.move(x, y)
    textDraw.move(xText, yText)
  }

  def getWidth(newLabel: String): Double = {
    newLabel.trim.split("\n").map(l => d.getFontMetrics(d.getFont).stringWidth(l)).max + marginWidth * 2
  }
}

object VisualLabelledRoundRectangle {

  def main(args: Array[String]): Unit = {
    val f = VisualFrame("toto")
    val d = VisualDrawing(flipped=false)
    val inf = f.createFrame("Drawing")

    val rect = new VisualLabelledRoundRectangle(d, 50, 50, "I'm a rectangle.\nJust a rectangle...", marginWidth=10)
    val _ = VisualLine(d, 50, 25, 50, 150)
    rect.toolTip = "Hello"

    inf.add(d)
    f.pack()

    Thread.sleep(1000)
    rect.innerCol = Color.red
    rect.textDraw.innerCol = Color.BLUE
    Thread.sleep(1000)
    rect.move(100, 20)
    for (_ <- 0 until 20) {
      Thread.sleep(50)
      rect.move(rect.x + 5, rect.y)
    }
  }
}
