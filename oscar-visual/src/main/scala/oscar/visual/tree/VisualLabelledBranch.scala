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
package oscar.visual.tree

import java.awt.geom.Line2D
import oscar.visual.shapes.VisualText
import oscar.visual.shapes.VisualLine
import oscar.visual.VisualDrawing
import oscar.visual.VisualFrame

/**
 * @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
 */
class VisualLabelledBranch(d: VisualDrawing, shapes: (VisualLine, VisualLine, VisualLine, VisualText)) {
  
  private val dist = 4
  def branch: (VisualLine, VisualLine, VisualLine, VisualText) = shapes
  
  def this(d: VisualDrawing, xorig: Double, yorig: Double, xdest: Double, ydest: Double, label: String) = {
    this(d, (new VisualLine(d, new Line2D.Double(xorig, yorig, xorig, (ydest + 2 * yorig) / 3)),
	    new VisualLine(d, new Line2D.Double(xorig, (ydest + 2 * yorig) / 3, xdest, (ydest + 2 * yorig) / 3)),
	    new VisualLine(d, new Line2D.Double(xdest, (ydest + 2 * yorig) / 3, xdest, ydest)),
	    new VisualText(d, (4 + xdest).toInt, (d.getFontMetrics(d.getFont).getHeight / 2 + (ydest + 2 * (ydest + 2 * yorig) / 3) / 3).toInt, label)))
  }
  
  /**
	* Moves to the specified coordinates
	*/
  def move(xorig: Double, yorig: Double, xdest: Double, ydest: Double): Unit = {
    branch._1.orig_=(xorig, yorig)
    branch._1.dest_=(xorig, (ydest + 2 * yorig) / 3)
    branch._2.orig_=(xorig, (ydest + 2 * yorig) / 3)
    branch._2.dest_=(xdest, (ydest + 2 * yorig) / 3)
    branch._3.orig_=(xdest, (ydest + 2 * yorig) / 3)
    branch._3.dest_=(xdest, ydest)
    branch._4.move(xText, yText)
  }
  
  /**
    * X coordinates of bottom left corner
	* @return
	*/
  def xText: Int = (dist + branch._3.orig._1).toInt
  
  /**
	* Y coordinates of bottom left corner
	* @return
	*/
  def yText: Int = (d.getFontMetrics(d.getFont).getHeight / 2 + (branch._3.dest._2 + 2 * branch._3.orig._2) / 3).toInt
}

object VisualLabelledBranch {
  	
  def main(args : Array[String]): Unit = {
    val f = VisualFrame("toto")
    val d = VisualDrawing(flipped=false)
    val inf = f.createFrame("Drawing")
    inf.add(d)
    f.pack()

    val line = new VisualLabelledBranch(d, 200, 200, 300, 300, "I'm a branch! Yeepee^^")
    Thread.sleep(2000)
    line.move(200, 300, 100, 200)
  }
}
