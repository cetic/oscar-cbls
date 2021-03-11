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


package oscar.visual

import java.awt.geom.Line2D
import java.awt.Color

import oscar.visual.shapes.VisualRectangle

/**
 * @author Pierre Schaus, pschaus@gmail.com
 */
class VisualGrid(nRow: Int, nCol: Int) extends VisualDrawing(false, true) {

  val sc = 100
  val board = Array.tabulate(nRow, nCol) { case (i, j) => new VisualRectangle(this, 50 + j * sc, 50 + i * sc, sc, sc) }
  
  def apply(i: Int) = board(i)

}


object VisualGrid extends App {

  val frame = VisualFrame("Example")
  val inFrame = frame.createFrame("Table")
  val table = new VisualGrid(800,800)
  inFrame.add(table)
  inFrame.pack
}
