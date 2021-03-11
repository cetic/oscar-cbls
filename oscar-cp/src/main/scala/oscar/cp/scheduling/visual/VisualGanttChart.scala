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

package oscar.cp.scheduling.visual

import oscar.visual._
import oscar.cp.scheduling._
import oscar.cp.core.variables.CPIntVar
import java.awt.Color
import oscar.visual.shapes.VisualText
import oscar.visual.shapes.VisualLine
import oscar.visual.shapes.VisualRectangle

class VisualGanttChart(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], f: (Int) => Int, colors: (Int) => Color = i => Color.WHITE) extends VisualDrawing(false, false) {

  private val rectangles: Array[VisualRectangle] = Array.tabulate(starts.length)(a => {
    val rect = new VisualRectangle(this, 0, 0, 0, 0)
    rect.innerCol = colors(a)
    rect
  })

  private val max = starts.indices.map(i => f(i)).max

  private val text: VisualText = new VisualText(this, 50, 50, "")
  text.innerCol = Color.RED
  text.centered = true

  private val makespanLine: VisualLine = VisualLine(this, 0, 0, 0, 0)
  makespanLine.outerCol = Color.RED

  def update(xScale: Double, yScale: Double): Unit = {

    for (i <- starts.indices) {

      rectangles(i).width = durations(i).max * xScale
      rectangles(i).height = yScale

      rectangles(i).move(starts(i).min * xScale, f(i) * yScale)

      rectangles(i).innerCol = colors(i)
    }

    val makespan = ends.map(_.max).max

    makespanLine.orig = (makespan * xScale, 0)
    makespanLine.dest = (makespan * xScale, (max + 1) * yScale)

    text.text = makespan.toString
    text.move(makespan * xScale, (max + 2) * yScale)
    repaint()
  }


  def printTkz() : Unit = {
    //\fill[blue!40!white] (0,0) rectangle (4,4);
    val scale = 25.0
    var i = 0
    for (r <- rectangles) {
      i += 1
      println(s"\\definecolor{mycolor$i}{RGB}{${r.innerCol.getRed},${r.innerCol.getGreen},${r.innerCol.getBlue}}")
      val out = s"\\draw [fill=mycolor$i] (${r.x/scale},${r.y/scale}) rectangle (${(r.x+r.width)/scale},${(r.y+r.height)/scale});"

      //val out = s"\\draw [fill={RGB:red,${r.innerCol.getRed};green,${r.innerCol.getGreen};blue,${r.innerCol.getBlue}}] (${r.x/scale},${r.y/scale}) rectangle (${(r.x+r.width)/scale},${(r.y+r.height)/scale});"
      println(out)
    }



    var out = s"\\draw [fill=red] (${makespanLine.orig._1/scale},${makespanLine.orig._2/scale-0.5}) rectangle (${makespanLine.dest._1/scale+0.1},${makespanLine.dest._2/scale-0.5});"
    println(out)

    out = s"\\draw[thick,->] (0,-1) -- (15,-1) node[anchor=north west] {time};"
    println(out)

  }
}
