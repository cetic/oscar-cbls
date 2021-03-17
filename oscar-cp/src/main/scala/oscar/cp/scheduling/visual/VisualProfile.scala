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
package oscar.cp.scheduling.visual
import java.awt.Color
import oscar.cp.core.variables.CPIntVar
import oscar.cp._
import oscar.visual.shapes.VisualPolygon
import oscar.visual.shapes.VisualLine
import oscar.visual.VisualDrawing
import oscar.algo.HeightProfile
import oscar.cp.core.CPSolver
import oscar.visual.VisualFrame


/**
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert
 */
class VisualProfile(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], 
                    demands: Array[CPIntVar], resourcesVar: Array[CPIntVar] ,
                    capa: Int, id: Int, color: Color = Color.WHITE) extends VisualDrawing(true, false) {
  
  def this(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], 
                    demands: Array[CPIntVar],capa: Int, color: Color) = this(starts,durations,ends,demands,Array.fill(starts.length)(CPIntVar(0)(starts(0).store)),capa,0,color)
  

  def compulsary(i: Int): Option[(Int,Int,Int)] = {
    if (resourcesVar(i).isBoundTo(id) && starts(i).max < ends(i).min) Some(starts(i).max,ends(i).min-starts(i).max,demands(i).min)
    else None
  }
  
  
  // The profile is represented by a polygon
  private val polygon: VisualPolygon = VisualPolygon(this)
  polygon.innerCol = color
  polygon.autoRepaint = false

  // The capacity limit
  private val capaLine: VisualLine = VisualLine(this, 0, 0, 0, 0)
  capaLine.outerCol = Color.RED

  // The zero line
  private val zeroLine: VisualLine = VisualLine(this, 0, 0, 0, 0)
  zeroLine.outerCol = Color.BLUE


  def update(xScale: Int, yScale: Int): Unit = {
    

    val rectangles = (for (i <- starts.indices; j <- compulsary(i)) yield j).toArray
    
    val profile = HeightProfile.computeProfile(rectangles)
    val start = profile(0)._1 
    val end = {
      val (s,d,_) = profile.last
      s+d
    }

    val points = Array((start,0)) ++ profile.flatMap{case(s,d,h) => Array((s,h),(s+d,h))} ++ Array((end,0))

    val minY = points.map(_._2).min
    val maxX = points.map(_._1).max

    polygon.update(points.map(p => (p._1 * xScale, (p._2 + minY) * yScale)))

    capaLine.orig = (0, (capa + minY) * yScale)
    capaLine.dest = (xScale * maxX, (capa + minY) * yScale)

    zeroLine.orig = (0, minY * yScale)
    zeroLine.dest = (xScale * maxX, minY * yScale)

    repaint()
  }
}

object VisualProfile extends App {
  implicit val cp = CPSolver()
  
  val s1 = CPIntVar(0)
  val d1 = CPIntVar(10)
  val e1 = s1+d1
  val h1 = CPIntVar(3)
  
  val s2 = CPIntVar(5)
  val d2 = CPIntVar(10)
  val e2 = s2+d2
  val h2 = CPIntVar(3)  
  
  val frame = new VisualFrame("Test Profile",1, 1)
  val f1 = frame.createFrame("profile")
  val vp = new VisualProfile(Array(s1,s2),Array(d1,d2),Array(e1,e2),Array(h1,h2),7,Color.CYAN)
  f1.add(vp)
  f1.pack()
  frame.pack()
  vp.update(100,100)
  
}
