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

package oscar.cp.examples.scheduling

import oscar.cp._

import oscar.visual._
import scala.math
import java.awt.Color
import oscar.cp.constraints._
import oscar.visual.shapes.VisualRectangle

/**
 * Perfect Square Problem
 *
 *  The problem is to fully cover a big 112 square with
 *  21 different smaller squares with no overlap between squares.
 *
 *  @author Pierre Schaus  pschaus@gmail.com
 *  @author Renaud Hartert ren.hartert@gmail.com
 */
object PerfectSquare extends CPModel with App {

  val s = 112
  val side = Array(50, 42, 37, 35, 33, 29, 27, 25, 24, 19, 18, 17, 16, 15, 11, 9, 8, 7, 6, 4, 2)

  val nSquare = side.size
  val Square = 0 until nSquare

  val durationsX = Array.tabulate(nSquare)(t => CPIntVar(side(t)))
  val startsX = Array.tabulate(nSquare)(t => CPIntVar(0 to s - side(t)))
  val endsX = Array.tabulate(nSquare)(t => CPIntVar(side(t) to s))
  val demandsX = Array.tabulate(nSquare)(t => CPIntVar(side(t)))
  val resourcesX = Array.fill(nSquare)(CPIntVar(0))

  val durationsY = Array.tabulate(nSquare)(t => CPIntVar(side(t)))
  val startsY = Array.tabulate(nSquare)(t => CPIntVar(0 to s - side(t)))
  val endsY = Array.tabulate(nSquare)(t => CPIntVar(side(t) to s))
  val demandsY = Array.tabulate(nSquare)(t => CPIntVar(side(t)))
  val resourcesY = Array.fill(nSquare)(CPIntVar(1))

  onSolution {
    // Visualization
    val f = VisualFrame("Pefect Square")
    val ff = f.createFrame("Square")
    val d = VisualDrawing(false)
    ff.add(d)
    def scale = 5
    val bg = new VisualRectangle(d, 0, 0, s * scale, s * scale)
    bg.innerCol = Color.black
    (Square).foreach { i =>
      val r = new VisualRectangle(d, startsX(i).value * scale, startsY(i).value * scale, side(i) * scale, side(i) * scale)
      r.innerCol = VisualUtil.getRandomColor
    }
  }

  // Consistency
  for (t <- Square) {
    add(endsX(t) === startsX(t) + durationsX(t))
    add(endsY(t) === startsY(t) + durationsY(t))
  }

  // Cumulative
  add(maxCumulativeResource(startsX, durationsX, endsX, demandsX, resourcesX, CPIntVar(s), 0))
  add(minCumulativeResource(startsX, durationsX, endsX, demandsX, resourcesX, CPIntVar(s), 0))
  add(maxCumulativeResource(startsY, durationsY, endsY, demandsY, resourcesY, CPIntVar(s), 1))
  add(minCumulativeResource(startsY, durationsY, endsY, demandsY, resourcesY, CPIntVar(s), 1))

  // Overlapping
  for (i <- 0 until nSquare; j <- i + 1 until nSquare) {
    add((endsX(i) ?<= startsX(j)) || (endsX(j) ?<= startsX(i)) || (endsY(i) ?<= startsY(j)) || (endsY(j) ?<= startsY(i)))
  }

  import oscar.util._
  import oscar.algo.search._

  def myCustomBranching(w: Array[CPIntVar]) = Branching {
    // Minimum x position
    selectMin(w)(x => !x.isBound)(_.min) match {
      case None => noAlternative
      case Some(z) => {
        val v = z.min
        branch(post(z === v))(post(z !== v))
      }
    }
  }
  
  search { myCustomBranching(startsX) ++ myCustomBranching(startsY) }
  println(start(1))
}

