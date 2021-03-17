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

package oscar.dfo.visual

import oscar.visual._
import javax.swing.SwingUtilities
import oscar.dfo.utils._
import oscar.visual.plot.PlotScatter
import oscar.algo.paretofront.ParetoElement

/**
 * @author Cyrille Dejemeppe
 * @author Pierre Schaus
 */
class PlotDFOPareto2D[E <% Ordered[E]] (
  title: String,
  xlab: String, 
  ylab: String, 
  nbPareto: Int, 
  objMax1: Boolean, 
  objMax2: Boolean
) extends PlotScatter(title, xlab, ylab, nbPareto) {
  
  var currentIterate: Array[Double] = null

  def highLightIterate(iterate: Array[Double]): Unit = {
    currentIterate = iterate
    highlight(iterate(0).asInstanceOf[Double], iterate(1).asInstanceOf[Double])
  }

  def update(archive: Set[ParetoElement[E]], paretoIndex: Int = 0): Unit = {
    SwingUtilities.invokeLater(new Runnable() {
      def run(): Unit = {
        removeAllPoints(paretoIndex)
        if (currentIterate != null) {
          addPoint(currentIterate(0).asInstanceOf[Double], currentIterate(1).asInstanceOf[Double], paretoIndex)
          highLightIterate(currentIterate)
        }
        for (p <- archive) {
          val x = p.objectives(0)
          val y = p.objectives(1)
          addPoint(x, y, paretoIndex)
        }
      }
    })
  }
}

object PlotDFOPareto2D {
  def apply[E <% Ordered[E]] (
    title: String = "Pareto",
    xlab: String = "Obj1",
    ylab: String = "Obj2",
    nbPareto: Int = 1,
    objMax1: Boolean = false, // true if obj1 is a maximization objective
    objMax2: Boolean = false // true if obj2 is a maximization objective
  ) = new PlotDFOPareto2D[E](title, xlab, ylab, nbPareto, objMax1, objMax2)
}
