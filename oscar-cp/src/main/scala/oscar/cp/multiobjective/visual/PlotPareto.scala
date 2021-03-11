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


package oscar.cp.multiobjective.visual

import oscar.visual._
import javax.swing.SwingUtilities
import oscar.cp.multiobjective.ListPareto
import oscar.visual.plot.PlotScatter

/**
 * @author Pierre Schaus
 */
class PlotPareto(
  title: String = "Pareto",
  xlab: String = "Obj1",
  ylab: String = "Obj2",
  nbPareto: Int = 1,
  objMax1: Boolean = false, // true if obj1 is a maximization objective
  objMax2: Boolean = false  // true if obj2 is a maximization objective
) extends PlotScatter(title, xlab, ylab, nbPareto) {

  val paretos = Array.fill(nbPareto)(new ListPareto[Object](Array(objMax1, objMax2)))
 
  def insert(obj1: Int, obj2: Int, paretoIdx: Int = 0): Unit = {
    paretos(paretoIdx).insert(null, obj1, obj2)
    SwingUtilities.invokeLater(new Runnable() {
      def run(): Unit = {
        removeAllPoints(paretoIdx)
        for (p <- paretos(paretoIdx).objectiveSols) {
          val x = p(0)
          val y = p(1)
          addPoint(x, y, paretoIdx)
        }
      }
    })
  }

}
