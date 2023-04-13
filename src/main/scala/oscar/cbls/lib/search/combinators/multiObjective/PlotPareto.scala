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

package oscar.cbls.lib.search.combinators.multiObjective

import oscar.visual.plot.PlotScatter

import javax.swing.SwingUtilities

/**
 * @author Pierre Schaus
 */
class PlotPareto(
                  title: String = "Pareto",
                  xlab: String = "Obj1",
                  ylab: String = "Obj2",
                ) extends  PlotScatter(title, xlab, ylab, 2){

  def reDrawPareto(paretoPoints:Iterable[(Long,Long)],
                   oldParetoPoints:Option[Iterable[(Long,Long)]] = None): Unit ={

    SwingUtilities.invokeLater(new Runnable() {
      def run(): Unit = {

        removeAllPoints(0)
        removeAllPoints(1)

        oldParetoPoints match{
          case None => ;
          case Some(old) =>
            for (p <- old) {
              addPoint(p._1.toDouble, p._2.toDouble, 1)
            }
        }

        for (p <- paretoPoints) {
          addPoint(p._1.toDouble, p._2.toDouble, 0)
        }
      }
    })
  }
}
