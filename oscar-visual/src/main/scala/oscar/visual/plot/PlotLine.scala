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
package oscar.visual.plot
import org.jfree.chart.{ChartFactory, JFreeChart}
import org.jfree.chart.plot.PlotOrientation
import oscar.visual.VisualFrame


/**
 * @author Pierre Schaus
 */
class PlotLine(title: String, xlab: String, ylab: String, nbSeries: Int = 1) extends Plot(title,xlab,ylab, nbSeries) {
	def createChart(): JFreeChart = ChartFactory.createXYLineChart(title,xlab,ylab,xyDataset,PlotOrientation.VERTICAL,false,false, false)
}

object PlotLine extends App {
		val f = VisualFrame("toto")
		val inf = f.createFrame("Drawing")
		val myplot = new PlotLine("My Line Plot","xlab","ylab",2)
		inf.add(myplot)
		inf.pack()
		
		myplot.xDom = 0 to 10
		myplot.yDom = 0 to 1
		
		for (i <- 0 until 10) {
			myplot.addPoint(i, Math.random())
			Thread.sleep(500)
		} 
		// add points on second series
		for (i <- 0 until 10) {
			myplot.addPoint(i, Math.random(),1)
			Thread.sleep(500)
		}
		
}
