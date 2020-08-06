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

import javax.swing.JPanel
import java.awt.BorderLayout

import org.jfree.data.xy.XYSeries
import org.jfree.chart.JFreeChart
import org.jfree.chart.ChartPanel
import org.jfree.data.xy.XYSeriesCollection
import java.awt.Color

import org.jfree.chart.plot.XYPlot
import org.jfree.chart.plot.ValueMarker
import javax.swing.SwingUtilities
import org.jfree.data

/**
 * @author Pierre Schaus
 */
abstract class Plot(title: String,
                    xlab: String,
                    ylab: String,
                    nbSeries: Int = 1) extends JPanel(new BorderLayout()) {

  var series: Array[XYSeries] = Array.tabulate(nbSeries)(i => new XYSeries(i))
  val xyDataset: XYSeriesCollection = new XYSeriesCollection()
  for (s <- series) {xyDataset.addSeries(s)}
  val chart: JFreeChart = createChart()
  chart.getPlot.setBackgroundPaint(Color.white)
  val panel: ChartPanel = new ChartPanel(chart)
  panel.setVisible(true)
  add(panel)

  val plot: XYPlot = chart.getPlot.asInstanceOf[XYPlot]

  val xMarker = new ValueMarker(0.5)
  val yMarker = new ValueMarker(0.5)

  hideHighlight()

  def highlight(x: Double, y: Double, col: Color = Color.LIGHT_GRAY): Unit = {
    SwingUtilities.invokeLater(() => {
      xMarker.setPaint(col)
      yMarker.setPaint(col)
      plot.addDomainMarker(xMarker)
      plot.addRangeMarker(yMarker)
      xMarker.setValue(x)
      yMarker.setValue(y)
      chart.fireChartChanged()
    })
  }

  def hideHighlight(): Boolean = {
    plot.removeDomainMarker(xMarker)
    plot.removeRangeMarker(yMarker)
  }

  def addPoint(x: Double, y: Double, ser: Int = 0): Unit = {
    series(ser).add(x, y)
    series(ser).fireSeriesChanged()

    //xyDataset.ss
    //chart.fireChartChanged();
  }

  def removeAllPoints(ser: Int = 0): Unit = {
    series(ser).clear()
  }

  def getPoints(ser: Int = 0): XYSeries = series(ser)

  def minMax(dom: Range): (Int, Int) = {
    val min = dom.start
    val max = if (dom.isInclusive) dom.end else dom.end - 1
    (min, max)
  }

  def xDom: data.Range = chart.getPlot.asInstanceOf[XYPlot].getDomainAxis.getRange

  def yDom: data.Range = chart.getPlot.asInstanceOf[XYPlot].getRangeAxis.getRange

  def xDom_=(dom: Range): Unit = {
    val (min, max) = minMax(dom)
    val xyPlot = chart.getPlot.asInstanceOf[XYPlot]
    xyPlot.getDomainAxis().setRange(min, max)
  }

  def yDom_=(dom: Range): Unit = {
    val (min, max) = minMax(dom)
    val xyPlot = chart.getPlot.asInstanceOf[XYPlot]
    xyPlot.getRangeAxis().setRange(min, max)
  }

  def createChart(): JFreeChart

}

