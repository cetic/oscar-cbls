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
package oscar.cbls.visual.obj

import javax.swing.JPanel
import java.awt.BorderLayout

import org.jfree.data.xy.XYSeries
import org.jfree.chart.{ChartFactory, ChartPanel, JFreeChart}
import org.jfree.data.xy.XYSeriesCollection
import java.awt.Color

import org.jfree.chart.plot.{PlotOrientation, ValueMarker, XYPlot}
import javax.swing.SwingUtilities

class LongPlotLine(title: String,
                   xlab: String,
                   ylab: String,
                   series: List[String] = List("default"))
  extends LongPlot(title,xlab,ylab, series) {

  def createChart(): JFreeChart = ChartFactory.createXYLineChart(title,xlab,ylab,xyDataset,PlotOrientation.VERTICAL,false,false, false)
}

abstract class LongPlot(title: String, xlab: String, ylab: String, series: List[String]) extends JPanel(new BorderLayout()) {

  var xySeries: List[XYSeries] = series.map(new XYSeries(_))

  val xyDataset: XYSeriesCollection = new XYSeriesCollection();
  for (s <- xySeries) {xyDataset.addSeries(s)}
  val chart: JFreeChart = createChart()
  chart.getPlot().setBackgroundPaint(Color.white);
  val panel: ChartPanel = new ChartPanel(chart);
  panel.setVisible(true);
  add(panel);

  val plot = chart.getPlot().asInstanceOf[XYPlot];

  val xMarker = new ValueMarker(0.5)
  val yMarker = new ValueMarker(0.5)

  hideHighlight()

  def highlight(x: Double, y: Double, col: Color = Color.LIGHT_GRAY): Unit = {
    SwingUtilities.invokeLater(new Runnable() {
      def run(): Unit = {
        xMarker.setPaint(col);
        yMarker.setPaint(col);
        plot.addDomainMarker(xMarker)
        plot.addRangeMarker(yMarker)
        xMarker.setValue(x)
        yMarker.setValue(y)
        chart.fireChartChanged()

      }
    })
  }

  def hideHighlight(): Boolean = {
    plot.removeDomainMarker(xMarker)
    plot.removeRangeMarker(yMarker)
  }

  def addPoint(x: Long, y: Long, ser: Int): Unit ={
    xySeries(ser).add(x,y,true)
  }

  def addPoint(x: Double, y: Long, ser: Int): Unit ={
    xySeries(ser).add(x,y,true)
  }

  def removeAllPoints(ser: Int = 0): Unit = {
    xySeries(ser).clear();
  }

  def getPoints(ser: Int = 0): XYSeries = xySeries(ser)

  def minMax(dom: org.jfree.data.Range): (Double,Double) = {
    val min = dom.getLowerBound
    val max = dom.getUpperBound
    (min, max)
  }

  def xDom:org.jfree.data.Range = chart.getPlot().asInstanceOf[XYPlot].getDomainAxis().getRange()

  def yDom:org.jfree.data.Range = chart.getPlot().asInstanceOf[XYPlot].getRangeAxis().getRange()

  def xDom_=(dom: org.jfree.data.Range): Unit = {
    val (min, max) = minMax(dom)
    val xyPlot = chart.getPlot().asInstanceOf[XYPlot]
    xyPlot.getDomainAxis().setRange(min, max)
  }

  def yDom_=(dom: org.jfree.data.Range): Unit = {
    val (min, max) = minMax(dom)
    val xyPlot = chart.getPlot().asInstanceOf[XYPlot]
    xyPlot.getRangeAxis().setRange(min, max)
  }

  def createChart(): JFreeChart

}
