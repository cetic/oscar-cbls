package oscar.visual.plot

import javax.swing.JPanel
import java.awt.BorderLayout
import org.jfree.data.xy.XYSeries
import org.jfree.data.xy.XYSeriesCollection
import org.jfree.chart.ChartFactory
import org.jfree.chart.plot.PlotOrientation
import java.awt.Color
import org.jfree.chart.ChartPanel
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import oscar.visual.VisualFrame

class ComparativePlot2D(title: String, xLabel: String, yLabel: String, seriesName: Array[String], lineVisible: Array[Boolean], pointVisible: Array[Boolean]) extends JPanel(new BorderLayout) {
  
  val nbSeries = seriesName.size
  val seriesTab: Array[XYSeries] = Array.tabulate(nbSeries)(i => new XYSeries(seriesName(i)))

  val xyDataSet: XYSeriesCollection = new XYSeriesCollection
  
  for (i <- 0 until nbSeries) {
    xyDataSet.addSeries(seriesTab(i))
  }
  
  val chart = ChartFactory.createXYLineChart(title, xLabel, yLabel, xyDataSet, PlotOrientation.VERTICAL, true, false, false)
  val plot = chart.getXYPlot
  plot.setBackgroundPaint(Color.white)
  val panel = new ChartPanel(chart)
  
  val renderer = new XYLineAndShapeRenderer
  for (i <- 0 until nbSeries) {
    renderer.setSeriesLinesVisible(i, lineVisible(i))
    renderer.setSeriesShapesVisible(i, pointVisible(i))
  }
  
  plot.setRenderer(renderer)
  panel.setVisible(true)
  add(panel)
  
  def addPoint(x: Double, y: Double, seriesIndex: Int): Unit = {
    seriesTab(seriesIndex).add(x, y)
    chart.fireChartChanged()
  }
}

object ComparativePlot2D {
  
  def apply(title: String, xLabel: String, yLabel: String, seriesName: Array[String], lineVisible: Array[Boolean], pointVisible: Array[Boolean]): ComparativePlot2D = {
    new ComparativePlot2D(title, xLabel, yLabel, seriesName, lineVisible, pointVisible)
  }
  
  def apply(title: String, xLabel: String, yLabel: String, seriesName: Array[String], lineVisible: Boolean = true, pointVisible: Boolean = true): ComparativePlot2D = {
    val lineVisibleArray = Array.fill(seriesName.size)(lineVisible)
    val pointVisibleArray = Array.fill(seriesName.size)(pointVisible)
    new ComparativePlot2D(title, xLabel, yLabel, seriesName, lineVisibleArray, pointVisibleArray)
  }
}

object ComparativePlot2DExample extends App {
  
  val frame = VisualFrame("Example")
  val inFrame = frame.createFrame("ComparativePlot2D")
  val plot = ComparativePlot2D("My Plot", "xlab", "ylab", Array("First", "Second", "Third"), Array(true, false, true), Array(true, true, false))
  inFrame.add(plot)
  inFrame.pack()
  
  for (i <- 0 until 10) {
    plot.addPoint(i, math.random(), 0)
    plot.addPoint(i, math.random(), 2)
    plot.addPoint(i, math.random(), 1)
    Thread.sleep(1000)
  }
}
