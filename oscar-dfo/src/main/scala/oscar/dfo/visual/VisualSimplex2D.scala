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
import java.awt.Color
import oscar.dfo.utils.MOOPoint
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.ChartPanel
import org.jfree.chart.plot.ValueMarker
import org.jfree.chart.JFreeChart
import javax.swing.SwingUtilities
import org.jfree.data.xy.XYSeriesCollection
import javax.swing.JPanel
import org.jfree.data.xy.XYSeries
import java.awt.BorderLayout
import org.jfree.chart.ChartFactory
import org.jfree.chart.plot.PlotOrientation
import java.awt.geom.Ellipse2D
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.chart.annotations.XYTextAnnotation

class VisualSimplex2D(simplex: Array[MOOPoint], inputSpace: Boolean = false) extends JPanel(new BorderLayout()) {

  val textDistFactor = 10.0
  val originalSerie = new XYSeries(0, false)
  val transformSeries:  Array[XYSeries] = Array.tabulate(4)(i => new XYSeries(i + 1, false))
  val xyDataset: XYSeriesCollection = new XYSeriesCollection()
  val lineRenderer = new XYLineAndShapeRenderer(true, true)
  xyDataset.addSeries(originalSerie)
  transformSeries.foreach(xyDataset.addSeries(_))
  val chart: JFreeChart = createChart()
  chart.getPlot().setBackgroundPaint(Color.white);
  val panel: ChartPanel = new ChartPanel(chart);
  panel.setVisible(true)
  add(panel)
  
  val plot = chart.getPlot().asInstanceOf[XYPlot]

  val xMarker = new ValueMarker(0.5)
  val yMarker = new ValueMarker(0.5)
  
  val offsetMultiplier = 2.2
  
  initGraph
  
  def initGraph: Unit = {
    initPoints
    setRanges
    hideHighlight()
  }
  
  def initPoints = {
    for (i <- 0 to 4) plot.setRenderer(i, lineRenderer)
    val circleShape = new Ellipse2D.Double(-3, -3, 6, 6)
    for (i <- 0 to 4) lineRenderer.setSeriesShape(i, circleShape)
    for (i <- 1 to 4) lineRenderer.setSeriesPaint(i, Color.red)
    lineRenderer.setSeriesPaint(0, Color.blue)
    for (i <- 0 until simplex.length) {
      val (x, y) = {
        if (inputSpace) (simplex(i).coordinates(0), simplex(i).coordinates(1))
        else (simplex(i).getEvaluation(0), simplex(i).getEvaluation(1))
      }
      originalSerie.add(x, y)
    }
    for (i <- 0 until simplex.length){
      val (x, y) = {
        if (inputSpace) (simplex(i).coordinates(0), simplex(i).coordinates(1))
        else (simplex(i).getEvaluation(0), simplex(i).getEvaluation(1))
      }
      val xyTextAnnotation = new XYTextAnnotation("y" + i, x + deltaX(x), y + deltaY(y))
      lineRenderer.addAnnotation(xyTextAnnotation)
    }
    
    if (inputSpace) originalSerie.add(simplex(0).coordinates(0), simplex(0).coordinates(1))
    else originalSerie.add(simplex(0).getEvaluation(0), simplex(0).getEvaluation(1))
  }
  
  def setRanges = {
    val xDist = originalSerie.getMaxX() - originalSerie.getMinX()
    val yDist = originalSerie.getMaxY() - originalSerie.getMinY()
    val xyPlot = chart.getPlot().asInstanceOf[XYPlot];
    xyPlot.getDomainAxis().setRange(originalSerie.getMinX() - offsetMultiplier * xDist, originalSerie.getMaxX() + offsetMultiplier * xDist);
    xyPlot.getRangeAxis().setRange(originalSerie.getMinY() - offsetMultiplier * yDist, originalSerie.getMaxY() + offsetMultiplier * yDist);
  }
  
  def deltaX(x: Double, serieIndex: Int = 0) = {
    val serie = if (serieIndex == 0) originalSerie else transformSeries(serieIndex - 1)
    if (x == serie.getMinX()) (-origXDist / textDistFactor)
    else if (x == serie.getMaxX()) (origXDist / textDistFactor)
    else 0.0
  }
  
  def deltaY(y: Double, serieIndex: Int = 0) = {
    val serie = if (serieIndex == 0) originalSerie else transformSeries(serieIndex - 1)
    println("y: " + y)
    println("serie.getMinY(): " + serie.getMinY())
    println("serie.getMaxY(): " + serie.getMaxY())
    if (y == serie.getMinY()) (-origYDist / textDistFactor)
    else if (y == serie.getMaxY()) (origYDist / textDistFactor)
    else 0.0
  }
  
  def origXDist = originalSerie.getMaxX() - originalSerie.getMinX()
  def origYDist = originalSerie.getMaxY() - originalSerie.getMinY()
  
  def showReflexion(simplex: Array[MOOPoint], newPoint: MOOPoint) = showSingleTransform(simplex, newPoint, "yr", 1)
  def showExpansion(simplex: Array[MOOPoint], newPoint: MOOPoint) = showSingleTransform(simplex, newPoint, "ye", 2)
  def showInsideContraction(simplex: Array[MOOPoint], newPoint: MOOPoint) = showSingleTransform(simplex, newPoint, "yic", 3)
  def showOutsideContraction(simplex: Array[MOOPoint], newPoint: MOOPoint) = showSingleTransform(simplex, newPoint, "yoc", 3)
  
  def showSingleTransform(simplex: Array[MOOPoint], newPoint: MOOPoint, label: String, serieIndex: Int): Unit = {
    if (inputSpace) {
      transformSeries(serieIndex - 1).add(simplex(0).coordinates(0), simplex(0).coordinates(1))
      transformSeries(serieIndex - 1).add(newPoint.coordinates(0), newPoint.coordinates(1))
      transformSeries(serieIndex - 1).add(simplex(1).coordinates(0), simplex(1).coordinates(1))
      val xyTextAnnotation = new XYTextAnnotation(label, newPoint.coordinates(0) + deltaX(newPoint.coordinates(0), serieIndex), newPoint.coordinates(1) + deltaY(newPoint.coordinates(1), serieIndex))
      lineRenderer.addAnnotation(xyTextAnnotation)
      println("deltaY(newPoint.coordinates(1), serieIndex - 1): " + deltaY(newPoint.coordinates(1), serieIndex - 1))
      println("origYDist: " + origYDist)
    }
    else {
      transformSeries(serieIndex- 1).add(simplex(0).evaluations(0), simplex(0).evaluations(1))
      transformSeries(serieIndex- 1).add(newPoint.evaluations(0), newPoint.evaluations(1))
      transformSeries(serieIndex- 1).add(simplex(1).evaluations(0), simplex(1).evaluations(1))
      val xyTextAnnotation = new XYTextAnnotation(label, newPoint.evaluations(0) + deltaX(newPoint.evaluations(0), serieIndex), newPoint.evaluations(1) + deltaY(newPoint.evaluations(1), serieIndex))
      lineRenderer.addAnnotation(xyTextAnnotation)
    }
  }
  
  def highlight(x: Double, y: Double, col: Color = Color.LIGHT_GRAY) = {
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

  def hideHighlight() = {
    plot.removeDomainMarker(xMarker)
    plot.removeRangeMarker(yMarker)
  }

  def removeAllPoints(): Unit = {
    originalSerie.clear()
    for (i <- 0 until 4) transformSeries(i).clear()
  }

  def getPoints: XYSeries = originalSerie

  def createChart(): JFreeChart = {
    if (inputSpace) ChartFactory.createScatterPlot("Input Simplex", "x", "y", xyDataset, PlotOrientation.VERTICAL,false,false, false)
    else ChartFactory.createScatterPlot("Output Simplex", "f1", "f2", xyDataset, PlotOrientation.VERTICAL,false,false, false)
  }

}

object VisualSimplex2D extends App {
  val waitingDelay = 2000
  val f = VisualFrame("toto");
  val inf1 = f.createFrame("Simplex Input")
  val inf2 = f.createFrame("Simplex Output")
  val simplex = Array(MOOPoint(Array(10.0, 10.0), Array(1.0, 1.0)), MOOPoint(Array(42.0, 10.0), Array(1.0, 2.0)), MOOPoint(Array(10.0, 42.0), Array(2.0, 1.0)))
	
  val myInputSimplexPlot = new VisualSimplex2D(simplex, true)
  val myOutputSimplexPlot = new VisualSimplex2D(simplex, false)
  inf1.add(myInputSimplexPlot);
  inf2.add(myOutputSimplexPlot);
  inf1.pack()
  inf2.pack()
  
  Thread.sleep(waitingDelay)
  val reflectedPoint = MOOPoint(Array(42.0, -22.0), Array(3.0, 3.0))
  myInputSimplexPlot.showReflexion(simplex, reflectedPoint)
  myOutputSimplexPlot.showReflexion(simplex, reflectedPoint)
  
  Thread.sleep(waitingDelay)
  val expandedPoint = MOOPoint(Array(58.0, -54.0), Array(4.0, 2.0))
  myInputSimplexPlot.showExpansion(simplex, expandedPoint)
  myOutputSimplexPlot.showExpansion(simplex, expandedPoint)
  
  Thread.sleep(waitingDelay)
  val contractedInsidePoint = MOOPoint(Array(18.0, 26.0), Array(2.0, 4.0))
  myInputSimplexPlot.showInsideContraction(simplex, contractedInsidePoint)
  myOutputSimplexPlot.showInsideContraction(simplex, contractedInsidePoint)
  
  Thread.sleep(waitingDelay)
  val contractedOutsidePoint = MOOPoint(Array(34.0, -6.0), Array(3.0, -5.0))
  myInputSimplexPlot.showOutsideContraction(simplex, contractedOutsidePoint)
  myOutputSimplexPlot.showOutsideContraction(simplex, contractedOutsidePoint)
}
