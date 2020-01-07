package oscar.cbls.visual.obj

import java.awt.Color

import org.jfree.chart.ChartFactory
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import oscar.cbls._
import oscar.cbls.algo.search.KSmallest
import oscar.cbls.util.StopWatch
import oscar.visual.plot.Plot

class ObjectiveFunctionDisplay(title: String, cap:Long = Long.MaxValue, percentile: Option[Double] = None)
  extends LongPlot(title,"Time","Objective function value", 2) with StopWatch {

  xDom = new org.jfree.data.Range(0,1)
  yDom = new org.jfree.data.Range(0,100)

  var allValues: Array[Long] = Array.empty

  private lazy val startingAt = getWatch
  private var best = Long.MaxValue
  panel.setMouseWheelEnabled(true)
  panel.setMouseZoomable(true)
  panel.setHorizontalAxisTrace(true)
  panel.setVerticalAxisTrace(true)

  val r1 = new XYLineAndShapeRenderer
  r1.setSeriesPaint(1, Color.GREEN)
  r1.setSeriesPaint(0, Color.RED)
  r1.setSeriesShapesVisible(0, false)
  r1.setSeriesShapesVisible(1, false)

  plot.setRenderer(r1)

  this.setDoubleBuffered(true)
  panel.setDoubleBuffered(true)

  def createChart =
    ChartFactory.createXYLineChart(
      null,
      null,
      null,
      xyDataset,PlotOrientation.VERTICAL,
      false,
      false,
      false)

  def drawFunction(value: Long) ={

    val at = (getWatch - startingAt)/1000.0
    allValues = allValues :+ value

    // Update Y axis height
    yDom = new org.jfree.data.Range(0,upper(
      if(percentile.isDefined) percentileBound() else value,
      cap))

    // Update X axis length
    if(xDom.getUpperBound < at)
      xDom = new org.jfree.data.Range(0,upper(at.toLong))

    addPoint(at,value,0)

    if(value <= best) {
      best = value
    }
    addPoint(at, best, 1)

  }

  private def percentileBound(): Long ={
    val nbValues = allValues.size
    val valuesToConsider = KSmallest.kFirst((nbValues/100.0*percentile.get).ceil.toLong, KSmallest.lazySort(allValues, key = x => x))
    valuesToConsider.last
  }

  private def upper(value: Long, cap:Long = Long.MaxValue): Long ={
    var resExp:Long = 10
    while(value/resExp > 1) {
      resExp = resExp * 10
    }
    resExp = resExp/10
    var res = resExp
    while(value/res >= 1){
      res += resExp
    }
    res min cap
  }

}

object ObjectiveFunctionDisplay{
  def apply(title: String, cap:Long = Long.MaxValue, percentile: Option[Double] = None): ObjectiveFunctionDisplay =
    new ObjectiveFunctionDisplay(title,cap, percentile)
}
