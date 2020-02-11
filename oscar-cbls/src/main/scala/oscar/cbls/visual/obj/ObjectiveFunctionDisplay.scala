package oscar.cbls.visual.obj

import java.awt.Color

import org.jfree.chart.ChartFactory
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import oscar.cbls._
import oscar.cbls.util.StopWatch
import oscar.visual.plot.Plot

class ObjectiveFunctionDisplay(title: String)
  extends Plot(title,"Time","Objective function value", 2) with StopWatch {

  xDom = 0 to 1
  yDom = 0 to 100

  private var startTime = -1
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

    val at = if(startTime == -1){
      startTime = getWatch
      0
    }else{
      (getWatch - startTime).toDouble/1000
    }

    //TODO: this is a bit slow isnt'it?
    //TODO: this fails if the value is Long.MaxValue
    if(yDom.getUpperBound < value.toDouble)
      yDom = Range.inclusive(0L,upper(value))
    if(xDom.getUpperBound < at)
      xDom = Range.inclusive(0L,upper(at))

    addPoint(at,value.toDouble,0)

    if(value <= best) {
      best = value
    }
    addPoint(at, best.toDouble, 1)

  }

  private def upper(value: Double): Long ={
    var resExp = 10L
    while(value/resExp > 1) {
      resExp = resExp * 10
    }
    resExp = resExp/10
    var res = resExp
    while(value/res > 1){
      res += resExp
    }
    res
  }
}

object ObjectiveFunctionDisplay{
  def apply(title: String): ObjectiveFunctionDisplay = new ObjectiveFunctionDisplay(title)
}
