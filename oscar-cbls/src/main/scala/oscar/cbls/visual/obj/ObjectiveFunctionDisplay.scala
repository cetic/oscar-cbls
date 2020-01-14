package oscar.cbls.visual.obj

import java.awt.Color

import org.jfree.chart.ChartFactory
import org.jfree.chart.plot.{PlotOrientation, ValueMarker}
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import oscar.cbls._
import oscar.cbls.algo.search.KSmallest
import oscar.cbls.util.StopWatch

class ObjectiveFunctionDisplay(title: String, cap:Long = Long.MaxValue, percentile: Option[Double] = None)
  extends LongPlot(title,"Time","Objective function value", 2) with StopWatch {

  xDom = new org.jfree.data.Range(0,1)
  yDom = new org.jfree.data.Range(0,100)

  private var allValues: Array[(Double,Long)] = Array.empty
  private val considerPercentile = percentile.isDefined

  private lazy val startingAt = getWatch
  private var best = Long.MaxValue
  private var decreasing = false
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
    val boundsValue =
      if(considerPercentile) {
        allValues = allValues :+ (at,value)
        percentileBounds()
      } else {
        (value, 0.0)
      }


    if(value < best) {
      if(!decreasing) {
        addStartDecreasingMark(at)
        decreasing = true
      }
      best = value
    } else {
      decreasing = false
    }



    // Update Y axis height
    yDom = new org.jfree.data.Range(lower(best),
      upper(boundsValue._1, cap))

    // Update X axis length
    if(xDom.getUpperBound < at)
      xDom = new org.jfree.data.Range(
        boundsValue._2.toLong,
        at.toLong + 1)

    addPoint(at,value,0)


    addPoint(at, best, 1)

  }

  private def addStartDecreasingMark(at: Double): Unit ={
    val marker = new ValueMarker(at)
    marker.setPaint(Color.GREEN)
    plot.addDomainMarker(marker)
  }

  private def percentileBounds(): (Long, Double) ={
    val nbValues = allValues.size
    val array = Array.tabulate(nbValues)(x => x.toLong)
    val valuesToConsider = KSmallest.kFirst(
      (nbValues/100.0*percentile.get).ceil.toLong,
      KSmallest.lazySort(array, key = i => {
        allValues(i)._2
      }))
    val last = valuesToConsider.last
    (allValues(last)._2, allValues(last)._1)
  }

  /**
   * Ex :
   * 38427 ==> 39000
   * 38000 ==> 39000
   * 20 ==> 19
   * 5 ==> 0
   */
  private def upper(value: Long, cap:Long = Long.MaxValue): Long ={
    var resExp:Long = 10
    while(value/resExp > 1) {
      resExp = resExp * 10
    }
    resExp = if(resExp <= 100) resExp/10 else resExp/100
    val res = ((value/resExp)+1)*resExp
    res min cap
  }

  /**
   * Ex :
   * 38427 ==> 38000
   * 38000 ==> 37000
   * 20 ==> 19
   * 5 ==> 0
   */
  private def lower(value: Long): Long ={
    var resExp: Long = 1
    while(value/resExp > 0) resExp = resExp * 10
    resExp = resExp/100
    if(resExp <= 0) return 0
    if(value%resExp > 0) (value/resExp)*resExp
    else ((value/resExp)+1)*resExp
  }

}

object ObjectiveFunctionDisplay{
  def apply(title: String, cap:Long = Long.MaxValue, percentile: Option[Double] = None): ObjectiveFunctionDisplay =
    new ObjectiveFunctionDisplay(title,cap, percentile)
}
