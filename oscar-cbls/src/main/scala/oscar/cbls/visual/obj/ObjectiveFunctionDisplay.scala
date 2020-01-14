package oscar.cbls.visual.obj

import java.awt.Color
import java.awt.event.{MouseWheelEvent, MouseWheelListener}

import org.jfree.chart.{ChartFactory, JFreeChart}
import org.jfree.chart.plot.{PlotOrientation, ValueMarker}
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import oscar.cbls._
import oscar.cbls.algo.search.KSmallest
import oscar.cbls.util.StopWatch

class ObjectiveFunctionDisplay(title: String, cap:Long = Long.MaxValue, defaultPercentile: Int)
  extends LongPlot(title,"Time","Objective function value", 2) with StopWatch {

  private lazy val startingAt = getWatch
  private var lastValueAt = 0.0

  private var allValues: Array[(Double,Long)] = Array.empty
  private var best = Long.MaxValue
  private var decreasing = false
  private var percentile = defaultPercentile

  xDom = new org.jfree.data.Range(0,1)
  yDom = new org.jfree.data.Range(0,100)

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
  // Custom Mouse wheel listener (increasing / decreasing percentile)
  panel.getMouseWheelListeners.foreach(mwl => panel.removeMouseWheelListener(mwl))
  panel.addMouseWheelListener(new MouseWheelListener {
    override def mouseWheelMoved(e: MouseWheelEvent): Unit = {
      if(e.getWheelRotation < 0){
        percentile = Math.max(1, percentile-1)
      } else {
        percentile = Math.min(100, percentile+1)
      }
      reDrawFunction()
    }
  })

  def createChart(): JFreeChart =
    ChartFactory.createXYLineChart(
      null,
      null,
      null,
      xyDataset,PlotOrientation.VERTICAL,
      false,
      false,
      false)

  def reDrawFunction(): Unit ={
    val (maxY, minX) = percentileBounds()
    val minY = lower(best)
    val maxX = Math.max(xDom.getUpperBound, lastValueAt.toLong + 1)

    // Update Y axis height
    yDom = new org.jfree.data.Range(minY,upper(maxY, cap))

    // Update X axis length
    xDom = new org.jfree.data.Range(minX,maxX)
  }

  def drawFunction(value: Long): Unit ={

    lastValueAt = (getWatch - startingAt)/1000.0
    allValues = allValues :+ (lastValueAt,value)

    // Update best value
    if(value <= best) {
      if(!decreasing) addStartDecreasingMark(lastValueAt)
      decreasing = true
      best = value
    } else {
      decreasing = false
    }

    addPoint(lastValueAt,value,0)
    addPoint(lastValueAt, best, 1)

    reDrawFunction()
  }

  private def addStartDecreasingMark(at: Double): Unit ={
    val marker = new ValueMarker(at)
    marker.setPaint(Color.GREEN)
    plot.addDomainMarker(marker)
  }

  /**
   * This method return the minX and maxY bounds based on the percentile value.
   * The lower bounds are determined like this :
   *  - minX = The earliest acceptable value (taking his time information)
   *  - minY = The best value (taking his value)
   *  - maxX = The last encountered value (taking his time information)
   *  - maxY = The worst acceptable value (taking his value)
   *
   * If percentile >= 100 ==> it means that we display all the data.
   * If percentile == 50 ==> it means that we display 50% of the data.
   * If percentile == 0 ==> it means that we display 0% of the data.
   */
  private def percentileBounds(): (Long, Double) ={
    val nbValues = allValues.length
    val array = Array.tabulate(nbValues)(x => x.toLong)
    val valuesToConsider = KSmallest.kFirst(
      (nbValues/100.0*percentile).ceil.toLong,
      KSmallest.lazySort(array, key = i => {
        allValues(i)._2
      }))
    val worstAcceptableValue = valuesToConsider.last
    // The first (in time) acceptable value could be before the worstAcceptableValue
    val earliestAcceptableValue = valuesToConsider.minBy(allValues(_)._1)
    (allValues(worstAcceptableValue)._2, allValues(earliestAcceptableValue)._1)
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
  def apply(title: String, cap:Long = Long.MaxValue, percentile: Int = 100): ObjectiveFunctionDisplay =
    new ObjectiveFunctionDisplay(title,cap, percentile)
}
