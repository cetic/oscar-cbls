package oscar.cbls.visual.obj

import java.awt.Color
import java.awt.event.{MouseWheelEvent, MouseWheelListener}

import org.jfree.chart.{ChartFactory, JFreeChart}
import org.jfree.chart.plot.{PlotOrientation, ValueMarker}
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import oscar.cbls._
import oscar.cbls.algo.search.KSmallest
import oscar.cbls.util.StopWatch
import oscar.cbls.visual.ColorGenerator

class ObjectiveFunctionDisplay(title: String, minCap:Long, maxCap:Long, basePercentile: Int, nbOtherValues: Int)
  extends LongPlot(title,"Time","Objective function value", nbOtherValues + 2) with StopWatch {

  require(minCap < maxCap, "Min cap should be lesser thant max cap. Got minCap : " + minCap + ", maxCap : " + maxCap)

  private lazy val startingAt = getWatch
  private var lastValueAt = 0.0

  private var allValues: Array[(Double,Long,List[Long])] = Array.empty
  private var best = Long.MaxValue
  private var decreasing = false
  private var percentile = basePercentile

  xDom = new org.jfree.data.Range(0,1)
  yDom = new org.jfree.data.Range(0,100)

  panel.setMouseWheelEnabled(true)
  panel.setMouseZoomable(true)
  panel.setHorizontalAxisTrace(true)
  panel.setVerticalAxisTrace(true)

  ColorGenerator.setSeed(0)
  val otherSeriesColors: Array[Color] = ColorGenerator.generateRandomColors(nbOtherValues)
  val r1 = new XYLineAndShapeRenderer
  r1.setSeriesPaint(1, Color.GREEN)
  r1.setSeriesPaint(0, Color.RED)
  r1.setSeriesShapesVisible(0, false)
  r1.setSeriesShapesVisible(1, false)
  (0 until nbOtherValues).foreach( i => {
    r1.setSeriesPaint(i+2, otherSeriesColors(i))
    r1.setSeriesShapesVisible(i+2, false)
  })

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
    val (minX, maxX, minY, maxY) = percentileBounds()

    // Update Y axis height
    // It's the lowest
    yDom = new org.jfree.data.Range(lower(minY),upper(maxY))

    // Update X axis length
    xDom = new org.jfree.data.Range(minX,Math.max(xDom.getUpperBound, maxX))
  }

  def drawFunction(value: Long, otherValues: Array[() => Long]): Unit ={

    lastValueAt = (getWatch - startingAt)/1000.0
    allValues = allValues :+ (lastValueAt,value,otherValues.map(_.apply()).toList)

    // Update best value
    if(value < best) {
      if(!decreasing) addStartDecreasingMark(lastValueAt)
      decreasing = true
      best = value
    } else {
      decreasing = false
    }

    addPoint(lastValueAt,value,0)
    addPoint(lastValueAt, best, 1)
    otherValues.indices.foreach(i => addPoint(lastValueAt,otherValues(i).apply(),i+2))

    reDrawFunction()
  }

  private def addStartDecreasingMark(at: Double): Unit ={
    val marker = new ValueMarker(at)
    marker.setPaint(Color.GREEN)
    plot.addDomainMarker(marker)
  }

  /**
   * This method return the bounds based on the percentile value.
   * The lower bounds are determined like this :
   *  First we determine the x Axis :
   *    - minX = The earliest acceptable value (taking his time information)
   *    - maxX = The last encountered value (taking his time information)
   *
   *  Then considering only the values between minX and maxX :
   *    - minY = The lowest value (considering all the values (obj and others))
   *    - maxY = The highest value (considering all the values (obj and others))
   *
   * If percentile >= 100 ==> it means that we display all the data.
   * If percentile == 30 ==> it means that we display the 30% best data (based on obj value)
   * If percentile == 0 ==> it means that we display 0% of the data.
   */
  private def percentileBounds(): (Double, Double, Long, Long) ={
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
    val latestAcceptableValue = valuesToConsider.maxBy(allValues(_)._1)
    val minX = allValues(earliestAcceptableValue)._1
    val maxX = allValues(latestAcceptableValue)._1
    val displayedValues = allValues.splitAt(earliestAcceptableValue)._2.flatMap(x => x._2 :: x._3)
    val minY = displayedValues.min
    val maxY = displayedValues.max
    (minX, maxX, minY, maxY)
  }

  /**
   * Ex :
   * 38427 ==> 39000
   * 38000 ==> 39000
   * 20 ==> 19
   * 5 ==> 0
   */
  private def upper(value: Long): Long ={
    var resExp:Long = 10
    while(value/resExp > 1) {
      resExp = resExp * 10
    }
    resExp = if(resExp <= 100) resExp/10 else resExp/100
    val maxValue = ((value/resExp)+1)*resExp
    (maxValue min maxCap) max (minCap + 1)
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
    val minValue = {
      if(resExp <= 0) 0
      else if(value%resExp > 0) (value/resExp)*resExp
      else ((value/resExp)-1)*resExp
    }

    (minValue max minCap) min (maxCap - 1)
  }
}

object ObjectiveFunctionDisplay{
  def apply(title: String, minCap: Long, maxCap:Long, percentile: Int, nbOtherValues: Int): ObjectiveFunctionDisplay =
    new ObjectiveFunctionDisplay(title, minCap, maxCap, percentile, nbOtherValues)
}
