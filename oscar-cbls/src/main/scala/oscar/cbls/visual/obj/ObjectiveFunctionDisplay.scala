package oscar.cbls.visual.obj

import collection.JavaConverters._
import java.awt.{BorderLayout, Color, Paint}
import java.awt.event.MouseWheelEvent

import javax.swing.JPanel
import org.jfree.chart.axis.{LogAxis, NumberAxis}
import org.jfree.chart.labels.XYToolTipGenerator
import org.jfree.chart.{ChartColor, ChartPanel, JFreeChart}
import org.jfree.chart.plot.{CombinedDomainXYPlot, IntervalMarker, XYPlot}
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.chart.ui.Layer
import org.jfree.data.xy.{XYDataset, XYSeries, XYSeriesCollection}
import oscar.cbls.util.StopWatch
import oscar.cbls.visual.ColorGenerator

/**
 * This implementation use the CombinedDomainXYPlot of library org.jfree
 * This is the structure :
 *  - The main panel contains a JFreeChart
 *  - The JFreeChart holds a collection of XYPlot, each displaying a graph with some data
 *  - Each XYPlot contains a XYSeriesCollection holding a collection of XYSeries
 *
 *  The data are added to the XYSeries.
 *  The domain and range (X and Y axes) are defined for each plot.
 *  A renderer is define for each plot and contains the visual customization of each plot.
 *
 *
 *  For this implementation we have the following :
 *
 *  The main plot contains the series : objective function and best values
 *    In RED and GREEN colors
 *  Each potential other values are displayed in there own plot with a random color.
 *  The zoom is the same for each plot. Meaning, if we displays only the 50% best values,
 *  the domain of all plot is resized based on the 50% best value of the objective function.
 * @param title
 * @param minCap
 * @param maxCap
 * @param basePercentile
 * @param otherValues
 */
class ObjectiveFunctionDisplay(title: String, minCap:Long, maxCap:Long, basePercentile: Int, otherValues: List[String])
  extends JPanel(new BorderLayout()) with StopWatch {

  require(minCap+5 < maxCap, "Min cap should be lesser than max cap - 5 (for proper display purpose) . Got minCap : " + minCap + ", maxCap : " + maxCap)
  // 1 series per value source
  val seriesIds: Range = 0 until 2 + otherValues.size
  // Only the obj values and his best values are on the same graph
  val datasetsIds: Range = 0 until 1 + otherValues.size

  // Series colors

  ColorGenerator.setSeed(0)
  private final val plotColors: Array[List[Paint]] = {
    // Initializing colors using the one proposed by Jfree library + adding random color if not enough
    // NB : The dark red is for the obj function and the dark green for the best value
    val chartColors: Array[Paint] = ChartColor.createDefaultPaintArray().drop(6).
      filterNot(p => p == ChartColor.DARK_RED || p == ChartColor.DARK_GREEN)
    val otherSeriesColors = chartColors ++
      (if(otherValues.size > chartColors.length)
        ColorGenerator.generateRandomColors(otherValues.size - chartColors.length)
      else Array.empty[Paint])
    datasetsIds.toArray.map{
      case 0 => List(Color.RED, Color.GREEN)
      case x => List(otherSeriesColors(x-1))
    }
  }

  // The plots
  private val combinedPlots = new CombinedDomainXYPlot(new NumberAxis("Time (s)"))
  private val series = seriesIds.toArray.map {
    case 0 => new XYSeries("Obj values")
    case 1 => new XYSeries("Best values")
    case id => new XYSeries(otherValues(id - 2))
  }
  private val datasets: List[XYSeriesCollection] = datasetsIds.toList.map {
    case 0 =>
      val collection = new XYSeriesCollection()
      // Adding obj and best series
      collection.addSeries(series(0))
      collection.addSeries(series(1))
      collection
    case id =>
      // The first dataset contains the first 2 series so we must add 1 to the id
      new XYSeriesCollection(series(id + 1))
  }

  val xyToolTipGenerator: XYToolTipGenerator = (dataset: XYDataset, series: Int, item: Int) => {
    val stringBuilder = new StringBuilder
    stringBuilder.append(String.format("<html>"))

    datasets.foreach(d => d.getSeries.asScala.foreach(s => {
      val series = s.asInstanceOf[XYSeries]
      stringBuilder.append(s"<p style='color:#0000ff;'>${series.getKey()} : ${series.getY(item).intValue()}</p>")
    }))
    stringBuilder.append(s"<p style='color:#0000ff;'>At : ${dataset.getX(series, item)} s </p>")
    stringBuilder.append("</html>")
    stringBuilder.toString()
  }
  private val plots = generatePlots()
  combinedPlots.add(plots.head,2)
  plots.drop(1).foreach(combinedPlots.add(_,1))


  // Data values
  private lazy val startingAt = getWatch
  private var lastValueAt = 0.0

  private var allValues: Array[(Double,Long)] = Array.empty
  private var best = Long.MaxValue
  private var decreasing = false
  private var percentile = basePercentile


  private val chart: JFreeChart = createChart()
  private val panel: ChartPanel = new ChartPanel(chart)

  panel.setMouseWheelEnabled(true)
  panel.setMouseZoomable(true)

  // Display interactions
  this.setDoubleBuffered(true)
  panel.setDoubleBuffered(true)
  // Custom Mouse wheel listener (increasing / decreasing percentile)
  panel.getMouseWheelListeners.foreach(mwl => panel.removeMouseWheelListener(mwl))
  panel.addMouseWheelListener((e: MouseWheelEvent) => {
    if (e.getWheelRotation < 0) {
      percentile = Math.max(1, percentile - 2)
    } else {
      percentile = Math.min(100, percentile + 2)
    }
    resizeGraphs()
  })

  panel.setVisible(true)
  add(panel)

  private def generatePlots(): List[XYPlot] ={
    def generatePlot(dataSetsId: Int, seriesColors: List[Paint]): XYPlot ={
      val plot = new XYPlot()
      plot.setDataset(datasets(dataSetsId))
      // Starting X axis domain
      plot.setDomainAxis({
        val domain = new NumberAxis("Time (s)")
        domain.setAutoRangeIncludesZero(true)
        domain
      })
      // Setting Y axis range
      plot.setRangeAxis({
        if (dataSetsId == 0) {
          val axis = new LogAxis()
          axis.setAutoTickUnitSelection(true)
          axis.setBase(10)
          axis.setMinorTickMarksVisible(true)
          axis.setRange(new org.jfree.data.Range(minCap, maxCap), true, true)
          axis
        } else {
          val axis = new NumberAxis()
          axis.setAutoRange(true)
          axis.setAutoRangeIncludesZero(false)
          axis
        }
      })

      val renderer = new XYLineAndShapeRenderer()
      seriesColors.zipWithIndex.foreach(colorId =>
        renderer.setSeriesPaint(colorId._2,colorId._1))
      renderer.setDefaultShapesVisible(false)
      renderer.setDefaultToolTipGenerator(xyToolTipGenerator)
      plot.setRenderer(renderer)
      plot
    }
    datasetsIds.toList.map(i => generatePlot(i, plotColors(i)))
  }

  def createChart(): JFreeChart = {
    val chart = new JFreeChart(title, JFreeChart.DEFAULT_TITLE_FONT, combinedPlots, true)
    chart
  }


  def resizeGraphs(): Unit ={
    val (pMinX, pMaxX, pMinY, pMaxY) = percentileBounds()
    plots.head.getRangeAxis.setRange(lower(pMinY), upper(pMaxY))
    plots.indices.foreach(i => {
      plots(i).getDomainAxis.setRange(pMinX, pMaxX)
      datasets(i).getSeries(0).fireSeriesChanged()
    })
  }

  def addValues(at: Double, objFunctionValue: Long, otherValues: Array[Long]): Unit ={
    allValues = allValues :+ (at, objFunctionValue)
    seriesIds.foreach({
      case 0 => series(0).add(lastValueAt,objFunctionValue)
      case 1 => series(1).add(lastValueAt, best)
      case x => series(x).add(lastValueAt, otherValues(x-2))
    })
  }

  /**
   * This methods purpose is to redraw the display.
   * 1° Save the new values
   * 2° Update the X axis for ALL drawings
   */
  def drawFunction(value: Long, otherValues: Array[() => Long]): Unit ={
    lastValueAt = (getWatch - startingAt)/1000.0

    if(value < best){
      // The obj value starts decreasing ==> adding a new interval marker
      if(!decreasing) {
        val marker = new IntervalMarker(lastValueAt, lastValueAt)
        marker.setPaint(new Color(55, 255, 55))
        marker.setAlpha(0.2f)
        marker.setOutlinePaint(new Color(55, 255, 55))
        plots.head.addDomainMarker(marker)
      } else {
        // Still decreasing ==> increase the interval
        val currentMarker =
          plots.head.getDomainMarkers(Layer.FOREGROUND).asScala.last.asInstanceOf[IntervalMarker]
        currentMarker.setEndValue(lastValueAt)
      }
    }
    decreasing = value < best
    best = Math.min(best, value)

    addValues(lastValueAt, value, otherValues.map(_.apply()))

    resizeGraphs()
  }

  /**
   * This method return the bounds based on the percentile value.
   * The x axis lower bounds are determined like this :
   *  - minX = The earliest acceptable value (taking his time information)
   *  - maxX = The last encountered value (taking his time information)
   *
   * If percentile >= 100 ==> it means that we display all the data.
   * If percentile == 30 ==> it means that we display the 30% best data (based on obj value)
   * If percentile == 0 ==> it means that we display 0% of the data.
   */
  private def percentileBounds(): (Double, Double, Long, Long) ={
    val nbValues = allValues.length
    val array = Array.tabulate(nbValues)(x => x)
    val sortedValues = allValues.zipWithIndex.sortBy(_._1._2)
    val valuesIdToConsider = sortedValues.take((nbValues/100.0*percentile).ceil.toInt).map(_._2)
    // The first (in time) acceptable value could be before the worstAcceptableValue
    val earliestAcceptableValue = valuesIdToConsider.minBy(allValues(_)._1)
    val latestAcceptableValue = valuesIdToConsider.maxBy(allValues(_)._1)
    val maxX = allValues(latestAcceptableValue)._1
    val minX = Math.min(maxX - 1, allValues(earliestAcceptableValue)._1)
    (minX, maxX, allValues(valuesIdToConsider.head)._2, allValues(valuesIdToConsider.last)._2)
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

    // Min cap must be >= 1 otherwise the logarithmic scale doesn't work
    (minValue max minCap max 1) min (maxCap - 1)
  }
}

object ObjectiveFunctionDisplay{
  def apply(title: String, minCap: Long, maxCap:Long, percentile: Int, otherValues: List[String]): ObjectiveFunctionDisplay =
    new ObjectiveFunctionDisplay(title, minCap, maxCap, percentile, otherValues)
}
