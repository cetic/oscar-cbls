package oscar.cbls.lib.search.combinators.distributed

import org.jfree.chart.axis.{LogAxis, NumberAxis}
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.chart.{ChartPanel, JFreeChart}
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}

import java.awt.BorderLayout
import javax.swing.JPanel
import scala.collection.SortedMap

//TODO: this one should be part of the supervisor.
class DistributedObjDisplay()
  extends JPanel(new BorderLayout()) {

  //series and dataset
  private var series:SortedMap[Long,XYSeries] = SortedMap.empty[Long,XYSeries]
  private val dataset: XYSeriesCollection = new XYSeriesCollection()

  //Adding bestSeries
  val bestSeriesId = 0
  val bestSeries = new XYSeries(s"best")
  series = series + (-1L -> bestSeries)
  dataset.addSeries(bestSeries)

  //Adding cancel series
  val abortedSeriesId = 1
  val abortedSeries = new XYSeries(s"aborted")
  series = series + (-2L -> abortedSeries)
  dataset.addSeries(abortedSeries)

  private var startingAtMS:Long = -1

  // The plots
  val plot = new XYPlot(
    dataset,
    {
      val domain = new NumberAxis("Time (s)")
      domain.setAutoRangeIncludesZero(true)
      domain
    },
    {
      /*
       * val axis = new NumberAxis()
       * axis.setAutoRange(true)
       * axis.setAutoRangeIncludesZero(false)
       * axis
       * */
      val axis = new LogAxis()
      axis.setAutoTickUnitSelection(true)
      axis.setBase(10)
      axis.setMinorTickMarksVisible(true)
      axis.setAutoRange(true)
      axis.setAutoRangeMinimumSize(1)
      axis
    },
    {
      val r = new XYLineAndShapeRenderer(true,false)
      r.setSeriesShapesFilled(bestSeriesId,false)
      r.setSeriesShapesVisible(bestSeriesId,true)
      r.setSeriesLinesVisible(bestSeriesId,false)

      r.setSeriesShapesFilled(abortedSeriesId,true)
      r.setSeriesShapesVisible(abortedSeriesId,true)
      r.setSeriesLinesVisible(abortedSeriesId,false)

      r
    }
  )

  //the window stuff, displaying the plots
  private val chart: JFreeChart = new JFreeChart(null, JFreeChart.DEFAULT_TITLE_FONT, plot, false)
  private val panel: ChartPanel = new ChartPanel(chart)
  this.setDoubleBuffered(true)
  panel.setDoubleBuffered(true)
  panel.setVisible(true)
  add(panel)

  def addValue(searchId:Long, obj: Long, timeMS:Long, aborted:Boolean): Unit ={
    if(startingAtMS < 0){
      startingAtMS = timeMS
    }
    val currentTime = timeMS - startingAtMS
    if(series.isDefinedAt(searchId)){
      series(searchId).add(currentTime,obj)
    }else{
      val newSeries = new XYSeries(s"search $searchId")
      newSeries.add(currentTime,obj)
      series = series + (searchId -> newSeries)
      dataset.addSeries(newSeries)
    }

    if(aborted){
      abortedSeries.add(currentTime,obj)
    }
    if(bestSeries.isEmpty || obj < bestSeries.getMinY){
      bestSeries.add(currentTime,obj)
    }
  }
}
