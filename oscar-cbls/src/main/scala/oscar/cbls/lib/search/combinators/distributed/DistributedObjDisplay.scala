package oscar.cbls.lib.search.combinators.distributed

import org.jfree.chart.axis.{LogAxis, NumberAxis}
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.chart.{ChartPanel, JFreeChart}
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}

import java.awt.BorderLayout
import javax.swing.JPanel
import scala.collection.SortedMap

class DistributedObjDisplay(title: String)
  extends JPanel(new BorderLayout()) {

  //series and dataset
  private var series:SortedMap[Long,XYSeries] = SortedMap.empty[Long,XYSeries]
  private val dataset: XYSeriesCollection = new XYSeriesCollection()

  // Data values
  private lazy val startingAtMS = System.currentTimeMillis()

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
            val axis = new NumberAxis()
            axis.setAutoRange(true)
            axis.setAutoRangeIncludesZero(false)
            axis
            */
      val axis = new LogAxis()
      axis.setAutoTickUnitSelection(true)
      axis.setBase(10)
      axis.setMinorTickMarksVisible(true)
      axis.setAutoRange(true)
      axis.setAutoRangeMinimumSize(1)
      axis
    },
    {
      val r = new XYLineAndShapeRenderer()
      r.setDefaultShapesVisible(false)
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


  def addValue(searchId:Long, obj: Long): Unit ={
    val currentTime = System.currentTimeMillis() - startingAtMS
    if(series.isDefinedAt(searchId)){
      series(searchId).add(currentTime,obj)
    }else{
      val newSeries = new XYSeries(s"search $searchId")
      newSeries.add(currentTime,obj)
      series = series + (searchId -> newSeries)
      dataset.addSeries(newSeries)
      plot.getRangeAxis.setUpperBound(obj)
    }
  }
}
