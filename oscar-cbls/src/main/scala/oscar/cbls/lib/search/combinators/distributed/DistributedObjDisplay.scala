package oscar.cbls.lib.search.combinators.distributed

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.jfree.chart.axis.{LogAxis, NumberAxis}
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.chart.{ChartPanel, JFreeChart}
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import oscar.cbls.core.distrib.{MessagesToSupervisor, SearchProgress, Supervisor}
import oscar.cbls.lib.search.combinators.distributed.TestDistributedObjDisplay.d
import oscar.cbls.util.StopWatch
import oscar.cbls.visual.SingleFrameWindow

import java.awt.BorderLayout
import javax.swing.JPanel
import scala.collection.SortedMap
import scala.concurrent.Await
import scala.concurrent.duration.Duration



//SearchProgress(searchId:Long, obj:Long)



object TestDistributedObjDisplay extends App{
  val d = new DistributedObjDisplay("toto")
  val window = SingleFrameWindow.show(d,"toto")

  for(r <-(0 until 10).par){
    Thread.sleep(1000*r)
    for (t <- 0 until 100) {
      d.synchronized {
        d.addValue(r, (r+1) * (100 + r - t))
      }
      Thread.sleep(10*t)
    }
  }
}

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
    }
  }
}
