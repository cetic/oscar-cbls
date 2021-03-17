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

package oscar.modeling.solvers.cp

import java.awt._
import java.util.TimerTask
import javax.swing._

import oscar.modeling.misc.TimeHelper._
import oscar.modeling.misc._
import org.jfree.chart.axis.{AxisLocation, NumberAxis}
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.chart.util.ShapeUtils
import org.jfree.chart.{ChartFactory, ChartPanel}
import org.jfree.data.xy
import org.jfree.data.xy.XYSeries
import oscar.modeling.solvers.cp.distributed.{SubProblem, SubProblemCartesianProductLog, SubProblemDiscrepancy}

import scala.List
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.swing.Swing
import scala.math.Ordering.Double.TotalOrdering

/**
  * A GUI that shows various stats about a multithreaded solve
  *
  * @param nbSubproblems
  * @param nbThreads
  * @tparam T
  */
class SubproblemGraphicalProgressBar[T](nbSubproblems: Int, nbThreads: Int) extends JPanel() with Watcher[T] {
  this.setLayout(new BoxLayout(this, BoxLayout.Y_AXIS))

  private val progressBar: JProgressBar = new JProgressBar(0, nbSubproblems)
  private val progressBarCartProduct: JProgressBar = new JProgressBar(0, 100)
  private val subproblemStatus: JLabel = new JLabel("")
  private val cpuWallRatio: JLabel = new JLabel("")
  private val wallTimeRemaining: JLabel = new JLabel("")
  private val cpuTimeRemaining: JLabel = new JLabel("")
  private val wallTimeElapsed: JLabel = new JLabel("")
  private val cpuTimeElapsed: JLabel = new JLabel("")
  private val wallTimeTotal: JLabel = new JLabel("")
  private val cpuTimeTotal: JLabel = new JLabel("")
  private val solutionsFoundLabel: JLabel = new JLabel("0")

  private var startTime = 0.0
  def start(): Unit = startTime = getClockTime

  private val timeStorage = new ArrayBuffer[(Double, Double, Double, Double, Double)]
  private var solutionsFound = 0

  private var lastElapsedWallTime = 0.0
  private var lastRemainingWallTime = 0.0
  private var lastElapsedCPUTime = 0.0
  private var lastRemainingCPUTime = 0.0

  private var lastBound: Option[Int] = None

  //subproblemsTime contains:
  // - for done subproblems: the time taken to solve them (cpu)
  // - for not started subproblems: 0.0
  // - for started but not yet solved subproblems: the solve start time (clock)
  private val subproblemsTime = Array.tabulate(nbSubproblems)(i => 0.0)
  private var subproblemsDone = 0
  private val subproblemsResolving = new mutable.HashSet[Int]
  private val instantMeanTime = new SimpleMovingAverage(10)
  private val exponentialMeanTime = new ExponentialMovingAverage(0.05) //TODO: value
  private var subproblemsTotalCPUTime = 0.0

  /*
    CHARTS
   */
  private val totalWallTimePlotPoints = new XYSeries("Wall")
  private val totalWallTimeCartCardPlotPoints = new XYSeries("Wall - Cart. Prod. Cardinality")
  private val totalCPUTimePlotPoints = new XYSeries("CPU")
  private val totalCPUTimeExpPlotPoints = new XYSeries("CPU (exp average)")
  private val timeProblemPlotPoints = new XYSeries("Time")
  private val ccProblemPlotPoints = new XYSeries("Cart. Prod. Cardinality")
  private val minBoundProblemPlotPoints = new XYSeries("Min/Max Bound")
  private val meanWallTimePlotPoints = new XYSeries("Mean (Wall)")
  private val meanCPUTimePlotPoints = new XYSeries("Mean (CPU)")
  private val instantMeanCPUTimePlotPoints = new XYSeries("Simple moving mean (size 10) (CPU)")
  private val exponentialMeanCPUTimePlotPoints = new XYSeries("Exp moving mean (0.1) (CPU)")

  private val boundPlotPoints = new XYSeries("Bound Value")

  private val subproblemCPUTimeHistogram = new FixedBinsHistogramDataset("CPU", 50)

  private val collectionTotal = new xy.XYSeriesCollection()
  private val collectionMean = new xy.XYSeriesCollection()
  private val collectionBound = new xy.XYSeriesCollection()
  private val collectionProblem = new xy.XYSeriesCollection()
  private val collectionProblemCC = new xy.XYSeriesCollection()
  private val collectionProblemMMB = new xy.XYSeriesCollection()
  collectionTotal.addSeries(totalWallTimePlotPoints)
  collectionTotal.addSeries(totalWallTimeCartCardPlotPoints)
  collectionTotal.addSeries(totalCPUTimePlotPoints)
  collectionTotal.addSeries(totalCPUTimeExpPlotPoints)
  
  collectionMean.addSeries(meanWallTimePlotPoints)
  collectionMean.addSeries(meanCPUTimePlotPoints)
  collectionMean.addSeries(instantMeanCPUTimePlotPoints)
  collectionMean.addSeries(exponentialMeanCPUTimePlotPoints)

  collectionBound.addSeries(boundPlotPoints)

  collectionProblem.addSeries(timeProblemPlotPoints)
  collectionProblemCC.addSeries(ccProblemPlotPoints)
  collectionProblemMMB.addSeries(minBoundProblemPlotPoints)

  private val totalTimeChart = ChartFactory.createXYLineChart("", "Wall time", "Total time estimated",
    collectionTotal, PlotOrientation.VERTICAL, true, true, false)
  private val meanTimeChart = ChartFactory.createXYLineChart("", "Wall time", "Time taken per subproblem",
    collectionMean, PlotOrientation.VERTICAL, true, true, false)
  private val boundChart = ChartFactory.createXYLineChart("", "Wall time", "Bound value",
    collectionBound, PlotOrientation.VERTICAL, true, true, false)
  private val problemDescriptionChart = ChartFactory.createScatterPlot("", "Problem ID", "Wall time taken",
    collectionProblem, PlotOrientation.VERTICAL, true, true, false)
  private val subproblemCPUTimeChart = ChartFactory.createHistogram("", "CPU Time", "#",
    subproblemCPUTimeHistogram.getRealDataset, PlotOrientation.VERTICAL, true, true, false)


  problemDescriptionChart.getXYPlot.getRenderer.setSeriesShape(0, ShapeUtils.createDiamond(0.5f))
  val renderer2 = new XYLineAndShapeRenderer(false, true)
  val axis2 = new NumberAxis("Log of Cart. Prod. Cardinality")
  axis2.setAutoRangeIncludesZero(false)
  problemDescriptionChart.getXYPlot.setRangeAxisLocation(1, AxisLocation.BOTTOM_OR_RIGHT)
  problemDescriptionChart.getXYPlot.setRangeAxis(1, axis2)
  problemDescriptionChart.getXYPlot.setDataset(1, collectionProblemCC)
  problemDescriptionChart.getXYPlot.setRenderer(1, renderer2)
  problemDescriptionChart.getXYPlot.mapDatasetToRangeAxis(1, 1)
  renderer2.setSeriesShape(0, ShapeUtils.createDownTriangle(0.5f))
  val renderer3 = new XYLineAndShapeRenderer(false, true)
  val axis3 = new NumberAxis("Min/Max Bound subproblem value")
  axis3.setAutoRangeIncludesZero(false)
  problemDescriptionChart.getXYPlot.setRangeAxisLocation(2, AxisLocation.BOTTOM_OR_RIGHT)
  problemDescriptionChart.getXYPlot.setRangeAxis(2, axis3)
  problemDescriptionChart.getXYPlot.setDataset(2, collectionProblemMMB)
  problemDescriptionChart.getXYPlot.setRenderer(2, renderer3)
  problemDescriptionChart.getXYPlot.mapDatasetToRangeAxis(2, 2)
  renderer3.setSeriesShape(0, ShapeUtils.createUpTriangle(0.5f))

  totalTimeChart.setBackgroundPaint(new Color(255, 255, 255, 0))
  totalTimeChart.setBackgroundImageAlpha(0.0f)
  meanTimeChart.setBackgroundPaint(new Color(255, 255, 255, 0))
  meanTimeChart.setBackgroundImageAlpha(0.0f)
  boundChart.setBackgroundPaint(new Color(255, 255, 255, 0))
  boundChart.setBackgroundImageAlpha(0.0f)
  problemDescriptionChart.setBackgroundPaint(new Color(255, 255, 255, 0))
  problemDescriptionChart.setBackgroundImageAlpha(0.0f)
  subproblemCPUTimeChart.setBackgroundPaint(new Color(255, 255, 255, 0))
  subproblemCPUTimeChart.setBackgroundImageAlpha(0.0f)

  private val totalTimeChartPanel: ChartPanel = new ChartPanel(totalTimeChart)
  //totalTimeChartPanel.setPreferredSize(new java.awt.Dimension(560, 367))
  private val meanTimeChartPanel: ChartPanel = new ChartPanel(meanTimeChart)
  //meanTimeChartPanel.setPreferredSize(new java.awt.Dimension(560, 367))
  private val boundChartPanel: ChartPanel = new ChartPanel(boundChart)
  //boundChartPanel.setPreferredSize(new java.awt.Dimension(560, 367))
  private val problemDescriptionChartPanel: ChartPanel = new ChartPanel(problemDescriptionChart)
  //problemDescriptionChartPanel.setPreferredSize(new java.awt.Dimension(560, 367))
  private val subproblemCPUTimeChartPanel: ChartPanel = new ChartPanel(subproblemCPUTimeChart)
  //subproblemCPUTimeChartPanel.setPreferredSize(new java.awt.Dimension(560, 367))

  /*
    INITIALISE GUI
   */
  progressBar.setValue(0)
  progressBar.setStringPainted(true)
  progressBarCartProduct.setValue(0)
  progressBarCartProduct.setStringPainted(true)

  private val barPanel = new JPanel(new GridLayout(2, 2))
  barPanel.add(new JLabel("Subproblems done", SwingConstants.CENTER))
  barPanel.add(new JLabel("Search space explored", SwingConstants.CENTER))
  barPanel.add(progressBar)

  barPanel.add(progressBarCartProduct)

  private val timePanel = new JPanel(new GridLayout(0, 3))
  timePanel.add(new JLabel("", SwingConstants.LEFT))
  timePanel.add(new JLabel("Wall", SwingConstants.LEFT))
  timePanel.add(new JLabel("CPU", SwingConstants.LEFT))
  timePanel.add(new JLabel("Estimated remaining time: ", SwingConstants.LEFT))
  timePanel.add(wallTimeRemaining)
  timePanel.add(cpuTimeRemaining)
  timePanel.add(new JLabel("Elapsed time: ", SwingConstants.LEFT))
  timePanel.add(wallTimeElapsed)
  timePanel.add(cpuTimeElapsed)
  timePanel.add(new JLabel("Total estimated time: ", SwingConstants.LEFT))
  timePanel.add(wallTimeTotal)
  timePanel.add(cpuTimeTotal)

  private val statPanel = new JPanel(new GridLayout(0, 2))
  statPanel.add(new JLabel("Subproblems: ", SwingConstants.LEFT))
  statPanel.add(subproblemStatus)
  statPanel.add(new JLabel("Solutions found: ", SwingConstants.LEFT))
  statPanel.add(solutionsFoundLabel)
  statPanel.add(new JLabel("CPU/Wall ratio: ", SwingConstants.LEFT))
  statPanel.add(cpuWallRatio)

  private val tabbedPane = new JTabbedPane()
  tabbedPane.addTab("Total time estimation", null, totalTimeChartPanel, "Display a graph of the total time estimation")
  tabbedPane.addTab("Mean time estimation", null, meanTimeChartPanel, "Display a graph of the mean time estimation")
  tabbedPane.addTab("Bound value", null, boundChartPanel, "Display a graph of the evolution of the bound")
  tabbedPane.addTab("CPU time histogram", null, subproblemCPUTimeChartPanel, "Display an histogram of the time taken by subproblem")
  tabbedPane.addTab("Problem description", null, problemDescriptionChartPanel, "Display a graph showing some data about the subproblems")

  /*private val chartPanel = new JPanel(new GridLayout(2, 3))
  chartPanel.add(totalTimeChartPanel)
  chartPanel.add(meanTimeChartPanel)
  chartPanel.add(boundChartPanel)
  chartPanel.add(subproblemCPUTimeChartPanel)
  chartPanel.add(problemDescriptionChartPanel)*/

  add(barPanel)
  //add(chartPanel)
  add(tabbedPane)
  add(statPanel)
  add(timePanel)

  setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20))

  val timer = new java.util.Timer
  timer.scheduleAtFixedRate(new TimerTask() {
    override def run() = {
      Swing.onEDTWait {
        updateOnEDT()
      }
    }
  }, 0, 100)

  //C is for Corrected: we divide all values by max(cartesian product log) to have more sustainable values in memory
  var cartesianProductCSum: Double = 0
  var cartesianProductC: Array[Double] = null
  var cartesianProductCCurrent: Double = 0

  def setSubproblemsData(cc: Iterable[(Int, SubProblem)]) = {
    Swing.onEDT {
      for ((i, j) <- cc) {
        j.getData(SubProblemDiscrepancy).foreach(v => minBoundProblemPlotPoints.add(i, v))
        j.getData(SubProblemCartesianProductLog).foreach(v => ccProblemPlotPoints.add(i, v))
      }
    }

    val cartProds = cc.flatMap(v => v._2.getData(SubProblemCartesianProductLog))
    if(cartProds.nonEmpty) {
      val cartesianProductLogMax = cartProds.max
      cartesianProductC = cartProds.map(v => Math.exp(v-cartesianProductLogMax)).toArray
      cartesianProductCSum = cartesianProductC.foldLeft(0.0d)((current, v) => current + v)
      cartesianProductCCurrent = 0
    }
  }

  private def updateOnEDT(): Unit = {
    progressBar.setValue(subproblemsDone)
    progressBarCartProduct.setValue(Math.round(100.0*cartesianProductCCurrent/cartesianProductCSum).toInt)

    subproblemStatus.setText(s"$subproblemsDone/$nbSubproblems")
    solutionsFoundLabel.setText(solutionsFound.toString)

    wallTimeRemaining.setText(s"${round1000(lastRemainingWallTime / math.pow(10, 9))}s")
    wallTimeElapsed.setText(s"${round1000(lastElapsedWallTime / math.pow(10, 9))}s")
    wallTimeTotal.setText(s"${round1000((lastElapsedWallTime + lastRemainingWallTime) / math.pow(10, 9))}s")

    cpuTimeRemaining.setText(s"${round1000(lastRemainingCPUTime / math.pow(10, 9))}s")
    cpuTimeElapsed.setText(s"${round1000(lastElapsedCPUTime / math.pow(10, 9))}s")
    cpuTimeTotal.setText(s"${round1000((lastElapsedCPUTime + lastRemainingCPUTime) / math.pow(10, 9))}s")

    cpuWallRatio.setText(round1000(lastElapsedCPUTime / lastElapsedWallTime).toString)

    totalWallTimePlotPoints.fireSeriesChanged()
    totalWallTimeCartCardPlotPoints.fireSeriesChanged()
    totalCPUTimePlotPoints.fireSeriesChanged()
    totalCPUTimeExpPlotPoints.fireSeriesChanged()
    meanWallTimePlotPoints.fireSeriesChanged()
    meanCPUTimePlotPoints.fireSeriesChanged()
    instantMeanCPUTimePlotPoints.fireSeriesChanged()
    exponentialMeanCPUTimePlotPoints.fireSeriesChanged()
  }

  private def round1000(v: Double): Double = (v * 1000).toInt.toDouble / 1000

  private def updateTime(): Unit = {
    lastElapsedWallTime = getClockTime - startTime
    val meanWallTime = lastElapsedWallTime / subproblemsDone.toDouble
    lastRemainingWallTime = (nbSubproblems - subproblemsDone).toDouble * meanWallTime

    lastElapsedCPUTime = subproblemsTotalCPUTime
    val meanCPUTime: Double = subproblemsTotalCPUTime / subproblemsDone.toDouble
    lastRemainingCPUTime = (nbSubproblems - subproblemsDone).toDouble * meanCPUTime
    val remainingCPUTimeExp = (nbSubproblems - subproblemsDone).toDouble * exponentialMeanTime.get

    totalWallTimePlotPoints.add(lastElapsedWallTime / math.pow(10, 9), (lastElapsedWallTime + lastRemainingWallTime) / math.pow(10, 9), false)
    totalCPUTimePlotPoints.add(lastElapsedWallTime / math.pow(10, 9), (lastElapsedCPUTime + lastRemainingCPUTime) / math.pow(10, 9), false)
    totalCPUTimeExpPlotPoints.add(lastElapsedWallTime / math.pow(10, 9), (lastElapsedCPUTime + remainingCPUTimeExp) / math.pow(10, 9), false)

    meanWallTimePlotPoints.add(lastElapsedWallTime / math.pow(10, 9), meanWallTime / math.pow(10, 9), false)
    meanCPUTimePlotPoints.add(lastElapsedWallTime / math.pow(10, 9), meanCPUTime / math.pow(10, 9), false)
    instantMeanCPUTimePlotPoints.add(lastElapsedWallTime / math.pow(10, 9), instantMeanTime.get / math.pow(10, 9), false)
    exponentialMeanCPUTimePlotPoints.add(lastElapsedWallTime / math.pow(10, 9), exponentialMeanTime.get / math.pow(10, 9), false)

    //Estimation with Cartesian Product Cardinality
    //println(cartesianProductCCurrent/cartesianProductCSum)
    val cpTotalTime = (lastElapsedWallTime/math.pow(10, 9))*cartesianProductCSum/cartesianProductCCurrent
    totalWallTimeCartCardPlotPoints.add(lastElapsedWallTime / math.pow(10, 9), cpTotalTime)

    lastBound match {
      case Some(b) => boundPlotPoints.add(lastElapsedWallTime / math.pow(10, 9), b)
      case None =>
    }

    timeStorage += ((lastElapsedWallTime, lastRemainingWallTime, lastElapsedCPUTime, lastRemainingCPUTime, cpTotalTime*math.pow(10, 9)))
  }

  override def startedSubproblem(spid: Int): Unit = {
    subproblemsTime(spid) = getClockTime
    subproblemsResolving += spid
  }

  override def newSolution(solution: T, newBound: Option[Int]): Unit = {
    solutionsFound += 1
    lastElapsedWallTime = getClockTime - startTime
    lastBound = newBound
    lastBound match {
      case Some(b) => boundPlotPoints.add(lastElapsedWallTime / math.pow(10, 9), b)
        println("-----------------")
        println("SOLUTION IMPROVED")
        println((lastElapsedWallTime / math.pow(10, 9)).toString + "\t" + b)
        println("-----------------")
      case None =>
    }
  }


  override def endedSubproblem(spid: Int, timeTaken: Double, ss: SPSearchStatistics): Unit = {
    subproblemsTime(spid) = timeTaken
    subproblemsResolving -= spid
    subproblemsDone += 1
    instantMeanTime.update(timeTaken)
    exponentialMeanTime.update(timeTaken)
    subproblemsTotalCPUTime += timeTaken
    cartesianProductCCurrent += cartesianProductC(spid)
    updateTime()

    Swing.onEDT {
      timeProblemPlotPoints.add(spid, timeTaken / Math.pow(10, 9))
      subproblemCPUTimeHistogram.addObservation(timeTaken / Math.pow(10, 9))
    }
  }

  override def allDone(completed: Boolean): Unit = {
    val totalWallTime: Double = getClockTime - startTime
    val totalCPUTime: Double = subproblemsTotalCPUTime

    Swing.onEDTWait {
      subproblemCPUTimeHistogram.reinit()

      val errorTotalWallTime = new XYSeries("Total wall time error")
      val errorTotalCPCWallTime = new XYSeries("Total cart. prod. cardinality wall time error")
      val errorTotalCPUTime = new XYSeries("Total CPU time error")
      val errorRemainingWallTime = new XYSeries("Remaining wall time error")
      val errorRemainingCPUTime = new XYSeries("Remaining CPU time error")

      val collectionError = new xy.XYSeriesCollection()
      collectionError.addSeries(errorTotalWallTime)
      collectionError.addSeries(errorTotalCPCWallTime)
      collectionError.addSeries(errorTotalCPUTime)
      collectionError.addSeries(errorRemainingWallTime)
      collectionError.addSeries(errorRemainingCPUTime)

      for ((ewall, rwall, ecpu, rcpu, cpTotalTime) <- timeStorage) {
        val twall = ewall + rwall
        val tcpu = ecpu + rcpu
        val t = ewall / math.pow(10, 9)
        errorTotalWallTime.add(t, 100.0 * math.abs((totalWallTime - twall) / totalWallTime))
        errorTotalCPCWallTime.add(t, 100.0 * math.abs((totalWallTime - cpTotalTime) / totalWallTime))
        errorTotalCPUTime.add(t, 100.0 * math.abs((totalCPUTime - tcpu) / totalCPUTime))
        errorRemainingWallTime.add(t, 100.0 * math.abs(((totalWallTime - ewall) - rwall) / (totalWallTime - ewall)))
        errorRemainingCPUTime.add(t, 100.0 * math.abs(((totalCPUTime - ecpu) - rcpu) / (totalCPUTime - ecpu)))
      }

      val errorChart = ChartFactory.createXYLineChart("", "Wall time", "% error",
        collectionError, PlotOrientation.VERTICAL, true, true, false)

      errorChart.setBackgroundPaint(new Color(255, 255, 255, 0))
      errorChart.setBackgroundImageAlpha(0.0f)

      val errorChartPanel: ChartPanel = new ChartPanel(errorChart)
      errorChartPanel.setPreferredSize(new java.awt.Dimension(560, 367))
      errorChartPanel.setRangeZoomable(true)

      tabbedPane.addTab("Time estimation error", null, errorChartPanel, "Display a graph showing the evolution of the error")
      tabbedPane.updateUI()
      //chartPanel.add(errorChartPanel)
      //chartPanel.updateUI()
      timer.cancel()
    }
  }

  /**
    * Called when a solver give a recap of the found solution (after the search, no duplicates with newSolution)
    *
    * @param solutions
    */
  override def solutionRecap(nbSolutions: Int, solutions: List[T]): Unit = {
    solutionsFound += 1
    lastElapsedWallTime = getClockTime - startTime
  }
}

object SubproblemGraphicalProgressBar {
  def apply[T](nbSubproblems: Int, nbThreads: Int, windowName: String = "Progress Bar"): SubproblemGraphicalProgressBar[T] = {
    var gpb: SubproblemGraphicalProgressBar[T] = null

    Swing.onEDTWait {
      val frame = new JFrame(windowName)
      frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      val newContentPane = new SubproblemGraphicalProgressBar[T](nbSubproblems, nbThreads)
      newContentPane.setOpaque(true)
      frame.setContentPane(newContentPane)
      frame.pack()
      frame.setVisible(true)
      gpb = newContentPane
    }

    gpb
  }

  def getRegisterer[RetVal] = {
    val a = (subproblems: scala.collection.immutable.List[SubProblem]) => {
      val pb = SubproblemGraphicalProgressBar[RetVal](subproblems.length, 0)
      pb.setSubproblemsData(subproblems.zipWithIndex.map(m => (m._2, m._1)))
      pb
    }
    val b = (w: Watcher[RetVal]) => {
      w.asInstanceOf[SubproblemGraphicalProgressBar[RetVal]].start()
    }
    (a,b)
  }
}
