package oscar.cp.examples.scheduling

import java.awt.Color

import oscar.cp._
import oscar.cp.scheduling.visual.{VisualGanttChart, VisualReservoirProfile}
import oscar.visual.VisualFrame

import scala.reflect.ClassTag

/**
 * Created on 11/06/15.
 *
 * Reservoir Shop Problem
 *
 *  A Job is a a sequence of n Activities that must be executed one after the
 *  others. There are n reservoirs and each activity of the jobs produces or consumes
 *  a given amount of resource on one of the reservoirs. The objective is to assign
 *  the starting time of each activity minimizing the total makespan and such that
 *  the minimal and maximal capacity of the reservoirs is respected.
 *
 * @author Cyrille Dejemeppe (cyrille.dejemeppe@gmail.com)
 */
object SingleReservoirShop extends App {

  val nTasks = 8
  val nReservoirs = 2

  val durations = Array(4, 5, 10, 6, 8, 10, 7, 8)
  val productions = Array(3, 1, 0, 0, 1, 1, 0, 3)
  val consumptions = Array(0, 0, 1, 1, 0, 0, 4, 0)
  val temporary = Array(false, false, false, false, true, false, false, false)
  val minCapa = 5
  val maxCapa = 10
  val initialAmount = 7

  val horizon = durations.sum
  implicit val cp = CPSolver()

  val durationVars = Array.tabulate(nTasks)(t => CPIntVar(durations(t)))
  val startVars = Array.tabulate(nTasks)(t => CPIntVar(0 to horizon - durationVars(t).min))
  val endVars = Array.tabulate(nTasks)(t => startVars(t) + durationVars(t))
  val productionVars = productions.map(v => CPIntVar(v))
  val consumptionVars = consumptions.map(v => CPIntVar(v))

  val makespan = maximum(endVars)

  add(reservoirResource(startVars, durationVars, endVars, productionVars, consumptionVars, temporary, minCapa, maxCapa, initialAmount))

  minimize(makespan) search {
    binaryFirstFail(startVars)
  }

  /*********************************   VISUALISATION   *********************************/
  val frame = new VisualFrame("Test Reservoir Profile", 2, 1)
  val colors = Array(Color.GREEN, Color.RED)
  val gantt = new VisualGanttChart(startVars, durationVars, endVars, i => i, colors = i => if (productionVars(i).max > 0) colors(0) else colors(1))
  val gf = frame.createFrame("Activities").add(gantt)
  val vp = VisualReservoirProfile(startVars, durationVars, endVars, productionVars, consumptionVars, temporary, minCapa, maxCapa, initialAmount, Color.CYAN)
  val f1 = frame.createFrame("Reservoir Profile").add(vp)
  frame.pack()
  onSolution{
    gantt.update(20, 10)
    vp.update(20, 10)
  }

  println(start())
}
