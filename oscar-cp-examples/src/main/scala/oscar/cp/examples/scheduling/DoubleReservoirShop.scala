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
object DoubleReservoirShop extends App {

  val nTasks = 16
  val nReservoirs = 2

  val durations = Array(4, 5, 2, 9, 10, 3, 5, 6, 7, 1, 8, 10, 9, 7, 6, 8)
  val productions = Array(3, 1, 0, 0, 0, 2, 4, 0, 2, 0, 1, 1, 2, 0, 0, 3)
  val consumptions = Array(0, 0, 2, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 4, 2, 0)
  val temporary = Array(false, false, false, true, false, true, false, false, false, false, true, false, true, false, false, false) //Array.fill(nTasks)(false)
  val reservoirs = Array(0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0)
  val minCapas = Array(5, 3)
  val maxCapas = Array(10, 7)
  val initialAmounts = Array(7, 3)

  val horizon = durations.sum
  implicit val cp = CPSolver()

  val durationVars = Array.tabulate(nTasks)(t => CPIntVar(durations(t)))
  val startVars = Array.tabulate(nTasks)(t => CPIntVar(0 to horizon - durationVars(t).min))
  val endVars = Array.tabulate(nTasks)(t => startVars(t) + durationVars(t))
  val productionVars = productions.map(v => CPIntVar(v))
  val consumptionVars = consumptions.map(v => CPIntVar(v))

  val makespan = maximum(endVars)

  def filterByReservoir[A](reservoir: Int, variables: Array[A])(implicit m: ClassTag[A]): Array[A] = {
    var myVars = List[A]()
    for (i <- reservoirs.indices) {
      if (reservoirs(i) == reservoir) {
        myVars ::= variables(i)
      }
    }
    val res  = myVars.reverse
    res.toArray
  }

  for (r <- 0 until nReservoirs) {
    add(reservoirResource(filterByReservoir(r, startVars), filterByReservoir(r, durationVars),
      filterByReservoir(r, endVars), filterByReservoir(r, productionVars),
      filterByReservoir(r, consumptionVars), filterByReservoir(r, temporary),
      minCapas(r), maxCapas(r), initialAmounts(r)))
  }

  minimize(makespan) search {
    binaryFirstFail(startVars)
  }

  /*********************************   VISUALISATION   *********************************/
  val frame = new VisualFrame("Test Reservoir Profile", 3, 1)
  val colors = Array(Color.GREEN, Color.RED)
  val gantt = new VisualGanttChart(startVars, durationVars, endVars, i => i, colors = i => if (productionVars(i).max > 0) colors(0) else colors(1))
  val gf = frame.createFrame("Gantt Chart").add(gantt)
  val vps = Array.tabulate(nReservoirs)(r =>
    VisualReservoirProfile(filterByReservoir(r, startVars), filterByReservoir(r, durationVars),
      filterByReservoir(r, endVars), filterByReservoir(r, productionVars),
      filterByReservoir(r, consumptionVars), filterByReservoir(r, temporary),
      minCapas(r), maxCapas(r), initialAmounts(r), Color.CYAN)
  )

  val fs = Array.tabulate(nReservoirs)(i => frame.createFrame("Profile of Reservoir " + (i + 1)).add(vps(i)))
  frame.pack()
  onSolution{
    gantt.update(20, 10)
    for (r <- 0 until nReservoirs) {
      vps(r).update(20, 10)
    }

  }

  println(start())
}
