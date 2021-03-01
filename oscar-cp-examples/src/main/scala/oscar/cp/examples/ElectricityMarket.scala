package oscar.cp.examples


import oscar.cp._

import scala.io.Source
import oscar.visual._
import oscar.visual.plot.PlotLine
import scala.collection.mutable.Map

/**
 * Game invented by Bertrand Cornelusse and Gilles Scouvart for the 10 years of n-Side:
 * Maximize the total market exchange such that demand and supply match at any time
 * @author Pierre Schaus pschaus@gmail.com
 */
object ElectricityMarket extends CPModel with App {

  class Order(data: Array[Int]) {
    val qty = data(0) // amount of electricity he is ready to produce (>0) or consume (<0)
    val start = data(1) // [start,end] is the interval of validity of the order.
    val end = data(2)
    val selected = CPBoolVar() // If the order is selected the orderer will have to produce/consume
    // the quantity at each period: start, start+1, ...., end-1, end.
    def energy = qty.abs * (end - start + 1)
    def overlap(t: Int) = t <= end && t >= start
    var sol = true
    def bound = selected.isBound
  }

  val firstLine :: restLines = Source.fromFile("data/electricityMarket.txt").getLines.toList
  val n = firstLine.toInt

  val orders = restLines.map(_.split(" ").map(_.toInt)).map(new Order(_)).toArray
  val producers = orders.filter(_.qty > 0)
  val consumers = orders.filter(_.qty < 0)

  val tmin = orders.map(_.start).min
  val tmax = orders.map(_.end).max

  // -------------visual components ------------
  val f = VisualFrame("Electricity Market")
  // creates the plot and place it into the frame
  val plot = new PlotLine("", "Solution number", "Qty")
  f.createFrame("Objective").add(plot)
  val barPlot = BarChart("", "Time", "Qty", tmax - tmin + 1)
  f.createFrame("Qty Exchange").add(barPlot)
  f.pack()
  // ------------------------------------------

  // one var for each time slot = the quantity exchanged on that slot
  val varMapQty = Map[Int, CPIntVar]()
  for (t <- tmin to tmax) {
    val prodUB = producers.map(_.qty.abs).sum
    varMapQty += (t -> CPIntVar(0 to prodUB))
  }
  var nbSol = 0
  // total amount of exchanged quantity
  val obj: CPIntVar = sum(tmin to tmax)(t => varMapQty(t))

  for (t <- tmin to tmax) {
    val prodVars = producers.filter(_.overlap(t)).map(_.selected)
    val prodQty = producers.filter(_.overlap(t)).map(_.qty)
    val consVars = consumers.filter(_.overlap(t)).map(_.selected)
    val consQty = consumers.filter(_.overlap(t)).map(_.qty.abs)

    add(binaryKnapsack(prodVars, prodQty, varMapQty(t)), Strong)
    add(binaryKnapsack(consVars, consQty, varMapQty(t)), Strong)
  }

  maximize(obj) search {
    if (allBounds(orders.map(_.selected))) noAlternative
    else {
      val unboundOrders = orders.filter(!_.bound)
      val order = unboundOrders.maxBy(_.energy)
      // select orders on the left
      branch { post(order.selected === 1) } { post(order.selected === 0) }
    }
  } onSolution {
    // update visualization
    for (t <- tmin to tmax) {
      barPlot.setValue(t - tmin, varMapQty(t).value)
    }
    plot.addPoint(nbSol, obj.value)
    nbSol += 1
  }


  val stats = start()

  println(stats)
}
