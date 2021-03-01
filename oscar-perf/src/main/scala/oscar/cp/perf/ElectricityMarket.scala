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

package oscar.cp.perf


import oscar.cp._

import scala.collection.mutable.Map
import scala.io.Source
/** 
 * Game invented by Bertrand Cornelusse and Gilles Scouvart for the 10 years of n-Side:
 * Maximize the total market exchange such that demand and supply match at any time
 * @author Pierre Schaus pschaus@gmail.com
 */
object ElectricityMarket {
  def main(args: Array[String]): Unit = {

    implicit val cp = CPSolver()
    cp.silent = true
    case class Order(data: Array[Int]) {

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
    //val firstLine :: restLines = Source.fromFile("data/electricityMarket.txt").getLines.toList
    val firstLine::restLines = Source.fromFile("data/electricityMarketMedium.txt").getLines.toList
    val n = firstLine.toInt

    val orders = restLines.map(_.split(" ").map(_.toInt)).map(Order(_)).toArray
    val producers = orders.filter(_.qty > 0)
    val consumers = orders.filter(_.qty < 0)

    val tmin = orders.map(_.start).min
    val tmax = orders.map(_.end).max

    // one var for each time slot = the quantity exchanged on that slot
    val varMapQty = Map[Int, CPIntVar]()
    for (t <- tmin to tmax) {
      val prodUB = producers.map(_.qty.abs).sum
      varMapQty += (t -> CPIntVar(cp, 0 to prodUB))
    }
    // total amount of exchanged quantity
    val obj: CPIntVar = sum(tmin to tmax)(t => varMapQty(t))

    for (t <- tmin to tmax) {
      val prodVars = producers.filter(_.overlap(t)).map(_.selected)
      val prodQty = producers.filter(_.overlap(t)).map(_.qty)
      val consVars = consumers.filter(_.overlap(t)).map(_.selected)
      val consQty = consumers.filter(_.overlap(t)).map(_.qty.abs)

      cp.add(binaryKnapsack(prodVars, prodQty, varMapQty(t)), Strong)
      cp.add(binaryKnapsack(consVars, consQty, varMapQty(t)), Strong)
    }

    cp.maximize(obj)
    cp.search {
      if (allBounds(orders.map(_.selected))) noAlternative
      else {
        val unboundOrders = orders.filter(!_.bound)
        val order = unboundOrders.maxBy(_.energy)
        // select orders on the left
        branch { cp.add(order.selected === 1) } { cp.add(order.selected === 0) }
      }
    }
    println(cp.start())

	  
	}
}
