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

/**
 * The problem consist of plugging a set of electronic cards into racks with electric connectors.
 * Each card is characterized by the power it requires, while each rack model is characterized by
 * the maximal power it can supply, its number of connectors and its price.
 * Each card plugged into a rack uses a connector.
 * The goal is to find an allocation of a given set of cards into the available racks at the smallest cost.
 *
 * @author Pierre Schaus pschaus@gmail.com
 */
object Rack {

  class ModelType(val power: Int, val connectors: Int, val price: Int)

  class CardType(val power: Int, val quantity: Int)

  def main(args: Array[String]): Unit = {

    // Data

    val models = Array(new ModelType(0, 0, 0),
      new ModelType(150, 8, 150),
      new ModelType(200, 16, 200))

    val cards = Array(
      new CardType(20, 20),
      new CardType(40, 8),
      new CardType(50, 6),
      new CardType(75, 7))

    val nbRack = 10
    val Racks = 0 until nbRack
    val nbModel = models.size
    val Models = 0 until nbModel
    val nbCard = cards.size
    val Cards = 0 until nbCard
    val powers = Models.map(models(_).power)
    val connectors = Models.map(models(_).connectors)
    val prices = Models.map(models(_).price)
    val maxPrice = prices.max
    val maxConnector = connectors.max
    val maxCost = nbRack * maxPrice

    // CP Model

    val cp = CPSolver()
    val rack = Racks.map(r => CPIntVar(cp, 0 to nbModel)) // the model type in each rack
    val counters = Array.tabulate(nbRack, nbCard)((r, c) => CPIntVar(cp, 0 to cards(c).quantity)) //for each rack, how many cards of each type do you plug
    val cost = CPIntVar(cp, 0 to maxCost)

    for (r <- Racks) {
      // do not exceed the power capacity
      cp.add(sum(Cards)(c => counters(r)(c) * cards(c).power) <= element(powers, rack(r)))
      // do not exceed the connectors capacity
      cp.add(sum(Cards)(counters(r)(_)) <= element(connectors, rack(r)))
    }

    for (c <- Cards) {
      // all the cards of type c are placed
      cp.add(sum(Racks)(counters(_)(c)) === cards(c).quantity)
    }

    cp.add(sum(Racks)(r => element(prices, rack(r))) === cost)

    // symmetry breaking constraints
    for (r <- 1 until nbRack) {
      val var_r: Array[CPIntVar] = rack(r) :: (Cards.map(c => counters(r)(c)) toList) toArray
      val var_r_1: Array[CPIntVar] = rack(r - 1) :: (Cards.map(c => counters(r - 1)(c)) toList) toArray;
      cp.add(lexLeq(var_r, var_r_1))
    }

    cp.minimize(cost)
    cp.search {
      binaryFirstFail(rack) ++ binaryFirstFail(counters.flatten.toSeq)
    }
    println(cp.start())


  }

}
