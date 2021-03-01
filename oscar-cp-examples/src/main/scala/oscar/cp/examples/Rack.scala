package oscar.cp.examples

import oscar.cp.{CPIntVar, CPModel, add, binaryFirstFail, element, lexLeq, minimize, start, sum}

/**
 * The problem consist of plugging a set of electronic cards into racks with electric connectors.
 * Each card is characterized by the power it requires, while each rack model is characterized by
 * the maximal power it can supply, its number of connectors and its price.
 * Each card plugged into a rack uses a connector.
 * The goal is to find an allocation of a given set of cards into the available racks at the smallest cost.
 *
 * @author Pierre Schaus pschaus@gmail.com
 */
object Rack extends CPModel with App {

  case class ModelType(power: Int, connectors: Int, price: Int)
  case class CardType(power: Int, quantity: Int)

  // Data
  val models = Array(ModelType(0, 0, 0), ModelType(150, 8, 150), ModelType(200, 16, 200))

  val cards = Array(CardType(20, 20), CardType(40, 8), CardType(50, 4), CardType(75, 2))

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
  val rack = Racks.map(r => CPIntVar(0 to nbModel)) // the model type in each rack
  val counters = Array.tabulate(nbRack, nbCard)((r, c) => CPIntVar(0 to cards(c).quantity)) //for each rack, how many cards of each type do you plug
  val cost = CPIntVar(0 to maxCost)

  for (r <- Racks) {
    // do not exceed the power capacity
    add(sum(Cards)(c => counters(r)(c) * cards(c).power) <= element(powers, rack(r)))
    // do not exceed the connectors capacity
    add(sum(Cards)(counters(r)(_)) <= element(connectors, rack(r)))
  }

  for (c <- Cards) {
    // all the cards of type c are placed
    add(sum(Racks)(counters(_)(c)) === cards(c).quantity)
  }

  add(sum(Racks)(r => element(prices, rack(r))) === cost)

  // symmetry breaking constraints
  for (r <- 1 until nbRack) {
    val var_r: Array[CPIntVar] = rack(r) :: (Cards.map(c => counters(r)(c)) toList) toArray
    val var_r_1: Array[CPIntVar] = rack(r - 1) :: (Cards.map(c => counters(r - 1)(c)) toList) toArray;
    add(lexLeq(var_r, var_r_1))
  }

  minimize(cost) search {
    binaryFirstFail(rack) ++ binaryFirstFail(counters.flatten.toSeq)
  }

  val stats = start()
  println(stats)
}
