package oscar.cp.examples

import oscar.cp.{CPIntVar, CPModel, Weak, add, branch, minimize, noAlternative, onSolution, search, start, subCircuit, sum}
import oscar.util.selectMin

/**
  * Type of Traveling Salesman: find shortest tour of 10 cities among 20 possible cities
  * Given a distance matrix between 20 cities,
  * find the shortest tour visiting each city exactly once.
  *
  * @author Pierre Schaus  pschaus@gmail.com
  */
object SubCircuitVisu extends CPModel with App {
  // Data
  val nCities = 20
  val Cities = 0 until nCities
  val (distMatrix, coordinates) = TSPGenerator.randomInstance(nCities)

  for (i <- 0 until nCities) {
    println(distMatrix(i).mkString(","))
  }

  // Variables
  val succ = Array.fill(nCities)(CPIntVar(Cities))
  val totDist = CPIntVar(0 to distMatrix.flatten.sum)
  add(sum(Cities)(i => distMatrix(i)(succ(i))) === totDist)

  val nEdge = sum(Cities)(i => succ(i) ?!== i)
  add(nEdge >= 12)

  // Constraints
  add(subCircuit(succ),Weak)

  // Search heuristic
  minimize(totDist)

  search {
    // Select the not yet bound city with the smallest number of possible successors
    selectMin(Cities)(!succ(_).isBound)(succ(_).size) match {
      case None => noAlternative
      case Some(x) => {
        // Select the closest successors of the city x
        val v = selectMin(Cities)(succ(x).hasValue(_))(distMatrix(x)(_)).get
        branch(add(succ(x) === v))(add(succ(x) !== v))
      }
    }
  }

  // Visual Component
  val visual = new VisualTSP(coordinates, succ)

  var nSols = 0
  onSolution {
    nSols += 1
    visual.updateTour(nSols, totDist.value)
  }

  println(start())
}
