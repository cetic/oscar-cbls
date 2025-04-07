// OscaR is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 2.1 of the License, or
// (at your option) any later version.
//
// OscaR is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License  for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with OscaR.
// If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html

package oscar.cbls.examples

// Imports to model a problem and a search procedure
import oscar.cbls._
import oscar.cbls.modeling.{Invariants, Neighborhoods}

// Object to generate random data
import oscar.cbls.algo.generator.WarehouseLocationGenerator

object WLPBeginnerModelingExample {

  def main(args: Array[String]): Unit = {

    // Model definition
    val m: Model = model("WLP example")

    // Problem instance parameters
    val nFacilities    = 300
    val deliveryPoints = 1000
    val (fixedCosts, _, _, distanceMatrix, _) =
      WarehouseLocationGenerator.generateRandomWLP(nFacilities, deliveryPoints)

    // Decision variables
    val facilitiesVariables =
      Array.tabulate(nFacilities)(f => m.binaryVar(0, s"facility_${f}_open"))

    // Derived variables (using invariants)
    val openFacilities =
      Invariants.logic.filter(facilitiesVariables, name = "Set of open facilities")(m)

    val distancesToNearestOpenFacility = Array.tabulate(deliveryPoints)(d =>
      Invariants.minMax
        .minOfConstants(
          distanceMatrix(d),
          openFacilities,
          name = s"Distance of $d to nearest facility"
        )(m)
    )

    val objExpr =
      sum(distancesToNearestOpenFacility)(m) + partialSumOfConstants(
        fixedCosts,
        indices = openFacilities
      )(m)

    // Objective function
    // The second term is obviously unnecessary, it's here to demonstrate how to add constant
    val obj = m.minimize(objExpr + int2IntConst(2)(m))

    // Closes the model
    // No variable or invariant can be added past that point
    m.close()

    val search: Neighborhood =
      Neighborhoods.assign(facilitiesVariables, varsDomain = (_, _) => List(0, 1))

    // Profiling initialization and setting up verbosity
    search.profileSearch()
    search.verbosityLevel = 1

    // Performs the search
    search.doAllMoves(obj)

    // Displays results
    search.displayProfiling()
    println(s"Open facilities: ${openFacilities.value()}")
    println(s"Number of open facilities: ${openFacilities.value().size}")
    println(s"Best objective: ${obj.objValue}")

  }
}
