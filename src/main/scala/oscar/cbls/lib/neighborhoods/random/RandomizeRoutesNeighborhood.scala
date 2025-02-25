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

package oscar.cbls.lib.neighborhoods.random

import oscar.cbls.algo.sequence.IntSequence
import oscar.cbls.core.computation.Solution
import oscar.cbls.core.computation.objective.Exploration
import oscar.cbls.core.search.{MoveFound, NoMoveFound, SimpleNeighborhood}
import oscar.cbls.lib.neighborhoods.AssignSeqMove
import oscar.cbls.modeling.routing.VRS

import scala.util.Random

/** Factory for [[RandomizeRoutesNeighborhood]] */
object RandomizeRoutesNeighborhood {

  /** Creates a Neighborhood that tries random sequences to improve the objective.
    *
    * @param vrs
    *   The vehicle routing structure on which the neighborhood operates.
    * @param numTriesWithoutImprovement.
    *   How many tries to find an improving random solution.
    * @param generateRandomRoute
    *   How to generate a random routes. The returned routes are supposed to respect routing
    *   conventions.
    */
  def apply(vrs: VRS, generateRandomRoute: () => IntSequence, numTriesWithoutImprovement: Int = 1) =
    new RandomizeRoutesNeighborhood(vrs, generateRandomRoute, numTriesWithoutImprovement)

  /** Creates a RandomizeRoutesNeighborhood. The randomized routes are obtained by shuffling the
    * routed node of the current routes.
    *
    * @param vrs
    *   The vehicle routing structure on which the neighborhood operates.
    * @param numTriesWithoutImprovement.
    *   How many tries to find an improving random solution.
    * @param rng
    *   The random number generator used to shuffle the routed nodes.
    */
  def shuffle(
    vrs: VRS,
    numTriesWithoutImprovement: Int = 1,
    rng: Random = Random
  ): RandomizeRoutesNeighborhood = {
    val generateRandomRoute = () => vrs.shuffleRoutes(rng)
    RandomizeRoutesNeighborhood(vrs, generateRandomRoute, numTriesWithoutImprovement)
  }

  /** Creates a RandomizeRoutesNeighborhood. This neighborhood generates random routes.
    *
    * @param vrs
    *   The vehicle routing structure on which the neighborhood operates.
    * @param numNodeToInsert
    *   How many nodes (vehicles excluded) has to be routed in the generated routes.
    * @param numTriesWithoutImprovement.
    *   How many tries to find an improving random solution.
    * @param rng
    *   The random number generator used to generate a random valid route.
    */
  def randomize(
    vrs: VRS,
    numNodeToInsert: () => Int,
    numTriesWithoutImprovement: Int = 1,
    rng: Random = Random
  ): RandomizeRoutesNeighborhood = {
    val generateRandomRoute = () => vrs.generateValidRandomRoute(numNodeToInsert(), rng)
    RandomizeRoutesNeighborhood(vrs, generateRandomRoute, numTriesWithoutImprovement)
  }

  /** Creates a RandomizeRoutesNeighborhood. This neighborhood randomly removes nodes from the
    * current route.
    *
    * @param vrs
    *   The vehicle routing structure on which the neighborhood operates.
    * @param numNodesToRemove
    *   How many nodes has to be removed.
    * @param numTriesWithoutImprovement.
    *   How many tries to find an improving random solution.
    * @param rng
    *   The random number generator used to select nodes to remove.
    */
  def removeNodes(
    vrs: VRS,
    numNodesToRemove: () => Int,
    numTriesWithoutImprovement: Int = 1,
    rng: Random = Random
  ): RandomizeRoutesNeighborhood = {
    val generateRandomRoute = () => vrs.removeRandomNodes(numNodesToRemove(), rng)
    RandomizeRoutesNeighborhood(vrs, generateRandomRoute, numTriesWithoutImprovement)
  }
}

/** Neighborhood that tries random sequences to improve the objective.
  *
  * @note
  *   The moves used by this neighborhood call the
  *   [[oscar.cbls.core.computation.seq.SeqVariable.setValue]] method, which cannot be used if a
  *   checkpoint is defined. Be careful when this Neighborhood is used. This neighborhood is
  *   designed to works as the restart neighborhood of
  *   [[oscar.cbls.lib.neighborhoods.metaheuristics.Restart]].
  *
  * @param vrs
  *   The vehicle routing structure on which the neighborhood operates.
  * @param numTriesWithoutImprovement.
  *   How many tries to find an improving random solution.
  * @param generateRandomRoute
  *   How to generate a random routes. The returned routes are supposed to respect routing
  *   conventions.
  */
class RandomizeRoutesNeighborhood(
  vrs: VRS,
  generateRandomRoute: () => IntSequence,
  numTriesWithoutImprovement: Int
) extends SimpleNeighborhood[AssignSeqMove]("Randomize routes") {

  private var tries: Int = 0

  override protected def exploreNeighborhood(exploration: Exploration[AssignSeqMove]): Unit = {
    val currentSolution: Solution = vrs.store.extractSolution()

    while (tries < numTriesWithoutImprovement) {
      val newSeq: IntSequence = generateRandomRoute()
      vrs.routes := newSeq
      searchProfiler().foreach(_.neighborSelected())

      exploration.checkNeighborWP(objValue => AssignSeqMove(vrs.routes, newSeq, objValue, name))

      currentSolution.restoreSolution()

      exploration.toReturn match {
        case NoMoveFound  => tries += 1
        case _: MoveFound => tries = numTriesWithoutImprovement
      }
    }
    reset()
  }

  override def doMove(move: AssignSeqMove): Unit = move.commit()

  override def reset(): Unit = tries = 0
}
