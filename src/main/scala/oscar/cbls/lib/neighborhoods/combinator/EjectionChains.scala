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

package oscar.cbls.lib.neighborhoods.combinator

import oscar.cbls.core.computation.objective.{CustomObjective, Objective}
import oscar.cbls.core.computation.{GlobalCheckpoint, Store}
import oscar.cbls.core.search._
import oscar.cbls.core.search.profiling.CompositionProfiler

/** Companion object of the [[EjectionChains]] class.
  *
  * @note
  *   The ejections chains are not designed to be used as a left neighborhood of a [[DynAndThen]]
  *   combinator. However, you can achieve the same combined neighborhood by putting you right
  *   neighborhood as the last neighborhood of your ejection chain.
  */
object EjectionChains {

  /** Returns an instance of the [[EjectionChains]] combinator which composes a list of move that
    * individually can worsen the objective function but put together improve the objective.
    *
    * @param nextNeighborhood
    *   Function that, given previous committed moves, returns the next neighborhood to explore.
    * @param relaxedObjective
    *   A relaxed objective that can be used to ease the exploration of the sub-neighborhoods.
    * @param relaxedAcceptanceCriterion
    *   A function that, given the initial objective value and a new objective value, returns if the
    *   move can be accepted.
    * @param stopAtFirstImprovement
    *   Whether the exploration can stop after the first improvement of the objective value.
    * @param name
    *   The name of the neighborhood combinator.
    */
  def apply(
    nextNeighborhood: List[Move] => Option[Neighborhood],
    relaxedObjective: Option[Objective] = None,
    relaxedAcceptanceCriterion: Option[(Long, Long) => Boolean] = None,
    stopAtFirstImprovement: Boolean = false,
    name: String = "Ejection Chains"
  ): EjectionChains[Unit] = {
    new EjectionChains[Unit](
      (),
      (_, m) => nextNeighborhood(m).map(((), _)),
      relaxedObjective,
      relaxedAcceptanceCriterion,
      stopAtFirstImprovement,
      name
    )
  }

  /** Returns an instance of Ejection that use an accumulator to generate the neighborhoods to
    * explore.
    *
    * @param initAcc
    *   Initial value of the accumulator used to generate the next neighborhood.
    * @param nextNeighborhood
    *   Function that, given the previous committed moves and an accumulator, returns the next
    *   neighborhood to explore.
    * @param relaxedObjective
    *   A relaxed objective that can be used to ease the exploration of the sub-neighborhoods.
    * @param relaxedAcceptanceCriterion
    *   A function that, given the initial objective value and a new objective value, returns if the
    *   move can be accepted.
    * @param stopAtFirstImprovement
    *   Whether the exploration can stop after the first improvement of the objective value.
    * @param name
    *   The name of the neighborhood combinator.
    * @tparam T
    *   The type of the accumulator.
    */
  def fold[T](initAcc: T)(
    nextNeighborhood: (T, List[Move]) => Option[(T, Neighborhood)],
    relaxedObjective: Option[Objective] = None,
    relaxedAcceptanceCriterion: Option[(Long, Long) => Boolean] = None,
    stopAtFirstImprovement: Boolean = false,
    name: String = "Ejection Chains"
  ): EjectionChains[T] = {
    new EjectionChains[T](
      initAcc,
      nextNeighborhood,
      relaxedObjective,
      relaxedAcceptanceCriterion,
      stopAtFirstImprovement,
      name
    )
  }
}

/** Combinator that implements ejections chains. This combinator composes a bunch of neighborhood
  * that individually can worsen the objective function but put together improve the objective.
  *
  * @note
  *   The ejections chains are not designed to be used as a left neighborhood of a [[DynAndThen]]
  *   combinator. However, you can achieve the same combined neighborhood by putting you right
  *   neighborhood as the last neighborhood of your ejection chain.
  *
  * @param initAcc
  *   Initial value of the accumulator used to generate the next neighborhood.
  * @param nextNeighborhood
  *   Function that, given the previous committed moves and optionally an accumulator, returns the
  *   next neighborhood to explore.
  * @param relaxedObjective
  *   A relaxed objective that can be used to ease the exploration of the sub-neighborhoods.
  * @param relaxedAcceptanceCriterion
  *   A function that, given the initial objective value and a new objective value, returns if the
  *   move can be accepted.
  * @param stopAtFirstImprovement
  *   Whether the exploration can stop after the first improvement of the objective value.
  * @param name
  *   The name of the neighborhood combinator.
  * @tparam T
  *   The type of the accumulator.
  */
class EjectionChains[T](
  initAcc: T,
  nextNeighborhood: (T, List[Move]) => Option[(T, Neighborhood)],
  relaxedObjective: Option[Objective],
  relaxedAcceptanceCriterion: Option[(Long, Long) => Boolean],
  stopAtFirstImprovement: Boolean = false,
  name: String
) extends NeighborhoodCombinator(name, List()) {

  private var _compositionProfilerOpt: Option[CompositionProfiler] = None

  override def searchProfiler(): Option[CompositionProfiler] = _compositionProfilerOpt

  override def profileSearch(): Unit = {
    _compositionProfilerOpt match {
      case None =>
        _compositionProfilerOpt = Some(CompositionProfiler(this))
        subNeighborhoods.foreach(_.profileSearch())
      case _ => ;
    }

  }

  override protected[this] def exploreCombinator(objective: Objective): SearchResult = {
    val model: Store                        = objective.objValue.model
    val initialObjValue: Long               = objective.objValue.value()
    val initialCheckpoint: GlobalCheckpoint = model.createCheckpoint()

    val relaxedObj: Objective = (relaxedObjective, relaxedAcceptanceCriterion) match {
      case (Some(obj), Some(criterion)) => CustomObjective(obj.objValue, criterion, obj.mustBeZero)
      case (None, Some(criterion)) =>
        CustomObjective(objective.objValue, criterion, objective.mustBeZero)
      case (Some(obj), None) => obj
      case (None, None)      => objective
    }

    var currentObjValue: Long     = relaxedObj.objValue.value()
    var previousMoves: List[Move] = List.empty
    var continue: Boolean         = true
    var acc: T                    = initAcc
    var lastFound: SearchResult   = NoMoveFound
    while (continue) {

      if (stopAtFirstImprovement) {
        if (objective.isValueNewBest(initialObjValue, currentObjValue)) {
          rollBackToGlobalCheckpoint(initialCheckpoint, objective, initialObjValue)
          if (previousMoves.nonEmpty)
            return MoveFound(CompositeMovesWithList(previousMoves.reverse, currentObjValue, name))
          else
            return NoMoveFound
        }
      }

      nextNeighborhood(acc, previousMoves) match {
        case None => continue = false
        case Some((nextAcc, neighborhood)) =>
          acc = nextAcc
          _compositionProfilerOpt.foreach(profiler => {
            neighborhood.profileSearch()
            profiler.setCurrentRight(neighborhood.searchProfiler().get)
          })
          neighborhood.getMove(relaxedObj) match {
            case NoMoveFound =>
              _compositionProfilerOpt.foreach(_.mergeDynProfiler())
              continue = false
            case mf: MoveFound =>
              if (mf equals lastFound) {
                continue = false
              } else {
                lastFound = mf
                commitMove(objective, mf.move)
                previousMoves = mf.move :: previousMoves
                currentObjValue = mf.move.objValueAfter
                _compositionProfilerOpt.foreach(_.mergeDynProfiler())
              }
          }
      }
    }
    rollBackToGlobalCheckpoint(initialCheckpoint, objective, initialObjValue)

    if (previousMoves.nonEmpty && objective.isValueNewBest(initialObjValue, currentObjValue)) {
      MoveFound(CompositeMovesWithList(previousMoves.reverse, currentObjValue, name))
    } else {
      NoMoveFound
    }
  }

}
