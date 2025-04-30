package oscar.cbls.modeling.search

import oscar.cbls.core.computation.objective.Objective
import oscar.cbls.core.search.{Move, Neighborhood}
import oscar.cbls.lib.neighborhoods.combinator._
import oscar.cbls.visual.OscaRDisplay

/** This trait collects methods used to construct combinators, which are mechanisms used to combine
  * neighborhoods in order to construct complex search procedures. To allow chaining operations,
  * combinators themselves extend the [[Neighborhood]] type.
  */
trait Combinator {

  /** Constructs a combinator which performs a round-robin metaheuristic on a list of given
    * neighborhoods. This metaheuristic changes the currently explored neighborhood after it has
    * been invoked k times (where k is given as input) or if it is exhausted.
    *
    * @param robins
    *   A sequence of tuples (n, k) where n is a Neighborhood to be explored and k is the number of
    *   times n can be explored in a row.
    * @param tabu
    *   The number of iterations a neighborhood has to wait before being explored again (after a
    *   failure).
    * @param name
    *   The name of the neighborhood combinator.
    * @return
    *   An instance of a [[oscar.cbls.lib.neighborhoods.combinator.RoundRobin]] combinator.
    */
  def roundRobin(
    robins: Seq[(Neighborhood, Int)],
    tabu: Int = 1,
    name: String = "RoundRobin"
  ): RoundRobin = {
    RoundRobin(robins.toArray, tabu, name)
  }

  /** Constructs a combinator that triggers the redrawing of the display after a move is performed.
    *
    * @param n
    *   The Neighborhood used to explore the solution space.
    * @param display
    *   The display used to visualize the problem.
    * @return
    *   An instance of an [[oscar.cbls.lib.neighborhoods.combinator.UpdateDisplay]] combinator.
    */
  def updateDisplay(n: Neighborhood, display: OscaRDisplay): UpdateDisplay = {
    UpdateDisplay(n, display)
  }

  /** Constructs an [[oscar.cbls.lib.neighborhoods.combinator.Atomic]] combinator.
    *
    * Atomic combinator explores the neighborhood and squash the result of all taken moves in a
    * single one.
    *
    * @note
    *   The atomic combinator is not designed to be used as a left neighborhood of a
    *   [[oscar.cbls.lib.neighborhoods.combinator.DynAndThen]] combinator. However, you can achieve
    *   the same combined neighborhood by performing a search with your right neighborhood after
    *   performing a search with your atomic neighborhood.<br> In addition, if you set the
    *   `aggregateIntoSingleMove` to `true`, it is not compatible at all with the DynAndThen.
    * @param n
    *   The neighborhood to squash into a single move.
    * @param aggregateIntoSingleMove
    *   Whether the moves must be aggregated into a single one or not. If not, the returned
    *   neighborhood is a [[oscar.cbls.lib.neighborhoods.combinator.EjectionChains]] that uses only
    *   the input neighborhood. By default, set to `false`.
    * @param shouldStop
    *   Given the number of performed moves, determines whether we should continue searching for new
    *   moves.
    * @param name
    *   The name of the neighborhood combinator.
    * @return
    *   An instance of an [[oscar.cbls.lib.neighborhoods.combinator.Atomic]] combinator.
    */
  def atomic(
    n: Neighborhood,
    aggregateIntoSingleMove: Boolean = false,
    shouldStop: Int => Boolean = _ => false,
    name: String = "Atomic"
  ): Neighborhood = Atomic(n, aggregateIntoSingleMove, shouldStop, name)

  /** Constructs a [[oscar.cbls.lib.neighborhoods.combinator.Best]] combinator.
    *
    * During exploration this combinator generate a neighbor from each subNeighborhood a take the
    * one that improves the objective function the most.
    *
    * @param subNeighborhoods
    *   The Neighborhoods that this combinator handles. Those can be
    *   [[oscar.cbls.core.search.SimpleNeighborhood]] or other NeighborhoodCombinator.
    * @param name
    *   The name of the neighborhood combinator.
    * @return
    *   An instance of a [[oscar.cbls.lib.neighborhoods.combinator.Best]] combinator.
    */
  def best(subNeighborhoods: List[Neighborhood], name: String = "Best"): Best =
    Best(subNeighborhoods, name)

  /** Constructs a [[oscar.cbls.lib.neighborhoods.combinator.BestSlopeFirst]].
    *
    * This combinator sorts subNeighborhoods given their efficiency (`gain/timeSpent`) and
    * prioritizes the most efficient.
    *
    * @param subNeighborhoods
    *   The Neighborhood that this combinator handles. Those can be
    *   [[oscar.cbls.core.search.SimpleNeighborhood]] or other NeighborhoodCombinator.
    * @param tabuLength
    *   The number of iterations a neighborhood has to wait before being explored again.
    * @param overrideTabuOnFullExhaust
    *   If all the neighborhoods had been exhausted, this combinator tries the oldest tabu
    *   neighborhoods. This parameter must be lesser than `tabuLength` and is used to determine
    *   which tabu neighborhood can ben explored again.
    * @param refreshRate
    *   Each time the number of iteration is a multiple of `refreshRate`, the profiled statistics
    *   are reset.
    * @param name
    *   The name of the neighborhood combinator.
    * @return
    *   An instance of a [[oscar.cbls.lib.neighborhoods.combinator.BestSlopeFirst]].
    */
  def bestSlopeFirst(
    subNeighborhoods: List[Neighborhood],
    tabuLength: Int = 10,
    overrideTabuOnFullExhaust: Int = 9,
    refreshRate: Int = 100,
    name: String = "BestSlopFirst"
  ): BestSlopeFirst = {
    BestSlopeFirst(subNeighborhoods, tabuLength, overrideTabuOnFullExhaust, refreshRate, name)
  }

  /** Constructs a [[oscar.cbls.lib.neighborhoods.combinator.DoOnMove]] combinator.
    *
    * @param n
    *   A neighborhood.
    * @param procedureBeforeMove
    *   The procedure to execute just before the move is taken.
    * @param procedureAfterMove
    *   The procedure to execute just after the move is taken.
    * @param procedureOnNoMoveFound
    *   The procedure to execute when the called neighborhood returns no move.
    * @param name
    *   The name of the neighborhood combinator.
    * @return
    *   An instance of a [[oscar.cbls.lib.neighborhoods.combinator.DoOnMove]] combinator.
    */
  def doOnMove(
    n: Neighborhood,
    procedureBeforeMove: Move => Unit,
    procedureAfterMove: Move => Unit,
    procedureOnNoMoveFound: () => Unit,
    name: String = "DoOnMove"
  ): DoOnMove = DoOnMove(n, procedureBeforeMove, procedureAfterMove, procedureOnNoMoveFound, name)

  /** Constructs a variant of [[oscar.cbls.lib.neighborhoods.combinator.DoOnMove]] combinator that
    * only executes a custom unit function ''before'' the move is committed.
    *
    * @param n
    *   A neighborhood.
    * @param procedureBeforeMove
    *   The procedure to execute just before the move is taken.
    * @param name
    *   The name of the neighborhood combinator.
    * @return
    *   An instance of [[oscar.cbls.lib.neighborhoods.combinator.DoOnMove]] that only executes a
    *   custom unit function ''before'' the move is committed.
    */
  def beforeMove(
    n: Neighborhood,
    procedureBeforeMove: Move => Unit,
    name: String = "BeforeMove"
  ): DoOnMove = DoOnMove.beforeMove(n, procedureBeforeMove, name)

  /** Constructs a variant of [[oscar.cbls.lib.neighborhoods.combinator.DoOnMove]] combinator that
    * only executes a custom unit function ''after'' the move is committed.
    *
    * @param n
    *   A neighborhood.
    * @param procedureAfterMove
    *   The procedure to execute just after the move is taken.
    * @param name
    *   The name of the neighborhood combinator.
    * @return
    *   An instance of [[oscar.cbls.lib.neighborhoods.combinator.DoOnMove]] that only executes a
    *   custom unit function ''after'' the move is committed.
    */
  def afterMove(
    n: Neighborhood,
    procedureAfterMove: Move => Unit,
    name: String = "AfterMove"
  ): DoOnMove = DoOnMove.afterMove(n, procedureAfterMove, name)

  /** Constructs a [[oscar.cbls.lib.neighborhoods.combinator.DynAndThen]] combinator.
    *
    * Tries to find a cartesian product (X * Y) where X is a neighbor generated by left and Y a
    * neighbor generated by right depending on the left neighbor.
    *
    * @param left
    *   The left neighborhood; all moves delivered by this one will be considered.
    * @param right
    *   The moves returned by this Neighborhood must improve the main Objective, taking into account
    *   that a left move has been already applied.
    * @param name
    *   The name of the neighborhood combinator.
    * @return
    *   An instance of a [[oscar.cbls.lib.neighborhoods.combinator.DynAndThen]] combinator.
    */
  def dynAndThen(
    left: Neighborhood,
    right: Move => Neighborhood,
    name: String = "DynAndThen"
  ): DynAndThen = DynAndThen(left, right, name)

  /** Constructs a specialized [[oscar.cbls.lib.neighborhoods.combinator.DynAndThen]] combinator
    * where the right move does not depend on the left one.
    *
    * Tries to find a cartesian product (X * Y) where X is a neighbor generated by left and Y a
    * neighbor generated by right.
    *
    * @param left
    *   The first neighborhood to be explored.
    * @param right
    *   The second neighborhood to be explored.
    * @param name
    *   The name of the neighborhood combinator.
    * @return
    *   An instance of [[oscar.cbls.lib.neighborhoods.combinator.DynAndThen]] combinator where the
    *   right move does not depend on the left one.
    */
  def andThen(left: Neighborhood, right: Neighborhood, name: String = "AndThen"): DynAndThen =
    AndThen(left, right, name)

  /** Constructs a [[oscar.cbls.lib.neighborhoods.combinator.Exhaust]] combinator.
    *
    * Exhausts first neighborhood then second neighborhood.
    *
    * @param first
    *   The first Neighborhood to be exhausted by this combinator. It can be
    *   [[oscar.cbls.core.search.SimpleNeighborhood]] or other
    *   [[oscar.cbls.core.search.NeighborhoodCombinator]].
    * @param second
    *   The second Neighborhood to be exhausted by this combinator. It can be
    *   [[oscar.cbls.core.search.SimpleNeighborhood]] or other
    *   [[oscar.cbls.core.search.NeighborhoodCombinator]].
    * @param name
    *   The name of the neighborhood combinator.
    * @return
    *   An instance of [[oscar.cbls.lib.neighborhoods.combinator.Exhaust]] combinator.
    */
  def exhaust(first: Neighborhood, second: Neighborhood, name: String = "Exhaust"): Exhaust =
    Exhaust(first, second, name)

  /** Constructs a [[oscar.cbls.lib.neighborhoods.combinator.ExhaustBack]] combinator.
    *
    * Exhausts first neighborhood then second neighborhood, finaly goes back to first neighborhood.
    * Until no move can be found using either first or second neighborhood.
    *
    * @param first
    *   The first Neighborhood to be exhausted by this combinator. It can be
    *   [[oscar.cbls.core.search.SimpleNeighborhood]] or other
    *   [[oscar.cbls.core.search.NeighborhoodCombinator]].
    * @param second
    *   The second Neighborhood to be exhausted by this combinator. It can be
    *   [[oscar.cbls.core.search.SimpleNeighborhood]] or other
    *   [[oscar.cbls.core.search.NeighborhoodCombinator]].
    * @param name
    *   The name of the neighborhood combinator.
    * @return
    *   An instance of [[oscar.cbls.lib.neighborhoods.combinator.ExhaustBack]] combinator.
    */
  def exhaustBack(
    first: Neighborhood,
    second: Neighborhood,
    name: String = "ExhaustBack"
  ): ExhaustBack = ExhaustBack(first, second, name)

  /** Constructs a [[oscar.cbls.lib.neighborhoods.combinator.Restart]] combinator.
    *
    * @param n
    *   The neighborhood that performs the search.
    * @param neighborhoodForRestart
    *   The neighborhood used to modify the current solution. The move returned by this neighborhood
    *   is always commited.
    * @param maxConsecutiveRestartWithoutImprovement
    *   Maximum times in a row a cycle (restart + search) can end up without improvement of the
    *   objective function. When this value is reached, the best solution found is restored.
    * @param maxNumberOfRestartInTotal
    *   How many restart (improving or not) can be performed in total.
    * @param restartFromBest
    *   Whether the best solution has to be reloaded before a restart.
    * @param name
    *   The name of the combinator.
    * @return
    *   An instance of a [[oscar.cbls.lib.neighborhoods.combinator.Restart]] combinator.
    */
  def restart(
    n: Neighborhood,
    neighborhoodForRestart: Neighborhood,
    maxConsecutiveRestartWithoutImprovement: Int,
    maxNumberOfRestartInTotal: Int,
    restartFromBest: Boolean = false,
    name: String = "Restart"
  ): Restart = {
    Restart(
      n,
      neighborhoodForRestart,
      maxConsecutiveRestartWithoutImprovement,
      maxNumberOfRestartInTotal,
      restartFromBest,
      name
    )
  }

  /** Constructs an [[oscar.cbls.lib.neighborhoods.combinator.EjectionChains]] combinator.
    *
    * @note
    *   The ejections chains are not designed to be used as a left neighborhood of a
    *   [[oscar.cbls.lib.neighborhoods.combinator.DynAndThen]] combinator. However, you can achieve
    *   the same combined neighborhood by putting you right neighborhood as the last neighborhood of
    *   your ejection chain.
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
  def ejectionChains(
    nextNeighborhood: List[Move] => Option[Neighborhood],
    relaxedObjective: Option[Objective] = None,
    relaxedAcceptanceCriterion: Option[(Long, Long) => Boolean] = None,
    stopAtFirstImprovement: Boolean = false,
    name: String = "Ejection Chains"
  ): EjectionChains[Unit] = {
    EjectionChains(
      nextNeighborhood,
      relaxedObjective,
      relaxedAcceptanceCriterion,
      stopAtFirstImprovement,
      name
    )
  }

  /** Constructs a [[oscar.cbls.lib.neighborhoods.combinator.BackTrackedEjectionChains]] combinator.
    *
    * @note
    *   The ejections chains are not designed to be used as a left neighborhood of a
    *   [[oscar.cbls.lib.neighborhoods.combinator.DynAndThen]] combinator. However, you can achieve
    *   the same combined neighborhood by putting you right neighborhood as the last neighborhood of
    *   your ejection chain.
    *
    * @param firstNeighborhood
    *   The neighborhood defining the first layer of the exploration.
    * @param nextNeighborhood
    *   Function that, given the move selected from the first layer, returns a function used by the
    *   ejection chain to generate sub-neighborhood to explore.
    * @param relaxedObjective
    *   A relaxed objective that can be used to ease the exploration of the sub-neighborhoods.
    * @param relaxedAcceptanceCriterion
    *   A function that, given the initial objective value and a new objective value, returns if the
    *   move can be accepted.
    * @param stopAtFirstImprovement
    *   Whether the exploration can stop after the first improvement of the objective value.
    * @param name
    *   The name of the neighborhood.
    */
  def backTrackedEjectionChains(
    firstNeighborhood: Neighborhood,
    nextNeighborhood: Move => List[Move] => Option[Neighborhood],
    relaxedObjective: Option[Objective] = None,
    relaxedAcceptanceCriterion: Option[(Long, Long) => Boolean] = None,
    stopAtFirstImprovement: Boolean = false,
    name: String = "Ejection Chains"
  ): Neighborhood = {
    BackTrackedEjectionChains(
      firstNeighborhood,
      nextNeighborhood,
      relaxedObjective,
      relaxedAcceptanceCriterion,
      stopAtFirstImprovement,
      name
    )
  }

}
