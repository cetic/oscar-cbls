package oscar.cbls.lib.neighborhoods.combinator

import oscar.cbls.core.computation.objective.Objective
import oscar.cbls.core.search.{Neighborhood, NeighborhoodCombinator, NoMoveFound, SearchResult}

import scala.concurrent.duration.Duration

/** Companion object of the [[SoftTimeout]] class. */
object SoftTimeout {

  /** A combinator that implements a soft Timeout; ie the search will only be stopped between
    * neighborhood explorations. If you use very long-lasting neighborhood, this is not very helpful
    * for you.
    *
    * If you use a fast neighborhood that searches in less than the granularity of
    * [[https://docs.oracle.com/en/java/javase/21/docs/api/java.base/java/lang/System.html#nanoTime() System.nanoTime()]]
    * on your JVM, this variant might fail to work properly, and it is advised that you use
    * [[SoftTimeoutGlobal]] instead.
    *
    * @param neighborhood
    *   A neighborhood.
    * @param maxTimeBudgetNanosecond
    *   The maximal amount of time that the neighborhood is allowed to search, summing up on all its
    *   explorations.
    */
  def apply(neighborhood: Neighborhood, maxTimeBudgetNanosecond: Long): SoftTimeoutPerMove =
    new SoftTimeoutPerMove(neighborhood, maxTimeBudgetNanosecond)

  /** A combinator that implements a soft Timeout; ie the search will only be stopped between
    * neighborhood explorations. If you use very long-lasting neighborhood, this is not very helpful
    * for you.
    *
    * This variant consider the amount of time between the first exploration of the neighborhood and
    * the start of each exploration. If you perform additional actions in between explorations, such
    * as visualization or saving result, this time is also considered in te timeout. If this is an
    * issue for you, you should use the [[SoftTimeoutPerMove]] combinator instead
    *
    * @param neighborhood
    *   A neighborhood.
    * @param maxTimeBudgetNanosecond
    *   The maximal amount of time that the neighborhood is allowed to search, taken as the time
    *   difference between the first exploration and the start of each exploration .
    */
  def global(neighborhood: Neighborhood, maxTimeBudgetNanosecond: Long): SoftTimeoutGlobal =
    new SoftTimeoutGlobal(neighborhood, maxTimeBudgetNanosecond)

}

/** A combinator that implements a soft Timeout; ie the search will only be stopped between
  * neighborhood explorations. If you use very long-lasting neighborhood, this is not very helpful
  * for you.
  *
  * If you use a fast neighborhood that searches in less than the granularity of
  * [[https://docs.oracle.com/en/java/javase/21/docs/api/java.base/java/lang/System.html#nanoTime() System.nanoTime()]]
    * on your JVM, this variant might fail to work properly, and it is advised that you use
    * [[SoftTimeoutGlobal]] instead.
  *
  * @param neighborhood
  *   a neighborhood
  * @param maxTimeBudgetNanosecond
  *   the maximal amount of time that the neighborhood is allowed to search, summing up on all its
  *   explorations.
  */
class SoftTimeoutPerMove(neighborhood: Neighborhood, maxTimeBudgetNanosecond: Long)
    extends NeighborhoodCombinator(
      "SoftTimeoutPerMove(timeout=[" + Duration.fromNanos(
        maxTimeBudgetNanosecond
      ) + "] on " + neighborhood + ")",
      List(neighborhood)
    ) {

  private var remainingBudgetNanoSecond: Long = maxTimeBudgetNanosecond

  override protected[this] def exploreCombinator(objective: Objective): SearchResult = {
    if (remainingBudgetNanoSecond < 0) {
      if (verbosityLevel >= 2) println("" + this + ": timeout")
      NoMoveFound
    } else {
      val startTime = System.nanoTime()
      val toReturn  = neighborhood.getMove(objective)
      val endTime   = System.nanoTime()
      remainingBudgetNanoSecond -= (endTime - startTime)
      toReturn
    }
  }

  override def reset(): Unit = {
    remainingBudgetNanoSecond = maxTimeBudgetNanosecond
  }
}

/** A combinator that implements a soft Timeout; ie the search will only be stopped between
  * neighborhood explorations. If you use very long-lasting neighborhood, this is not very helpful
  * for you.
  *
  * This variant consider the amount of time between the first exploration of the neighborhood and
  * the start of each exploration. If you perform additional actions in between explorations, such
  * as visualization or saving result, this time is also considered in te timeout. If this is an
  * issue for you, you should use the [[SoftTimeoutPerMove]] combinator instead
  *
  * @param neighborhood
  *   A neighborhood.
  * @param maxTimeBudgetNanosecond
  *   The maximal amount of time that the neighborhood is allowed to search, taken as the time
  *   difference between the first exploration and the start of each exploration.
  */
class SoftTimeoutGlobal(neighborhood: Neighborhood, maxTimeBudgetNanosecond: Long)
    extends NeighborhoodCombinator(
      "SoftTimeoutGlobal(timeout=[" + Duration.fromNanos(
        maxTimeBudgetNanosecond
      ) + "] on " + neighborhood + ")",
      List(neighborhood)
    ) {

  require(
    maxTimeBudgetNanosecond > 0,
    "maxTimeBudgetNanosecond must be greater than zero:" + maxTimeBudgetNanosecond
  )
  private var maxEndTime: Long = -1

  override protected[this] def exploreCombinator(objective: Objective): SearchResult = {
    if (maxEndTime < 0) maxEndTime = System.nanoTime() + maxTimeBudgetNanosecond
    if (maxEndTime < System.nanoTime()) {
      if (verbosityLevel >= 2) println("" + this + ": timeout")
      NoMoveFound
    } else {
      neighborhood.getMove(objective)
    }
  }

  override def reset(): Unit = {
    maxEndTime = -1
  }
}
