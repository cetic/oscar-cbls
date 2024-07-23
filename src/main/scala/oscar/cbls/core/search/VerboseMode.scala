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

package oscar.cbls.core.search

import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.objective.Objective
import oscar.cbls.util.PrettyPrinting

import scala.collection.mutable
import scala.concurrent.duration.Duration

object VerboseMode {
  def unapply(verboseMode: VerboseMode): Option[Int] = Some(verboseMode.verbosityLevel)

  def apply(verbosityLevel: Int): VerboseMode = {
    new VerboseMode(verbosityLevel)
  }
}

/** This class handles the verbosity aspect of the search, meaning the information displayed during
  * the search.
  *
  * '''NOTE''' : It has no impact on the profiling.
  *
  * It's behavior depends on the verbosityLevel parameter. The level varies from 0 to 4 :
  *   - 0 : Nothing is displayed
  *   - 1 : Every SUMMARIZED_MOVE_DELAY milli-second a summary of the last taken moves is printed.
  *   - 2 : Every taken moves are printed
  *   - 3 : Every taken moves are printed and the exploration progress (start, end) as well
  *   - 4 : Every explored moves are printed
  *
  * @param verbosityLevel
  *   The verbosity level of the search (see full class description for more information)
  */
class VerboseMode(val verbosityLevel: Int) {

  require(verbosityLevel >= 0 && verbosityLevel <= 4)

  private var summarizedLastPrint: Long                    = System.currentTimeMillis()
  private var summarizedLastValue: Long                    = Long.MaxValue
  private var summarizedMove: mutable.HashMap[String, Int] = mutable.HashMap.empty
  private val SUMMARIZED_MOVE_DELAY: Long                  = 100L
  private var searchStartAt: Long                          = -1

  /** Displays some information about the starting search.
    *
    * Nothing is displayed if verbosityLevel is == 0
    */
  @inline
  final def searchStarted(objective: Objective, objValue: IntVariable): Unit = {
    searchStartAt = System.currentTimeMillis()
    if (verbosityLevel != 0) {
      println(s"Starting local search : $objective $objValue")
      println(s"Start time : ${java.time.LocalDateTime.now}")
    }
    if (verbosityLevel == 1) summarizedLastPrint = System.currentTimeMillis()
  }

  /** Displays the exploration result of a move.
    *
    * Nothing is displayed if verbosityLevel is < 4
    *
    * There are three distinct parameters. In some Objective definition you could save a move that
    * is not the best. Or decide to not save this particular new best value for some reason.
    *
    * @param move
    *   The explored move
    * @param valid
    *   Whether or not the move is accepted by the acceptance criterion. Default = false
    * @param newBest
    *   Whether or not the move has the best recorded value of the search. Default = false
    * @param saved
    *   Whether or not the move has been saved. Default = false
    */
  @inline
  final def moveExplored(
    move: () => Move,
    valid: Boolean = false,
    newBest: Boolean = false,
    saved: Boolean = false
  ): Unit = {
    if (verbosityLevel >= 4) {
      val acceptedAsString: String = if (valid) "valid move" else "invalid move"
      val newBestAsString: String  = if (newBest) "new best" else "not the new best"
      val savedAsString: String    = if (saved) "saved" else "not saved"
      println(s"Explored ${move()}, $newBestAsString, $acceptedAsString, $savedAsString")
    }
  }

  /** Displays the start of an exploration of a neighborhood
    *
    * Nothing is displayed if verbosityLevel is < 3
    *
    * @param neighborhoodName
    *   The explored Neighborhood
    */
  @inline
  final def explorationStarted(neighborhoodName: String): Unit = {
    if (verbosityLevel >= 3)
      println(s"$neighborhoodName : start exploration")
  }

  /** Displays the exploration result of a neighborhood
    *
    * Nothing is displayed if verbosityLevel is < 3
    *
    * @param neighborhood
    *   The explored Neighborhood
    * @param searchResult
    *   The exploration result
    */
  @inline
  final def explorationEnded(neighborhood: Neighborhood, searchResult: SearchResult): Unit = {
    if (verbosityLevel >= 3)
      searchResult match {
        case NoMoveFound   => println(s"$neighborhood : No move found")
        case mf: MoveFound => println(s"$neighborhood : Move found ${mf.move}")
      }
  }

  /** Displays the taken moves if the verbosity level is at least 1.
    *
    * Nothing is displayed if verbosityLevel is < 1
    *
    * If verbosityLevel is 1, a summarized of taken moves will be displayed every 0.1 second. Else
    * if verbosityLevel is 2 or higher, each taken move will be displayed individually.
    *
    * @param move
    *   The taken move
    * @param newValue
    *   The new value of the objective function
    * @param prevValue
    *   The previous value of the objective function
    * @param bestValue
    *   The best value of the objective function so far
    * @param newBestValue
    *   Whether or not this new value is the new best value
    * @param forcePrint
    *   Whether or not we should display the summarize even if the delay hasn't passed since last
    *   one
    */
  @inline
  final def moveTaken(
    move: Move,
    newValue: Long,
    prevValue: Long,
    bestValue: Long,
    newBestValue: Boolean,
    forcePrint: Boolean = false
  ): Unit = {
    if (verbosityLevel >= 2) {
      val prefix_1 = prefix1(newValue, prevValue)
      val prefix_2 = prefix2(newBestValue, newValue, bestValue)
      println(s"$prefix_1 $prefix_2 $newValue\t$move")
    } else if (verbosityLevel == 1) {
      if (System.currentTimeMillis() - summarizedLastPrint > SUMMARIZED_MOVE_DELAY || forcePrint) {
        val prefix_1 = prefix1(newValue, summarizedLastValue)
        val prefix_2 = prefix2(newBestValue, newValue, bestValue)
        println(
          s"$prefix_1 $prefix_2 $newValue\t${summarizedMove.map(sm => s"${sm._1}:${sm._2}").mkString("\t")}"
        )
        summarizedMove = mutable.HashMap.empty
        summarizedLastValue = newValue
        summarizedLastPrint = System.currentTimeMillis()
      } else {
        val neighborhoodName = move.neighborhood.name
        if (summarizedMove.contains(neighborhoodName))
          summarizedMove(neighborhoodName) = summarizedMove(neighborhoodName) + 1
        else summarizedMove += neighborhoodName -> 1
      }
    }
  }

  /** Prints the summary of the search and, if verbosity level is equal to 1, prints the last taken
    * moves.
    */
  @inline
  final def searchEnded(endValue: Long, moveCount: Int): Unit = {
    if (verbosityLevel == 1) {
      val prefix_1 =
        if (endValue < summarizedLastValue) '-'
        else if (endValue == summarizedLastValue) '='
        else '+'
      val prefix_2 = '#'
      println(
        s"$prefix_1 $prefix_2 $endValue\t${summarizedMove.map(sm => s"${sm._1}:${sm._2}").mkString("\t")}"
      )
    }
    if (verbosityLevel >= 1) {
      val totalDurationMs = System.currentTimeMillis() - searchStartAt
      val duration        = Duration.fromNanos(totalDurationMs)
      println(s"No more move found after $moveCount it, duration:${PrettyPrinting(duration)}")
    }
  }

  private def prefix1(valueToDisplay: Long, lastDisplayedValue: Long): Char = {
    if (valueToDisplay < lastDisplayedValue) '-'
    else if (valueToDisplay == lastDisplayedValue) '='
    else '+'
  }
  private def prefix2(newBestValue: Boolean, newValue: Long, bestValue: Long): Char = {
    if (newBestValue) '#'
    else if (newValue == bestValue) '°'
    else ' '
  }

}
