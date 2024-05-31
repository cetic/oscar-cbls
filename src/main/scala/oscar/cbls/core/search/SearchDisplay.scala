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

import oscar.cbls.util.PrettyPrinting

import scala.collection.mutable
import scala.concurrent.duration.Duration

object SearchDisplay {
  def unapply(searchDisplayer: SearchDisplay): Option[Int] = Some(searchDisplayer.verbosityLevel)

  def apply(verbosityLevel: Int): SearchDisplay = {
    new SearchDisplay(verbosityLevel)
  }
}

class SearchDisplay(val verbosityLevel: Int) {

  private var summarizedLastPrint: Long                    = System.currentTimeMillis()
  private var summarizedLastValue: Long                    = Long.MaxValue
  private var summarizedMove: mutable.HashMap[String, Int] = mutable.HashMap.empty
  private var searchStartAt: Long                          = System.currentTimeMillis()

  /** Displays some information about the starting search.
    *
    * Nothing is displayed if verbosityLevel is == 0
    */
  def searchStarted(detailedObj: String): Unit = {
    if (verbosityLevel != 0) {
      println(s"start doAllMove at ${java.time.LocalDateTime.now}")
      println(s"initial objective function:$detailedObj")
    }
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
    * @param accepted
    *   whether or not the move is accepted by the acceptance criterion
    * @param newBest
    *   whether or not the move has the best recorded value of the search
    * @param saved
    *   whether or not the move has been saved
    */
  def moveExplored(move: Move, accepted: Boolean, newBest: Boolean, saved: Boolean): Unit = {
    if (verbosityLevel >= 4) {
      val acceptedAsString: String = if (accepted) "accepted" else "not accepted"
      val newBestAsString: String  = if (newBest) "new best" else "not the new best"
      val savedAsString: String    = if (saved) "saved" else "not saved"
      println(s"Explored $move, $newBestAsString, $acceptedAsString, $savedAsString")
    }
  }

  /** Displays the start of an exploration of a neighborhood
    *
    * Nothing is displayed if verbosityLevel is < 3
    *
    * @param neighborhoodName
    *   The explored Neighborhood
    */
  def startExploration(neighborhoodName: String): Unit = {
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
  def neighborhoodExplored(neighborhood: SimpleNeighborhood, searchResult: SearchResult): Unit = {
    if (verbosityLevel >= 3)
      searchResult match {
        case NoMoveFound   => println(s"$neighborhood : No move found")
        case mf: MoveFound => println(s"$neighborhood : Move found ${mf.move}")
      }
  }

  /** Displays the taken moves if the verbosity level is 1 or above.
    *
    * Nothing is displayed if verbosityLevel is < 1
    *
    * If verbosityLevel is 1, a summarized of taken moves will be displayed every 0.1 second. Else
    * if verbosityLevel is 2 or higher, each taken move will be displayed individually.
    *
    * @param neighborhoodName
    *   The neighborhood that defined the taken move
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
  def moveTaken(
    neighborhoodName: String,
    move: Move,
    newValue: Long,
    prevValue: Long,
    bestValue: Long,
    newBestValue: Boolean,
    forcePrint: Boolean = false
  ): Unit = {
    if (verbosityLevel >= 2) {
      val prefix_1 = if (newValue < prevValue) '-' else if (newValue == prevValue) '=' else '+'
      val prefix_2 = if (newBestValue) '#' else if (newValue == bestValue) '°' else ' '
      println(s"$prefix_1 $prefix_2 $newValue\t$neighborhoodName : $move")
    } else if (verbosityLevel == 1) {
      if (System.currentTimeMillis() - summarizedLastPrint > 100L || forcePrint) {
        val prefix_1 =
          if (newValue < summarizedLastValue) '-'
          else if (newValue == summarizedLastValue) '='
          else '+'
        val prefix_2 = if (newBestValue) '#' else if (newValue == bestValue) '°' else ' '
        println(
          s"$prefix_1 $prefix_2 $newValue\t${summarizedMove.map(sm => s"${sm._1}:${sm._2}").mkString("\t")}"
        )
        summarizedMove = mutable.HashMap.empty
        summarizedLastValue = newValue
        summarizedLastPrint = System.currentTimeMillis()
      } else {
        if (summarizedMove.contains(neighborhoodName))
          summarizedMove(neighborhoodName) = summarizedMove(neighborhoodName)
        else summarizedMove += neighborhoodName -> 1
      }
    }
  }

  def searchEnded(endValue: Long, moveCount: Int): Unit = {
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

}
