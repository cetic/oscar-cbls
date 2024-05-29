package oscar.cbls.core.search

import scala.collection.mutable
import scala.collection.mutable.HashMap

object SearchVerbosity {
  def apply(verbosityLevel: Int): SearchVerbosity = {
    new SearchVerbosity(verbosityLevel)
  }
}

class SearchVerbosity(val verbosityLevel: Int) {

  private var summarizedLastPrint: Long                    = System.currentTimeMillis()
  private var summarizedLastValue: Long                    = Long.MaxValue
  private var summarizedMove: mutable.HashMap[String, Int] = mutable.HashMap.empty

  /** Displays the exploration result of a move if the verbosity level is 4 or above
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

  /** Displays the start of an exploration of a neighborhood if the verbosity level is 3 or above
    *
    * @param neighborhood
    *   The explored Neighborhood
    */
  def startExploration(neighborhood: Neighborhood): Unit = {
    if (verbosityLevel >= 3)
      println(s"$neighborhood : start exploration")
  }

  /** Displays the exploration result of a neighborhood if the verbosity level is 3 or above
    *
    * @param neighborhood
    *   The explored Neighborhood
    * @param searchResult
    *   The exploration result
    */
  def neighborhoodExplored(neighborhood: Neighborhood, searchResult: SearchResult): Unit = {
    if (verbosityLevel >= 3)
      searchResult match {
        case NoMoveFound   => println(s"$neighborhood : No move found")
        case mf: MoveFound => println(s"$neighborhood : Move found ${mf.move}")
      }
  }

  def moveTaken(
    neighborhood: Neighborhood,
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
      println(s"$prefix_1 $prefix_2 $newValue\t$neighborhood : $move")
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
        if (summarizedMove.contains(neighborhood.name))
          summarizedMove(neighborhood.name) = summarizedMove(neighborhood.name)
        else summarizedMove += neighborhood.name -> 1
      }
    }

  }

}
