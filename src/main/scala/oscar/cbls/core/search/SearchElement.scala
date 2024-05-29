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

import oscar.cbls.core.computation.objective.Objective

abstract class SearchElement(_name: String) {

  private var _verbosity: SearchVerbosity = SearchVerbosity(0)

  /** The verbosity level of the search.
    *
    *   - 0: Nothing is printed
    *   - 1: Every 10th of a second, summarises all performed moves, by neighborhoods
    *   - 2: Prints every taken move
    *   - 3: Prints every search result
    *   - 4: Prints every explored neighbors \==WARNING==: Slows down the search
    */
  def verbosity: Int = _verbosity.verbosityLevel

  /** Sets the new verbosity level of the search.
   *
   *   - 0: Nothing is printed
   *   - 1: Every 10th of a second, summarises all performed moves, by neighborhoods
   *   - 2: Prints every taken move
   *   - 3: Prints every search result
   *   - 4: Prints every explored neighbors \==WARNING==: Slows down the search
   */
  def verbosity(verbosityLvl: Int): Unit = _verbosity = SearchVerbosity(verbosityLvl)

  def getMove(): SearchResult

  def doAllMoves(objective: Objective, shouldStop: Int => Boolean = _ => false): Int = {0}

  def name: String = _name
}
