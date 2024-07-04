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

package oscar.cbls.visual.profiling

import oscar.cbls.core.search.profiling.{CombinatorProfiler, SearchProfiler}
import oscar.cbls.util.Tabulator

object ProfilingConsole {
  def apply(rootProfiler: SearchProfiler, headers: List[String]): Unit = {
    new ProfilingConsole(rootProfiler, headers).displayOnConsole()
  }
}

/** Displays the profiling result on the terminal.
  *
  * How it is displayed :
  *
  *   - The search structure is on the left side as a tree structure, each
  *     [[oscar.cbls.core.search.NeighborhoodCombinator]] have their
  *     [[oscar.cbls.core.search.Neighborhood]] as child. And the
  *     [[oscar.cbls.core.search.SimpleNeighborhood]] are the leaf the tree.
  *   - Right to the tree structure, common statistics are displayed in a table. The description of
  *     each columns are at the top of each one of them.
  *   - Below the table you may see several other table. Those are the combinator specifics
  *     profiling data.
  *
  * The Search profiling follows the same idea of search procedure, a root Profiler has children who
  * may have children...
  *
  * @param rootProfiler
  *   The first Search linked to all other with a child-parent relation.
  * @param headers
  *   The headers of all common statistics
  */
class ProfilingConsole(rootProfiler: SearchProfiler, headers: List[String]) {

  private val allProfilingTableNodes: List[ProfilingTableNode] = ProfilingTableNode(rootProfiler)

  private def gatherRelevantCombinatorData: List[List[List[List[String]]]] = {
    allProfilingTableNodes
      .collect(_.profiler match {
        case c: CombinatorProfiler
            if c.collectCombinatorSpecificStatistics.filterNot(_.isEmpty).size > 1 =>
          c.collectCombinatorSpecificStatistics.filterNot(_.isEmpty)
      })
  }

  private def displayOnConsole(): Unit = {
    val allData = allProfilingTableNodes.map(_.data())
    val toPrint = List(headers) ++ allData
    println(Tabulator.format(toPrint.map(_.toList)))
    val allRelevantCombinatorData = gatherRelevantCombinatorData
    if (allRelevantCombinatorData.nonEmpty) {
      println("\n#######################\tCombinators specifics data\t##########################")
      allRelevantCombinatorData.foreach(relevantCombinatorData => {
        println(relevantCombinatorData.head.head.head)
        println(
          relevantCombinatorData.tail
            .map(rcd => Tabulator.format(rcd))
            .mkString("\n\n")
        )
      })
    }
  }

}
