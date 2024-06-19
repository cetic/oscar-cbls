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

class ProfilingConsole(rootProfiler: SearchProfiler, headers: List[String]) {

  private val allProfilingTableNodes: List[ProfilingTableNode] = ProfilingTableNode(rootProfiler)

  private def gatherRelevantCombinatorData: List[List[List[List[String]]]] = {
    allProfilingTableNodes
      .collect(_.profiler match {
        case c: CombinatorProfiler
            if c.collectCombinatorSpecificStatistics.filterNot(_.isEmpty).size > 1 =>
          c.collectCombinatorSpecificStatistics.filterNot(_.isEmpty)
      })
      .toList
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
