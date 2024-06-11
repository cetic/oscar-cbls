package oscar.cbls.visual.profiling

import oscar.cbls.core.search.profiling.{CombinatorProfiler, SearchProfiler}
import oscar.cbls.util.Tabulator

object ProfilingConsole {
  def apply(rootProfiler: SearchProfiler, headers: Array[String]): Unit = {
    new ProfilingConsole(rootProfiler, headers).displayOnConsole()
  }
}

class ProfilingConsole(rootProfiler: SearchProfiler, headers: Array[String]) {

  private val allProfilingTableNodes: Array[ProfilingTableNode] = ProfilingTableNode(rootProfiler)

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
    val toPrint = Array(headers) ++ allData
    println(Tabulator.format(toPrint.toList.map(_.toList)))
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
