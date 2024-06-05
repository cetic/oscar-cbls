package oscar.cbls.visual.profiling

import oscar.cbls.core.search.profiling.{CombinatorProfiler, SearchProfiler}
import oscar.cbls.util.Tabulator


object ProfilingConsole{
  def apply(rootProfiler: SearchProfiler, headers: Array[String]): Unit = {
    new ProfilingConsole(rootProfiler,headers).displayOnConsole()
  }
}

class ProfilingConsole(rootProfiler: SearchProfiler, headers: Array[String]) {

  private val allProfilingTableNodes: Array[ProfilingTableNode] = ProfilingTableNode(rootProfiler)

  private def relevantCombinatorData: List[List[List[String]]] = {
    allProfilingTableNodes.collect(_.profiler match {
      case c: CombinatorProfiler if c.collectCombinatorSpecificStatistics.filterNot(_.isEmpty).size > 1 =>
        c.collectCombinatorSpecificStatistics.filterNot(_.isEmpty)
    }).toList
  }

  private def displayOnConsole(): Unit = {
    val allData = allProfilingTableNodes.map(_.data())
    val toPrint = Array(headers) ++ allData
    println(Tabulator.format(toPrint.toList.map(_.toList)).mkString("\n"))
    println("\n#######################\tCombinators specifics data\t##########################")
    println(relevantCombinatorData.map(_.map(_.mkString("\n")).mkString("\n\n")).mkString("\n----------------------------------------------------------\n"))
  }

}
