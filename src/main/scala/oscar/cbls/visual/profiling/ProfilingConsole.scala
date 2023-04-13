package oscar.cbls.visual.profiling

import oscar.cbls.core.search.profiling.{CombinatorProfiler, Profiler}
import oscar.cbls.util.Properties

object ProfilingConsole{
  def apply(rootProfiler: Profiler, headers: Array[String]): Unit = {
    new ProfilingConsole(rootProfiler,headers).displayOnConsole()
  }
}

class ProfilingConsole(rootProfiler: Profiler, headers: Array[String]) {

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
    println(Properties.justifyLeftArray(toPrint.toList).mkString("\n"))
    println("\n#######################\tCombinators specifics data\t##########################")
    println(relevantCombinatorData.map(_.map(_.mkString("\n")).mkString("\n\n")).mkString("\n----------------------------------------------------------\n"))
  }

}
