/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/
package oscar.cbls.util

import oscar.cbls.core.objective.Objective
import oscar.cbls.core.search.Neighborhood
import scala.util.control.Breaks._

object Benchmark extends StopWatch{

  case class RunValues(it:Long,duration:Long,quality:Long)
  case class RunStatistics(it:Statistics,duration:Statistics,quality:Statistics){
    override def toString: String = s"(it:$it dur:$duration obj:$quality)"
    def denseString:String = s"${it.denseString}|${duration.denseString}|${quality.denseString}"
    def csvString: String = s"${it.csvString};${duration.csvString};${quality.csvString}"
  }

  val firstColumnForStatisticsString = 40
  def nSpace(n:Int):String = if(n <= 0) "" else s"${nSpace(n-1)}"
  private def padToLength(s: String, l: Int) = (s + nSpace(l)).substring(0, l)
  def benchToStatistics(obj: Objective, nRuns: Int, strategies: Iterable[() => (String, Neighborhood)], warmupTimeInSeconds: Long, verbose: Int) =
    benchToTrace(obj, nRuns, strategies, warmupTimeInSeconds, verbose).map {
      case (s: String, t: IndexedSeq[RunValues]) => (s, aggregate(t.toList))
    }

  def benchToStringSimple(obj:Objective, nRuns:Int, strategies:Iterable[Neighborhood],verbose:Int = 0):String = {
    benchToStringFull(obj,nRuns,strategies.map(n => (() => {n.reset(); (n.toString,n)})),verbose)
  }

  def statsToString(stats: Iterable[(String, RunStatistics)]) = {
    padToLength("nbRuns:" + stats.size,firstColumnForStatisticsString) + "|it                                  |dur[ms]                             |obj" + "\n" +
      nSpace(firstColumnForStatisticsString) + "|" + Statistics.statisticsHeader + "|" + Statistics.statisticsHeader + "|" + Statistics.statisticsHeader + "\n" +
      stats.map({ case (s: String, stats: RunStatistics) => padToLength(s, firstColumnForStatisticsString) + "|" + stats.denseString }).mkString("\n")
  }

  def benchToStringFull(obj: Objective, nRuns: Int, strategies: Iterable[() => (String, Neighborhood)], warmup: Long = 1L, verbose: Int = 0): String = {
    val stats = benchToStatistics(obj, nRuns, strategies, warmup, verbose)

    statsToString(stats)
  }

  def benchToTrace(obj: Objective, nRuns: Int, strategies: Iterable[() => (String, Neighborhood)], warmupTimeInSeconds: Long, verbose: Int) = {
    val m = obj.model
    val initialSolution = m.solution()

    // warm run
    if (verbose > 1) println(s"Warming up for $warmupTimeInSeconds seconds...")
    val warmupInMs = warmupTimeInSeconds * 1000L
    this.startWatch()
    breakable {
      while (this.getWatch < warmupInMs) {
        for (n <- strategies) {
          m.restoreSolution(initialSolution)
          val strategyInstance = n()
          strategyInstance._2.verbose = 0
          if (verbose > 1) println(s"Warm up run of ${strategyInstance._1}")
          strategyInstance._2.doAllMoves(_ => false, obj)
          if (this.getWatch >= warmupInMs) break
        }
      }
    }

    for (n <- strategies) yield {
      if (verbose > 1) println(s"Benchmarking ${n()._1}")
      (n()._1,
        for (trial <- 1 to nRuns) yield {
          m.restoreSolution(initialSolution)
          val strategyInstance = n()
          strategyInstance._2.verbose = if (verbose > 0) verbose else 0
          if (verbose > 1) println(s"Benchmarking ${strategyInstance._1} run $trial of $nRuns")
          this.startWatch()
          val it = strategyInstance._2.doAllMoves(_ => false, obj)
          val time = this.getWatch
          val quality = obj.value
          RunValues(it, time.toInt, quality)
        })
    }
  }

  def aggregate(l:List[RunValues]):RunStatistics = {
    RunStatistics(it=Statistics(l.map(_.it)),
      duration=Statistics(l.map(_.duration)),
      quality=Statistics(l.map(_.quality)))
  }
}

case class Statistics(min:Long, max:Long, avg:Long, med:Long){
  override def toString: String = s"(min:$min max:$max avg:$avg med:$med)"
  def denseString:String = padToLength("" + min,8) + " " + padToLength("" + max,8) + " " + padToLength("" + avg,8) + " " + padToLength("" + med,9)
  def csvString:String = s"$min;$max;$avg;$med"
  def nSpace(n:Int):String = if(n <= 0) "" else s"${nSpace(n-1)}"
  private def padToLength(s: String, l: Int) = (s + nSpace(l)).substring(0, l)
}

object Statistics {
  def apply(l: List[Long]): Statistics = {
    require(l.nonEmpty)
    val sorted = l.sorted
    val size = l.size
    Statistics(min=sorted.head, max = sorted.last, avg=l.sum/size, med= if(size ==1) sorted.head else sorted.apply( size/2))
  }

  val statisticsHeader = "min      max      avg      med      "
}
