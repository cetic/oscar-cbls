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

package oscar.cbls.core.search.profiling

import oscar.cbls.core.search.{Neighborhood, NeighborhoodCombinator}

/** Base profiler for Selection type combinator. It add a comparison dimension, allowing the user to
  * see which neighborhood has been used and in what proportion...
  *
  * @param combinator
  *   The profiled combinator
  * @param neighborhoods
  *   The list of supervised neighborhood
  */
class SelectionProfiler(combinator: NeighborhoodCombinator, val neighborhoods: List[Neighborhood])
    extends CombinatorProfiler(combinator) {

  override def collectCombinatorSpecificStatistics: List[List[List[String]]] = {
    val selectionProfilerData = List(
      List(List("Name", "Usage", "Success")) :::
        profilers.indices
          .map(pi =>
            List(
              s"${profilers(pi).neighborhood.name}",
              s"${neighborhoodUsage(pi).toString}%",
              s"${neighborhoodSuccess(pi).toString}%"
            )
          )
          .toList
    )
    super.collectCombinatorSpecificStatistics ::: selectionProfilerData
  }

  ///////////////////////////////////////
  // Selection-Neighborhood management //
  ///////////////////////////////////////

  val profilers: Array[SearchProfiler] = neighborhoods.map(_.searchProfiler().get).toArray

  protected def totalTimeSpentSubN(i: Int): Long = profilers(
    i
  ).commonProfilingData.timeSpentMillisForSelection

  private def neighborhoodUsage(neighborhoodId: Int): Double =
    ((profilers(neighborhoodId).commonProfilingData.nbFound.toDouble / profilers
      .map(_.commonProfilingData.nbFound)
      .sum) * 10000).round / 100.0

  private def neighborhoodSuccess(neighborhoodId: Int): Double =
    ((profilers(neighborhoodId).commonProfilingData.nbFound.toDouble / profilers(
      neighborhoodId
    ).commonProfilingData.nbCalls) * 10000).round / 100.0

  override def detailedRecursiveName: String =
    s"${neighborhood.toString}(${profilers.map(_.detailedRecursiveName).mkString(",")})"
}
