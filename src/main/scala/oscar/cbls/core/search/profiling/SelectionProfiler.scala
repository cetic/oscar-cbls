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

import oscar.cbls.core.search.profiling.profilingData.CommonProfilingData
import oscar.cbls.core.search.{Neighborhood, NeighborhoodCombinator}

import scala.collection.mutable

/** A selection profiler specialized for BestFirstNeighborhood Combinator
  *
  * It collects some data from the supervised [[oscar.cbls.core.search.Neighborhood]] profiler.
  * Those can be used for Neighborhood selection heuristics. For instance, using the
  * efficiencySlope(...) method and selecting the Neighborhood with the highest one first.
  *
  * Those data can be reset without impacting the supervised Neighborhood profiler.
  *
  * @param combinator
  *   The profiled combinator
  * @param neighborhoods
  *   The list of supervised neighborhood
  */
class SelectionProfiler(
  override val combinator: NeighborhoodCombinator,
  val neighborhoods: List[Neighborhood]
) extends CombinatorProfiler(combinator) {

  private val profilerArray: Array[SearchProfiler] =
    neighborhoods.map(_.searchProfiler().get).toArray

  // SearchProfiler -> (nbCall, nbFound, gain, timeSpend)
  private val subProfilersData: mutable.HashMap[SearchProfiler, CommonProfilingData] =
    mutable.HashMap.from(neighborhoods.map(_.searchProfiler().get -> new CommonProfilingData()))
  private var _firstFailed: Boolean = false

  /** Resets the aggregated subProfiler data.
    *
    * The data of the [[SearchProfiler]] of the handled Neighborhood won't be reset. Only the one
    * gathered in this class. This will allow you to reset the selection process.
    */
  def resetSelectionNeighborhoodStatistics(): Unit = {
    nbOccurrencePerIterationNextIteration("NbFirstFailedPerReset")
    _firstFailed = false
    subProfilersData.map(p => (p._1, (0, 0, 0L, 0L)))
  }

  /** Returns the number of successful explorations made by the specified Neighborhood for this
    * iteration.
    *
    * @param i
    *   The id of the Neighborhood.
    * @return
    *   the number of successful explorations made by the specified Neighborhood for this iteration
    */
  def nbFoundOfNeighborhood(i: Int): Long = subProfilersData(profilerArray(i)).nbFound

  /** Returns the total gain of the specified Neighborhood for this iteration.
    *
    * @param i
    *   The id of the Neighborhood.
    * @return
    *   The total gain of Neighborhood i for this iteration.
    */
  def gainOfNeighborhood(i: Int): Long = subProfilersData(profilerArray(i)).gain

  /** Returns the total time spent of the specified Neighborhood for this iteration in milliseconds.
    *
    * @param i
    *   The id of the Neighborhood.
    * @return
    *   The total time spent of Neighborhood i for this iteration in milliseconds.
    */
  def timeSpentOfNeighborhood(i: Int): Long = subProfilersData(profilerArray(i)).timeSpentMillis

  /** Returns the efficiency slope of the given [[oscar.cbls.core.search.Neighborhood]] `totalGain`
    * `/` `totalTime`.
    *
    * @param neighborhoodId
    *   The id of the Neighborhood in the used neighborhoods list for which we want to know the
    *   slope.
    * @param defaultIfNoCall
    *   Default value if no call was made for this Neighborhood.
    */
  def efficiencySlope(neighborhoodId: Int, defaultIfNoCall: Long = Long.MaxValue): Long =
    if (subProfilersData(profilerArray(neighborhoodId)).nbCalls == 0) defaultIfNoCall
    else {
      ((1000.0 * gainOfNeighborhood(neighborhoodId)) / timeSpentOfNeighborhood(
        neighborhoodId
      ).toDouble).toLong
    }

  def firstHasFailed(): Unit = {
    _firstFailed = true
    nbOccurrencePerIterationEventOccurred("NbFirstFailedPerReset")
  }

  private def neighborhoodUsage(profiler: SearchProfiler): Double =
    ((profiler.commonProfilingData.nbFound.toDouble / this.commonProfilingData.nbFound) * 10000).round / 100.0

  private def neighborhoodSuccess(profiler: SearchProfiler): Double =
    ((profiler.commonProfilingData.nbFound.toDouble / profiler.commonProfilingData.nbCalls) * 10000).round / 100.0

  /** Aggregates the data sent by the sub profilers.
    *
    * Used by combinator that need underlying Neighborhood data to operate. For instance
    * BestSlopeFirst needs to know the efficiency of its Neighborhoods. Default behavior : Nothing
    * is done.
    *
    * @param profiler
    *   The profiler sending the data.
    */
  def aggregateSubProfilerData(profiler: SearchProfiler): Unit = {
    val subProfilerData = subProfilersData(profiler)
    val found           = profiler.commonProfilingData.lastCallFound()
    if (!found && subProfilerData.nbFound == 0 && !_firstFailed) firstHasFailed()
    if (found) {
      subProfilerData.callInc()
      subProfilerData.foundInc()
      subProfilerData.gainPlus(profiler.commonProfilingData.lastCallGain())
      subProfilerData.timeSpentMoveFoundPlus(profiler.commonProfilingData.lastCallDurationNano())
    } else {
      subProfilerData.callInc()
      subProfilerData.timeSpentNoMoveFoundPlus(profiler.commonProfilingData.lastCallDurationNano())
    }
  }

  override def collectCombinatorSpecificStatistics: List[List[List[String]]] = {
    val selectionProfilerData = List(
      List(List("Name", "Usage", "Success")) :::
        neighborhoods
          .map(n => {
            val profiler = n.searchProfiler().get
            List(
              s"${profiler.neighborhood.name}",
              s"${neighborhoodUsage(profiler).toString}%",
              s"${neighborhoodSuccess(profiler).toString}%"
            )
          })
    )
    super.collectCombinatorSpecificStatistics ::: selectionProfilerData
  }
  override def detailedRecursiveName: String =
    s"${neighborhood.toString}(${subProfilers.map(_.detailedRecursiveName).mkString(",")})"

}
