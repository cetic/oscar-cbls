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

import oscar.cbls.core.search.NeighborhoodCombinator
import oscar.cbls.core.search.profiling.profilingData.{
  CombinatorProfilingData,
  MinMeanMaxData,
  NbOccurrencesPerIteration,
  PercentageEventOccurrence,
  SummedValue
}

import scala.collection.mutable

/** Base class for combinator profiler. It allows you to profile specifics data following one of the
  * four CombinatorProfilingData defined in ProfilingData file.
  *
  * @param combinator
  *   The profiled combinator
  */
class CombinatorProfiler(val combinator: NeighborhoodCombinator)
    extends SearchProfiler(combinator) {

  override def subProfilers: List[SearchProfiler] =
    combinator.subNeighborhoods.map(_.searchProfiler().get)

  override def explorationPaused(): Unit = {
    super.explorationPaused()
    subProfilers.foreach(_.explorationPaused())
  }

  override def explorationResumed(): Unit = {
    super.explorationResumed()
    subProfilers.foreach(_.explorationResumed())
  }

  // Merge this profiler data and the sub-profiler data (recursively)
  override def merge(profiler: SearchProfiler): Unit = {
    val combinatorProfiler = profiler.asInstanceOf[CombinatorProfiler]
    commonProfilingData.merge(profiler.commonProfilingData)
    mergeSpecificStatistics(combinatorProfiler)
    combinator.subNeighborhoods
      .zip(combinatorProfiler.combinator.subNeighborhoods)
      .foreach(sn => sn._1.searchProfiler().get.merge(sn._2.searchProfiler().get))
  }

  override def detailedRecursiveName: String =
    s"${neighborhood.toString}(${subProfilers.map(_.detailedRecursiveName).mkString(",")})"

  // MinMeanMax : See ProfilingData.MinMeanMaxData
  /////////////
  private val minMeanMaxProfiledData: mutable.HashMap[String, MinMeanMaxData] =
    mutable.HashMap.empty
  // Profile a new value
  def minMeanMaxAddValue(name: String, value: Long): Unit = minMeanMaxProfiledData.get(name) match {
    case Some(minMeanMaxData: MinMeanMaxData) => minMeanMaxData.add(value)
    case None => minMeanMaxProfiledData.addOne(name, MinMeanMaxData(value))
  }

  // occurrence per iteration : See ProfilingData.NbOccurrencesPerIteration
  ///////////////////////////
  private val nbOccurrencesPerIterationData: mutable.HashMap[String, NbOccurrencesPerIteration] =
    mutable.HashMap.empty
  // Profile a new value
  def nbOccurrencePerIterationNextIteration(name: String): Unit =
    nbOccurrencesPerIterationData.get(name) match {
      case Some(nbOccPerIteration) => nbOccPerIteration.nextIteration()
      case None =>
        nbOccurrencesPerIterationData.addOne(
          name,
          NbOccurrencesPerIteration(startIncIteration = true)
        )
    }
  def nbOccurrencePerIterationEventOccurred(name: String): Unit =
    nbOccurrencesPerIterationData.get(name) match {
      case Some(nbOccPerIteration) => nbOccPerIteration.eventOccurred()
      case None =>
        nbOccurrencesPerIterationData.addOne(
          name,
          NbOccurrencesPerIteration(startIncOccurrence = true)
        )
    }

  // percentage occurrence : See ProfilingData.PercentageEventOccurrence
  ////////////////////////
  private val percentageEventOccurrenceData: mutable.HashMap[String, PercentageEventOccurrence] =
    mutable.HashMap.empty

  /** Pushes a notification about a event occurrence
    *
    * If this event name is already registered, just push the notification. Otherwise it creates a
    * new event and push this notification.
    * @param name
    *   The name of the event
    * @param occurred
    *   Whether it occurred or not
    */
  def percentageEventOccurrencePushEvent(name: String, occurred: Boolean): Unit =
    percentageEventOccurrenceData.get(name) match {
      case Some(percentageEventOccur) => percentageEventOccur.pushEvent(occurred)
      case None => percentageEventOccurrenceData.addOne(name, PercentageEventOccurrence())
    }

  // summed value : See ProfilingData.SummedValue
  ///////////////
  private val summedValueProfiledData: mutable.HashMap[String, SummedValue] = mutable.HashMap.empty

  /** Sums a value to an existing SummedValue or create a new one (with this value)
    *
    * @param name
    *   The name of the summed value
    * @param value
    *   The value to sum
    */
  def summedValuePlus(name: String, value: Long): Unit = {
    summedValueProfiledData.get(name) match {
      case Some(summedValue: SummedValue) => summedValue.plus(value)
      case None => summedValueProfiledData.addOne(name, SummedValue(value))
    }
  }

  private def collectSpecificStatistic(
    data: mutable.HashMap[String, CombinatorProfilingData]
  ): List[List[String]] =
    if (data.isEmpty) List.empty[List[String]]
    else {
      List(List("Profiled var") ::: data.values.head.collectStatisticsHeaders()) :::
        data.keys.map(key => List(key) ::: data(key).collectStatisticsData()).toList
    }

  protected def mergeSpecificStatistics(other: CombinatorProfiler): Unit = {
    minMeanMaxProfiledData.keys.foreach(key =>
      minMeanMaxProfiledData(key).merge(other.minMeanMaxProfiledData(key))
    )
    nbOccurrencesPerIterationData.keys.foreach(key =>
      nbOccurrencesPerIterationData(key).merge(other.nbOccurrencesPerIterationData(key))
    )
    percentageEventOccurrenceData.keys.foreach(key =>
      percentageEventOccurrenceData(key).merge(other.percentageEventOccurrenceData(key))
    )
    summedValueProfiledData.keys.foreach(key =>
      summedValueProfiledData(key).merge(other.summedValueProfiledData(key))
    )
  }

  /** Collects combinators statistics in a 3 level List of strings. Grouped by profiled data type,
    * instance of profiled data and data
    */
  def collectCombinatorSpecificStatistics: List[List[List[String]]] = {
    List(
      List(List(combinator.getClass.getSimpleName)),
      collectSpecificStatistic(
        minMeanMaxProfiledData.asInstanceOf[mutable.HashMap[String, CombinatorProfilingData]]
      ),
      collectSpecificStatistic(
        nbOccurrencesPerIterationData.asInstanceOf[mutable.HashMap[String, CombinatorProfilingData]]
      ),
      collectSpecificStatistic(
        percentageEventOccurrenceData.asInstanceOf[mutable.HashMap[String, CombinatorProfilingData]]
      ),
      collectSpecificStatistic(
        summedValueProfiledData.asInstanceOf[mutable.HashMap[String, CombinatorProfilingData]]
      )
    )
  }
}
