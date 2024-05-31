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
import oscar.cbls.core.search.profiling.profilingData.{CombinatorProfilingData, MinMeanMaxData, NbOccurrencesPerIteration, PercentageEventOccurrence, SummedValue}

import scala.collection.mutable

/**
 * Base class for combinator profiler.
 * It allows you to profile specifics data following one of the four CombinatorProfilingData defined in ProfilingData file.
 *
 * @param combinator The profiled combinator
 */
class CombinatorProfiler(val combinator: NeighborhoodCombinator) extends SearchProfiler(combinator) {

  override def subProfilers: List[SearchProfiler] = combinator.subNeighborhoods.toList.map(_._searchProfiler)

  override def explorationPaused(): Unit = {
    super.explorationPaused()
    subProfilers.foreach(_.explorationPaused())
  }

  override def explorationResumed(): Unit = {
    super.explorationResumed()
    subProfilers.foreach(_.explorationResumed())
  }

  // Merge this profiler data and the sub-profiler data (recursively)
  override def merge(profiler: SearchProfiler): Unit ={
    val combinatorProfiler = profiler.asInstanceOf[CombinatorProfiler]
    commonProfilingData.merge(profiler.commonProfilingData)
    mergeSpecificStatistics(combinatorProfiler)
    combinator.subNeighborhoods.zip(combinatorProfiler.combinator.subNeighborhoods).
      foreach(sn => sn._1._searchProfiler.merge(sn._2._searchProfiler))
  }

  override def detailedRecursiveName: String = s"${neighborhood.toString}(${subProfilers.map(_.detailedRecursiveName).mkString(",")})"

  // MinMeanMax : See ProfilingData.MinMeanMaxData
  /////////////
  private val minMeanMaxProfiledData: mutable.HashMap[String, MinMeanMaxData] = mutable.HashMap.empty
  // Profile a new value
  def minMeanMaxProfile(name: String): Unit = minMeanMaxProfiledData.addOne(name,MinMeanMaxData())
  def minMeanMaxAddValue(name: String,value: Long): Unit = minMeanMaxProfiledData(name).add(value)

  // occurrence per iteration : See ProfilingData.NbOccurrencesPerIteration
  ///////////////////////////
  private val nbOccurrencesPerIterationData: mutable.HashMap[String,NbOccurrencesPerIteration] = mutable.HashMap.empty
  // Profile a new value
  def nbOccurrencePerIterationProfile(name: String, initFirstIteration: Boolean = false): Unit =
    nbOccurrencesPerIterationData.addOne(name,NbOccurrencesPerIteration(initFirstIteration))
  def nbOccurrencePerIterationNextIteration(name: String): Unit =
    nbOccurrencesPerIterationData(name).nextIteration()
  def nbOccurrencePerIterationEventOccurred(name: String): Unit =
    nbOccurrencesPerIterationData(name).eventOccurred()

  // percentage occurrence : See ProfilingData.PercentageEventOccurrence
  ////////////////////////
  private val percentageEventOccurrenceData: mutable.HashMap[String,PercentageEventOccurrence] = mutable.HashMap.empty
  // Profile a new value
  def percentageEventOccurrenceProfile(name: String): Unit =
    percentageEventOccurrenceData.addOne(name, PercentageEventOccurrence())
  def percentageEventOccurrencePushEvent(name: String,occurred: Boolean): Unit =
    percentageEventOccurrenceData(name).pushEvent(occurred)

  // summed value : See ProfilingData.SummedValue
  ///////////////
  private val summedValueProfiledData: mutable.HashMap[String,SummedValue] = mutable.HashMap.empty
  // Profile a new value
  def summedValueProfile(name: String): Unit = summedValueProfiledData.addOne(name, SummedValue())
  def summedValuePlus(name: String, value: Long): Unit = summedValueProfiledData(name).plus(value)

  private def collectSpecificStatistic(data: mutable.HashMap[String,CombinatorProfilingData]): List[List[String]] =
    List(
      if (data.isEmpty) List.empty[String]
      else{
        // TODO map this
        /*Properties.justifyRightArray(List(Array("Profiled var") ++ data.values.head.collectStatisticsHeaders()) ++
          data.keys.map(key => Array(key) ++ data(key).collectStatisticsData()))*/
        List.empty[String]
      })


  protected def mergeSpecificStatistics(other: CombinatorProfiler): Unit ={
    minMeanMaxProfiledData.keys.foreach(key => minMeanMaxProfiledData(key).merge(other.minMeanMaxProfiledData(key)))
    nbOccurrencesPerIterationData.keys.foreach(key => nbOccurrencesPerIterationData(key).merge(other.nbOccurrencesPerIterationData(key)))
    percentageEventOccurrenceData.keys.foreach(key => percentageEventOccurrenceData(key).merge(other.percentageEventOccurrenceData(key)))
    summedValueProfiledData.keys.foreach(key => summedValueProfiledData(key).merge(other.summedValueProfiledData(key)))
  }

  def collectCombinatorSpecificStatistics: List[List[String]] = {
    List(List(combinator.getClass.getSimpleName)) :::
      collectSpecificStatistic(minMeanMaxProfiledData.asInstanceOf[mutable.HashMap[String,CombinatorProfilingData]]) :::
      collectSpecificStatistic(nbOccurrencesPerIterationData.asInstanceOf[mutable.HashMap[String,CombinatorProfilingData]]) :::
      collectSpecificStatistic(percentageEventOccurrenceData.asInstanceOf[mutable.HashMap[String,CombinatorProfilingData]]) :::
      collectSpecificStatistic(summedValueProfiledData.asInstanceOf[mutable.HashMap[String,CombinatorProfilingData]])
  }
}
