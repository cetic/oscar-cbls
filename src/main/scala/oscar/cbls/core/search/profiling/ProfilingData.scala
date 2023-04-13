package oscar.cbls.core.search.profiling

abstract class ProfilingData(){
  def merge(other: ProfilingData): Unit
}

/**
 * This class contains all profiling data common to Neighborhood and Combinator.
 * You can see duplicated data like _nbCalls and _nbCallsForSelection.
 * The second one is meant to be reset upon SelectionCombinator reset action.
 * The first one is never reset and contains the data of the whole search.
 */
class CommonProfilingData() extends ProfilingData() {
  private var _nbCalls: Long = 0L
  private var _nbFound: Long = 0L
  private var _gain: Long = 0L
  private var _timeSpentMoveFoundNano: Long = 0L
  private var _timeSpentNoMoveFoundNano: Long = 0L

  private var _nbCallsForSelection: Long = 0L
  private var _nbFoundsForSelection: Long = 0L
  private var _gainForSelection: Long = 0L
  private var _timeSpentMoveFoundNanoForSelection: Long = 0L
  private var _timeSpentNoMoveFoundNanoForSelection: Long = 0L

  override def merge(other: ProfilingData): Unit ={
    other match{
      case bpd: CommonProfilingData =>
        _nbCalls += bpd._nbCalls
        _nbFound += bpd._nbFound
        _gain += bpd._gain
        _timeSpentMoveFoundNano += bpd._timeSpentMoveFoundNano
        _timeSpentNoMoveFoundNano += bpd._timeSpentNoMoveFoundNano
      case _ => require(requirement = false, s"Unable to merge different types of profiling data (${this.getClass} vs ${other.getClass}")
    }

  }

  def gainPlus(gain: Long): Unit = {
    this._gain += gain
    this._gainForSelection += gain
  }
  def callInc(): Unit = {
    _nbCalls+=1
    _nbCallsForSelection+=1
  }
  def foundInc(): Unit = {
    _nbFound+=1
    _nbFoundsForSelection+=1
  }
  def timeSpentMoveFoundPlus(timeNano: Long): Unit = {
    this._timeSpentMoveFoundNano+=timeNano
    this._timeSpentMoveFoundNanoForSelection+=timeNano
  }
  def timeSpentNoMoveFoundPlus(timeNano: Long): Unit = {
    this._timeSpentNoMoveFoundNano+=timeNano
    this._timeSpentNoMoveFoundNanoForSelection+=timeNano
  }

  def nbCalls: Long = _nbCalls
  def nbFound: Long = _nbFound
  def gain: Long = _gain
  def timeSpentMoveFoundMillis: Long = _timeSpentMoveFoundNano/1000000
  def timeSpentNoMoveFoundMillis: Long = _timeSpentNoMoveFoundNano/1000000
  def timeSpentMillis: Long = (_timeSpentMoveFoundNano + _timeSpentNoMoveFoundNano)/1000000

  def nbCallsForSelection: Long = _nbCallsForSelection
  def nbFoundForSelection: Long = _nbFoundsForSelection
  def gainForSelection: Long = _gainForSelection
  def timeSpentMoveFoundMillisForSelection: Long = _timeSpentMoveFoundNanoForSelection / 1000000
  def timeSpentNoMoveFoundMillisForSelection: Long = _timeSpentNoMoveFoundNanoForSelection / 1000000
  def timeSpentMillisForSelection: Long = (_timeSpentMoveFoundNanoForSelection + _timeSpentNoMoveFoundNanoForSelection) / 1000000

  def resetStatisticsForSelection(): Unit = {
    _nbCallsForSelection = 0L; _nbFoundsForSelection = 0L; _gainForSelection = 0L
    _timeSpentMoveFoundNanoForSelection = 0L; _timeSpentNoMoveFoundNanoForSelection = 0L
  }

  override def toString: String = {
    s"Calls :${_nbCalls} | Founds :${_nbFound} | gain :${_gain} | " +
      s"total time move (nano):${_timeSpentMoveFoundNano} | total time no move (nano):${_timeSpentNoMoveFoundNano}"
  }
}

/**
 * Add final Neighborhood specifics profiling data :
 * - nbExplored
 * - first and not first movement selection duration
 */
case class NeighborhoodProfilingData() extends CommonProfilingData {

  private var _nbExplored: Long = 0L
  private var _firstNeighborSelectionCounter: Long = 0L
  private var _firstNeighborSelectionDuration: Long = 0L
  private var _notFirstNeighborSelectionCounter: Long = 0L
  private var _notFirstNeighborSelectionDuration: Long = 0L

  override def merge(other: ProfilingData): Unit ={
    super.merge(other)
    other match{
      case npd: NeighborhoodProfilingData =>
        _nbExplored += npd._nbExplored
      case _ => require(requirement = false, s"Unable to merge different types of profiling data (${this.getClass} vs ${other.getClass}")
    }
  }

  override def resetStatisticsForSelection(): Unit = {
    super.resetStatisticsForSelection()
    _nbExplored = 0L
  }

  def exploreInc(): Unit = _nbExplored+=1
  def nbExplored: Long = _nbExplored

  def firstNeighborSelectionCounterInc(): Unit = _firstNeighborSelectionCounter += 1
  def firstNeighborSelectionDurationPlus(duration: Long): Unit = _firstNeighborSelectionDuration += duration
  def avgTimeFirstNeighborSelectionMillis(): String =
    if (_firstNeighborSelectionCounter == 0) "NA"
    else s"${(_firstNeighborSelectionDuration.toDouble / _firstNeighborSelectionCounter) / 1000000}"
  def notFirstNeighborSelectionCounterInc(): Unit = _notFirstNeighborSelectionCounter += 1
  def notFirstNeighborSelectionDurationPlus(duration: Long): Unit = _notFirstNeighborSelectionDuration += duration
  def avgTimeNotFirstNeighborSelectionMillis(): String =
    if (_notFirstNeighborSelectionCounter == 0) "NA"
    else s"${(_notFirstNeighborSelectionDuration.toDouble / _notFirstNeighborSelectionCounter) / 1000000}"

  override def toString: String = super.toString + s" | Explored :${_nbExplored}"
}

/**
 * Abstract class for Combinator specifics profiling data
 */
abstract class CombinatorProfilingData(){
  def merge(other: CombinatorProfilingData): Unit

  def collectStatisticsHeaders(): Array[String]

  def collectStatisticsData(): Array[String]
}

/**
 * Profile and compute the percentage of occurrence of a specific event
 * Ex: The percentage of else choice with OrElse
 */
case class PercentageEventOccurrence() extends CombinatorProfilingData {
  private var _occurrences: Int = 0
  private var _iterations: Int = 0

  def occurrences: Int = _occurrences
  def iterations: Int = _iterations

  def pushEvent(occurred: Boolean): Unit = {
    if (occurred) _occurrences += 1
    _iterations += 1
  }

  override def merge(other: CombinatorProfilingData): Unit = {
    other match{
      case o: PercentageEventOccurrence =>
        _occurrences += o.occurrences
        _iterations += o.iterations
      case _ =>
    }
  }

  override def collectStatisticsHeaders(): Array[String] = Array("Occurrences", "percentage")
  override def collectStatisticsData(): Array[String] = Array(_occurrences.toString,(_occurrences/_iterations).toString)
}

/**
 * Profile and compute the number of occurrences of an event over iterations.
 * (min/mean/max occurrences of an event during an iteration)
 * ex: Max movement reach before reset
 */
case class NbOccurrencesPerIteration(initFirstIteration: Boolean) extends CombinatorProfilingData {
  private var _occurrences: Int = 0
  private var _minOccurrences: Int = Int.MaxValue
  private var _maxOccurrences: Int = Int.MinValue
  private var _summedOccurrences: Int = 0
  private var _iterations: Int = if(initFirstIteration)1 else 0

  def occurrences: Int = _occurrences
  def minOccurrences: Int = _minOccurrences
  def maxOccurrences: Int = _maxOccurrences
  def summedOccurrences: Int = _summedOccurrences
  def iterations: Int = _iterations

  def nextIteration(): Unit = {
    _minOccurrences = Math.min(_minOccurrences, _occurrences)
    _maxOccurrences = Math.max(_maxOccurrences, _occurrences)
    _summedOccurrences = _summedOccurrences + _occurrences
    _iterations = _iterations + 1
    _occurrences = 0
  }

  def eventOccurred(): Unit = _occurrences += 1

  def merge(other: CombinatorProfilingData): Unit ={
    other match {
      case o: NbOccurrencesPerIteration =>
        o.nextIteration() // We have to "commit" the last iteration
        _minOccurrences = Math.min(_minOccurrences,o.minOccurrences)
        _maxOccurrences = Math.max(_maxOccurrences,o.maxOccurrences)
        _summedOccurrences += o.summedOccurrences
        _iterations += o.iterations
    }
  }

  override def collectStatisticsHeaders(): Array[String] = {
    nextIteration()
    Array("MinOccurrencePerIteration", "MeanOccurrencePerIteration", "MaxOccurrencePerIteration", "NbIterations","TotalOccurrences")
  }
  override def collectStatisticsData(): Array[String] = {
    Array(_minOccurrences.toString, ((_summedOccurrences*1000/_iterations)/1000.0).toString, _maxOccurrences.toString, _iterations.toString, _summedOccurrences.toString)
  }
}

/**
 * Profile and compute the min, mean and max value of a variable during a search
 * ex: Exhaust time
 */
case class MinMeanMaxData() extends CombinatorProfilingData {
  private var _min: Long = Long.MaxValue
  private var _max: Long = Long.MinValue
  private var _count: Int = 0
  private var _sum: Long = 0L

  def min: Long = _min
  def max: Long = _max
  def count: Int = _count
  def sum: Long = _sum

  def add(value: Long): Unit = {
    _min = Math.min(value, _min)
    _max = Math.max(value, _max)
    _count += 1
    _sum += value
  }

  override def merge(other: CombinatorProfilingData): Unit = {
    other match {
      case o: MinMeanMaxData =>
        _min = Math.min(_min, o.min)
        _max = Math.max(_max, o.max)
        _sum += o.sum
        _count += o.count
    }
  }

  override def collectStatisticsHeaders(): Array[String] =
    Array("Min", "Mean", "Max", "Sum", "Count")

  override def collectStatisticsData(): Array[String] = {
    Array(_min.toString, ((_sum*1000/_count)/1000.0).toString, _max.toString, _sum.toString, _count.toString)
  }
}

/**
 * Profile and compute the summed value of a variable during the search.
 * Ex : the number of time Guard prevent a movement
 */
case class SummedValue() extends CombinatorProfilingData {
  private var _sum: Long = 0L

  def sum: Long = _sum

  def plus(value: Long): Unit = _sum += value

  override def merge(other: CombinatorProfilingData): Unit ={
    other match {
      case o: SummedValue => _sum += o.sum
    }
  }

  override def collectStatisticsHeaders(): Array[String] = Array("Sum")
  override def collectStatisticsData(): Array[String] = Array(_sum.toString)
}