package oscar.cbls.core.search.profiling

object ProfilingData {
  def apply(): ProfilingData = new ProfilingData()
}

class ProfilingData(){
  private var _nbCalls: Long = 0L
  private var _nbFound: Long = 0L
  private var _gain: Long = 0L
  private var _timeSpentMoveFound: Long = 0L
  private var _timeSpentNoMoveFound: Long = 0L

  def merge(bpd: ProfilingData): Unit ={
    _nbCalls+=bpd._nbCalls; _nbFound+=bpd._nbFound; _gain+=bpd._gain
    _timeSpentMoveFound+=bpd._timeSpentMoveFound; _timeSpentNoMoveFound+=bpd._timeSpentNoMoveFound
  }

  def gainPlus(gain: Long): Unit = this._gain += gain
  def callIncr(): Unit = _nbCalls+=1
  def foundIncr(): Unit = _nbFound+=1
  def timeSpentMoveFoundPlus(time: Long): Unit = this._timeSpentMoveFound+=time
  def timeSpentNoMoveFoundPlus(time: Long): Unit = this._timeSpentNoMoveFound+=time

  def setNbCalls(value: Long): Unit = _nbCalls = value
  def setNbFounds(value: Long): Unit = _nbFound = value
  def setGain(value: Long): Unit = _gain = value
  def setTimeSpentMoveFound(value: Long): Unit = _timeSpentMoveFound = value
  def setTimeSpentNoMoveFound(value: Long): Unit = _timeSpentNoMoveFound = value

  def nbCalls: Long = _nbCalls
  def nbFound: Long = _nbFound
  def gain: Long = _gain
  def timeSpentMoveFound: Long = _timeSpentMoveFound
  def timeSpentNoMoveFound: Long = _timeSpentNoMoveFound
  def timeSpent: Long = _timeSpentMoveFound + _timeSpentNoMoveFound

  def resetAll(): Unit = {
    _nbCalls = 0L; _nbFound = 0L; _gain = 0L
    _timeSpentMoveFound = 0L; _timeSpentNoMoveFound = 0L
  }

  override def toString: String = {
    s"Calls :${_nbCalls} | Founds :${_nbFound} | gain :${_gain} | " +
      s"total time move :${_timeSpentMoveFound} | total time no move :${_timeSpentNoMoveFound}"
  }
}

case class NeighborhoodProfilingData() extends ProfilingData {
  private var _nbExplored: Long = 0L

  override def merge(other: ProfilingData): Unit ={
    super.merge(other)
    other match{
      case o: NeighborhoodProfilingData =>
        _nbExplored += o._nbExplored
      case _ =>
        println(s"WARNING (profiling) : merging NeighborhoodProfilingData with a ${other.getClass}")
    }
  }

  override def resetAll(): Unit = {
    super.resetAll()
    _nbExplored = 0L
  }

  def exploreIncr(): Unit = _nbExplored+=1
  def explorePlus(value: Long): Unit = _nbExplored+=value
  def nbExplored: Long = _nbExplored
  def setNbExplored(value: Long): Unit = _nbExplored = value

  override def toString: String = super.toString + s" | Explored :${_nbExplored}"
}
