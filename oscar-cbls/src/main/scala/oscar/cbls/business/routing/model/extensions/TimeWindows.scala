package oscar.cbls.business.routing.model.extensions


object TimeWindows{
  /**
    * @param earliestArrivalTimes An array that contains the earliest arrival time of each node. If arriving before this value we must wait.
    * @param latestArrivalTimes An array that contains the latest arrival time of each node. We can't start the task after this time.
    * @param earliestLeavingTimes An array that contains the earliest leaving time of each node.
    * @param latestLeavingTimes An array that contains the latest leaving time of each node.
    * @param taskDurations An array that contains the task duration of each node.
    * @param maxWaitingDurations For each node the maximum among of time we can wait before starting the task.
    *                            e.g.: You can stay at a parking for a limited among of time.
    */
    @deprecated("this is over packaging and should be deleted","")
  def apply(earliestArrivalTimes: Option[Array[Long]] = None,
            latestArrivalTimes: Option[Array[Long]] = None,
            earliestLeavingTimes: Option[Array[Long]] = None,
            latestLeavingTimes: Option[Array[Long]] = None,
            taskDurations: Array[Long],
            maxWaitingDurations: Option[Array[Long]] = None): TimeWindows ={
    val eat =
      if(earliestArrivalTimes.isDefined) earliestArrivalTimes.get
      else if(earliestLeavingTimes.isDefined) Array.tabulate(taskDurations.length)(x => earliestLeavingTimes.get(x) - taskDurations(x))
      else Array.fill(taskDurations.length)(0L)
    val lat =
      if(latestArrivalTimes.isDefined) latestArrivalTimes.get
      else if(latestLeavingTimes.isDefined) Array.tabulate(taskDurations.length)(x => latestLeavingTimes.get(x) - taskDurations(x))
      else Array.fill(taskDurations.length)(Long.MaxValue)
    val elt =
      if(earliestLeavingTimes.isDefined) earliestLeavingTimes.get
      else if(earliestArrivalTimes.isDefined) Array.tabulate(taskDurations.length)(x => earliestArrivalTimes.get(x) + taskDurations(x))
      else Array.fill(taskDurations.length)(0L)
    val llt =
      if(latestLeavingTimes.isDefined) latestLeavingTimes.get
      else if(latestArrivalTimes.isDefined) Array.tabulate(taskDurations.length)(x => latestArrivalTimes.get(x) + taskDurations(x))
      else Array.fill(taskDurations.length)(Long.MaxValue)
    new TimeWindows(eat,lat,elt,llt,taskDurations,maxWaitingDurations.getOrElse(Array.fill(taskDurations.length)(Long.MaxValue)))
  }
}

/**
  * This class is only used to simplify the constraints creation.
  * The invariant used to set the time constraint is not a time dedicated invariant
  * so we need some extra information like earliestArrivalTimes, latestLeavingTimes...
  *
  * This class serve as a data package
  *
  * @param earliestArrivalTimes An array that contains the earliest arrival time of each node. If arriving before this value we must wait.
  * @param latestArrivalTimes An array that contains the latest arrival time of each node. We can't start the task after this time.
  * @param earliestLeavingTimes An array that contains the earliest leaving time of each node.
  * @param latestLeavingTimes An array that contains the latest leaving time of each node.
  * @param taskDurations An array that contains the task duration of each node.
  * @param maxWaitingDurations For each node the maximum among of time we can wait before starting the task.
  *                            e.g.: You can stay at a parking for a limited among of time.
  */
@deprecated("this is over packaging and should be deleted","")
class TimeWindows(val earliestArrivalTimes: Array[Long],
                  val latestArrivalTimes: Array[Long],
                  val earliestLeavingTimes: Array[Long],
                  val latestLeavingTimes: Array[Long],
                  val taskDurations: Array[Long],
                  val maxWaitingDurations: Array[Long]){
}