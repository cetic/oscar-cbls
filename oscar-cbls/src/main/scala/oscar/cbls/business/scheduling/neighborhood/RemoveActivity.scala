package oscar.cbls.business.scheduling.neighborhood

import oscar.cbls.algo.search.HotRestart
import oscar.cbls.business.scheduling.model.Schedule
import oscar.cbls.core.search.{EasyNeighborhoodMultiLevel, First, LoopBehavior}

class RemoveActivity(schedule: Schedule,
                     neighborhoodName: String,
                     selectIndexBehavior:LoopBehavior = First(),
                     searchIndices: Option[() => Iterable[Int]] = None)
  extends EasyNeighborhoodMultiLevel[RemoveActivityMove](neighborhoodName) {

  var currentIndex: Int = -1

  /**
    * This is the method you must implement and that performs the search of your neighborhood.
    * every time you explore a neighbor, you must perform the calls to notifyMoveExplored or moveRequested(newObj) && submitFoundMove(myMove)){
    * as explained in the documentation of this class
    */
  override def exploreNeighborhood(initialObj: Long): Unit = {
    // Iteration zone on indices to remove (optional activities)
    // Checking the Hot Restart
    val iterationZone1: () => Iterable[Int] = searchIndices.getOrElse(() =>
      0 until schedule.activityPriorityList.value.size
    )
    val hotRestart = true
    val iterationZone: Iterable[Int] =
      if (hotRestart) HotRestart(iterationZone1(), currentIndex)
      else iterationZone1()
    // iterating over the values in the activity list
    val (indicesIterator, notifyIndexFound) = selectIndexBehavior.toIterator(iterationZone)
    // Define checkpoint on sequence (activities list)
    val seqValueCheckPoint = schedule
      .activityPriorityList
      .defineCurrentValueAsCheckpoint(true)
    while (indicesIterator.hasNext) {
      currentIndex = indicesIterator.next().toInt
      // perform move
      performMove(currentIndex)
      val newObj = obj.value
      // Rollback to checkpoint
      schedule.activityPriorityList.rollbackToTopCheckpoint(seqValueCheckPoint)
      // Notification of finding indices
      if (evaluateCurrentMoveObjTrueIfSomethingFound(newObj)) {
        notifyIndexFound()
      }
    }
    schedule.activityPriorityList.releaseTopCheckpoint()
  }

  override def instantiateCurrentMove(newObj: Long): RemoveActivityMove =
    RemoveActivityMove(currentIndex, schedule.activityPriorityList.value.size,
      this, neighborhoodNameToString, newObj)

  def performMove(indAct: Int): Unit = {
    schedule.activityPriorityList.remove(indAct)
  }
}

case class RemoveActivityMove(removeIndex: Int,
                              numActiveActivities: Int,
                              override val neighborhood: RemoveActivity,
                              override val neighborhoodName: String = "RemoveActivityMove",
                              override val objAfter: Long)
  extends SchedulingMove(neighborhood, neighborhoodName, objAfter)  {
  override def impactedActivities: Iterable[Int] = for {i <- removeIndex until numActiveActivities} yield i

  /** to actually take the move */
  override def commit(): Unit = {
    neighborhood.performMove(removeIndex)
  }
}
