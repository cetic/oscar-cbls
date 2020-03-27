package oscar.cbls.business.scheduling.neighborhood

import oscar.cbls.LoopBehavior
import oscar.cbls.algo.search.HotRestart
import oscar.cbls.business.scheduling.ActivityId
import oscar.cbls.business.scheduling.model.Schedule
import oscar.cbls.core.search.{Best, EasyNeighborhoodMultiLevel, First}

class ReplaceActivity(schedule: Schedule,
                      neighborhoodName: String,
                      selectRemoveBehavior:LoopBehavior = First(),
                      selectActToAddBehavior:LoopBehavior = First(),
                      selectReinsertBehavior:LoopBehavior = Best(),
                      searchValues: Option[() => Iterable[Int]] = None)
  extends EasyNeighborhoodMultiLevel[ReplaceActivityMove](neighborhoodName) {

  var indActToRemove: Int = -1
  var actToAdd: ActivityId = -1
  var indActToAdd: Int = -1

  /**
    * This is the method you must implement and that performs the search of your neighborhood.
    * every time you explore a neighbor, you must perform the calls to notifyMoveExplored or moveRequested(newObj) && submitFoundMove(myMove)){
    * as explained in the documentation of this class
    */
  override def exploreNeighborhood(initialObj: Long): Unit = {
    // Iteration zone on activities indices to remove
    // Checking the Hot Restart
    val iterationZone1: () => Iterable[Int] = searchValues.getOrElse(() =>
      0 until schedule.activityPriorityList.value.size
    )
    val hotRestart = true
    val iterationZone: Iterable[Int] =
      if (hotRestart) HotRestart(iterationZone1(), indActToRemove)
      else iterationZone1()
    // iterating over the values in the activity list
    val (indicesIterator, notifyIndexToRemoveFound) = selectRemoveBehavior.toIterator(iterationZone)
    // Define checkpoint on sequence (activities list)
    val seqValueCheckPoint = schedule.activityPriorityList.defineCurrentValueAsCheckpoint(true)
    while (indicesIterator.hasNext) {
      indActToRemove = indicesIterator.next().toInt
      // iterating over the possible activities to add after
      val iterationZone2: () => Iterable[Int] = () => {
        schedule
          .activities
          .filterNot(schedule.activityPriorityList.value.contains(_))
      }
      val iterationZoneAdding: Iterable[Int] =
        if (hotRestart) HotRestart(iterationZone2(), actToAdd)
        else iterationZone2()
      // iterating over the activities to add
      val (actToAddIterator, notifyActToAddFound) = selectActToAddBehavior.toIterator(iterationZoneAdding)
      while (actToAddIterator.hasNext) {
        actToAdd = actToAddIterator.next().toInt
        // Iteration over the reinsertable zone
        val reinsertableZone = schedule.insertableIndices(actToAdd).map { ind =>
          if (ind > indActToRemove) ind-1
          else ind
        }
        val (reinsertableIterator, notifyRensertFound) = selectReinsertBehavior.toIterator(reinsertableZone)
        while (reinsertableIterator.hasNext) {
          indActToAdd = reinsertableIterator.next()
          // Perform move on sequence
          performMove(indActToRemove, actToAdd, indActToAdd)
          val newObj = obj.value
          // Rollback to checkpoint
          schedule.activityPriorityList.rollbackToTopCheckpoint(seqValueCheckPoint)
          // Notification of finding indices
          if (evaluateCurrentMoveObjTrueIfSomethingFound(newObj)) {
            notifyIndexToRemoveFound()
            notifyActToAddFound()
            notifyRensertFound()
          }
        }
      }
    }
    schedule.activityPriorityList.releaseTopCheckpoint()
  }

  override def instantiateCurrentMove(newObj: Long): ReplaceActivityMove =
    ReplaceActivityMove(indActToRemove, actToAdd, indActToAdd, this, neighborhoodNameToString, newObj)

  def performMove(indActToRm: Int, actToAd: Int, indActToAd: Int): Unit = {
    schedule.activityPriorityList.remove(indActToRm)
    schedule.activityPriorityList.insertAtPosition(actToAd, indActToAd)
  }
}

case class ReplaceActivityMove(removeIndex: Int,
                               actToAdd: Int,
                               addIndex: Int,
                               override val neighborhood: ReplaceActivity,
                               override val neighborhoodName: String = "ReplaceActivityMove",
                               override val objAfter: Long)
  extends SchedulingMove(neighborhood, neighborhoodName, objAfter) {
  override def impactedActivities: Iterable[Int] = {
    val minIndex = math.min(removeIndex, addIndex)
    val maxIndex = math.max(removeIndex, addIndex)
    for { i <- minIndex to maxIndex } yield i
  }

  /** to actually take the move */
  override def commit(): Unit = neighborhood.performMove(removeIndex, actToAdd, addIndex)
}
