package oscar.cbls.business.scheduling.neighborhood

import oscar.cbls.algo.search.HotRestart
import oscar.cbls.business.scheduling.model.Schedule
import oscar.cbls.core.search.{Best, EasyNeighborhoodMultiLevel, First, LoopBehavior}

class AddActivity(schedule: Schedule,
                  neighborhoodName: String,
                  selectValueBehavior:LoopBehavior = First(),
                  selectIndexBehavior:LoopBehavior = Best(),
                  searchValues: Option[() => Iterable[Int]] = None)
  extends EasyNeighborhoodMultiLevel[AddActivityMove](neighborhoodName) {

  var currentValue: Int = -1
  var insertIndex: Int = -1

  /**
    * This is the method you must implement and that performs the search of your neighborhood.
    * every time you explore a neighbor, you must perform the calls to notifyMoveExplored or moveRequested(newObj) && submitFoundMove(myMove)){
    * as explained in the documentation of this class
    */
  override def exploreNeighborhood(initialObj: Long): Unit = {
    // Iteration zone on values to add (optional activities)
    // Checking the Hot Restart
    val iterationZone1: () => Iterable[Int] = searchValues.getOrElse(() =>
      schedule
        .activities
        .filterNot(schedule.activityPriorityList.value.contains(_))
    )
    val hotRestart = true
    val iterationZone: Iterable[Int] =
      if (hotRestart) HotRestart(iterationZone1(), currentValue)
      else iterationZone1()
    // iterating over the values in the activity list
    val (valuesIterator, notifyValueFound) = selectValueBehavior.toIterator(iterationZone)
    // Define checkpoint on sequence (activities list)
    val seqValueCheckPoint = schedule.activityPriorityList.defineCurrentValueAsCheckpoint(true)
    // Main loop
    while (valuesIterator.hasNext) {
      currentValue = valuesIterator.next()
      // explore the insertable zone of the current value
      val insertableZone = schedule.insertableIndices(currentValue)
      val (insertableIterator, notifyInsertFound) = selectIndexBehavior.toIterator(insertableZone)
      while (insertableIterator.hasNext) {
        insertIndex = insertableIterator.next()
        // perform move
        performMove(currentValue, insertIndex)
        val newObj = obj.value
        // Rollback to checkpoint
        schedule.activityPriorityList.rollbackToTopCheckpoint(seqValueCheckPoint)
        // Notification of finding indices
        if (evaluateCurrentMoveObjTrueIfSomethingFound(newObj)) {
          notifyValueFound()
          notifyInsertFound()
        }
      }
    }
    schedule.activityPriorityList.releaseTopCheckpoint()
  }

  override def instantiateCurrentMove(newObj: Long): AddActivityMove =
    AddActivityMove(currentValue, insertIndex, schedule.activityPriorityList.value.size, this, neighborhoodNameToString, newObj)

  def performMove(actValue: Int, addIndex: Int): Unit = {
    schedule.activityPriorityList.insertAtPosition(actValue, addIndex)
  }
}

case class AddActivityMove(actValue: Int,
                           addIndex: Int,
                           numActiveActivities: Int,
                           override val neighborhood: AddActivity,
                           override val neighborhoodName: String = "AddActivityMove",
                           override val objAfter: Long)
  extends SchedulingMove(neighborhood, neighborhoodName, objAfter) {
  override def impactedActivities: Iterable[Int] = for {i <- addIndex until numActiveActivities} yield i

  /** to actually take the move */
  override def commit(): Unit = {
    neighborhood.performMove(actValue, addIndex)
  }
}