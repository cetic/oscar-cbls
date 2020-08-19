package oscar.cbls.business.routing.invariants

import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.model.VehicleLocation
import oscar.cbls.core.computation.{ChangingSeqValue, Invariant, SeqCheckpointedValueStack, SeqNotificationTarget, SeqUpdate, SeqUpdateAssign, SeqUpdateDefineCheckpoint, SeqUpdateInsert, SeqUpdateLastNotified, SeqUpdateMove, SeqUpdateRemove, SeqUpdateRollBackToCheckpoint}
import oscar.cbls.core.propagation.Checker

/**
 * This invariant has no output, only a method to get the vehicle reaching a given position, and a method to get the stat point of a vehicle
  * it is intended to make neighborhoods simpler to implement
 * EXPERIMENTAL
 */
class StartPointOfVehicles(routes:ChangingSeqValue,
                           v:Int)
  extends Invariant() with SeqNotificationTarget {

  private val vehicleStartStack = new SeqCheckpointedValueStack[VehicleLocation]()
  private var currentVehicleLocation : VehicleLocation = VehicleLocation(v, routes.value.positionOfAnyOccurrence(_).get)

  def startPosOfVehicle(vehicle : Int) : Int = {
    currentVehicleLocation.startPosOfVehicle(vehicle)
  }

  def vehicleReachingPosition(position : Int) : Int = {
    currentVehicleLocation.vehicleReachingPosition(position)
  }

  def vehicleReachingNode(node: Int) : Option[Int] = {
    routes.value.positionOfAnyOccurrence(node) match{
      case Some(nodePosition) => Some(vehicleReachingPosition(nodePosition))
      case None => None
    }
  }

  def onSameVehicleOrBothUnrouted(node1:Int,node2:Int): Boolean={
    vehicleReachingNode(node1) == vehicleReachingNode(node2)
  }

  def onVehicle(vehicle: Int,node: Int): Boolean = {
    vehicleReachingNode(node) contains vehicle
  }

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    currentVehicleLocation = digestUpdates(changes)
  }

  def digestUpdates(changes : SeqUpdate) : VehicleLocation = {
    changes match {
      case s@SeqUpdateInsert(value : Int, posOfInsert : Int, prev : SeqUpdate) =>
        digestUpdates(prev).push(s.oldPosToNewPos)

      case r@SeqUpdateRemove(pos : Int, prev : SeqUpdate) =>
        digestUpdates(prev).push(r.oldPosToNewPos)

      case m@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        digestUpdates(prev).push(m.oldPosToNewPos)

      case SeqUpdateAssign(value) =>
        VehicleLocation(v, value.positionOfAnyOccurrence(_).get)

      case SeqUpdateLastNotified(value) =>
        currentVehicleLocation

      case s@SeqUpdateDefineCheckpoint(prev : SeqUpdate, isStarMode : Boolean, checkpointLevel : Int) =>
        val previousVehicleStart = if (checkpointLevel == 0) digestUpdates(prev).regularize else digestUpdates(prev)
        vehicleStartStack.defineCheckpoint(prev.newValue, checkpointLevel, previousVehicleStart)
        previousVehicleStart

      case u@SeqUpdateRollBackToCheckpoint(checkpoint : IntSequence, level : Int) =>
        vehicleStartStack.rollBackAndOutputValue(checkpoint, level)
    }
  }

  override def checkInternals(c : Checker): Unit ={
    currentVehicleLocation.checkOnSequence(routes.value)
  }
}
