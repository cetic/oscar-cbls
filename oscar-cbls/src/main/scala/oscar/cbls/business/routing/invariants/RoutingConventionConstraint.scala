package oscar.cbls.business.routing.invariants

import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.model.VehicleLocation
import oscar.cbls.core.computation.{ChangingSeqValue, Invariant, SeqNotificationTarget, SeqUpdate, SeqUpdateAssign, SeqUpdateDefineCheckpoint, SeqUpdateInsert, SeqUpdateLastNotified, SeqUpdateMove, SeqUpdateRemove, SeqUpdateRollBackToCheckpoint}

import scala.collection.immutable.HashMap

object RoutingConventionConstraint {
  def apply(routes: ChangingSeqValue, n: Int, v: Int): RoutingConventionConstraint ={
    new RoutingConventionConstraint(routes, n,v)
  }
}

/**
  * This class purpose is to test if the search procedure doesn't try unauthorized movement, meaning movement that doesn't respect the OscaR's routing convention.
  * For instance it's not authorized to insert/move/remove vehicle, insert already inserted node ...
  * 
  * By doing so we avoid to have to do this verification for every constraint based on routing.
  * 
  * This class is automatically called by setting 'debug' parameter of class VRP at 'true'
  * 
  * @param routes the route of the VRP problem
  * @param n the total amount of node
  * @param v the total amount of vehicle
  */
class RoutingConventionConstraint(routes: ChangingSeqValue, n: Int, v: Int) extends Invariant with SeqNotificationTarget{

  registerStaticAndDynamicDependency(routes)

  finishInitialization()

  checkVehicleOrder

  def checkVehicleOrder {
    val positionOfVehicles = (0 until v).map(routes.value.positionOfAnyOccurrence(_).get).toArray
    for (vehicle <- 0 until v - 1) {
      require(positionOfVehicles(vehicle) < positionOfVehicles(vehicle + 1),
        "The vehicle depot are not sorted properly ! Their position should be strictly increasing, got : " +
          positionOfVehicles.toList.zipWithIndex.map(x => x._2 + " -> " + x._1))
    }
  }

  private val routedNodes: Array[Boolean] = Array.tabulate(n)(node => if(node < v) true else false)
  // A stack of checkpoint changes. It contains at least an empty HashMap (so we avoid building a new HashMap each time we roll-back or define checkpoint level 0
  private var checkpointsChanges: QList[HashMap[Int, Boolean]] = QList(HashMap.empty)
  private var currentChanges: HashMap[Int, Boolean] = checkpointsChanges.head
  private var checkpointLevel: Int = -1

  private var checkpointAtLevel0 = routes.newValue

  override def notifySeqChanges(routes: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit ={
    digestUpdates(changes)
  }

  private def isRouted(node: Int): Boolean ={
    currentChanges.getOrElse(node, routedNodes(node))
  }

  private def checkRequirement(requirement: Boolean, errorMsg: String, prevUpdates: SeqUpdate): Unit ={
    require(requirement,
      s"""$errorMsg
      |Previous movements : $prevUpdates""".stripMargin)
  }

  /**
   * Update the variable routedNodes + update the data at level 0
   * We need an updated vehicle searcher + an empty map of changes
   *
   * 1° Apply changes ==> taking the current changes and updating the values
   * 2° Remove saved data at checkpoints
   * 3° Updating the vehicle searcher at checkpoint level 0
   * 4° Resetting the current changes
   */
  private def applyChangesAndReset(): Unit ={
    for (element <- currentChanges)
      routedNodes(element._1) = element._2

    for(_ <- 0 until checkpointLevel)
      checkpointsChanges = checkpointsChanges.tail

    checkpointsChanges = QList(checkpointsChanges.head)
    currentChanges = checkpointsChanges.head
  }

  private def digestUpdates(changes: SeqUpdate): Boolean ={
    changes match {
      case _@SeqUpdateDefineCheckpoint(prev: SeqUpdate,isStarMode: Boolean,checkpointLevel: Int) => {
        if(!digestUpdates(prev)) return false
        // if checkpoint = 0 ==> movement validation we need to apply all the recorded changes

        if(checkpointLevel == 0) {
          if (this.checkpointLevel != -1)
            applyChangesAndReset()
          checkpointAtLevel0 = changes.newValue
          // if checkpoint != 0 ==> add a new layer of changes
        } else if(checkpointsChanges.size == checkpointLevel) {
          checkpointsChanges = QList(currentChanges, checkpointsChanges)
          currentChanges = new HashMap[Int, Boolean]() ++ currentChanges
        }

        this.checkpointLevel = checkpointLevel
        true
      }
      case sui@SeqUpdateInsert(value: Int, pos: Int, prev: SeqUpdate) => {
        if(!digestUpdates(prev)) return false
        val errorDataMsg = s"""
             |Got:
             |    Insert value -> $value
             |    Insert pos -> $pos""".stripMargin

        //The pos of the value will be at this position so we need to check if the previous position is within the sequence
        //(otherwise we'll have problem in case we insert at the end of the sequence)
        checkRequirement(pos != 0, "Trying to insert at a depot !" + errorDataMsg, prev)
        checkRequirement(value >= v, "Trying to insert a vehicle !" + errorDataMsg, prev)
        checkRequirement(!isRouted(value), "Node already inserted !" + errorDataMsg, prev)

        currentChanges = currentChanges + ((value, true))
        true
      }
      case sum@SeqUpdateMove(fromPos: Int, toPos: Int, afterPos: Int, flip: Boolean, prev: SeqUpdate) => {
        if(!digestUpdates(prev)) return false
        val errorDataMsg = "\nGot : \n    From position -> " + fromPos + "\n    To position -> " + toPos + "\n    After position -> " + afterPos

        checkRequirement(!sum.movedValuesQList.exists(_<v), "Trying to move a vehicle !" + errorDataMsg, prev)

        true
      }
      case sur@SeqUpdateRemove(pos: Int, prev: SeqUpdate) => {
        if(!digestUpdates(prev)) return false
        val errorDataMsg = "\nGot : \n    Remove pos -> " + pos
        val errorDataMsg = s"""
             |Got:
             |    Remove pos -> $pos""".stripMargin

        checkRequirement(sur.removedValue >= v, "Trying to remove a vehicle !" + errorDataMsg, prev)

        val value = prev.newValue.valueAtPosition(pos).get
        currentChanges = currentChanges + ((value, false))

        true
      }
      case _@SeqUpdateRollBackToCheckpoint(checkpoint:IntSequence,checkpointLevel:Int) =>
        if(checkpointLevel == 0) require(checkpoint quickEquals this.checkpointAtLevel0)

        // Pop required (this.checkpointLevel - checkpointLevel) checkpoint level data
        for(_ <- 0 until this.checkpointLevel - checkpointLevel)
          checkpointsChanges = checkpointsChanges.tail

        currentChanges = checkpointsChanges.head
        this.checkpointLevel = checkpointLevel

        true

      case _@SeqUpdateLastNotified(value: IntSequence) =>
        require(value quickEquals routes.value)
        true

      case _@SeqUpdateAssign(value: IntSequence) => false
    }
  }

  override def checkInternals(c: Checker): Unit = {}

}
