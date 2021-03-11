package oscar.cp.scheduling.search

import oscar.algo.reversible.ReversibleInt
import oscar.algo.search.{Branching, Decision}
import oscar.cp.{Alternative, CPIntVar, noAlternative}

/**
  * Created by saschavancauwelaert on 16/03/2017.
  */
class StaticBranchingWithOptionalActivities(starts: Array[CPIntVar], runOnResource: Array[CPIntVar], resourceID: Int) extends Branching {

  private[this] val nActivities = starts.length
//  private[this] val variables = runOnResource ++ starts

  private[this] val store = starts(0).store
//  private[this] val nVariables = variables.length

  private[this] val nPlannedActivitiesRev = new ReversibleInt(store, 0)
  private[this] var nPlannedActivities = 0

  private[this] val nAllocatedActivitiesRev = new ReversibleInt(store, 0)
  private[this] var nAllocatedActivities = 0

  final override def alternatives(): Seq[Alternative] = {
    // Cache
    nPlannedActivities = nPlannedActivitiesRev.value

    // Update nPlannedActivities
    while (nPlannedActivities < nActivities && runOnResource(nPlannedActivities).isBound) nPlannedActivities += 1

    if (nPlannedActivities == nActivities) {
      nAllocatedActivities = nAllocatedActivitiesRev.value
      // Update nAllocatedActivities (activities not running on machine are considered to be allocated)
      while (nAllocatedActivities < nActivities && (!runOnResource(nAllocatedActivities).hasValue(resourceID) || starts(nAllocatedActivities).isBound)) nAllocatedActivities += 1

      if (nAllocatedActivities == nActivities)
        noAlternative
      else {
        // Trail new nAllocatedActivities
        nAllocatedActivitiesRev.value = nAllocatedActivities
        // Alternatives
        val variable = starts(nAllocatedActivities)
        val value = starts(nAllocatedActivities).min
        List(Decision.assign(variable, value), Decision.remove(variable, value))
      }
    }
    else { //still have to decide if some activities are scheduled or not
      // Trail new nPlannedActivities
      nPlannedActivitiesRev.value = nPlannedActivities
      val variable = runOnResource(nPlannedActivities)
      List(Decision.assign(variable, resourceID), Decision.remove(variable, resourceID))
    }

  }

}
