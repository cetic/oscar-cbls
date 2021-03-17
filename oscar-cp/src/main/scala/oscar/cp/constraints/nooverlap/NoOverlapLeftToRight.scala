package oscar.cp.constraints.nooverlap

import oscar.algo.Inconsistency
import oscar.algo.SortUtils.mergeSort
import oscar.cp.Constraint
import oscar.cp.constraints.nooverlap.thetalambdatree.ThetaLambdaTree
import oscar.cp.constraints.nooverlap.util.OptionalIntervalDomain
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.{CPIntVar, CPVar}

class NoOverlapLeftToRight(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], runOnResource: Array[CPIntVar], resourceId : Int) extends Constraint(starts(0).store){

  protected[this] val nTasks = starts.length
  override def associatedVars(): Iterable[CPVar] = starts ++ durations ++ ends ++ runOnResource

  //caches
  protected[this] val currentMinStarts: Array[Int] = Array.ofDim[Int](nTasks)
  protected[this] val currentMaxStarts: Array[Int] = Array.ofDim[Int](nTasks)
  protected[this] val currentMaxEnds: Array[Int] = Array.ofDim[Int](nTasks)
  protected[this] val currentMinEnds: Array[Int] = Array.ofDim[Int](nTasks)
  protected[this] val currentMinDurations: Array[Int] = Array.ofDim[Int](nTasks)
  protected[this] val newMinStarts: Array[Int] = Array.ofDim[Int](nTasks)
  protected[this] val newMaxEnds: Array[Int] = Array.ofDim[Int](nTasks)
  protected[this] val optionalDomains = Array.tabulate(nTasks)(t => new OptionalIntervalDomain(starts(t), durations(t), ends(t), runOnResource(t), resourceId))
  protected[this] val orderedMinStartIds: Array[Int] = Array.tabulate(nTasks)(i => i)
  protected[this] val orderedMaxStartIds: Array[Int] = Array.tabulate(nTasks)(i => i)
  protected[this] val orderedMinEndIds: Array[Int] = Array.tabulate(nTasks)(i => i)
  protected[this] val orderedMaxEndIds: Array[Int] = Array.tabulate(nTasks)(i => i)

  // Temporary arrays needed by the mergeSort Algorithm
  protected[this] val mergeSortRuns = Array.ofDim[Int](nTasks + 1)
  protected[this] val mergeSortAux = Array.ofDim[Int](nTasks)

  protected[this] val tree : ThetaLambdaTree = new ThetaLambdaTree(starts.length)

  override def setup(l: CPPropagStrength): Unit = {
    for (i <- 0 until nTasks) {
      starts(i).callPropagateWhenBoundsChange(this)
      ends(i).callPropagateWhenBoundsChange(this)
      runOnResource(i).callPropagateWhenDomainChanges(this)
    }

    propagate()
  }

  override def propagate(): Unit = {
    do {
      updateCaches()
    }
    while(overloadChecking() || detectablePrecedence() || notLast() || edgeFinding())
  }


  def overloadChecking() : Boolean = {
    // Clearing the tree
    mergeSort(orderedMinStartIds, currentMinStarts, 0, nTasks, mergeSortRuns, mergeSortAux)

    //find out if there are still optional activities
    var someActiStillOptional = false
    var i = 0
    while(i < nTasks && !someActiStillOptional) {
      if(stillPossiblyAssignedToThisResource(i))
        someActiStillOptional = true
      i += 1
    }

    tree.clearAndPlaceLeaves(orderedMinStartIds, currentMinStarts, currentMinDurations, someActiStillOptional)

    //sorting activities in non-decreasing lct
    mergeSort(orderedMaxEndIds, currentMaxEnds, 0, nTasks, mergeSortRuns, mergeSortAux)
    i = 0
    while (i < nTasks) {
      val activityIndex = orderedMaxEndIds(i)
      if(stillPossiblyAssignedToThisResource(activityIndex)) { //the activity is still optional
        tree.insert(activityIndex) //TODO: we should insert and gray in one pass
        tree.grayActivity(activityIndex)
      }
      else if(runOnResource(activityIndex).isBoundTo(resourceId)) { //the activity is running (if the activity does NOT run, we just skip it)
        tree.insert(activityIndex)
        if(tree.ect > currentMaxEnds(activityIndex)) {
          throw Inconsistency
        }
        while (tree.ectBar > currentMaxEnds(activityIndex)) {
          val responsibleActivityIndex = orderedMinStartIds(tree.responsibleEctBar)
          if(runOnResource(responsibleActivityIndex).isBoundTo(resourceId)){
            throw Inconsistency
          }
          runOnResource(responsibleActivityIndex).removeValue(resourceId)
          tree.remove(responsibleActivityIndex)
        }
      }
      i += 1
    }
    false
  }

  def detectablePrecedence(): Boolean = {
    // Clearing the tree
    mergeSort(orderedMinStartIds, currentMinStarts, 0, nTasks, mergeSortRuns, mergeSortAux)
    tree.clearAndPlaceLeaves(orderedMinStartIds, currentMinStarts, currentMinDurations, false)

    //sorting activities in non-decreasing lst
    mergeSort(orderedMaxStartIds, currentMaxStarts, 0, nTasks, mergeSortRuns, mergeSortAux)

    //sorting activities in non-decreasing ect
    mergeSort(orderedMinEndIds, currentMinEnds, 0, nTasks, mergeSortRuns, mergeSortAux)

    var i, j = 0
    while (i < nTasks) {
      val ectIndex = orderedMinEndIds(i)
      while (j < nTasks && (!runOnResource(orderedMaxStartIds(j)).hasValue(resourceId) || currentMinEnds(ectIndex) > currentMaxStarts(orderedMaxStartIds(j)))) {
        if(runOnResource(orderedMaxStartIds(j)).isBoundTo(resourceId)) { //if the activity is not running on the machine, we just skip it. //TODO: maybe it is worth maintaining the different sets of activities separate (running on resource, not running on resource, and undecided yet)
          tree.insert(orderedMaxStartIds(j))
          strengthenMinStart(j, ectIndex)
        }
        j += 1
      }
      val ectWithout = tree.ectWithoutActivity(ectIndex)
      newMinStarts(ectIndex) = math.max(newMinStarts(ectIndex), ectWithout)
      i += 1
    }

    updateMins()
  }

  def notLast(): Boolean  = {
    // Clearing the tree
    mergeSort(orderedMinStartIds, currentMinStarts, 0, nTasks, mergeSortRuns, mergeSortAux)
    tree.clearAndPlaceLeaves(orderedMinStartIds, currentMinStarts, currentMinDurations, false)

    //sorting activities in non-decreasing lst
    mergeSort(orderedMaxStartIds, currentMaxStarts, 0, nTasks, mergeSortRuns, mergeSortAux)

    //sorting activities in non-decreasing lct
    mergeSort(orderedMaxEndIds, currentMaxEnds, 0, nTasks, mergeSortRuns, mergeSortAux)

    var i, j, indexOfLastRunningActivity  = 0
    while(i < nTasks) {
      val lctIndex = orderedMaxEndIds(i)

      while(j < nTasks && (!runOnResource(orderedMaxStartIds(j)).hasValue(resourceId) || currentMaxEnds(lctIndex) > currentMaxStarts(orderedMaxStartIds(j)))) {
        if(runOnResource(orderedMaxStartIds(j)).isBoundTo(resourceId)) {
          // skips the optional activities
          val ect = tree.ect
          val jIndex = orderedMaxStartIds(j)
          if (ect > currentMaxStarts(jIndex)) {
            newMaxEnds(jIndex) = math.min(currentMaxStarts(orderedMaxStartIds(j - 1)) - additionalTime(jIndex, true), newMaxEnds(jIndex))
          }
          tree.insert(orderedMaxStartIds(j))
          indexOfLastRunningActivity = j // used to keep the last activity that actually runs on the machine
        }
        j += 1
      }

      val ectWithout = tree.ectWithoutActivity(lctIndex)
      if(ectWithout > currentMaxStarts(lctIndex)) {
        newMaxEnds(lctIndex) = math.min(currentMaxStarts(orderedMaxStartIds(indexOfLastRunningActivity)) - strengthenMaxEndWithout(lctIndex), newMaxEnds(lctIndex))
      }

      i += 1
    }

    updateMaxs()
  }

  def edgeFinding(): Boolean  = {
    // Inserting all activities in the tree
    mergeSort(orderedMinStartIds, currentMinStarts, 0, nTasks, mergeSortRuns, mergeSortAux)
    tree.fillAndPlaceLeaves(orderedMinStartIds, currentMinStarts, currentMinDurations) // true as we use gray nodes

    var a = 0
    while(a < nTasks) {
      if(!runOnResource(a).hasValue(resourceId))
        tree.remove(a) //TODO: for now we simply remove the activities that are not yet known to be running on the machine. We should modify fillAndPlaceLeaves to take that into account
      else if(!runOnResource(a).isBoundTo(resourceId))
        tree.grayActivity(a)
      a += 1
    }

    //sorting activities in non-decreasing lct
    //NOTE: we sort the array by increasing lct, we just will browse it from left to right
    mergeSort(orderedMaxEndIds, currentMaxEnds, 0, nTasks, mergeSortRuns, mergeSortAux)

    var estIndex = 0
    var j = nTasks - 1
    while (j > 0) {

      if(runOnResource(orderedMaxEndIds(j)).isBoundTo(resourceId)) {
        if(tree.ect > currentMaxEnds(orderedMaxEndIds(j)))
          throw Inconsistency

        tree.grayActivity(orderedMaxEndIds(j))

        j -= 1
        while(tree.ectBar > currentMaxEnds(orderedMaxEndIds(j))) {
          val responsibleEctBar = tree.responsibleEctBar
          if (tree.ectBar == tree.ect || responsibleEctBar < 0)
            throw Inconsistency

          estIndex = orderedMinStartIds(responsibleEctBar)
          newMinStarts(estIndex) = math.max(newMinStarts(estIndex), tree.ect + additionalTime(estIndex, false))
          tree.remove(estIndex)
        }
      }
      else{
        j -= 1
      }
    }
    updateMins()
  }

  private def updateCaches() = {
    var i = 0
    while(i < nTasks) {
      if(runOnResource(i).hasValue(resourceId)) { //skipped if not used
        currentMinDurations(i) = durations(i).min
        if(runOnResource(i).isBoundTo(resourceId)) { //use the actual domains since they are updated by the resource
          currentMinStarts(i) = starts(i).min
          currentMaxEnds(i) = ends(i).max
          currentMinEnds(i) = ends(i).min
          currentMaxStarts(i) = starts(i).max
        }
        else { //if the activity is still optional for this resource, use the internal knowledge about the domains too, since this constraint does not update the domains until the activity is known to be running on the resource
          currentMinStarts(i) = math.max(starts(i).min, optionalDomains(i).minStart.value)
          currentMaxEnds(i) = math.min(ends(i).max, optionalDomains(i).maxStart.value)
          currentMinEnds(i) = math.max(ends(i).min, optionalDomains(i).minEnd.value)
          currentMaxStarts(i) = math.min(starts(i).max, optionalDomains(i).maxEnd.value)
        }
        newMinStarts(i) = currentMinStarts(i)
        newMaxEnds(i) = currentMaxEnds(i)
      }
      i += 1
    }
  }

  protected def stillPossiblyAssignedToThisResource(activity: Int) = runOnResource(activity).hasValue(resourceId) && !runOnResource(activity).isBound

  private def updateMins(): Boolean = {
    var domainModified = false
    var i = 0
    while (i < nTasks) {
      if (runOnResource(i).hasValue(resourceId) && newMinStarts(i) > currentMinStarts(i)) {
        if (newMinStarts(i) > currentMaxStarts(i) || newMinStarts(i) + currentMinDurations(i) > currentMaxEnds(i)) {
          if(runOnResource(i).isBoundTo(resourceId)) {
            throw Inconsistency
          }
          else {
            runOnResource(i).removeValue(resourceId)
            domainModified = true
          }
        }
        else {
          optionalDomains(i).updateMinStart(newMinStarts(i))
          optionalDomains(i).updateMinEnd(newMinStarts(i) + currentMinDurations(i))
          domainModified = true
        }
      }

      i += 1
    }
    domainModified
  }

  private def updateMaxs(): Boolean = {
    var domainModified = false
    var i = 0
    while (i < nTasks) {
      if (runOnResource(i).hasValue(resourceId) && newMaxEnds(i) < currentMaxEnds(i)) {
        if (newMaxEnds(i) < currentMinEnds(i) || newMaxEnds(i) - currentMinDurations(i) < currentMinStarts(i)) {
          if(runOnResource(i).isBoundTo(resourceId)) {
            throw Inconsistency
          }
          else {
            runOnResource(i).removeValue(resourceId)
            domainModified = true
          }
        }
        else {
          optionalDomains(i).updateMaxEnd(newMaxEnds(i))
          optionalDomains(i).updateMaxStart(newMaxEnds(i) - currentMinDurations(i))
          domainModified = true
        }
      }
      i += 1
    }
    domainModified
  }

  protected def strengthenMinStart(j: Int, ectIndex: Int): Unit = {}//cannot strenghthen without transition times
  protected def additionalTime(task: Int, isFrom: Boolean): Int = 0
  protected def strengthenMaxEndWithout(notLastTask: Int): Int = 0

}
