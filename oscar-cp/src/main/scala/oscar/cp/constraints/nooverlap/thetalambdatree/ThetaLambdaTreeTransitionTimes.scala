package oscar.cp.constraints.nooverlap.thetalambdatree

class ThetaLambdaTreeTransitionTimes(nElements: Int, transitionSetLowerBounds: Array[Int]) extends ThetaLambdaTree(nElements) {

  private[this] val nLeaves = Array.fill(nPositions)(0)
  private[this] val nLeavesBar = Array.fill(nPositions)(0)

  override def clearAndPlaceLeaves(sortedIndexes: Array[Int], currentEsts: Array[Int], currentMinDurations: Array[Int], someActivityStillOptional: Boolean): Unit = {
    super.clearAndPlaceLeaves(sortedIndexes, currentEsts, currentMinDurations, someActivityStillOptional)
    var p = 0
    while(p < nPositions) {
      nLeaves(p) = 0
      nLeavesBar(p) = 0
      p += 1
    }
  }

  override def fillLeavesOnly(sortedIndexes: Array[Int], currentEsts: Array[Int], currentMinDurations: Array[Int]): Unit = {
    super.fillLeavesOnly(sortedIndexes, currentEsts, currentMinDurations)
    var p = firstUsedLeafPosition
    while(p < nPositions) {
      // if it is a used leaf, we update the ests and durations of the leaf according to corresponding activity
      if (p <= lastUsedLeafPosition) {
        nLeaves(p) = 1
      }
      // else default values
      else {
        nLeaves(p) = 0
      }
      nLeavesBar(p) = nLeaves(p)
      p += 1
    }
  }

  override def insertOnly(activityIndex: Int): Unit = {
    super.insertOnly(activityIndex)
    nLeaves(nodeIndex) = 1
    if(useOfGrayNodes) {
      nLeavesBar(nodeIndex) = 1
    }
  }

  override def removeOnly(activityIndex: Int): Unit = {
    super.removeOnly(activityIndex)
    nLeaves(nodeIndex) = 0
    if(useOfGrayNodes) {
      nLeavesBar(nodeIndex) = 0
    }
  }

  override def grayActivityOnly(activityIndex: Int): Unit = {
    super.grayActivityOnly(activityIndex)
    nLeaves(nodeIndex) = 0
    nLeavesBar(nodeIndex) = 1
  }

  override def computePotentialEctBar(): Unit = {
    ectBarIfRightDurationSumResponsible = ects(leftSon) + sumDurationsBar(rightSon) + transitionSetLowerBounds(nLeavesBar(rightSon))
    ectBarIfLeftEctResponsible = ectsBar(leftSon) + sumDurations(rightSon) + transitionSetLowerBounds(nLeaves(rightSon))
  }

  override def updateCurrentPosition(): Unit = {
    super.updateCurrentPosition()
    nLeaves(currentPos) = nLeaves(leftSon) + nLeaves(rightSon)
    ects(currentPos) = math.max(ects(rightSon), ects(leftSon) + sumDurations(rightSon) + transitionSetLowerBounds(nLeaves(rightSon))) //TODO:here we erase ects(currentPos) that was computed in super.updateCurrentPosition(). Small overhead.
  }

  override def updateCurrentPositionGray() = {
    super.updateCurrentPositionGray()
    nLeavesBar(currentPos) = if (sumDurationsBar(currentPos) == sumDurations(currentPos)) nLeaves(currentPos) else nLeaves(currentPos) + 1
  }

  override def getNodeString(positionIndex: Int): String = {
    val strBuilder = new StringBuilder()
    strBuilder.append("nL = ")
    strBuilder.append("\n")
    strBuilder.append(nLeaves(positionIndex))
    strBuilder.append("\n")
    strBuilder.append("nLBar = ")
    strBuilder.append(nLeavesBar(positionIndex))
    strBuilder.append("\n")
    super.getNodeString(positionIndex) ++ strBuilder.toString()
  }
}
