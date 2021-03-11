package oscar.cp.constraints.nooverlap.thetalambdatree

import java.awt.Color

import oscar.util.tree.Node
import oscar.visual.VisualFrame
import oscar.visual.tree.VisualLabelledTree

class ThetaLambdaTree(nElements: Int) {

  protected final val UNDEF = -1

  protected[this] val maxDepth = math.ceil(math.log(nElements) / math.log(2.0)).toInt
  protected[this] val nTotalLeaves = math.pow(2.0, maxDepth).toInt
  protected[this] val nPositions = (nTotalLeaves * 2) - 1
  protected[this] val firstUsedLeafPosition = nTotalLeaves - 1
  protected[this] val lastUsedLeafPosition = firstUsedLeafPosition + nElements - 1

  protected[this] val ests = Array.fill(nPositions)(Int.MinValue)
  protected[this] val durations = Array.fill(nPositions)(0)
  protected[this] val sumDurations = Array.fill(nPositions)(0)
  protected[this] val ects = Array.fill(nPositions)(Int.MinValue)
  protected[this] val isActivityPresent = Array.fill(nPositions)(false)
  protected[this] val sumDurationsBar = Array.fill(nPositions)(0)
  protected[this] val ectsBar = Array.fill(nPositions)(Int.MinValue)

  protected[this] val activityIndexToLeafIndex: Array[Int] = Array.fill(nElements)(UNDEF)

  // Variables used in update
  protected[this] var useOfGrayNodes = false
  protected[this] var currentPos = UNDEF
  protected[this] var leftSon = UNDEF
  protected[this] var rightSon = UNDEF
  protected[this] var sumDurationBarIfLeftGray = UNDEF
  protected[this] var sumDurationBarIfRightGray = UNDEF
  protected[this] var ectBarIfRightEctResponsible = UNDEF
  protected[this] var ectBarIfRightDurationSumResponsible = UNDEF
  protected[this] var ectBarIfLeftEctResponsible = UNDEF
  protected[this] var rootUpdated = false

  // Variables used in most functions
  protected[this] var actiIndex = UNDEF
  protected[this] var nodeIndex = UNDEF

  // Empties the tree and resets the node content to default values
  def clearAndPlaceLeaves(sortedIndexes: Array[Int], currentEsts: Array[Int], currentMinDurations: Array[Int], someActivityStillOptional: Boolean): Unit = {
    useOfGrayNodes = someActivityStillOptional
    var p = 0
    while(p < nPositions) {
      if (p < nElements) {
        activityIndexToLeafIndex(sortedIndexes(p)) = p
      }
      // if it is a used leaf, we update the ests and durations of the leaf according to corresponding activity
      if (p >= firstUsedLeafPosition && p <= lastUsedLeafPosition) {
        actiIndex = p - firstUsedLeafPosition
        ests(p) = currentEsts(sortedIndexes(actiIndex))
        durations(p) = currentMinDurations(sortedIndexes(actiIndex))
      }
      // else default values
      else {
        ests(p) = Int.MinValue
        durations(p) = 0
      }

      //reset node
      sumDurations(p) = 0
      ects(p) = Int.MinValue
      isActivityPresent(p) = false
      sumDurationsBar(p) = 0
      ectsBar(p) = Int.MinValue

      p += 1
    }
  }

  // Fills the tree and updates all the nodes to correct values
  def fillAndPlaceLeaves(sortedIndexes: Array[Int], currentEsts: Array[Int], currentMinDurations: Array[Int]) = {
    fillLeavesOnly(sortedIndexes, currentEsts, currentMinDurations)
    updateInternalNodesByLevel()
  }

  def fillLeavesOnly(sortedIndexes: Array[Int], currentEsts: Array[Int], currentMinDurations: Array[Int]): Unit = {
    useOfGrayNodes = true

    //Setting the leaves up to date
    var p = firstUsedLeafPosition
    while(p < nPositions) {
      // if it is a used leaf, we update the ests and durations of the leaf according to corresponding activity
      if (p <= lastUsedLeafPosition) {
        actiIndex = p - firstUsedLeafPosition
        activityIndexToLeafIndex(sortedIndexes(actiIndex)) = actiIndex
        ests(p) = currentEsts(sortedIndexes(actiIndex))
        durations(p) = currentMinDurations(sortedIndexes(actiIndex))
        isActivityPresent(p) = true
        sumDurations(p) = durations(p)
        ects(p) = ests(p) + durations(p)
        sumDurationsBar(p) = sumDurations(p)
        ectsBar(p) = ects(p)
      }
      // else default values
      else {
        ests(p) = Int.MinValue
        durations(p) = 0
        isActivityPresent(p) = false
        sumDurations(p) = 0
        ects(p) = Int.MinValue
        sumDurationsBar(p) = 0
        ectsBar(p) = Int.MinValue
      }
      //reset node
      p += 1
    }
  }

  final protected def updateInternalNodesByLevel() = {
    var nNodesAtLevel = nTotalLeaves / 2
    var p = (firstUsedLeafPosition - 1) / 2
    while (nNodesAtLevel > 0) {
      updateLevelPositions(p, p + nNodesAtLevel)
      nNodesAtLevel = nNodesAtLevel / 2
      p = (p - 1) / 2
    }
  }

  // Inserts the activityIndex^th activity in the tree and recursively updates its ancestors
  def insert(activityIndex: Int) = {
    insertOnly(activityIndex)
    update((nodeIndex - 1) / 2)
  }

  def insertOnly(activityIndex: Int): Unit = {
    nodeIndex = firstUsedLeafPosition + activityIndexToLeafIndex(activityIndex)
    sumDurations(nodeIndex) = durations(nodeIndex)
    ects(nodeIndex) = ests(nodeIndex) + durations(nodeIndex)
    isActivityPresent(nodeIndex) = true
    if(useOfGrayNodes) {
      sumDurationsBar(nodeIndex) = sumDurations(nodeIndex)
      ectsBar(nodeIndex) = ects(nodeIndex)
    }
  }

  // Removes the activityIndex^th activity in the tree and recursively updates its ancestors
  def remove(activityIndex: Int) = {
    removeOnly(activityIndex)
    update((nodeIndex - 1) / 2)
  }

  def removeOnly(activityIndex: Int) = {
    nodeIndex = firstUsedLeafPosition + activityIndexToLeafIndex(activityIndex)
    sumDurations(nodeIndex) = 0
    ects(nodeIndex) = Int.MinValue
    isActivityPresent(nodeIndex) = false
    if(useOfGrayNodes) {
      sumDurationsBar(nodeIndex) = 0
      ectsBar(nodeIndex) = Int.MinValue
    }
  }

  // Computes the ECT of the tree without the activity at activityIndex (without modifying the tree)
  // Returns a pair (ect, bitset)
  def ectWithoutActivity(activityIndex: Int): Int = {
    nodeIndex = firstUsedLeafPosition + activityIndexToLeafIndex(activityIndex)
    if (!isActivityPresent(nodeIndex)) {
      ect
    }
    else {
      remove(activityIndex)
      val ectWithout = ect
      insert(activityIndex)
      ectWithout
    }
  }

  final def grayActivity(activityIndex: Int): Unit = {
    grayActivityOnly(activityIndex)
    update((nodeIndex - 1) / 2)
  }

  def grayActivityOnly(activityIndex: Int) = {
    nodeIndex = firstUsedLeafPosition + activityIndexToLeafIndex(activityIndex)
    sumDurations(nodeIndex) = 0
    ects(nodeIndex) = Int.MinValue
    isActivityPresent(nodeIndex) = false
  }

  @inline def ect : Int = ects(0)
  @inline final def ectBar : Int = ectsBar(0)
  @inline final def sumDuration : Int = sumDurations(0)
  @inline final def sumDurationWithoutActivity(activityIndex: Int) : Int = {
    val tmp = firstUsedLeafPosition + activityIndexToLeafIndex(activityIndex)
    if (!isActivityPresent(tmp)) {
      sumDuration
    }
    else {
      sumDuration - sumDurations(tmp)
    }
  }

  @inline final def isActivityRemoved(activityIndex: Int) : Boolean = {
    val tmp = firstUsedLeafPosition + activityIndexToLeafIndex(activityIndex)
    sumDurationsBar(tmp) > 0
  }

  @inline final def sumDurationBar : Int = sumDurationsBar(0)
  @inline final def responsibleEctBar: Int = getResponsibleEctBar(0) - firstUsedLeafPosition

  @inline
  def getResponsibleEctBar(pos: Int): Int = {
    currentPos = pos
    while (currentPos < firstUsedLeafPosition || currentPos > lastUsedLeafPosition) {
      leftSon = 2 * currentPos + 1
      rightSon = 2 * currentPos + 2

      computePotentialEctBar()

      // This deals with the case where there are no gray activities in the tree anymore
      if (ects(currentPos) == ectsBar(currentPos)) {
        return UNDEF
      }
      else if (ectsBar(currentPos) == ectsBar(rightSon)) {
        currentPos = rightSon
      }
      else if (ectsBar(currentPos) == ectBarIfRightDurationSumResponsible) {
        return getResponsibleSumDurationsBar(rightSon)
      }
      else if (ectsBar(currentPos) == ectBarIfLeftEctResponsible) {
        currentPos = leftSon
      }
      else {
        throw new Exception("IMPOSSIBLE CASE")
      }
    }
    currentPos
  }

  @inline
  def getResponsibleSumDurationsBar(pos: Int): Int = {
    currentPos = pos
    while (currentPos < firstUsedLeafPosition || currentPos > lastUsedLeafPosition) {
      leftSon = 2 * currentPos + 1
      rightSon = 2 * currentPos + 2

      if(isSonResponsibleForDurationBar(currentPos, leftSon, rightSon))
        currentPos = leftSon
      else if (isSonResponsibleForDurationBar(currentPos, rightSon, leftSon))
        currentPos = rightSon
      else
        throw new Exception("IMPOSSIBLE CASE")
    }
    currentPos
  }

  protected def isSonResponsibleForDurationBar(pos: Int, responsibleSon: Int, brother: Int) : Boolean = {
    sumDurationsBar(pos) == sumDurationsBar(responsibleSon) + sumDurations(brother)
  }

  // Updates the values contained in node p
  @inline
  final protected def update(pos: Int) : Unit = {
    currentPos = pos
    rootUpdated = false

    while (!rootUpdated) {
      updateCurrentPosition()

      // Needed by EF and optional activities
      if (useOfGrayNodes) {
        updateCurrentPositionGray()
      }
      //did we just update the root ?
      rootUpdated = currentPos == 0

      //go to father
      currentPos = (currentPos - 1) / 2
    }
  }

  def updateCurrentPosition(): Unit = {
    leftSon = 2 * currentPos + 1
    rightSon = 2 * currentPos + 2
    sumDurations(currentPos) = sumDurations(leftSon) + sumDurations(rightSon)
    ects(currentPos) = math.max(ects(rightSon), ects(leftSon) + sumDurations(rightSon))
  }

  def updateCurrentPositionGray(): Unit = {
    sumDurationBarIfLeftGray = sumDurationsBar(leftSon) + sumDurations(rightSon)
    sumDurationBarIfRightGray = sumDurations(leftSon) + sumDurationsBar(rightSon)

    ectBarIfRightEctResponsible = ectsBar(rightSon)
    computePotentialEctBar()

    // Gray is on the right subtree and it is the right ectBar that is responsible
    if(ectBarIfRightEctResponsible >= ectBarIfRightDurationSumResponsible && ectBarIfRightEctResponsible >= ectBarIfLeftEctResponsible) {
      updateIfRightGrayEctResponsible()
    }
    // Gray is on the right subtree and it is the right sumDurBar that is responsible
    else if (ectBarIfRightDurationSumResponsible >= ectBarIfLeftEctResponsible && sumDurationsBar(rightSon) > 0) { // the second condition checks whether the right theta-lambda tree is not empty
      updateIfRightGrayDurResponsible()
    }
    // Gray is on the left subtree and it is the left ectBar that is responsible
    else {
      updateIfLeftGray()
    }
  }

  def computePotentialEctBar(): Unit = {
    ectBarIfRightDurationSumResponsible = ects(leftSon) + sumDurationsBar(rightSon)
    ectBarIfLeftEctResponsible = ectsBar(leftSon) + sumDurations(rightSon)
  }

  def updateIfLeftGray(): Unit = {
    ectsBar(currentPos) = ectBarIfLeftEctResponsible
    sumDurationsBar(currentPos) = sumDurationBarIfLeftGray
  }

  def updateIfRightGrayEctResponsible(): Unit = {
    ectsBar(currentPos) = ectBarIfRightEctResponsible
    sumDurationsBar(currentPos) = sumDurationBarIfRightGray
  }

  def updateIfRightGrayDurResponsible(): Unit = {
    ectsBar(currentPos) = ectBarIfRightDurationSumResponsible
    sumDurationsBar(currentPos) = sumDurationBarIfRightGray
  }

  // Updates the values contained in node from minPos until maxPos (updates a level of the tree)
  @inline
  protected def updateLevelPositions(minPos: Int, maxPos: Int) : Unit = {
    currentPos = minPos
    leftSon = UNDEF
    rightSon = UNDEF

    while (currentPos < maxPos) {
      updateCurrentPosition()
      resetGrayCurrentPosition()

      // go to next node
      currentPos += 1
    }
  }

  def resetGrayCurrentPosition(): Unit = {
    sumDurationsBar(currentPos) = sumDurations(currentPos)
    ectsBar(currentPos) = ects(currentPos)
  }

  // TODO: could be more efficient here by using incrementality
  def estOfCurrentTheta() : Int = {
    var p = firstUsedLeafPosition
    while (p <= lastUsedLeafPosition) {
      if(isActivityPresent(p)) {
        return ests(p)
      }
      p += 1
    }
    Int.MinValue
    //    ests(p)
  }

  def estOfCurrentThetaWithout(activityIndex: Int) : Int = {
    val activityLeafPosition = firstUsedLeafPosition + activityIndexToLeafIndex(activityIndex)
    var p = firstUsedLeafPosition
    while (p <= lastUsedLeafPosition) {
      if(p != activityLeafPosition && isActivityPresent(p)) {
        return ests(p)
      }
      p += 1
    }
    Int.MinValue
  }

  def buildNodes: Array[Node[String]] = {
    val nodeArray = Array.ofDim[Node[String]](nPositions)
    var nNodesAtLevel = nTotalLeaves
    var p = firstUsedLeafPosition
    while (nNodesAtLevel > 0) {
      buildNodeAux(p, p + nNodesAtLevel, nodeArray)
      nNodesAtLevel = nNodesAtLevel / 2
      p = (p - 1) / 2
    }
    nodeArray
  }

  def buildNodeAux(minPos: Int, maxPos: Int, nodeRef: Array[Node[String]]): Unit = {
    currentPos = minPos
    leftSon = UNDEF
    rightSon = UNDEF

    while (currentPos < maxPos) {
      if (maxPos < nPositions - 1) {
        leftSon = 2 * currentPos + 1
        rightSon = 2 * currentPos + 2
        nodeRef(currentPos) = Node(getNodeString(currentPos), List(nodeRef(leftSon), nodeRef(rightSon)), List("L", "R"))
      }
      else {
        if (sumDurationsBar(currentPos) > 0 && !isActivityPresent(currentPos)) {
          nodeRef(currentPos) = Node(getNodeString(currentPos), col = Color.GRAY)
        }
        else {
          nodeRef(currentPos) = Node(getNodeString(currentPos))
        }
      }
      currentPos += 1
    }
  }

  def getNodeString(positionIndex: Int): String = {
    val strBuilder = new StringBuilder()
    strBuilder.append("ect = ")
    strBuilder.append(ects(positionIndex))
    strBuilder.append("\n")
    strBuilder.append("sumD = ")
    strBuilder.append(sumDurations(positionIndex))
    strBuilder.append("\n")
    strBuilder.append("ectBar = ")
    strBuilder.append(ectsBar(positionIndex))
    strBuilder.append("\n")
    strBuilder.append("sumDBar = ")
    strBuilder.append(sumDurationsBar(positionIndex))
    strBuilder.append("\n")
    strBuilder.append("est = ")
    strBuilder.append(ests(positionIndex))
    strBuilder.toString()
  }

  def drawTreeForXSeconds(nSeconds: Int) = {
    drawTree
    Thread.sleep(nSeconds * 1000)
  }

  def drawTree = {
    val nodes = buildNodes
    val positionedRoot = Node.design(nodes(0), 180)
    val visualTree = new VisualLabelledTree(positionedRoot)
    val f = VisualFrame("Theta-Lambda Tree Visualizer")
    val inf = f.createFrame("Theta-Lambda Tree with variable EST")
    inf.add(visualTree)
    f.pack()
    visualTree.repaint()
  }
}
