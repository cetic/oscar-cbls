package oscar.cp.constraints.nooverlap.thetalambdatree

class ThetaLambdaTreeTransitionTimesFamilies(nElements: Int, family: Array[Int], minTransitionToFamily: Array[Int], familyTransitionSetLowerBounds: Array[Int]) extends ThetaLambdaTree(nElements) {

  protected[this] val families: Array[Int] = Array.fill(nPositions)(0)
  protected[this] val familiesBar = Array.fill(nPositions)(0)
  var lastFamiliesInThetaWithoutAnActivity = UNDEF
  private[this] var interSectSize = UNDEF
  private[this] var cardFamilies = UNDEF
  private[this] var interSectSizeWithGray = UNDEF
  private[this] var additionalGrayInRight = UNDEF
  private[this] var interSectSizeRightGray = UNDEF
  private[this] var cardFamiliesRightGray = UNDEF
  private[this] var cardFamiliesLeftGray = UNDEF

  override def clearAndPlaceLeaves(sortedIndexes: Array[Int], currentEsts: Array[Int], currentMinDurations: Array[Int], someActivityStillOptional: Boolean): Unit = {
    super.clearAndPlaceLeaves(sortedIndexes, currentEsts, currentMinDurations, someActivityStillOptional)
    var p = 0
    while(p < nPositions) {
      //reset node
      families(p) = 0
      familiesBar(p) = 0
      p += 1
    }
  }

  override def fillLeavesOnly(sortedIndexes: Array[Int], currentEsts: Array[Int], currentMinDurations: Array[Int]): Unit = {
    super.fillLeavesOnly(sortedIndexes, currentEsts, currentMinDurations)
    var p = firstUsedLeafPosition
    while(p < nPositions) {
      // if it is a used leaf, we update the ests and durations of the leaf according to corresponding activity
      if (p <= lastUsedLeafPosition) {
        actiIndex = p - firstUsedLeafPosition
        families(p) = 1 << family(sortedIndexes(actiIndex))
      }
      // else default values
      else {
        families(p) = 0
      }
      familiesBar(p) = families(p)
      p += 1
    }
  }

  override def insertOnly(activityIndex: Int): Unit = {
    super.insertOnly(activityIndex)
    families(nodeIndex) = 1 << family(activityIndex)
    if(useOfGrayNodes) {
      familiesBar(nodeIndex) = families(nodeIndex)
    }
  }

  override def removeOnly(activityIndex: Int): Unit = {
    super.removeOnly(activityIndex)
    families(nodeIndex) = 0
    if(useOfGrayNodes) {
      familiesBar(nodeIndex) = 0
    }
  }

  override def grayActivityOnly(activityIndex: Int): Unit = {
    super.grayActivityOnly(activityIndex)
    families(nodeIndex) = 0
  }

  override def computePotentialEctBar(): Unit = {
    // Computes ECTBar if gray activity is on the right
    interSectSizeRightGray = Integer.bitCount(familiesBar(rightSon) & families(leftSon))
    cardFamiliesRightGray = Integer.bitCount(familiesBar(rightSon)) - interSectSizeRightGray - 1
    if (familiesBar(rightSon) > 0 && families(leftSon) > 0) {
      cardFamiliesRightGray += 1
    }
    if (cardFamiliesRightGray < 0) {
      cardFamiliesRightGray = 0
    }
    ectBarIfRightDurationSumResponsible = ects(leftSon) + sumDurationsBar(rightSon) + familyTransitionSetLowerBounds(cardFamiliesRightGray)

    // Computes ECTBar if gray activity is on the left
    interSectSizeWithGray = Integer.bitCount(families(rightSon) & familiesBar(leftSon))
    cardFamiliesLeftGray = Integer.bitCount(families(rightSon)) - interSectSizeWithGray - 1
    if (families(rightSon) > 0 && familiesBar(leftSon) > 0) {
      cardFamiliesLeftGray += 1
    }
    if (cardFamiliesLeftGray < 0) {
      cardFamiliesLeftGray = 0
    }
    ectBarIfLeftEctResponsible = ectsBar(leftSon) + sumDurations(rightSon) + familyTransitionSetLowerBounds(cardFamiliesLeftGray)
  }

  override def isSonResponsibleForDurationBar(pos: Int, responsibleSon: Int, brother: Int) : Boolean = {
    super.isSonResponsibleForDurationBar(pos, responsibleSon, brother) &&
      familiesBar(pos) == (families(pos) | familiesBar(responsibleSon))
  }

  override def updateCurrentPosition(): Unit = {
    super.updateCurrentPosition()
    interSectSize = Integer.bitCount(families(rightSon) & families(leftSon))
    cardFamilies = Integer.bitCount(families(rightSon)) - interSectSize - 1
    if (families(rightSon) > 0 && families(leftSon) > 0) {
      cardFamilies += 1
    }
    if (cardFamilies < 0) {
      cardFamilies = 0
    }
    ects(currentPos) = math.max(ects(rightSon), ects(leftSon) + sumDurations(rightSon) + familyTransitionSetLowerBounds(cardFamilies))
    families(currentPos) = families(leftSon) | families(rightSon)
  }

  override def updateIfLeftGray(): Unit = {
    super.updateIfLeftGray()
    familiesBar(currentPos) = familiesBar(leftSon) | families(rightSon)
  }

  override def updateIfRightGrayEctResponsible(): Unit = {
    super.updateIfRightGrayEctResponsible()
    familiesBar(currentPos) = families(leftSon) | familiesBar(rightSon)
  }
  override def updateIfRightGrayDurResponsible(): Unit = {
    super.updateIfRightGrayDurResponsible()
    familiesBar(currentPos) = families(leftSon) | familiesBar(rightSon)
  }

  override def resetGrayCurrentPosition(): Unit = {
    super.resetGrayCurrentPosition()
    familiesBar(currentPos) = families(currentPos)
  }

  override def getNodeString(positionIndex: Int): String = {
    val strBuilder = new StringBuilder()
    strBuilder.append("families = ")
    strBuilder.append(("%0" + familyTransitionSetLowerBounds.length + "d").format(families(positionIndex).toBinaryString.toInt))
    strBuilder.append("\n")
    strBuilder.append("familyG = ")
    strBuilder.append(("%0" + familyTransitionSetLowerBounds.length + "d").format(familiesBar(positionIndex).toBinaryString.toInt))
    strBuilder.append("\n")
    super.getNodeString(positionIndex) ++ strBuilder.toString()
  }

  override def ectWithoutActivity(activityIndex: Int): Int = {
    val ectWithout : Int = super.ectWithoutActivity(activityIndex)
    val familiesInTheta = lastFamiliesInThetaWithoutAnActivity
    val fam : Int = family(activityIndex)
    if (familiesInTheta != 0 && ((1 << fam & familiesInTheta) == 0))
      ectWithout + minTransitionToFamily(fam)
    else
      ectWithout
  }

  @inline final def familiesInTheta : Int = families(0)
}
