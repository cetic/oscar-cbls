/**
 * *****************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 * ****************************************************************************
 */
package oscar.cbls.algo.fun

import oscar.cbls.algo._
import oscar.cbls.algo.rb.{RedBlackTreeMap, RedBlackTreeMapExplorer}

import scala.annotation.tailrec

object PiecewiseLinearFun{

  def identity = new PiecewiseLinearFun()

  implicit def toIterable(f:PiecewiseLinearFun):Iterable[Pivot] = f.transformation.values

  def createFromPivots(pivots:Iterable[Pivot]):PiecewiseLinearFun = {
    var acc = RedBlackTreeMap.empty[Pivot]
    val pivotIt = pivots.iterator
    while(pivotIt.hasNext){
      val currentPivot = pivotIt.next()
      acc =  acc.insert(currentPivot.fromValue,currentPivot)
    }
    new PiecewiseLinearFun(acc)
  }
}
/*
class CachedPiecewiseLinearFun(f:PiecewiseLinearFun) extends PiecewiseLinearFun(f.transformation){

  var cachedPivotF:LinearTransform = null
  var cachedPivotStart:Int = 0
  var cachedPivotEnd:Int = -1

  override def apply(value:Int):Int = {
    val toReturn = if(cachedPivotF != null && (cachedPivotStart <= value) && (value <= cachedPivotEnd)){
      cachedPivotF(value)
    }else {
      transformation.biggestLowerOrEqual(value) match {
        case None =>
          cachedPivotF = LinearTransform.identity
          cachedPivotStart = Int.MinValue
        case Some((k,pivot)) =>
          cachedPivotF = pivot.f
          cachedPivotStart = k
      }
      cachedPivotEnd = transformation.smallestBiggerOrEqual(value) match {
        case None => Int.MaxValue
        case Some((kAbove, _)) => kAbove-1
      }
      cachedPivotF(value)
    }
    assert(toReturn == super.apply(value))
    toReturn
  }
}
*/

class PiecewiseLinearFun(private[fun] val transformation: RedBlackTreeMap[Pivot] = RedBlackTreeMap.empty) {

  def equals(that:PiecewiseLinearFun):Boolean = {
    if(this.nbPivot != that.nbPivot) return false
    for((thisPivot,thatPivot) <- this.pivots.zip(that.pivots)){
      if(thisPivot.fromValue != thatPivot.fromValue) return false
      if(! thisPivot.f.equals(thatPivot.f)) return false
    }
    true
  }

  def firstPivot:Option[(Int,Pivot)] = {
    transformation.smallestBiggerOrEqual(Int.MinValue) match {
      case None => None
      case Some((key,value)) => Some((longToInt(key),value))
    }
  }

  def positionOfValue(value:Int):Option[RedBlackTreeMapExplorer[Pivot]] = transformation.positionOf(value)

  def isIdentity: Boolean = transformation.isEmpty

  def nbPivot: Int = transformation.size

  def pivots:List[Pivot] = transformation.values

  override def toString: String = {
    s"PiecewiseLinearFun(nbSegments:${transformation.size}, ${if(transformation.isEmpty) "identity" else s"segments:${transformation.values.mkString(",")}"})"
  }

  def apply(value:Int):Int = {
    transformation.biggestLowerOrEqual(value) match {
      case None => value
      case Some((_,pivot)) => pivot.f(value)
    }
  }

  def updateForCompositionBefore(updates:(Int,Int,LinearTransform)*):PiecewiseLinearFun = {
    var updatedTransform=transformation
    for((fromIncluded,toIncluded,update) <- updates) {
      updatedTransform = deleteUnnecessaryPivotStartingJustAfter(
        toIncluded,
        myUpdateForCompositionBefore(
          fromIncluded,
          toIncluded,
          update,
          removePivotsBetween(
            fromIncluded,
            toIncluded,
            updatedTransform)))
    }
    new PiecewiseLinearFun(updatedTransform)
  }

  def swapAdjacentZonesShiftBest(startZone1Included:Int, endZone1Included:Int, endZone2Included:Int):PiecewiseLinearFun = {
    val widthZone1 = endZone1Included - startZone1Included + 1
    val widthZone2 = endZone2Included - endZone1Included
    //TODO: the choice is based on the number of positions, it should be based on the number of segments instead (but this is probably the same very often)
    if(widthZone1 > widthZone2){
      swapAdjacentZonesShiftFirst(startZone1Included, endZone1Included, endZone2Included, flipZone2 = false)
    }else{
      val tmp = swapAdjacentZonesShiftSecond(startZone1Included, endZone1Included, endZone2Included, flipZone1 = false)
      assert(tmp equals swapAdjacentZonesShiftFirst(startZone1Included, endZone1Included, endZone2Included, flipZone2 = false))
      tmp
    }
  }

  def swapAdjacentZonesShiftFirst(startZone1Included:Int, endZone1Included:Int, endZone2Included:Int, flipZone2:Boolean):PiecewiseLinearFun = {
    val widthZone2 = endZone2Included - endZone1Included
    val widthZone1 = endZone1Included - startZone1Included + 1

    //remove pivot for destination of zone1
    val transformWithTargetZone2Cleaned = removePivotsBetween(endZone1Included+1, endZone2Included,transformation)

    //a pivot must be added after the end of update1 if 2 is the first zone
    val transformReadyForShiftOfZone1 = addRedundantPivotAt(startZone1Included,transformWithTargetZone2Cleaned)

    val f2 = LinearTransform(-widthZone2,minus = false)
    val f3 = LinearTransform(widthZone2,minus = false)

    val transformWithZone1Shifted =
      transformReadyForShiftOfZone1.updateDelta(
        startZone1Included,
        endZone1Included,
        widthZone2,
        (p:Pivot) => new Pivot(p.fromValue+widthZone2,if(p.f.minus) f3(p.f) else f2(p.f)))

    //now, we apply the second transformation that will add pivots at the zone occupied by zone1 (which is now cleaned)
    val transformationWithUpdate2Done = myUpdateForCompositionBefore(startZone1Included, startZone1Included + widthZone2 - 1,
      LinearTransform(if(flipZone2) endZone2Included + startZone1Included else widthZone1,flipZone2), transformWithZone1Shifted)

    //finally, we can clean the potentially redundant pivots
    new PiecewiseLinearFun(
      deleteUnnecessaryPivotStartingJustAfter(startZone1Included-1,
      deleteUnnecessaryPivotStartingJustAfter(startZone1Included + widthZone2 - 1,
        deleteUnnecessaryPivotStartingJustAfter(endZone2Included,transformationWithUpdate2Done))))
  }

  /**
   * flips the function between the given bounds. does not add closing pivots before
   * @param startZoneIncluded
   * @param endZoneIncluded
   * @return
   */
  def flipFunctionInInterval(startZoneIncluded:Int,endZoneIncluded:Int):PiecewiseLinearFun = {
    //a pivot must be added after the end of update1 if 2 is the first zone
    val transformReadyForFlipOnLeft = addRedundantPivotAt(startZoneIncluded,this.transformation)
    val transformReadyForFlip = addRedundantPivotAt(endZoneIncluded+1,transformReadyForFlipOnLeft)
    //at this point, there is a pivot at startZoneIncluded, and a pivot at endZoneIncluded+1

    //collecting the relevant pivots and their end into a stack
    val collectedPivotsForwardOrder:List[Pivot] = pivotsBetween(startZoneIncluded,endZoneIncluded,transformReadyForFlip)
    //TODO: use QLists here.

    //flip the collected pivots
    val flippedPivots = flipSubfunction(collectedPivotsForwardOrder,endZoneIncluded,endZoneIncluded)

    val flippedPivotsIterator = flippedPivots.iterator
    //update the pivots to the flipped pivot

    val updatedForwardFct = transformReadyForFlip.update(startZoneIncluded,transformReadyForFlip.biggestLowerOrEqual(endZoneIncluded).get._1,(_,_) => {
      val newPivot = flippedPivotsIterator.next()
      (newPivot.fromValue,newPivot)
    } )

    new PiecewiseLinearFun(updatedForwardFct)
  }

  private def mirrorPivot(p:Pivot,width:Int,newEnd:Int):Pivot = {
    val newFromValue = newEnd - width + 1
    val newMinus = !p.f.minus
    val newOffset = p.f(p.fromValue + newEnd)

    val newPivot = new Pivot(newFromValue,new LinearTransform(newOffset,newMinus))

    assert(p.f(p.fromValue) == newPivot.f(newEnd))
    assert(p.f(p.fromValue + width - 1) == newPivot.f(newPivot.fromValue))

    newPivot
  }

  @tailrec
  private def flipSubfunction(pivotList:List[Pivot], endOfLastPivotMoving:Int, endOfLastPivotUntouched:Int, acc:List[Pivot] = List.empty):List[Pivot] = {
    pivotList match {
      case p1 :: p2 :: tail =>
        val width = p2.fromValue - p1.fromValue
        flipSubfunction(p2 :: tail,endOfLastPivotMoving - width, endOfLastPivotUntouched, mirrorPivot(p1,width:Int,endOfLastPivotMoving) :: acc)
      case List(p1) =>
        //the updated pivot starts at endOfLastPIvot
        //its slope is the reverse of p1
        //the offset is such that
        //require(p1.f(p1.fromValue) == newPivot.f(endOfLastPivotMoving))
        //require(p1.f(endOfLastPivotUntouched) == newPivot.f(newPivot.fromValue))

        val width = endOfLastPivotUntouched - p1.fromValue + 1
        mirrorPivot(p1,width,endOfLastPivotMoving) :: acc
      case Nil => acc
    }
  }

  def shiftPivots(pivotList:List[Pivot],delta:Int):List[Pivot] = {
    pivotList.map(_.shiftOnX(delta))
  }

  def pivotsBetween(startPositionIncluded:Int,endPositionIncluded:Int,transform:RedBlackTreeMap[Pivot]):List[Pivot] = {
    transform.biggestLowerOrEqual(endPositionIncluded) match {
      case None => List.empty
      case Some((pivotStart, pivot)) =>
        var explorer = transform.positionOf(pivotStart).get
        var acc : List[Pivot] = List(explorer.value)
        while (explorer.prev match {
          case None => false
          case Some(prevExplorer) =>
            if (startPositionIncluded <= prevExplorer.key) {
              acc = prevExplorer.value :: acc
              explorer = prevExplorer
              true
            } else {
              false
            }
        }) {}
        acc
    }
  }

  def swapAdjacentZonesShiftSecond(startZone1Included:Int, endZone1Included:Int, endZone2Included:Int, flipZone1:Boolean):PiecewiseLinearFun = {

    val widthZone2 = endZone2Included - endZone1Included
    val widthZone1 = endZone1Included - startZone1Included + 1

    //remove pivot for destination of zone1
    val transformWithTargetZone1Cleaned = removePivotsBetween(startZone1Included, endZone1Included,transformation)

    //a pivot must be added after the end of update1 if 2 is the first zone
    val transformReadyForShiftOfZone2 = addRedundantPivotAt(endZone2Included+1,transformWithTargetZone1Cleaned)

    val f2 = LinearTransform(widthZone1,minus = false)
    val f3 = LinearTransform(-widthZone1,minus = false)

    val transformWithZone2Shifted =
      transformReadyForShiftOfZone2.updateDelta(
        endZone1Included +1,
        endZone2Included,
        -widthZone1,
        (p:Pivot) => new Pivot(p.fromValue-widthZone1,if(p.f.minus) f3(p.f) else f2(p.f)))

    //now, we apply the second transformation that will add pivots at the zone occupied by zone2 (which is now cleaned)
    val transformationWithUpdate1Done = myUpdateForCompositionBefore(startZone1Included+widthZone2, endZone2Included,
      LinearTransform(if(flipZone1) endZone1Included + startZone1Included + widthZone2 else -widthZone2,flipZone1), transformWithZone2Shifted)

    //finally, we can clean the potentially redundant pivots
    new PiecewiseLinearFun(
      deleteUnnecessaryPivotStartingJustAfter(startZone1Included-1,
        deleteUnnecessaryPivotStartingJustAfter(startZone1Included + widthZone2 - 1,
          deleteUnnecessaryPivotStartingJustAfter(endZone2Included,transformationWithUpdate1Done))))
  }

  def updateForCompositionBefore(fromIncluded:Int, toIncluded: Int, additionalFAppliedBefore: LinearTransform):PiecewiseLinearFun = {
    //step1: remove all pivots between fromIncluded and toIncluded
    //step2: recall the linear transform that was at the end, because we will need to re-put this one with an additional pivot, except if we get the same function with our new pivots
    //Step3: generate, and store all the new pivots
    //step4: add a finishing pivot if necessary (cfr. step2)

    //remove all the pivots between fromIncluded and ToIncluded
    val cleanedTransformation = removePivotsBetween(fromIncluded, toIncluded, transformation)
    val updatedTransform = myUpdateForCompositionBefore(fromIncluded, toIncluded, additionalFAppliedBefore,cleanedTransformation)
    val updatedTransformDeletedExtraPivot = deleteUnnecessaryPivotStartingJustAfter(toIncluded,updatedTransform)
    new PiecewiseLinearFun(updatedTransformDeletedExtraPivot)
  }

  private def deleteUnnecessaryPivotStartingJustAfter(toIncluded:Int,updatedTransform:RedBlackTreeMap[Pivot]):RedBlackTreeMap[Pivot] = {
    updatedTransform.get(toIncluded+1) match{
      case None => updatedTransform
      case Some(pivotStartingJustAfterToIncluded) =>
        updatedTransform.biggestLowerOrEqual(toIncluded) match{
          case None => if (pivotStartingJustAfterToIncluded.f.isIdentity) updatedTransform.remove(toIncluded+1) else updatedTransform
          case Some((_,pivotApplyingAtToIncluded)) =>
            if (pivotStartingJustAfterToIncluded.f equals pivotApplyingAtToIncluded.f) updatedTransform.remove(toIncluded+1) else updatedTransform
        }
    }
  }

  private def myUpdateForCompositionBefore(fromIncluded:Int, toIncluded: Int, additionalFAppliedBefore: LinearTransform,cleanedTransformation:RedBlackTreeMap[Pivot]):RedBlackTreeMap[Pivot] = {
    val isAdditionaFNegativeSlope = additionalFAppliedBefore.minus

    var currentFromIncluded = fromIncluded
    var currentTransformation = cleanedTransformation
    var currentIncludedFromAfterAdditionalF = additionalFAppliedBefore(currentFromIncluded)
    var positionOfPivotApplyingOnCurrentIncludedFromAfterAdditionalF =
      transformation.biggestLowerOrEqual(currentIncludedFromAfterAdditionalF) match {
        case None => None //no pivot applying at this point, consider identity function... until when?
        case Some((key, _)) => transformation.positionOf(key)
      }

    while(currentFromIncluded <= toIncluded) {
      positionOfPivotApplyingOnCurrentIncludedFromAfterAdditionalF match {
        case None => //no pivot at current point, so identity is assumed
          val (nextCurrentIncludedFrom, nextPivotPosition) = if (isAdditionaFNegativeSlope) {
            (toIncluded+1, None)
          } else {
            val nextPivotPosition:Option[RedBlackTreeMapExplorer[Pivot]] = transformation.smallestPosition
            (nextPivotPosition match{case None => toIncluded+1 case Some(position) => additionalFAppliedBefore.unApply(position.value.fromValue)},
              nextPivotPosition)
          }

          //delivers a new linear transform that is equal to this(that(value))
          val newF = additionalFAppliedBefore

          //Adds the pivot (only if different transform of course)
          if (currentTransformation.biggestLowerOrEqual(currentFromIncluded) match {
            case None => !newF.isIdentity
            case Some((key, existingPivot)) => !existingPivot.f.equals(newF)
          }){
            currentTransformation = currentTransformation.insert(currentFromIncluded, new Pivot(currentFromIncluded, newF))
          }

          currentFromIncluded = nextCurrentIncludedFrom
          currentIncludedFromAfterAdditionalF = additionalFAppliedBefore(currentFromIncluded)
          positionOfPivotApplyingOnCurrentIncludedFromAfterAdditionalF = nextPivotPosition
        case Some(position) =>
          //compute end of pivot (start is currentFromIncluded)
          val (nextCurrentIncludedFrom, nextPivotPosition) = if (isAdditionaFNegativeSlope) {
            (additionalFAppliedBefore.unApply(position.value.fromValue - 1), position.prev)
          } else {
            val nextPivotPosition = position.next
            (nextPivotPosition match{case None => toIncluded+1 case Some(position) => additionalFAppliedBefore.unApply(position.value.fromValue)},
              nextPivotPosition)
          }

          //delivers a new linear transform that is equal to this(that(value))
          val newF = position.value.f(additionalFAppliedBefore)

          //Adds the pivot (only if different transform of course)
          if (currentTransformation.biggestLowerOrEqual(currentFromIncluded) match {
            case None => !newF.isIdentity
            case Some((key, existingPivot)) => !existingPivot.f.equals(newF)
          }){
            currentTransformation = currentTransformation.insert(currentFromIncluded, new Pivot(currentFromIncluded, newF))
          }

          currentFromIncluded = nextCurrentIncludedFrom
          currentIncludedFromAfterAdditionalF = additionalFAppliedBefore(currentFromIncluded)
          positionOfPivotApplyingOnCurrentIncludedFromAfterAdditionalF = nextPivotPosition

        //compute end of this section
      }
    }
    currentTransformation
  }

  /**
   *
   * @param addedPivot
   * @param transform
   * @return  transform. there will always be a pivot at the addedPivot position.
   */
  private def addRedundantPivotAt(addedPivot:Int,transform:RedBlackTreeMap[Pivot]):RedBlackTreeMap[Pivot] = {
    transform.biggestLowerOrEqual(addedPivot) match {
      case Some((key, pivot)) =>
        if (key == addedPivot) transform //we do not add a redundant pivot because there is already a pivot here
        else transform.insert(addedPivot, new Pivot(addedPivot, pivot.f))
      case _ => transform.insert(addedPivot, new Pivot(addedPivot, LinearTransform.identity)) //implicitly, it was the identity function here, so add the pivot
    }
  }

  /**
   * removes all pivots between fromIncluded and toIncluded
   * also adds a pivot at toIncluded+1 if necessary to ensure that values starting at toIncluded+1 onwards are not impacted
   */
  private def removePivotsBetween(fromIncluded:Int,toIncluded:Int, transformToClean:RedBlackTreeMap[Pivot]):RedBlackTreeMap[Pivot] = {

    val transformWithAddedFinishingPivot = addRedundantPivotAt(toIncluded+1,transformToClean)

    var currentCleanedTransform = transformWithAddedFinishingPivot
    var pivotToRemove = currentCleanedTransform.biggestLowerOrEqual(toIncluded)
    while (true) {
      pivotToRemove match {
        case Some((key, pivot)) if key >= fromIncluded =>
          currentCleanedTransform = currentCleanedTransform.remove(key)
          pivotToRemove = currentCleanedTransform.biggestLowerOrEqual(toIncluded)
        case _ => return currentCleanedTransform
      }
    }
    null
  }

  def pivotWithPositionApplyingTo(value:Int):Option[RedBlackTreeMapExplorer[Pivot]] = {
    transformation.biggestLowerOrEqual(value) match{
      case None => None
      case Some((fromValueOfPivot,_)) => transformation.positionOf(fromValueOfPivot)
    }
  }

  def pivotWithPositionAfter(value:Int):Option[RedBlackTreeMapExplorer[Pivot]] = {
    transformation.smallestBiggerOrEqual(value) match{
      case None => None
      case Some((fromValueOfPivot,_)) => transformation.positionOf(fromValueOfPivot)
    }
  }

  def pivotApplyingTo(value:Int):Option[Pivot] = {
    transformation.biggestLowerOrEqual(value)  match {
      case None => None
      case Some((_, p)) => Some(p)
    }
  }

  def pivotAfter(value:Int):Option[Pivot] = {
    transformation.smallestBiggerOrEqual(value)  match {
      case None => None
      case Some((_, p)) => Some(p)
    }
  }

  def firstPivotAndPosition:Option[RedBlackTreeMapExplorer[Pivot]] = {
    transformation.smallest match{
      case None => None
      case Some((fromValueOfPivot,_)) => transformation.positionOf(fromValueOfPivot)
    }
  }

  def lastPivotAndPosition:Option[RedBlackTreeMapExplorer[Pivot]] = {
    transformation.biggest match{
      case None => None
      case Some((fromValueOfPivot,_)) => transformation.positionOf(fromValueOfPivot)
    }
  }
}

class Pivot(val fromValue:Int, val f: LinearTransform){
  override def toString: String = "Pivot(from:" + fromValue + " " + f + " f(from)=" + f(fromValue) +")"

  def shiftOnX(delta:Int):Pivot =
    new Pivot(fromValue+delta,f.shiftOnX(delta))

  def flip(endX:Int):Pivot = {
    if(f.minus){
      new Pivot(fromValue,LinearTransform(f(endX) - fromValue,minus = false))
    }else{
      new Pivot(fromValue,LinearTransform(fromValue + f(endX),minus = true))
    }
  }
}
