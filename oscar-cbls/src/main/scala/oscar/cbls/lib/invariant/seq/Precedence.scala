/*******************************************************************************
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
  ******************************************************************************/
package oscar.cbls.lib.invariant.seq

import oscar.cbls.algo.magicArray.MagicBoolArray
import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.business.routing.model.CachedPositionOf
import oscar.cbls.core.computation.{ChangingSeqValue, IntInvariant, SeqNotificationTarget, SeqUpdate, SeqUpdateAssign, SeqUpdateDefineCheckpoint, SeqUpdateInsert, SeqUpdateLastNotified, SeqUpdateMove, SeqUpdateRemove, SeqUpdateRollBackToCheckpoint}
import oscar.cbls.core.propagation.Checker

import scala.collection.immutable.SortedSet

object Precedence{

  /**
   * precedence assumes that number can occur only once in the sequence
   * so that the constraint is to be enforced from any occurrence to any occurrence,
   * "any" being chosen arbitrarily by tne invariant, and the choice an of course change at any time.
   * also if one of the two value of a precedence is not present in the sequence,
   * it is considered that the precedence is enforced.
   *
   * maintains the number of violated precedences.
   *
   * @param seq
   * @param beforeAfter
   * @author renaud.delandtsheer@cetic.be
   */
  def apply(seq:ChangingSeqValue,
            beforeAfter:List[(Int,Int)]):Precedence = new Precedence(seq,beforeAfter)
  //TODO: maintain set of nodes involved in precedence violation
}

/**
 * precedence assumes that number can occur only once in the sequence
 * so that the constraint is to be enforced from any occurrence to any occurrence,
 * "any" being chosen arbitrarily by tne invariant, and the choice an of course change at any time.
 * also if one of the two value of a precedence is not present in the sequence,
 * it is considered that the precedence is enforced.
 *
 * maintains the number of violated precedences.
 *
 * @param seq
 * @param beforeAfter
 * @author renaud.delandtsheer@cetic.be
 */
class Precedence(seq:ChangingSeqValue,
                 beforeAfter:List[(Int,Int)]) //supposed to have zero redundancies.
  extends IntInvariant()
  with SeqNotificationTarget {

  setName("PrecedenceViolation")

  registerStaticAndDynamicDependency(seq)
  finishInitialization()

  //saving precedences into arrays
  private val precedencesArray : Array[(Int, Int)] = beforeAfter.toArray
  val nbPecedences = precedencesArray.length
  val precedences = 0 until nbPecedences

  private val beforesToPrecedences : Array[QList[Int]] = Array.fill(seq.maxValue + 1)(null)
  private val aftersToPrecedences : Array[QList[Int]] = Array.fill(seq.maxValue + 1)(null)
  for (precedenceID <- precedencesArray.indices) {
    val (fromValue, toValue) = precedencesArray(precedenceID)
    require(fromValue != toValue, "there should be no self precedence!")
    beforesToPrecedences(fromValue) = QList(precedenceID, beforesToPrecedences(fromValue))
    aftersToPrecedences(toValue) = QList(precedenceID, aftersToPrecedences(toValue))
  }

  private val isPrecedenceViolated : Array[Boolean] = Array.fill(nbPecedences)(false)

  //TODO: use magic array here
  private val isViolationChangedSinceCheckpoint:Array[Boolean] = Array.fill(nbPecedences)(false)
  private var changedPrecedenceViolationsSinceCheckpoint:QList[Int] = null
  private val savedViolationAtCheckpoint:Array[Boolean] = Array.fill(nbPecedences)(false)
  private val cachedPositionFinderAtCheckpoint = new CachedPositionOf(seq.maxValue)
  private var checkpoint:IntSequence = null
  var violationAtCheckpoint:Int = -1

  //we set a frist checkpoint since we are computing everything from scratch here.
  cachedPositionFinderAtCheckpoint.updateToCheckpoint(seq.value)
  computeAndAffectViolationsFromScratch(seq.value)

  def nodesStartingAPrecedence:SortedSet[Int] = SortedSet.empty[Int] ++ beforeAfter.map(_._1)
  def nodesEndingAPrecedenceStartedAt:Int => Iterable[Int] = (before:Int) => beforesToPrecedences(before).map(p => precedencesArray(p)._2)

  def saveViolationForCheckpoint(precedence:Int): Unit ={
    if(!isViolationChangedSinceCheckpoint(precedence)){
      isViolationChangedSinceCheckpoint(precedence) = true
      changedPrecedenceViolationsSinceCheckpoint = QList(precedence,changedPrecedenceViolationsSinceCheckpoint)
      savedViolationAtCheckpoint(precedence) = isPrecedenceViolated(precedence)
    }
  }

  def reloadViolationsAtCheckpoint(): Unit ={
    //TODO: pas moyen de faire du O(1) ici, avec un tableau magique par exemple?
    for(precedence <- changedPrecedenceViolationsSinceCheckpoint){
      isViolationChangedSinceCheckpoint(precedence) = false
      isPrecedenceViolated(precedence) = savedViolationAtCheckpoint(precedence)
    }
    changedPrecedenceViolationsSinceCheckpoint = null
    this := violationAtCheckpoint
  }

  def defineCheckpoint(i:IntSequence): Unit ={
    for(precedence <- changedPrecedenceViolationsSinceCheckpoint) {
      isViolationChangedSinceCheckpoint(precedence) = false
    }
    changedPrecedenceViolationsSinceCheckpoint = null
    checkpoint = i
    violationAtCheckpoint = this.newValueInt
    cachedPositionFinderAtCheckpoint.updateToCheckpoint(i)
  }

  def clearAllViolatedPrecedences(): Unit ={
    for (precedence <- precedences) {
      saveViolationForCheckpoint(precedence)
      isPrecedenceViolated(precedence) = false
    }
  }

  def computeAndAffectViolationsFromScratch(seq : IntSequence): Unit ={
    //this assumes that we have lots of precedence constraints, so that we can afford crawling through the sequence (instead of iterating through the precedences)
    var totalViolation = 0
    val hasValueBeenSeen = Array.fill(this.seq.maxValue + 1)(false)

    clearAllViolatedPrecedences()

    var explorerOpt = seq.explorerAtPosition(0)

    while (explorerOpt match {
      case None => false
      case Some(explorer) =>

        val value = explorer.value

        hasValueBeenSeen(value) = true

        var precedencesThatShouldOpenNow = beforesToPrecedences(value)
        while (precedencesThatShouldOpenNow != null) {
          val precedenceID = precedencesThatShouldOpenNow.head
          precedencesThatShouldOpenNow = precedencesThatShouldOpenNow.tail
          val shouldBeAfter = precedencesArray(precedenceID)._2
          saveViolationForCheckpoint(precedenceID)
          if (hasValueBeenSeen(shouldBeAfter)) {
            //the precedence is violated!
            isPrecedenceViolated(precedenceID) = true
            totalViolation += 1
          }
        }
        explorerOpt = explorer.next
        true
    }) {}

    this := totalViolation
  }

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    if (!digestUpdates(changes)) {
      //this also updates checkpoint links
      computeAndAffectViolationsFromScratch(changes.newValue)
    }
  }

  //should always be false when not in use
  val tmpArrayForDigestUpdate = MagicBoolArray(seq.maxValue+1,false)

  private def digestUpdates(changes : SeqUpdate) : Boolean = {
    //println("precedence.digestUpdate(" + changes.getClass.getSimpleName + ")")
    changes match {
      case null => throw new Error()
      case SeqUpdateDefineCheckpoint(prev : SeqUpdate, isActive : Boolean, checkpointLevel:Int) =>
        //println("define checkpoint")
        if(checkpointLevel == 0){
          if(!digestUpdates(prev)){
            computeAndAffectViolationsFromScratch(changes.newValue)
          }
          defineCheckpoint(changes.newValue)
          true
        }else{
          digestUpdates(prev)
        }

      case x@SeqUpdateRollBackToCheckpoint(rollBackCheckpoint, checkpointLevel) =>
        if(checkpointLevel == 0) {
          require(checkpoint quickEquals rollBackCheckpoint)
          reloadViolationsAtCheckpoint()
          true
        }else{
          digestUpdates(x.howToRollBack)
        }

      case SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>

        if (!digestUpdates(prev)) return false

        var precedencesStartingAtInsertedValue = beforesToPrecedences(value)
        while (precedencesStartingAtInsertedValue != null) {
          val precedenceToCheck = precedencesStartingAtInsertedValue.head
          precedencesStartingAtInsertedValue = precedencesStartingAtInsertedValue.tail

          //searching for end of precedence that is already in
          val endValueOfPrecedence = this.precedencesArray(precedenceToCheck)._2
          cachedPositionFinderAtCheckpoint.positionOfAnyOccurrence(prev.newValue,endValueOfPrecedence) match {
            case None => ; //the precedence is fine
            case Some(positionOfEndValueOfPrecedence) =>
              if (positionOfEndValueOfPrecedence < pos) {
                //precedence is violated!
                saveViolationForCheckpoint(precedenceToCheck)
                this :+= 1
                isPrecedenceViolated(precedenceToCheck) = true
              } // else precedence is not violated
          }
        }

        var precedencesEndingAtInsertedValue = aftersToPrecedences(value)

        while (precedencesEndingAtInsertedValue != null) {
          val precedenceToCheck = precedencesEndingAtInsertedValue.head
          precedencesEndingAtInsertedValue = precedencesEndingAtInsertedValue.tail

          //searching for end of precedence that is already in
          val startValueOfPrecedence = this.precedencesArray(precedenceToCheck)._1
          cachedPositionFinderAtCheckpoint.positionOfAnyOccurrence(prev.newValue,startValueOfPrecedence) match {
            case None => ; //the precedence is fine

            case Some(positionOfStartValueOfPrecedence) =>
              if (pos <= positionOfStartValueOfPrecedence) {
                //precedence is violated!
                saveViolationForCheckpoint(precedenceToCheck)
                this :+= 1
                isPrecedenceViolated(precedenceToCheck) = true
              } // else precedence is not violated
          }
        }
        true

      case x@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        if (!digestUpdates(prev)) return false
        val removedValue = x.removedValue
        cachedPositionFinderAtCheckpoint.savePos(prev.newValue,removedValue,Some(position))

        for(precedencesThatShouldOpenAtValue <- beforesToPrecedences(removedValue)){
          if (isPrecedenceViolated(precedencesThatShouldOpenAtValue)) {
            saveViolationForCheckpoint(precedencesThatShouldOpenAtValue)
            isPrecedenceViolated(precedencesThatShouldOpenAtValue) = false
            this :-= 1
          }
        }

        for(precedenceThatShouldCloseAtValue <- aftersToPrecedences(removedValue)){
          if (isPrecedenceViolated(precedenceThatShouldCloseAtValue)) {
            saveViolationForCheckpoint(precedenceThatShouldCloseAtValue)
            isPrecedenceViolated(precedenceThatShouldCloseAtValue) = false
            this :-= 1
          }
        }

        true

      case x@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        //println("move")
        //on which vehicle did we move?
        //also from --> to cannot include a vehicle start.
        if (!digestUpdates(prev)) false
        else if (x.isNop) true
        else if (x.isSimpleFlip) {
          //this is a simple flip
          //println("simple flip")
          //violation is only impacted if the other value of the precedence is also in the flip.
          //in this case, it inverts the truth value of the violation.
          //two inversions will happen,
          //that'w why there is only the loop on precedences considering the precedences started at the flipped values,
          // not the ones ended at the flipped values

          val movedValues = x.movedValuesQList

          var digestedMovedValues = movedValues
          tmpArrayForDigestUpdate.all = false
          while(digestedMovedValues != null) {
            tmpArrayForDigestUpdate.update(digestedMovedValues.head,true)
            digestedMovedValues = digestedMovedValues.tail
          }

          for(value <- movedValues){
            //violations involved in value
            for(precedenceThatShouldOpenAtValue <- this.beforesToPrecedences(value)){
              val endValueOfPrecedence = precedencesArray(precedenceThatShouldOpenAtValue)._2
              if(tmpArrayForDigestUpdate(endValueOfPrecedence)){
                //the violation of this precedence is inverted
                if(isPrecedenceViolated(precedenceThatShouldOpenAtValue)){
                  //was violated, so flip to false
                  saveViolationForCheckpoint(precedenceThatShouldOpenAtValue)
                  isPrecedenceViolated(precedenceThatShouldOpenAtValue) = false
                  this :-=1
                }else{
                  //was not violated, now it is violated
                  saveViolationForCheckpoint(precedenceThatShouldOpenAtValue)
                  isPrecedenceViolated(precedenceThatShouldOpenAtValue) = true
                  this :+=1
                }
              }//else we do not have to check anything :-)
            }
          }
          true

        }else{ //move with/without flip

          val moveDownwards = x.moveDownwards
          val moveUpwards = x.moveUpwards

          //println("full move moveDownwards:" + moveDownwards + " moveUpwards:" + moveUpwards)

          var valuesInMovedSegment : QList[(Int,Int)] = prev.newValue.positionsBetweenFromToAndTheirValues(fromIncluded, toIncluded)
          if(prev.newValue quickEquals checkpoint) {
            var toDo = valuesInMovedSegment
            while(toDo != null){
              val (position, value) = toDo.head
              toDo = toDo.tail
              cachedPositionFinderAtCheckpoint.savePos(prev.newValue, value, Some(position))
            }
          }

          while(valuesInMovedSegment != null){
            val value = valuesInMovedSegment.head._2
            valuesInMovedSegment = valuesInMovedSegment.tail
            //violations involved in value
            for (precedenceStartedAtValue <- this.beforesToPrecedences(value)) {
              if (isPrecedenceViolated(precedenceStartedAtValue)) {
                //it was violated.
                //it will become non-violated if moved downwards and the other value passes up
                //or if there is a flip and the other value is in the flip

                val endValueOfPrecedence = precedencesArray(precedenceStartedAtValue)._2

                val positionOfEndValue = cachedPositionFinderAtCheckpoint.positionOfAnyOccurrence(prev.newValue, endValueOfPrecedence).get
                if (moveDownwards && positionOfEndValue > after && positionOfEndValue < fromIncluded) {
                  saveViolationForCheckpoint(precedenceStartedAtValue)
                  isPrecedenceViolated(precedenceStartedAtValue) = false
                  this :-= 1
                } else if (flip && fromIncluded <= positionOfEndValue && positionOfEndValue <= toIncluded) {
                  //the violation of this precedence is inverted (only do this for one half of the precedence)
                  //was violated, so flip to false
                  saveViolationForCheckpoint(precedenceStartedAtValue)
                  isPrecedenceViolated(precedenceStartedAtValue) = false
                  this :-= 1
                }

              } else {
                //it was not violated, thus is can only get violated is the segment moved upwards
                // of if the other value is also in the segment and there is a flip
                val endValueOfPrecedence = precedencesArray(precedenceStartedAtValue)._2
                cachedPositionFinderAtCheckpoint.positionOfAnyOccurrence(prev.newValue, endValueOfPrecedence) match {
                  case None => //nothing to do, other node is not in the sequence, it was not violated for this reason
                  case Some(positionOfEndValue) =>
                    if (moveUpwards && positionOfEndValue <= after && positionOfEndValue > toIncluded) {
                      saveViolationForCheckpoint(precedenceStartedAtValue)
                      isPrecedenceViolated(precedenceStartedAtValue) = true
                      this :+= 1
                    } else if (flip && fromIncluded <= positionOfEndValue && positionOfEndValue <= toIncluded) {
                      //the violation of this precedence is inverted (only do this for one half of the precedence)
                      //was not violated, now it is violated
                      saveViolationForCheckpoint(precedenceStartedAtValue)
                      isPrecedenceViolated(precedenceStartedAtValue) = true
                      this :+= 1
                    }
                }
              }
            }

            for (precedenceEndingAtValue <- this.aftersToPrecedences(value)) {
              if (isPrecedenceViolated(precedenceEndingAtValue)) {
                //it was violated.
                //it will become non-violated if moved upwards and the other value passes down
                //or if there is a flip and the other value is in the flip, but the flip case is handled in the precedenceStarted at value

                if (moveUpwards) {
                  val startValueOfPrecedence = precedencesArray(precedenceEndingAtValue)._1
                  val positionOfStartValue = cachedPositionFinderAtCheckpoint.positionOfAnyOccurrence(prev.newValue,startValueOfPrecedence).get
                  if (positionOfStartValue <= after && positionOfStartValue > toIncluded) {
                    saveViolationForCheckpoint(precedenceEndingAtValue)
                    isPrecedenceViolated(precedenceEndingAtValue) = false
                    this :-= 1
                  }
                }

              } else {
                //it was not violated, thus is can only get violated is the segment moves downwards and the other value passes up
                // of if the other value is also in the segment and there is a flip, but the flip case is handled in the precedenceStarted at value

                if (moveDownwards) {
                  val startValueOfPrecedence = precedencesArray(precedenceEndingAtValue)._1
                  cachedPositionFinderAtCheckpoint.positionOfAnyOccurrence(prev.newValue,startValueOfPrecedence) match {
                    case None =>
                    case Some(positionOfStartValue) =>
                      if (after < positionOfStartValue && positionOfStartValue < fromIncluded) {
                        saveViolationForCheckpoint(precedenceEndingAtValue)
                        isPrecedenceViolated(precedenceEndingAtValue) = true
                        this :+= 1
                      }
                  }
                }
              }
            }
          }
          true
        }

      case SeqUpdateLastNotified(value : IntSequence) =>
        true //we are starting from the previous value

      case SeqUpdateAssign(value : IntSequence) =>
        false //impossible to go incremental
    }
  }

  /** To override whenever possible to spot errors in invariants.
    * this will be called for each invariant after propagation is performed.
    * It requires that the Model is instantiated with the variable debug set to true.
    */
  override def checkInternals(c : Checker) : Unit = {
    var nbViol = 0
    val s = seq.value

    for(precedenceID <- precedences){
      val (startValue,endValue) = precedencesArray(precedenceID)

      s.positionOfAnyOccurrence(startValue) match {
        case None => ;
          c.check(!isPrecedenceViolated(precedenceID))
        case Some(positionOfStartValue) =>
          s.positionOfAnyOccurrence(endValue) match {
            case None => ;
              c.check(!isPrecedenceViolated(precedenceID))
            case Some(positionOfEndValue) =>
              c.check(isPrecedenceViolated(precedenceID) == (positionOfStartValue > positionOfEndValue),
                Some(s"error on violation of precedence $precedenceID considered as violated:${isPrecedenceViolated(precedenceID)}:(${precedencesArray(precedenceID)})"))
              if(isPrecedenceViolated(precedenceID)) nbViol += 1
          }
      }
    }
    c.check(this.value == nbViol,
      Some(s"this.value=${this.newValue} should== nbViol=$nbViol"))
  }
}
