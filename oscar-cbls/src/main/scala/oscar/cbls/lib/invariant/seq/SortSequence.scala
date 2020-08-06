package oscar.cbls.lib.invariant.seq

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

import oscar.cbls.algo.seq.{IntSequence, IntSequenceExplorer}
import oscar.cbls.core.computation.{ChangingSeqValue, SeqCheckpointedValueStack, SeqInvariant, SeqNotificationTarget, SeqUpdate, SeqUpdateAssign, SeqUpdateDefineCheckpoint, SeqUpdateInsert, SeqUpdateLastNotified, SeqUpdateMove, SeqUpdateRemove, SeqUpdateRollBackToCheckpoint, SeqValue}
import oscar.cbls.core.propagation.Checker

/**
 * maintains a sorted sequence out of a non-sorted one.
 * they have the same length
 * the sort is based on the sortValue,smaller first
 *
 * @param v the input sequence
 * @param sortValue a constant function that maps each value in v to a value that is used for the sort.
 *                  This value is not the one being put into the output sequence
 * @param orderName a name for the order
 */
case class SortSequence(v: SeqValue, sortValue:Int => Int, orderName:String="order")
  extends SeqInvariant(IntSequence.empty(),v.max)
  with SeqNotificationTarget{

  //TODO: this invariant is rather slow for updating on inserts and on its get method because it ues dichotomy.
  // possible to maintain a set of represented values, update log(n) query log(n)

  setName(s"SortSequence(${v.name} by:$orderName)")

  registerStaticAndDynamicDependency(v)
  finishInitialization()

  this := sortSequenceBy(v.value,sortValue)

  /**
   * @param value a value, that is compared against value in the current sequence
   * @param transformedValue is an optional argument that you can pass to this method.
   *                         Values v in the output sequence are ordered by lexicographic order on (sortValue(v),v)
   *                         if you want to use another value for the first position in this couple, and just for this query, you can specify it throug hthis parameter.
   *                         notice that if you search for a value and this value is already present in teh sequence, it might result in erroneous result,
   *                         so do not use this parameter unless you are sure that "value" is not in the output of this invariant (so not in the input sequence variable)
   * @return the position of the smallest value in the current output of this invariant such that it is the smallest one that is greater or equal to "value".
   *         It is actually the first value in the sequence starting from position zero that is greater or equal
   *         in case case there is no such value (including empty sequence), it returns None
   */
  def positionOfSmallestGreaterOrEqual(value:Int)(transformedValue:Int = sortValue(value)):Option[IntSequenceExplorer] = {
    val sortedSequence:IntSequence = this.value
    if(sortedSequence.isEmpty) {
      None
    }else {
      val position = searchPositionOfInsert(sortedSequence, value)(transformedValue)
      if(position == sortedSequence.size){
        None
      }else{
        var toReturn = sortedSequence.explorerAtPosition(position).get
        //we move a bit to the left in case of equal values
        while (toReturn.prev match{
          case Some(explorerAtPrevPosition) if explorerAtPrevPosition.value == value =>
            toReturn = explorerAtPrevPosition
            true
          case _ => false
        }){}
        Some(toReturn)
      }
    }
  }

  /**
   *
   * @param firstValue
   * @param secondValue
   * @param firstTransformedValue
   * @param secondTransformedValue
   * @return true if firstValue is smaller than secondValue
   */
  private def isSmaller(firstValue:Int,secondValue:Int)
                       (firstTransformedValue:Int = sortValue(firstValue),secondTransformedValue:Int = sortValue(secondValue)):Boolean = {

    assert(firstTransformedValue == sortValue(firstValue))
    assert(secondTransformedValue == sortValue(secondValue))

    if(firstTransformedValue < secondTransformedValue) true
    else if(firstTransformedValue > secondTransformedValue) false
    else firstValue < secondValue
  }

  private def searchPositionOfInsert(seq:IntSequence, value:Int)(transformedValue:Int = sortValue(value)):Int = {

    //the position of insert is the position where the value is the first occurrence
    // in the supposedly sorted sequence such that the value at this position is g value
    //println("searchPositionOfInsert(seq:" + seq + " valueToInsert:" + value + ")")
    def otherIsSmaller(otherValue:Int):Boolean = {
      !isSmaller(value,otherValue)(firstTransformedValue = transformedValue)
    }

    assert(seq.size == seq.toList.size, "size error")

    if(seq.size == 0) {
      return 0
    } else if(seq.size == 1) {
      val headValue = seq.head
      if (headValue == value || !otherIsSmaller(seq.head)) {
        return 0
      } else {
        return 1
      }
    }

    val last = seq.last
    if(last == value){
      return seq.size-1
    }else if (otherIsSmaller(last)){
      return seq.size
    }

    val first = seq.head
    if(!otherIsSmaller(first) || first == value){
      return 0
    }

    var lowerPositionOfInsert = 0
    var upperPositionOfInsert = seq.size-1

    while(lowerPositionOfInsert + 1 < upperPositionOfInsert) {
      assert(isSmaller(seq.valueAtPosition(lowerPositionOfInsert).get,value)(),s"expected ${seq.valueAtPosition(lowerPositionOfInsert).get} l $value")
      assert(isSmaller(value, seq.valueAtPosition(upperPositionOfInsert).get)(),"A")

      val midPosition = (lowerPositionOfInsert + upperPositionOfInsert) / 2
      val valueAtMidPosition = seq.valueAtPosition(midPosition).get
      if(valueAtMidPosition == value) return midPosition
      if (otherIsSmaller(valueAtMidPosition)) {
        lowerPositionOfInsert = midPosition
      } else {
        upperPositionOfInsert = midPosition
      }
    }
    upperPositionOfInsert
  }

  private val checkpointStack = new SeqCheckpointedValueStack[IntSequence]()

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    digestChanges(changes)
    //check(new ErrorChecker(),v.newValue,this.newValue)
  }

  private def digestChanges(changes : SeqUpdate): Unit ={
    changes match {
      case s@SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        digestChanges(prev)
        //find where the value should be located by dichotomy
        //println("inserting " + value + " into this.newValue:" + this.newValue)
        val positionOfInsert = searchPositionOfInsert(this.newValue, value)()
        //println("found position:" + positionOfInsert)
        this.insertAtPosition(value,positionOfInsert)

      case m@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        digestChanges(prev)

      case r@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        digestChanges(prev)

        val valueAtRemove = r.removedValue
        val posOfRemoveOut = this.newValue.positionOfAnyOccurrence(valueAtRemove).get
        this.remove(posOfRemoveOut)

      case u@SeqUpdateRollBackToCheckpoint(checkPoint,level) =>
        releaseTopCheckpointsToLevel(level,false)
        this.rollbackToTopCheckpoint(checkpointStack.rollBackAndOutputValue(checkPoint,level))

      case SeqUpdateDefineCheckpoint(prev : SeqUpdate, isStarMode, level) =>
        digestChanges(prev)

        releaseTopCheckpointsToLevel(level,true)
        this.defineCurrentValueAsCheckpoint(isStarMode)
        checkpointStack.defineCheckpoint(prev.newValue,level,this.newValue)

      case SeqUpdateLastNotified(value) =>
      //nothing to do

      case SeqUpdateAssign(value : IntSequence) =>
        this := sortSequenceBy(value,sortValue)
    }
  }

  private def sortSequenceBy(i:IntSequence,by:Int => Int):IntSequence = IntSequence(i.toList.sortBy(i => (by(i),i)))

  override def checkInternals(c: Checker): Unit = {
    check(c, v.value,this.value)
  }

  def check(c:Checker,in:IntSequence,out:IntSequence): Unit ={
    require(out quickEquals this.value)
    c.check(out.toList equals sortSequenceBy(in,sortValue).toList,
      Some(s"this.out=${out.toList} should be ${sortSequenceBy(in,sortValue).toList}"))
    /*
    for (value <-  if(in.nonEmpty) {in.min to in.max} else List(0,1,10)) {
      val optPositionOfSMallestGE = positionOfSmallestGreaterOrEqual(value)()
      optPositionOfSMallestGE match {
        case None =>
          c.check(in.isEmpty || out.forall((otherValue => otherValue != value && isSmaller(otherValue, value)())),
            Some("None on search " + value + " on " + out + " but there are: " + out.toList.filter(otherValue => otherValue == value || isSmaller(value,otherValue)())))

        case Some(positionOfSMallestGE) =>
          c.check(isSmaller(value, positionOfSMallestGE.value)() || positionOfSMallestGE.value == value, Some("returned value " + positionOfSMallestGE.value + " on search " + value + " on " + out))
      }
    }
    */
  }
}

/*
object TestSort extends App{

  val m = new Store(verbose = false,propagateOnToString = true, checker = Some(new ErrorChecker()))
  val a = new CBLSSeqVar(m,IntSequence(List(1,2,3,5)), n = "toto")

  val sort = SortSequence(a,v => v,"increasing values")

  m.close()
  /*
    println("insertAtPosition(45,3)")

    a.insertAtPosition(45,3)
    m.propagate()

    a.move(1,3,4,false)
    m.propagate()
    a.insertAtPosition(12,5)
    m.propagate()
    a.remove(a.value.positionOfFirstOccurrence(2).head)
    m.propagate()
    a.move(1,3,4,true)
    m.propagate()
    a.move(1,3,4,true)
    */
  m.propagate()

  a:= IntSequence(List(56))
  m.propagate()
  a.insertAtPosition(30,0)
  m.propagate()
  // val checkpoint = a.defineCurrentValueAsCheckpoint(true)
  //println("defined checkpoint " + checkpoint)
  //println("insert&Move")


}
*/