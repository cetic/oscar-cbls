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
/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

package oscar.cbls.lib.invariant.numeric

import oscar.cbls._
import oscar.cbls.core._
import oscar.cbls.core.computation.DomainRange

import scala.collection.immutable.SortedSet


/** sum(i in cond) vars(i)
  * @param vars is an array of IntVars
  * @param cond is the condition for selecting variables in the array of summed ones, cannot be null
  * @author renaud.delandtsheer@cetic.be
  * */
case class SumConstants(vars: Array[Long], cond: SetValue)
  extends IntInvariant(cond.value.foldLeft(0L)((acc, i) => acc + vars(i)))
  with SetNotificationTarget{

  registerStaticAndDynamicDependency(cond)
  finishInitialization()

  override def notifySetChanges(v: ChangingSetValue, id: Int, addedValues: Iterable[Long], removedValues: Iterable[Long], oldValue: SortedSet[Long], newValue: SortedSet[Long]): Unit = {
    for (added <- addedValues)  this :+= vars(added)
    for (deleted <- removedValues) this :-= vars(deleted)
  }

  /** To override whenever possible to spot errors in invariants.
    * this will be called for each invariant after propagation is performed.
    * It requires that the Model is instantiated with the variable debug set to true.
    */
  override def checkInternals(c: Checker){
    c.check(this.value == cond.value.foldLeft(0L)((acc, i) => acc + vars(i)),
      Some("output.value == cond.value.foldLeft(0L)((acc, i) => acc + vars(i).value)"))
  }
}

/** sum(i in cond) vars(i)
 * @param vars is an array of IntVars
 * @param cond is the condition for selecting variables in the array of summed ones, cannot be null
  * @author renaud.delandtsheer@cetic.be
  * */
case class SumElements(vars: Array[IntValue], cond: SetValue)
  extends IntInvariant(initialValue=cond.value.foldLeft(0L)((acc, i) => acc + vars(i).value),
    initialDomain = Domain(vars.foldLeft(0L)((acc, v) => DomainHelper.safeAdd(acc,v.min min 0)), vars.foldLeft(0L)((acc, v) => DomainHelper.safeAdd(acc,v.max)))
  )
  with Bulked[IntValue, Unit]
  with VaryingDependencies
  with IntNotificationTarget
  with SetNotificationTarget{

  assert(vars.size > 0L, "Invariant SumElements declared with zero vars to max")
  assert(cond != null, "cond cannot be null for SumElements")

  val keyForRemoval: Array[KeyForElementRemoval] =  Array.fill(vars.length) {null}

  registerStaticDependency(cond)
  registerDeterminingDependency(cond)

  bulkRegister(vars)

  for(i <- cond.value){
    keyForRemoval(i) = registerDynamicDependency(vars(i),i)
  }
  finishInitialization()

  @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, OldVal: Long, NewVal: Long) {
    //it is always a listened one, but we could check this here
    assert(vars(index)==v)
    assert(keyForRemoval(index)!=null)
    this :+= (NewVal - OldVal)
  }

  override def notifySetChanges(v: ChangingSetValue, id: Int, addedValues: Iterable[Long], removedValues: Iterable[Long], oldValue: SortedSet[Long], newValue: SortedSet[Long]): Unit = {
    for (added <- addedValues) notifyInsertOn(v: ChangingSetValue, added)
    for(deleted <- removedValues) notifyDeleteOn(v: ChangingSetValue, deleted)
  }

  @inline
  def notifyInsertOn(v: ChangingSetValue, value: Long) {
    assert(v == cond)
    assert(keyForRemoval(value) == null)
    keyForRemoval(value) = registerDynamicDependency(vars(value),value)

    this :+= vars(value).value
  }

  @inline
  def notifyDeleteOn(v: ChangingSetValue, value: Long) {
    assert(v == cond)
    assert(keyForRemoval(value) != null)
    keyForRemoval(value).performRemove()
    keyForRemoval(value) = null

    this :-= vars(value).value
  }

  override def checkInternals(c:Checker) {
    c.check(this.value == cond.value.foldLeft(0L)((acc, i) => acc + vars(i).value),
        Some("output.value == cond.value.foldLeft(0L)((acc, i) => acc + vars(i).value)"))
  }
}


/** sum(i in cond) vars(i)
  * @param vars is an array of IntVars
  * @param cond is the condition for selecting variables in the array of summed ones, cannot be null
  * @author renaud.delandtsheer@cetic.be
  * */
case class ProdConstants(vars: Array[Long], cond: SetValue)
  extends IntInvariant()
  with SetNotificationTarget{

  registerStaticAndDynamicDependency(cond)
  finishInitialization()

  var NullVarCount = cond.value.count(i => vars(i) == 0L)
  var NonNullProd = cond.value.foldLeft(1L)((acc,i) => if(vars(i) == 0L){acc}else{acc*vars(i)})
  affectOutput()

  @inline
  private def affectOutput(){
    if (NullVarCount == 0L){
      this := NonNullProd
    }else{
      this := 0L
    }
  }

  override def notifySetChanges(v: ChangingSetValue, id: Int, addedValues: Iterable[Long], removedValues: Iterable[Long], oldValue: SortedSet[Long], newValue: SortedSet[Long]): Unit = {
    for (added <- addedValues) notifyInsertOn(v: ChangingSetValue, added)
    for(deleted <- removedValues) notifyDeleteOn(v: ChangingSetValue, deleted)
  }

  @inline
  def notifyInsertOn(v: ChangingSetValue, value: Long) {
    assert(v == cond)

    if(vars(value) == 0L){
      NullVarCount += 1L
    }else{
      NonNullProd *= vars(value)
    }
    affectOutput()
  }

  @inline
  def notifyDeleteOn(v: ChangingSetValue, value: Long) {

    if(vars(value) == 0L){
      NullVarCount -= 1L
    }else{
      NonNullProd = NonNullProd / vars(value)
    }
    affectOutput()
  }

  override def checkInternals(c:Checker) {
    c.check(this.value == cond.value.foldLeft(1L)((acc, i) => acc * vars(i)),
      Some("output.value (" + this.value
        + ") == cond.value.foldLeft(1L)((acc, i) => acc * vars(i).value) ("
        + cond.value.foldLeft(1L)((acc, i) => acc * vars(i)) + ")"))
  }
}


/** prod(i in cond) vars(i)
 * This invariant might modify vars array by cloning some variables to ensure that each variable only appears once.
 * @param vars is a set of IntVars
 * @param cond is the condition for selecting variables in the set of summed ones.
  * @author renaud.delandtsheer@cetic.be
  * */
case class ProdElements(vars: Array[IntValue], cond: SetValue)
  extends IntInvariant with Bulked[IntValue, Unit]
  with VaryingDependencies
  with IntNotificationTarget
  with SetNotificationTarget{

  assert(cond != null, "cond cannot be null for ProdElements")

  val keyForRemoval: Array[KeyForElementRemoval] =  Array.fill(vars.length) {null}

  registerStaticDependency(cond)
  registerDeterminingDependency(cond)

  bulkRegister(vars)

  for(i <- cond.value){
    keyForRemoval(i) = registerDynamicDependency(vars(i),i)
  }

  finishInitialization()

  var NullVarCount = cond.value.count(i => vars(i).value == 0L)
  var NonNullProd = cond.value.foldLeft(1L)((acc,i) => if(vars(i).value == 0L){acc}else{acc*vars(i).value})
  affectOutput()

  @inline
  private def affectOutput(){
    if (NullVarCount == 0L){
      this := NonNullProd
    }else{
      this := 0L
    }
  }

  @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, OldVal: Long, NewVal: Long) {
    //it is always a listened one, but we could check this here
    assert(vars(index) == v)
    assert(keyForRemoval(index)!=null)
    if (OldVal == 0L && NewVal != 0L){
      NullVarCount -=1L
      NonNullProd *=NewVal
    }else if(OldVal != 0L && NewVal == 0L){
      NullVarCount +=1L
      NonNullProd =NonNullProd/OldVal
    }else{
      NonNullProd = NonNullProd/OldVal
      NonNullProd = NonNullProd * NewVal
    }
    affectOutput()
  }

  override def notifySetChanges(v: ChangingSetValue, id: Int, addedValues: Iterable[Long], removedValues: Iterable[Long], oldValue: SortedSet[Long], newValue: SortedSet[Long]): Unit = {
    for (added <- addedValues) notifyInsertOn(v: ChangingSetValue, added)
    for(deleted <- removedValues) notifyDeleteOn(v: ChangingSetValue, deleted)
  }

  @inline
  def notifyInsertOn(v: ChangingSetValue, value: Long) {
    assert(v == cond)
    assert(keyForRemoval(value) == null)
    keyForRemoval(value) = registerDynamicDependency(vars(value),value)

    if(vars(value).value == 0L){
      NullVarCount += 1L
    }else{
      NonNullProd *= vars(value).value
    }
    affectOutput()
  }

  @inline
  def notifyDeleteOn(v: ChangingSetValue, value: Long) {
    assert(v == cond)
    assert(keyForRemoval(value) != null)

    keyForRemoval(value).performRemove()
    keyForRemoval(value) = null

    if(vars(value).value == 0L){
      NullVarCount -= 1L
    }else{
      NonNullProd = NonNullProd / vars(value).value
    }
    affectOutput()
  }

  override def checkInternals(c:Checker) {
    c.check(this.value == cond.value.foldLeft(1L)((acc, i) => acc * vars(i).value),
        Some("output.value (" + this.value
            + ") == cond.value.foldLeft(1L)((acc, i) => acc * vars(i).value) ("
            + cond.value.foldLeft(1L)((acc, i) => acc * vars(i).value) + ")"))
  }
}

/**
  *
  * @param vars
  * @param costs
  * @param capacity1
  * @param capacity2
  */

case class CostOfPackInTwoBins(vars : Array[IntValue],costs : Array[(Long,Long)],capacity1 : Long) extends IntInvariant
  with IntNotificationTarget {
  require(vars.length == costs.length,"There should be a cost for each variable")
  for ((c1,c2) <- costs)
    require(c1 <= c2,"The cost of bin2 should always be higher than the cost of bin1")
  require(capacity1 >= 0,"The capacity of the bins should be at least 0")
  for (v <- vars)
    require(v.min >= 0,"The variable shall not have a negative value")

  registerStaticAndDynamicDependencyArrayIndex(vars)

  finishInitialization()

  scheduleForPropagation()

  private val elementSortedByHighestCost = List.tabulate(vars.length)(i => ElementToProcess(costs(i)._1,costs(i)._2,i,vars(i))).sortBy(x => -x.costBin2)

  private var elementInBin1FIFO = Nil
  private var elementInBin2LIFO = Nil


  private var occupiedSpaceInBin1 = 0L
  private var occupiedSpaceInBin2 = 0L

  private val binOfElement = Array.tabulate(vars.length)(_ => 0) // The bin in wich the element is : 0 -> None, 1 -> Bin1, 2 -> Bin2, 3 -> Both

  private var shallPropagate = true

  val occupationBin1 = Array.tabulate(vars.length)(_ => 0L)
  val occupationBin2 = Array.tabulate(vars.length)(_ => 0L)

  private def schedule(): Unit = {
    shallPropagate = true
    scheduleForPropagation()
  }

  override def notifyIntChanged(v : ChangingIntValue,index : Int, OldVal : Long, NewVal : Long): Unit = {
    if (!shallPropagate) {
      binOfElement(index) match {
        case 0 =>
          schedule()
        case 1 =>
          if (occupiedSpaceInBin2 == 0 && occupiedSpaceInBin1 - OldVal + NewVal <= capacity1) {
            occupationBin1(index) += (NewVal - OldVal)
            occupiedSpaceInBin1 += (NewVal - OldVal)
            this :+= (NewVal - OldVal) * costs(index)._1
            if (occupationBin1(index) == 0)
              binOfElement(index) = 0
          }
          else
            schedule()
        case 2 =>
          this :+= (NewVal - OldVal) * costs(index)._2
          occupationBin2(index) += (NewVal - OldVal)
          occupiedSpaceInBin2 += (NewVal - OldVal)
          if (occupationBin2(index) == 0)
            binOfElement(index) = 0
        case 3 =>
          if (occupationBin2(index) + (NewVal - OldVal) >= 0) {
            this :+= (NewVal - OldVal) * costs(index)._2
            occupationBin2(index) += (NewVal  - OldVal)
            occupiedSpaceInBin2 += (NewVal - OldVal)
            if (occupationBin2(index) == 0)
              binOfElement(index) = 1
          } else
            schedule()
      }
    }
  }


  private def processChanges(elementToProcess : List[ElementToProcess]) : Long = {
    elementToProcess match {
      case Nil => 0
      case h :: t =>
        if (h.size.value > 0) {
          if (occupiedSpaceInBin1 < capacity1) {
            val inBin1 = h.size.value min (capacity1 - occupiedSpaceInBin1)
            val inBin2 = h.size.value - inBin1
            occupationBin1(h.index) = inBin1
            occupationBin2(h.index) = inBin2
            occupiedSpaceInBin2 += inBin2
            if (inBin2 > 0)
              binOfElement(h.index) = 3
            else
              binOfElement(h.index) = 1
            occupiedSpaceInBin1 += inBin1
            inBin1 * h.costBin1 + inBin2 * h.costBin2 + processChanges(t)
          } else {
            occupationBin2(h.index) = h.size.value
            occupationBin1(h.index) = 0
            occupiedSpaceInBin2 += h.size.value
            binOfElement(h.index) = 2
            h.costBin2 * h.size.value + processChanges(t)
          }
        } else {
          occupationBin2(h.index) = 0
          occupationBin1(h.index) = 0
          binOfElement(h.index) = 0
          processChanges(t)
        }

    }
  }

  override def performInvariantPropagation(): Unit = {
    if (shallPropagate) {
      occupiedSpaceInBin1 = 0L
      occupiedSpaceInBin2 = 0L
      val timeBeforeNotSmart = System.currentTimeMillis()
      this := processChanges(elementSortedByHighestCost)
    }

    shallPropagate = false

  }

  override def checkInternals(c: Checker): Unit = {

    val occupationBin1FromScratch = occupationBin1.fold(0L)(_ + _)
    val occupationBin2FromScratch = occupationBin2.fold(0L)(_ + _)

    require(occupiedSpaceInBin2 == occupationBin2.fold(0L)(_ + _),"Occupation Bin 2 wrong : From scratch " + occupationBin2FromScratch + " saved : " + occupiedSpaceInBin2)
    require(occupiedSpaceInBin1 == occupationBin1.fold(0L)(_ + _),"Occupation Bin 2 wrong : From scratch " + occupationBin1FromScratch + " saved : " + occupiedSpaceInBin1)

    for (i <- 0 until vars.length) {
      if (occupationBin1(i) == 0){
        if (occupationBin2(i) == 0)
          require(binOfElement(i) == 0,"Element " + vars(i) + " is neither in bin1 nor in bin2 but binOfElement is bin" + binOfElement(i))
        else
          require(binOfElement(i) == 2,"Element " + vars(i) + " is in bin2 but binOfElement is bin" + binOfElement(i))
      } else {
        if (occupationBin2(i) == 0)
          require(binOfElement(i) == 1,"Element " + vars(i) + " is in bin1 but binOfElement is bin" + binOfElement(i))
        else
          require(binOfElement(i) == 3,"Element " + vars(i) + " is in bin1 and in bin2 but binOfElement is bin" + binOfElement(i))

      }

    }

    def computeFromScratch(index : Int) : Long = {
      index match {
        case -1 => 0
        case n => occupationBin1(n) * costs(n)._1 + occupationBin2(n) * costs(n)._2 + computeFromScratch(n-1)
      }
    }
    val fromScratch = computeFromScratch(vars.length - 1)
    require(fromScratch == this.value,"Cost Incremental : " + this.value + " -- From Scratch : " + fromScratch)
  }

}

case class ElementToProcess(costBin1 : Long,
                            costBin2 : Long,
                            index : Int,
                            size : IntValue)


object testCostOfBinPacking extends App{
  val m = new Store(checker = Some(new ErrorChecker()))
  //val m = new Store()


  val domain = DomainRange(0,Long.MaxValue)

  val size = 5
  val production = Array(CBLSIntVar(m,0,domain),CBLSIntVar(m,0,domain),CBLSIntVar(m,3,domain),CBLSIntVar(m,4,domain),CBLSIntVar(m,5,domain))

  val costs = Array((1L,1L),(1L,2L),(1L,3L),(1L,4L),(1L,5L))

  val binPack = CostOfPackInTwoBins(production,costs,10)

  m.close()

  println(binPack.value)
  println(binPack.occupationBin1.mkString(";"))
  println(binPack.occupationBin2.mkString(";"))


  production(1) := 3



  println(binPack.value)
  println(binPack.occupationBin1.mkString(";"))
  println(binPack.occupationBin2.mkString(";"))

  production(0) := 0
  production(1) := 0
  //production(2) := 1

  println(binPack.value)
  println(binPack.occupationBin1.mkString(";"))
  println(binPack.occupationBin2.mkString(";"))

  production(2) := 1


  println(binPack.value)
  println(binPack.occupationBin1.mkString(";"))
  println(binPack.occupationBin2.mkString(";"))

  production(4) := 0
  production(2) :+= 5

  println(binPack.value)
  println(binPack.occupationBin1.mkString(";"))
  println(binPack.occupationBin2.mkString(";"))


}