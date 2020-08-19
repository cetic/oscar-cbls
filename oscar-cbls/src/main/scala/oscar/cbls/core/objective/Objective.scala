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
package oscar.cbls.core.objective

/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

import oscar.cbls
import oscar.cbls.core.computation._

object Objective{
  implicit def objToChangingIntValue(o:IntVarObjective):ChangingIntValue = o.objective
  implicit def objToFun(o:Objective):()=>Long = ()=>o.value
  implicit def funToObj(f:()=>Long) = new FunctionObjective(f)

  def apply(f:()=>Long,model:Store = null) = new FunctionObjective(f,model)

  implicit def apply(objective:IntValue) =
    objective match {
      case c: ChangingIntValue => new IntVarObjective(c)
      case c: CBLSIntConst => throw new Error("you do not want to have an objective that is actually a constant value!")
    }
}

/**
 * a common class for modeling an objective, and querying its variations on different basic moves
 *
 * All queries on the variation on the objective following moves are performed
 * by explicitly performing the move, evaluating the objective, and backtracking the move.
 *
 * hence all these queries return the value of the objective function after the move, and not a delta
 * as you will find in Comet for instance.
 *
 * The reason is that this value is computed through explicit state change and restore,
 * and since all moves are performed lazily,
 * computing a delta would not enable OscaR.cbls to perform the state restore
 * and the next state change in a single model propagation
 * (you generally call a bunch of such methods in a row to find the move you want to perform actually)
 * hence it would be much less efficient to compute a delta at this level than computing a value.
 *
 * If you need a delta, you should compute it by yourself.
 *
 * @author renaud.delandtsheer@cetic.be
 */
class IntVarObjective(val objective: ChangingIntValue) extends Objective {

  model.registerForPartialPropagation(objective)

  /**
   * This method returns the actual objective value.
   * It is easy to override it, and perform a smarter propagation if needed.
   * @return the actual objective value.
   */
  def value: Long = objective.value

  def model:Store = objective.model

  def detailedString(short:Boolean,indent:Long = 0L):String =
    s"IntVarObjective($objective)"
}

/**
 * if (objective1.value > 0L) Long.MaxValue/2L + objective1.value
 *   else objective2.value
 *
 *   this is computed partially both for objective and mustBeZeroObjective
 * @param mustBeZeroObjective
 */
class CascadingObjective(mustBeZeroObjective: Objective, secondObjective:Objective,cascadeSize:Long = Long.MaxValue) extends Objective {

  override def detailedString(short: Boolean, indent:Long = 0L): String =
    (if(short) {
      if (mustBeZeroObjective.value == 0L) {
        nSpace(indent) + "CascadingObjective(\n" +
          nSpace(indent + 2L) + "mustBeZeroObjective :=0L \n" +
          nSpace(indent + 2L) + "secondObjective:" + secondObjective.detailedString(true, indent + 2L) + "\n" +
          nSpace(indent) + ")"
      } else {
        nSpace(indent) + "CascadingObjective(\n" +
          nSpace(indent + 2L) + "mustBeZeroObjective:" + mustBeZeroObjective.detailedString(true, indent + 4L) + "\n" +
          nSpace(indent) + ")"
      }
    }else {
      nSpace(indent) + "CascadingObjective(\n" +
        nSpace(indent + 2L) + "mustBeZeroObjective:" + mustBeZeroObjective.detailedString(true, indent + 4L) + "\n" +
        nSpace(indent + 2L) + "secondObjective:" + secondObjective.detailedString(true, indent + 4L) + "\n" +
        nSpace(indent) + ")"
    })

  /**
   * This method returns the actual objective value.
   * It is easy to override it, and perform a smarter propagation if needed.
   * @return the actual objective value.
   */
  override def value: Long = {
    val firstObjectiveValue = mustBeZeroObjective.value
    if (firstObjectiveValue!=0L) cascadeSize
    else secondObjective.value
  }

  override def model: Store = mustBeZeroObjective.model
}

object PriorityObjective{
  def apply(objective1: Objective, objective2:Objective, maxObjective2:Long) = new PriorityObjective(objective1: Objective, objective2:Objective, maxObjective2:Long)

  def apply(firstObj:Objective,moreObjAndTheirMaxValue:List[(Objective,Long)]):Objective = {
    if(moreObjAndTheirMaxValue.isEmpty) firstObj
    else applyRecur(firstObj,moreObjAndTheirMaxValue)._1
  }

  private def applyRecur(objective1:Objective,moreObjAndTheirMaxValue:List[(Objective,Long)]):(PriorityObjective,Long) = {
    moreObjAndTheirMaxValue match {
      case List((objective2, maxObjective2)) =>
        val p = new PriorityObjective(objective1, objective2, maxObjective2)
        (p, maxObjective2)
      case (secondObj, maxSecondObj) :: tail =>
        val (tailOBj, max2Tail) = applyRecur(secondObj, tail)
        val newMAx2 = max2Tail + maxSecondObj
        val p = new PriorityObjective(objective1, tailOBj, newMAx2)
        (p, newMAx2)
    }
  }

  def prettyPrintObjectiveSequence(prioritizedNameAndValue:Iterable[(String,Long)]):String = {
    var allZeroSoFar:Boolean = true
    val coloredText = prioritizedNameAndValue.map({ case (name,value) =>
      val text = name + value
      if (value!=0 && allZeroSoFar){
        allZeroSoFar = false
        Console.BLUE + text + Console.GREEN
      }else{
        text
      }
    })
    Console.GREEN + coloredText.mkString(" ") + Console.RESET
  }
}

/**
 * if (objective1.value == 0) objective2.value
 * else objective1.value * (maxObjective2+1)
 *
 * If the product parameter is set to false, the formula uses a + operator instead of a *:
 *
 * if (objective1.value == 0) objective2.value
 * else objective1.value + (maxObjective2+1)
 *
 * this is when objective2 is expensive to evaluate
 * and we want to treat objective1 as a weak objective (opposite to the [[CascadingObjective]])
 *
 * @param objective1 the first objective to minimize
 * @param objective2 the one to minimize when the first one is zero
 * @param maxObjective2 the maximal value that objective2 will ever have when objective1 is zero.
 */
class PriorityObjective(val objective1: Objective,
                        val objective2:Objective,
                        val maxObjective2:Long) extends Objective {

  /**
   * This method returns the actual objective value.
   * It is easy to override it, and perform a smarter propagation if needed.
   * @return the actual objective value.
   */
  override def value: Long = {
    val firstObjectiveValue = objective1.value
    if (firstObjectiveValue==0) {
      val obj2Value = objective2.value
      cbls.warning(obj2Value <= maxObjective2,
        s"PriorityObjective:obj2Value($obj2Value) should be < maxObjective2($maxObjective2)")
      obj2Value
    }else{
      firstObjectiveValue + maxObjective2 + 1
    }
  }

  override def model: Store = objective1.model

  override def detailedString(short: Boolean, indent:Long = 0L): String =
    if(short) {
      if (objective1.value == 0) {
        s"PriorityObjective(value: ${this.value}\n" +
          nSpace(indent + 2L) + "objective1 :=0 \n" +
          nSpace(indent + 2L) + "objective2:" + objective2.detailedString(true, indent + 2L) + "\n" +
          nSpace(indent) + ")"
      } else {
        s"PriorityObjective(value:${this.value}\n" +
          nSpace(indent + 2L) + "objective1:" + objective1.detailedString(true, indent + 4L) + "\n" +
          nSpace(indent) + ")"
      }
    }else {
      s"PriorityObjective(value:${this.value}\n" +
        nSpace(indent + 2L) + "objective1:" + objective1.detailedString(false, indent + 4L) + "\n" +
        nSpace(indent + 2L) + "objective2:" + objective2.detailedString(false, indent + 4L) + "\n" +
        nSpace(indent) + ")"
    }
}

class FunctionObjective(f:()=>Long, m:Store = null) extends Objective{
  override def model: Store = m

  /**
   * This method returns the actual objective value.
   * It is easy to override it, and perform a smarter propagation if needed.
   * @return the actual objective value.
   */
  override def value: Long = f()

  override def detailedString(short: Boolean,indent:Long = 0L): String = s"${nSpace(indent)}FunctionObjective($value)"
}

trait Objective {

  protected def nSpace(n:Long):String = if(n <= 0L) "" else " " + nSpace(n-1L)
  override def toString: String = detailedString(false)
  def detailedString(short:Boolean, indent:Long = 0L):String

  def model:Store

  /**
   * This method returns the actual objective value.
   * It is easy to override it, and perform a smarter propagation if needed.
   * @return the actual objective value.
   */
  def value:Long
  def isZero:Boolean = value == 0L

  /**returns the value of the objective variable if the two variables a and b were swapped values.
   * This proceeds through explicit state change and restore.
   * this process is efficiently performed as the objective Variable is registered for partial propagation
   * @see registerForPartialPropagation() in [[oscar.cbls.core.computation.Store]]
   */
  def swapVal(a: CBLSIntVar, b: CBLSIntVar): Long = {
    a :=: b
    val newVal = value
    a :=: b
    newVal
  }

  /**returns the value of the objective variable if variable a was assigned the value v.
   * This proceeds through explicit state change and restore.
   * this process is efficiently performed as the objective Variable is registered for partial propagation
   * @see registerForPartialPropagation() in [[oscar.cbls.core.computation.Store]]
   */
  def assignVal(a: CBLSIntVar, v: Long): Long = assignVal(List((a,v)))

  /**returns the value of the objective variable if the assignment described by parameter a was performed
   * This proceeds through explicit state change and restore.
   * this process is efficiently performed as the objective Variable is registered for partial propagation
   * @see registerForPartialPropagation() in [[oscar.cbls.core.computation.Store]]
   */
  def assignVal(a: Iterable[(CBLSIntVar, Long)]): Long = {
    //memorize
    val oldvals: Iterable[(CBLSIntVar, Long)] = a.foldLeft(List.empty[(CBLSIntVar, Long)])(
      (acc, IntVarAndInt) => ((IntVarAndInt._1, IntVarAndInt._1.value)) :: acc)
    //excurse
    for (assign <- a)
      assign._1 := assign._2
    val newObj = value
    //undo
    for (assign <- oldvals)
      assign._1 := assign._2
    newObj
  }

  /**returns the value of the objective variable if i is inserted to a
   * this process is efficiently performed as the objective Variable is registered for partial propagation
   * @see registerForPartialPropagation() in [[oscar.cbls.core.computation.Store]]
   */
  def insertValAssumeNotAlreadyIn(a: CBLSSetVar, i:Int): Long = {
    a :+= i
    val newVal = value
    a :-= i
    newVal
  }

  /**returns the value of the objective variable if i is inserted to a
   * this process is efficiently performed as the objective Variable is registered for partial propagation
   * @see registerForPartialPropagation() in [[oscar.cbls.core.computation.Store]]
   */
  def insertVal(a: CBLSSetVar, i:Int): Long = {
    if(a.value.contains(i)) return value
    insertValAssumeNotAlreadyIn(a, i)
  }

  /**returns the value of the objective variable if i is removed from a
   * this process is efficiently performed as the objective Variable is registered for partial propagation
   * @see registerForPartialPropagation() in [[oscar.cbls.core.computation.Store]]
   */
  def removeValAssumeIn(a: CBLSSetVar, i:Int): Long = {
    a :-= i
    val newVal = value
    a :+= i
    newVal
  }

  /**returns the value of the objective variable if i is removed from a
   * this process is efficiently performed as the objective Variable is registered for partial propagation
   * @see registerForPartialPropagation() in [[oscar.cbls.core.computation.Store]]
   */
  def removeVal(a: CBLSSetVar, i:Int): Long = {
    if(!a.value.contains(i)) return value
    removeValAssumeIn(a, i)
  }
}

/**
 * This is a stub that logs the toString of the baseObjective every time its value is needed.
 * these logs can be retrieved through the getAndCleanEvaluationLog method.
 * The value of this objective function is really the one of baseObjective
 *
 * @param baseObjective the value of this objective function
 */
class LoggingObjective(baseObjective:Objective) extends Objective{
  private var evaluationsLog:List[String] = List.empty

  override def detailedString(short: Boolean, indent:Long = 0L): String = nSpace(indent) + "LoggingObjective(" + baseObjective.detailedString(short) + ")"

  override def model: Store = baseObjective.model

  override def value: Long = {
    val toReturn = baseObjective.value
    evaluationsLog = baseObjective.detailedString(true) :: evaluationsLog
    toReturn
  }

  def getAndCleanEvaluationLog:List[String] = {
    val toReturn = evaluationsLog
    evaluationsLog = List.empty
    toReturn
  }
}
