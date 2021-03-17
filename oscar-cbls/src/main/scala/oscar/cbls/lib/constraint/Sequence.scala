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
/******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Renaud De Landtsheer
  ******************************************************************************/
package oscar.cbls.lib.constraint

import oscar.cbls.core.computation.{CBLSIntVar, ChangingIntValue, Domain, IntValue, Invariant, ShortIntNotificationTarget, Value}
import oscar.cbls.core.constraint.Constraint
import oscar.cbls.core.propagation.Checker
import oscar.cbls.lib.invariant.logic.LazyIntInt2Int
import oscar.cbls.lib.invariant.numeric.Sum

import scala.collection.immutable.SortedMap
import scala.language.existentials

/**implements the sequence constraint:
  * in the array "variables", within each subsequence of length "length" there must be at most "Max" of the position i such that Predicate(variables(i))
  *
  * @param variables the "history variables"
  * @param length the length of the sequence
  * @param Max the max number of elements matching pred in all sequences of the history
  * @param predicate an array of bolean, covering all the values of the varaibles, and ith true or false if the value enforces the predicate or not, respectively
  * @param predicateIsToBeConsideredInVarViolation if false, the violation of a variable is the summed violation of all sequences it is involved in, if true,
  *                                                the violation is dependent on whether the variable enforces the predicate; if it enforces it,
  *                                                it is the other definition, if it does not, it is zero
  * @author renaud.delandtsheer@cetic.be
  */
case class Sequence(variables: Array[_ <: IntValue], length:Int, Max:Int, predicate:Array[Boolean], predicateIsToBeConsideredInVarViolation:Boolean = false)
  extends Invariant with Constraint with ShortIntNotificationTarget{

  assert(Max <= length, "the specified maximum is bigger than the ength of the sequences to consider")

  registerStaticAndDynamicDependencyArrayIndex(variables)
  registerConstrainedVariables(variables)

  finishInitialization()

  /**the number of items in the sequence starting here that enforce the predicate*/
  val count:Array[Int] = Array.tabulate(sequences.size)(i => 0)

  /**the violation of the sequence starting here*/
  val violated = Array.tabulate(sequences.size)(i => CBLSIntVar(model,0L, 0 to length - Max, s"is_violated_sequence$i"))

  for(v <- violated) v.setDefiningInvariant(this)

  /**the violation of a variable is the sum of the violation of each sequence it is involved in*/
  var Violations = SortedMap.empty[IntValue, IntValue]

  for(i <- variables.indices){
    val (lb,ub) = sequencesInvolving(i)
    val summedViolationOfSequencesVarIIsInvolvedIn = Sum((lb to ub).map(violated(_)))
    val violationOfVariableI = if(predicateIsToBeConsideredInVarViolation){
      new LazyIntInt2Int(summedViolationOfSequencesVarIIsInvolvedIn,
        variables(i),
        (summedViol,varValue) => {
          require(varValue <= Int.MaxValue, "varValue must be <= Int.MaxValue because it's used as an index")
          if (predicate(varValue.toInt)) summedViol else 0L}, summedViolationOfSequencesVarIIsInvolvedIn.domain)
    }else{
      summedViolationOfSequencesVarIIsInvolvedIn
    }
    Violations = Violations + ((variables(i),violationOfVariableI))
  }

  val Violation = CBLSIntVar(model,0L, Domain(0, variables.length * length) ,"sequence_violations")
  Violation.setDefiningInvariant(this)

  for(i <- variables.indices){
    if(predicate(variables(i).value.toInt)){
      val (lb,ub) = sequencesInvolving(i)
      var j = lb
      while(j <= ub){
        count(j) += 1
        if(count(j) > Max){
          violated(j) :+=1L
          Violation :+= 1L
        }
        j += 1
      }
    }
  }

  private def sequences: Range = 0 to variables.length - length

  /** returns the sequences that involve this position
    *
    * @param i the position
    * @return
    */
  @inline
  private def sequencesInvolving(i:Int):(Int,Int) = {
    val lb = 0 max 1+i-length
    val ub = i min variables.length - length
    (lb,ub)
  }

  @inline
  override def notifyIntChanged(v: ChangingIntValue, i: Int, OldVal: Int, NewVal: Int): Unit = {
    if (predicate(OldVal)){ //TODO: on peut éventuellement conserver predicate(OldVal) dans un tableau de booléens
      if(!predicate(NewVal)){
        //decrease the count
        val (lb,ub) = sequencesInvolving(i)
        var j = lb
        while(j <= ub){
          count(j) -= 1
          if(count(j) >= Max){
            violated(j) :-=1L
            Violation :-= 1L
          }
          j+=1
        }
      }
    }else{
      if(predicate(NewVal)){
        //increase the count
        val (lb,ub) = sequencesInvolving(i)
        var j = lb
        while(j <= ub){
          count(j) += 1
          if(count(j) > Max){
            violated(j) :+=1L
            Violation :+= 1L
          }
          j+=1
        }
      }
    }
  }

  private def min(a:Long, b:Long):Long = if (a>b) b else a

  def violation(v: Value): IntValue = Violations.getOrElse(v.asInstanceOf[IntValue],null)

  def violation: CBLSIntVar = Violation

  /** To override whenever possible to spot errors in invariants.
    * this will be called for each invariant after propagation is performed.
    * It requires that the Model is instantiated with the variable debug set to true.
    */
  override def checkInternals(c: Checker): Unit = {
    val countCheck:Array[Long] = Array.tabulate(sequences.size)(_ => 0L)
    /**the violation of the sequence starting here*/
    val violatedCheck = Array.tabulate(sequences.size)(_ => 0L)
    var violationCheck = 0L

    for(i <- variables.indices){
      if(predicate(variables(i).valueInt)){
        val (lb,ub) = sequencesInvolving(i)
        for(j <- lb to ub){
          countCheck(j) += 1L
          if(countCheck(j) > Max){
            violatedCheck(j) +=1L
            violationCheck += 1L
          }
        }
      }
    }

    for(s <- sequences)c.check(countCheck(s) == count(s),Some("countCheck(s) == count(s)"))
    for(s <- sequences)c.check(violatedCheck(s) == violated(s).value,Some("violatedCheck(s) == violated(s).value"))
    c.check(violationCheck == violation.value)
  }
}
