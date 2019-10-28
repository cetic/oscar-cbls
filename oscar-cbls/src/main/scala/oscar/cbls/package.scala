package oscar

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


import oscar.cbls.algo.search.InstrumentedRange
import oscar.cbls.core.computation._
import oscar.cbls.core.search.Neighborhood
import oscar.cbls.lib.constraint.{EQ, GE, LE, NE}
import oscar.cbls.lib.invariant.logic._
import oscar.cbls.lib.invariant.numeric._
import oscar.cbls.lib.invariant.set._
import oscar.cbls.modeling.{ModelingAPI, NeighborhoodOps}

import scala.collection.immutable.NumericRange
import scala.language.implicitConversions

/**
  * The `cbls` package provides useful functionalities to model problem using
  * the OscaR Constraint Based Local Search Library.
  *
  * By doing
  * {{{import oscar.cbls_}}}
  * you also import
  *
  *   - [[oscar.cbls `cbls`]] a package object that is a factory for all types of variable supported by OscaR.cbls:  [[oscar.cbls.CBLSIntVar `CBLSIntVar`]], [[oscar.cbls.CBLSSetVar `CBLSSetVar`]] and [[oscar.cbls.CBLSSeqVar `CBLSSeqVar`]]
  *   - [[oscar.cbls.modeling.ModelingAPI `ModelingAPI`]] a factory to instantiate all constraints, neighborhoods etc. of OscaR.cbls
  *   - [[oscar.cbls.modeling.CBLSModel `CBLSModel`]] an object you can extend and that instantiates some basic structure of a model, namely a store and adds some implicits
  *
  * === N-Queens Example ===
  *
  * {{{
  *import oscar.cbls._
  *import scala.util.Random
  *
  * /** Local Search for NQueens
  *  *  Moves are operated by swapping variables, using a standard neighborhood
  * */
  *object NQueensEasy extends CBLSModel with App{
  *
  *  val N = 1000L
  *  println("NQueensEasy(" + N + ")")
  *  val range:Range = Range(0L,N)
  *  val init = Random.shuffle(range.toList).iterator
  *
  *  //creating variables, one queen par column,
  *  //initialized on a permutation of the diagolal (all on different rows)
  *  val queens = Array.tabulate(N)((q:Long) => CBLSIntVar(init.next(),0L until N, "queen" + q))
  *
  *  c.post(allDiff( for (q <- range) yield queens(q) + q) )
  *  c.post(allDiff( for (q <- range) yield q - queens(q)) )
  *
  *  //the queens that are the most threatened
  *  val maxViolQueens = argMax(c.violations(queens)).setName("most violated queens")
  *
  *  val neighborhood =
  *    swapsNeighborhood(queens, "SwapQueens",
  *      searchZone2 = maxViolQueens,
  *      symmetryCanBeBrokenOnIndices = false)
  *
  *  close()
  *
  *  val it = neighborhood.doAllMoves(_ >= N || c.violation.value == 0L, c.violation)
  *
  *  println("finished: " + getWatchString)
  *}
  * }}}
  *
  *
  * === Implicit Conversions ===
  *
  * A number of commonly applied implicit conversions are also defined here.
  * Implicit conversions provide additional functions to core classes
  * such as infix `+` and `-` for [[oscar.cbls.core.computation.CBLSIntVar `CBLSIntVar`]],
  * or the combinator as infix notation for [[oscar.cbls.core.search.Neighborhood `Neighborhood`]].
  *
  * @author Renaud De Landtsheer renaud.delandtsheer@cetic.be
  */
package object cbls extends ModelingAPI{
  // Alias to useful classes and companion objects

  type CBLSModel = oscar.cbls.modeling.CBLSModel

  type Store = oscar.cbls.core.computation.Store
  final val Store = oscar.cbls.core.computation.Store

  type ErrorChecker = oscar.cbls.core.propagation.ErrorChecker
  final val ErrorChecker = oscar.cbls.core.propagation.ErrorChecker

  type Domain = oscar.cbls.core.computation.Domain
  final val Domain = oscar.cbls.core.computation.Domain

  final val fullRange = oscar.cbls.core.computation.FullRange
  final val positiveOrNullRange = oscar.cbls.core.computation.PositiveOrNullRange

  type Value = oscar.cbls.core.computation.Value
  type Variable = oscar.cbls.core.computation.Variable

  type Solution = oscar.cbls.core.computation.Solution
  type Snapshot = oscar.cbls.core.computation.Snapshot

  type AbstractVariable= oscar.cbls.core.computation.AbstractVariable

  type CascadingObjective = oscar.cbls.core.objective.CascadingObjective

  type IntVarObjective = oscar.cbls.core.objective.IntVarObjective

  implicit def instrumentNeighborhood(n:Neighborhood):NeighborhoodOps = new NeighborhoodOps(n)

  type LoopBehavior = oscar.cbls.core.search.LoopBehavior
  final val LoopBehavior = oscar.cbls.core.search.LoopBehavior

  // Alias to useful classes and companion objects
  type CBLSIntVar = oscar.cbls.core.computation.CBLSIntVar
  final val CBLSIntVar = oscar.cbls.core.computation.CBLSIntVar

  //integer types
  type IntValue = oscar.cbls.core.computation.IntValue
  final val IntValue = oscar.cbls.core.computation.IntValue

  type CBLSIntConst = oscar.cbls.core.computation.CBLSIntConst
  final val CBLSIntConst = oscar.cbls.core.computation.CBLSIntConst

  implicit def longToCBLSIntConst(i:Long):IntValue = CBLSIntConst(i)
  implicit def intToCBLSIntConst(i:Int):IntValue = CBLSIntConst(i)

  //set types
  type CBLSSetVar = oscar.cbls.core.computation.CBLSSetVar
  final val CBLSSetVar = oscar.cbls.core.computation.CBLSSetVar

  type SetValue = oscar.cbls.core.computation.SetValue
  final val SetValue = oscar.cbls.core.computation.SetValue

  type CBLSSetConst = oscar.cbls.core.computation.CBLSSetConst
  final val CBLSSetConst = oscar.cbls.core.computation.CBLSSetConst

  //seq types
  type CBLSSeqVar = oscar.cbls.core.computation.CBLSSeqVar
  final val CBLSSeqVar = oscar.cbls.core.computation.CBLSSeqVar

  type SeqValue = oscar.cbls.core.computation.SeqValue
  type CBLSSeqConst = oscar.cbls.core.computation.CBLSSeqConst
  final val CBLSSeqConst = oscar.cbls.core.computation.CBLSSeqConst

  //objective types
  type Objective = oscar.cbls.core.objective.Objective
  final val Objective = oscar.cbls.core.objective.Objective

  //constraint types
  type Constraint = oscar.cbls.core.constraint.Constraint

  type ConstraintSystem = oscar.cbls.core.constraint.ConstraintSystem
  final val ConstraintSystem = oscar.cbls.core.constraint.ConstraintSystem

  //implicits
  implicit def longToIntVarOps(i:Long):IntValOps = IntValOps(CBLSIntConst(i))
  implicit def intToIntVarOps(i:Int):IntValOps = IntValOps(CBLSIntConst(i))

  //some implicit for the CBLS variables, to add infix operators

  implicit def intVarOps(x:CBLSIntVar):IntValOps = new IntValOps(x)

  implicit class IntValOps(x: IntValue) {
    def +(v: IntValue): IntInvariant = Sum2(x, v)

    def -(v: IntValue): IntInvariant = Minus(x, v)

    def *(v: IntValue): IntInvariant = Prod2(x, v)

    def /(v: IntValue): IntInvariant = Div(x, v)
    def /(i:Long):IntInvariant = {
      var extremeValues:List[Long] = List(x.domain.min/i,x.domain.max/i)
      if(x.domain contains 0L) extremeValues = 0L :: extremeValues
      if(x.domain contains -1L) extremeValues = (-1L/i) :: extremeValues
      if(x.domain contains 1L) extremeValues = (1L/i) :: extremeValues
      new Int2Int(x,_/i,Domain(extremeValues.min , extremeValues.max))
    }

    def %(v: IntValue): IntInvariant = Mod(x, v)

    def le(b:IntValue) = LE(x,b)
    def ge(b:IntValue) = GE(x,b)

    def ===(b:IntValue) = EQ(x,b)
    def !==(b:IntValue) = NE(x,b)
    //we do not use the infix notation for comparison operators
    // because they are ambiguous with the <== operation of CBLS that is the "follows" operator)
  }

  implicit class SetVarOps(x: SetValue) {
    def union(v: SetValue): SetInvariant = Union(x, v)

    def inter(v: SetValue): SetInvariant = Inter(x, v)

    def minus(v: SetValue): SetInvariant = Diff(x, v)

    def map(fun: Long => Long,outputDomain:Domain) = SetMap(x, fun, outputDomain)
  }

  implicit class IntValueArrayOps(intValueArray: Array[IntValue]) {
    def element(index:IntValue) = IntElement(index, intValueArray)

    def elements(index:SetValue) = Elements(index, intValueArray)
  }

  implicit class SetValueOps[X<:SetValue](setArray: Array[X]) {
    def element(index:IntValue): SetInvariant = SetElement(index, setArray)
  }

  implicit class IntArrayOps(intArray: Array[Long]) {
    def element(index:CBLSIntVar) = ConstantIntElement(index, intArray)

    def elements(index:SetValue) = Elements(index, intArray.map(CBLSIntConst.apply))
  }

  implicit def cBLSIntVarArrayToCBLSIntValueArray(a:Array[CBLSIntVar]):Array[IntValue] = {
    a.asInstanceOf[Array[IntValue]]
  }

  implicit def cBLSIntVarArrayToCBLSIntValueOpsArray(a:Array[CBLSIntVar]):IntValueArrayOps = {
    IntValueArrayOps(a.asInstanceOf[Array[IntValue]])
  }

  implicit def cBLSetVarArrayToCBLSSetValueArray(a:Array[CBLSSetVar]):Array[SetValue] = {
    a.asInstanceOf[Array[SetValue]]
  }

  // implicit conversion of Range towards a RangeHotRestart, to use the StartBy keyword
  implicit def instrumentRange(r: NumericRange[Long]): InstrumentedRange = new InstrumentedRange(r)

  //this one has been added followingthe 32 to 64 bits port of oscar.cbls
  implicit def longToInt(l:Long):Int = Math.toIntExact(l)
  implicit def intToLong(i:Int):Long = i

  implicit def minMaxCoupleLongLongToDomain(minMaxCouple:((Long,Long))):Domain = DomainRange(minMaxCouple._1,minMaxCouple._2)
  implicit def minMaxCoupleIntIntToDomain(minMaxCouple:((Int,Int))):Domain = DomainRange(minMaxCouple._1,minMaxCouple._2)
  implicit def minMaxCoupleIntLongToDomain(minMaxCouple:((Int,Long))):Domain = DomainRange(minMaxCouple._1,minMaxCouple._2)
  implicit def minMaxCoupleLongIntToDomain(minMaxCouple:((Long,Int))):Domain = DomainRange(minMaxCouple._1,minMaxCouple._2)




  /** Tests an expression, prints a warning message on the console if false
    *  This method is similar to `require`, but does not stop the execution
    *
    *  @param requirement   the expression to test
    *  @param message       a String to print to he error console
    *  @group assertions
    */
  @inline final def warning(requirement: Boolean, message: => Any) {
    if (!requirement)
      System.err.println("WARNING: " + message)
  }

  @inline final def warning(message: => Any) {
      System.err.println("WARNING: " + message)
  }
}
