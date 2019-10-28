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

package oscar.cbls.modeling

import oscar.cbls._
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.core._
import oscar.cbls.lib.invariant.logic._
import oscar.cbls.lib.invariant.minmax._
import oscar.cbls.lib.invariant.numeric._
import oscar.cbls.lib.invariant.seq._
import oscar.cbls.lib.invariant.set._

import scala.collection.immutable.{SortedSet, SortedMap}

/**
 * modeling interface presenting the element invariants
 * @author renaud.delandtsheer@cetic.be
 */
trait ElementInvariants{
  /** if (ifVar >0L) then thenVar else elveVar
    * @param ifVar the condition (IntVar)
    * @param thenVar the returned value if ifVar > 0L
    * @param elseVar the returned value if ifVar <= 0L
    * */
  def intITE(ifVar:IntValue, thenVar:IntValue, elseVar:IntValue, pivot: Long = 0L) = IntITE(ifVar, thenVar, elseVar, pivot)

  /** inputarray[index]
    * @param inputarray is an array of IntVar
    * @param index is the index accessing the array*/
  def intElement(index:IntValue, inputarray:Array[IntValue]) = IntElement(index:IntValue, inputarray:Array[IntValue])

  /**Union(i in index) (array[i])
    * @param index is an IntSetVar denoting the set of positions in the array to consider
    * @param inputarray is the array of intvar that can be selected by the index
    */
  def intElements(index:SetValue, inputarray:Array[IntValue]) = Elements(index, inputarray)

  /** inputarray[index] on an array of IntSetVar
    * @param inputarray is the array of intsetvar
    * @param index is the index of the array access
    **/
  def intSetElement(index:IntValue, inputarray:Array[SetValue]) = SetElement(index, inputarray)

  /**
   * inputarray[index]
   * @param inputArray is an array of Long
   * @param index is the index accessing the array
   * @author renaud.delandtsheer@cetic.be
   * */
  def constantIntElement(index: IntValue, inputArray: Array[Long]) = ConstantIntElement(index, inputArray)
}

trait ClusterInvariants {

  def makeSparseCluster(values : Array[IntValue], clusters : Iterable[Long]) = Cluster.makeSparse(values, clusters)

  def makeDenseCluster(values : Array[IntValue]) = Cluster.makeDense(values)

  def makeDenseClusterAssumingMinMax(values : Array[IntValue], themin : Long, themax : Long) = Cluster.makeDenseAssumingMinMax(values, themin, themax)

  /** maintains a cluster of the indexes of array:  cluster(j) = {i in index of values | values[i] == j}
    * This is considered as a sparse cluster because Cluster is a map and must not cover all possibles values of the values in the array ''values''
    * */
  def sparseCluster(values : Array[IntValue], Clusters : SortedMap[Long, CBLSSetVar]) = SparseCluster(values, Clusters)

  /** Maintains a cluster of the indexes of array: cluster(j) = {i in index of values | values[i] == j}
    * This is considered as a dense cluster because Cluster is an array and must cover all the possibles values of the values in the array ''values''
    * */
  def denseCluster(values : Array[IntValue], clusters : Array[CBLSSetVar]) = DenseCluster(values, clusters)

}


trait RefInvariants{
  /**
   * Maintains the reverse references. Referencing(i) = {j | Reference(j) includes i}
   * */
  def denseRef(references:Array[SetValue], referencing:Array[CBLSSetVar]) = DenseRef(references, referencing)

  def makeDenseRef(references:Array[SetValue]) = DenseRef.makeDenseRef(references)
}


/**
 * modeling interface presenting the set invariants
 * @author renaud.delandtsheer@cetic.be
 */
trait SetInvariants{
  /** left UNION right
    * @param left is an intvarset
    * @param right is an intvarset
    * */
  def union(left:SetValue, right:SetValue) = Union(left:SetValue, right:SetValue)

  /** left INTER right
    * @param left is an intvarset
    * @param right is an intvarset
    * */
  def inter(left:SetValue, right:SetValue) = Inter(left:SetValue, right:SetValue)

  /** left MINUS right, the set diff operator
    * @param left is the base set
    * @param right is the set that is removed from left
    * */
  def diff(left:SetValue, right:SetValue) = Diff(left:SetValue, right:SetValue)

  /** #(v) (cardinality)
    * @param v is an IntSetVar, the set of integers to count
    */
  def cardinality(v:SetValue) = Cardinality(v:SetValue)

  /** makes an IntSetVar out of a set of IntVar. If several variables have the same value, the value is present only once in the resulting set
    * @param on is a set of IntVar
    * */
  def makeSet(on:SortedSet[IntValue]) = MakeSet(on:SortedSet[IntValue])

  /** makes a set out of an interval specified by a lower bound and an upper bound. if lb > ub, the set is empty.
    * @param lb is the lower bound of the interval
    * @param ub is the upper bound of the interval
    * */
  def interval(lb:IntValue,ub:IntValue) = Interval(lb:IntValue,ub:IntValue)

  /**maintains the output as any value taken from the intset var parameter.
    * if this set is empty, puts the default value ni output.
    * @param from
    * @param default
    */
  def takeAny(from:SetValue,  default:Long) = TakeAny(from:SetValue,  default:Long)

  /** Sum(i in on)(fun(i))
    * @param on is the set of integers to add
    * @param fun is an optional function Long -> Long to apply before summing elements. It is expected not to rely on any variable of the model.
    * */
  def setSum(on:SetValue, fun:(Long => Long) = (a:Long) => a) = SetSum(on, fun)

  /** PRod(i in on)(fun(i))
    * @param on is the set of integers to multiply
    * */
  def setProd(on:SetValue) = SetProd(on)
}


trait CumulativeInvariants{

  /**
   * Maintains a resource usage profile.
   * @param indices the indices of tasks
   * @param start the start time of tasks
   * @param duration the duration of tasks
   * @param amount the amount that tasks use of this resource
   * @param profile the usage profile of the resource maintained to profile(time) <== sum(task.amount | task.start <= time <= t.start+t.duration)
   * @param active the tasks that are active maintained to active(time) <== (task.indices | task.start <= time <= t.start+t.duration)
   */
  def cumulative(indices:Array[Long], start:Array[IntValue], duration:Array[IntValue], amount:Array[IntValue], profile:Array[CBLSIntVar], active:Array[CBLSSetVar])  =
    Cumulative(indices:Array[Long], start, duration, amount, profile, active)

  /**
   * Maintains a resource usage profile.
   * @param start the start time of tasks
   * @param duration the duration of tasks
   * @param amount the amount that tasks use of this resource
   * @param profile the usage profile of the resource maintained to profile(time) <== sum(task.amount | task.start <= time <= t.start+t.duration)
   * @author renaud.delandtsheer@cetic.be
   * @author Jean-Noel Monette
   */
  def cumulativeNoSet(start: Array[IntValue],
                      duration: Array[IntValue],
                      amount: Array[IntValue],
                      profile: Array[CBLSIntVar])
  = CumulativeNoSet(start, duration, amount, profile)
}


trait FilterInvariants{
  /** { i in index(values) | cond(values[i] }
    * @param values is an array of IntVar
    * @param cond is a function that selects values to be includes in the output set.
    * This ''cond'' function cannot depend on any IntVar, as updates to these IntVars will not trigger propagation of this invariant.
    */
  def filter(values:Array[IntValue], cond:(Long=>Boolean) = _ != 0L) =  Filter(values:Array[IntValue], cond:(Long=>Boolean))
}


trait HelperInvariants{
  /** This is a helper to define an invariant from an Long -> Long function.
    * Ths invariant is not incremental, so it should only be used for very simple functions.
    * it maintains output = fun(a)
    * @param a the parameter of the function
    * @param fun the function to maintain, it is supposed not to listen to any variable in the model
    * @param domain the expected domain of the output
    * @param cached set to true to have a cache of size 1L, zero to have no cache. cache can provide speedup if fun is time-consuming
    * @author renaud.delandtsheer@cetic.be
    * */
  def int2Int(a:IntValue, fun:Long => Long, domain:Domain = fullRange,cached:Boolean = false)
  = new Int2Int(a, fun, domain,cached)

  /** This is a helper to define an invariant from an Long x Long -> Long function.
    * Ths invariant is not incremental, so this should only be used for very simple functions.
    * it maintains output = fun(a,b)
    * @param a the first parameter of the function
    * @param b the second parameter of the function
    * @param fun the function to maintain, it is supposed not to listen to any variable in the model
    * @param domain the expected domain of the output
    * @author renaud.delandtsheer@cetic.be
    * */
  def intInt2Int(a:IntValue, b:IntValue, fun:((Long, Long) => Long), domain:Domain = fullRange) =
    new IntInt2Int(a, b, fun, domain)

  /** This is a helper to define an invariant from an Long x Long -> Long function.
    * Ths invariant is not incremental, so this should only be used for very simple functions.
    * it maintains output = fun(a,b) The difference with [[oscar.cbls.lib.invariant.logic.IntInt2Int]] is that this one performs the computation only after both variables have been updated.
    * @param a the first parameter of the function
    * @param b the second parameter of the function
    * @param fun the function to maintain, it is supposed not to listen to any variable in the model
    * @param domain the expected domain of the output
    * @author renaud.delandtsheer@cetic.be
    * */
  def lazyIntInt2Int(a:IntValue, b:IntValue, fun:((Long, Long) => Long), domain:Domain = fullRange)
  = new LazyIntInt2Int(a, b, fun, domain)
}


trait PivotInvariants{
  /** {i in index of values | values[i] <= boundary}
    * It is based on two heap data structure, hence updates are log(n) and all updates are allowed
    * @param values an array of intvar
    * @param boundary the boundary for comparison
    */
  def selectLEHeapHeap(values:Array[IntValue], boundary: IntValue) = SelectLEHeapHeap(values:Array[IntValue], boundary: IntValue)


  /**{i \in index of values | values[i] <= boundary}
    * It is based on a queue for the values above the boundary, hence all updates must be accepted by this scheme:
     - SelectLESetQueue does not allow boundary to decrease
     - SelectLESetQueue does not allow elements above boundary to change
     - SelectLESetQueue requires latest variables passing above boundary to be the biggest one
    * @param values: an array of intvar
    * @param boundary: the boundary for comparison
    */
  def selectLESetQueue(values:Array[IntValue], boundary: IntValue) = SelectLESetQueue(values, boundary)
}


/**
 * modeling interface presenting the complex logic invariants
 * @author renaud.delandtsheer@cetic.be
 */
trait SortInvariants{

  /**maintains a sorting of the ''values'' array:
    * @param ReversePerm   i < j => values(ReversePerm(i)) < values(ReversePerm(j))
    * see method GetForwardPerm() for the forward permutation: ReversePerm(ForwardPerm(i)) == i
    * */
  def sort(values:Array[IntValue], ReversePerm:Array[CBLSIntVar]) = new Sort(values, ReversePerm)

  /**returns the ForwardPerm for a given array
    * It instantiates an array of the appropriate size and populates it with IntVar.
    */
  def makeSort(values:Array[IntValue]) = Sort.MakeSort(values)
}

/**
 * modeling interface presenting the logic invariants
 * @author renaud.delandtsheer@cetic.be
 */
trait LogicInvariants extends
ElementInvariants
with ClusterInvariants
with CountInvariants
with RefInvariants
with CumulativeInvariants
with FilterInvariants
with HelperInvariants
with PivotInvariants
with SortInvariants

trait CountInvariants {
  /**
   * Maintains a count of the indexes of array: count(j) = #{i in index of values | values[i] == j}
   * This is considered as a dense count because counts is an array and must cover all the possibles values of the values in the array ''values''
   **/
  def denseCount(values : Array[IntValue], counts : Array[CBLSIntVar]) = DenseCount(values, counts)

  def makeDenseCount(vars: Array[IntValue]):DenseCount = DenseCount.makeDenseCount(vars)

  /**
   * Author: Jean-Noël Monette
   */
  def sparseCount(values: Array[IntValue], counts: Map[Long,CBLSIntVar]) = SparseCount(values, counts)
}

trait SeqInvariants {
  /**
   * builds a changing seq value that is maintained as the concatenation of a and b.
   * if either of them is a constant, the instantiated class is adapted
   *
   * @param a the first sequence
   * @param b the second sequence
   * @param maxPivotPerValuePercent the maxPErcentage for the created variable
   * @param maxHistorySize the max history size for the create variable
   * @return a changing seq value that is maintained as teh concatenation of a and b
   */
  def concatenate(a : SeqValue, b : SeqValue, maxPivotPerValuePercent : Long = 4L, maxHistorySize : Long = 20L) : SeqValue = {
    (a, b) match {
      case (ac : CBLSSeqConst, bc : CBLSSeqConst) =>
        CBLSSeqConst(IntSequence(ac.value ++ bc.value))
      case (ac : CBLSSeqConst, bv : ChangingSeqValue) =>
        new ConcatenateFirstConstant(ac.value.toList, bv, maxPivotPerValuePercent, maxHistorySize)
      case (av : ChangingSeqValue, bc : CBLSSeqConst) =>
        new ConcatenateSecondConstant(av, bc.value.toList, maxPivotPerValuePercent, maxHistorySize)
      case (av : ChangingSeqValue, bv : ChangingSeqValue) =>
        new Concatenate(av, bv, maxPivotPerValuePercent, maxHistorySize)
    }
  }

  /**
   * content of v
   * @param v is a SeqValue, the values appearing in the sequence
   * @author renaud.delandtsheer@cetic.be
   */
  def content(v : SeqValue) = Content(v)

  /**
   * maintains this as the flipped value of v
   * @param v
   * @param maxPivotPerValuePercent
   * @param maxHistorySize
   */
  def flipSequence(v : SeqValue, maxPivotPerValuePercent : Long = 10L, maxHistorySize : Long = 10L) =
    Flip(v : SeqValue, maxPivotPerValuePercent, maxHistorySize)

  /**
   * #(v) (cardinality, or length (since a SeqValue can only contain at most one instance of any Long value)
   * @param v is a SeqValue, containing a number of values, to count
   * @author renaud.delandtsheer@cetic.be
   */
  def length(v : SeqValue) = Length(v)

  /**
   * @param seq a sequence of integers
   * @param transform a function to apply to each value occuring in the sequence (it cannot be modified after this call)
   * @return a sequence where the value at any position p is equal to transform(seq(p))
   */
  def map(seq : ChangingSeqValue, transform : Long => Long, maxTransform : Long) =
    new MapConstantFun(seq : ChangingSeqValue, transform : Long => Long, maxTransform : Long)

  /**
   * @param seq a sequence of integers
   * @param mapArray an array that is taken as a function The value in this array an be variable that change value (althoug hthe content of the array cannot change after this call)
   * @return a sequence where the value at any position p is equal to mapArray(seq(p)).value
   */
  def map(seq : ChangingSeqValue, mapArray : Array[IntValue]) =
    new MapThroughArray(seq : ChangingSeqValue, mapArray)

  /**
   * the position of value a in sequence v; default if not in the sequence
   * @param v is a SeqValue
   * @param a is the value that is to locate in the sequence
   */
  def occurrencesOf(v : SeqValue, a : IntValue) = OccurrencesOf(v : SeqValue, a : IntValue)

  /**
   * Maintains the position of value of variable a in the sequence v.
   * @param v a sequence
   * @param a an intValue, which can be a CBLSIntVar for instance
   * @return a ChangingSetValue that is maintained as the set of position in v where the value is the one of a
   */
  def positionsOf(v : SeqValue, a : IntValue) = new PositionsOf(v, a)

  /**
   * Maintains the position of value of variable a in the sequence v.
   * @param v a sequence
   * @param a an integer
   * @return a ChangingSetValue that is maintained as the set of position in v where the value is a
   */
  def positionsOf(v : SeqValue, a : Long) = new PositionsOfConst(v, a)

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
  def precedence(seq : ChangingSeqValue, beforeAfter : List[(Long, Long)]) =
    Precedence(seq, beforeAfter)


  /**
   * sum(f(v))
   * the sum of all element in v after passing through f;
   * if a value has multiple occurrences, their f-transformed occurrences are summed
   * @param v is a SeqValue, containing a number of values, to sum
   * @param f is a function that is applied to every value in f prior to the sum
   * @author renaud.delandtsheer@cetic.be
   */
  def seqSum(v : SeqValue, f : (Long => Long) = (a : Long) => a) = SeqSum(v, f)

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
  def sortSequence(v : SeqValue, sortValue : Long => Long, orderName : String = "order")
  = SortSequence(v, sortValue, orderName)


  /**
   * Maintains and array telling, for each value (indice of the array) the set of value that can succeed it in the sequence.
   * There are multiple successors although we only consider the next value
   * because a value can appear several time in the sequence, it can therefore have several successors.
   *
   * @param seq
   * @return the array of SetVar that mention the set of successor for each possible value.
   * @author renaud.delandtsheer@cetic.be
   */
  def successors(seq : ChangingSeqValue) : Array[CBLSSetVar] = Successors(seq : ChangingSeqValue)
}


/**
 * modeling interface presenting the numeric invariants
 * @author renaud.delandtsheer@cetic.be
 */
trait NumericInvariants{

  /** sum(i in cond) vars(i)
    * @param vars is an array of IntVars
    * @param cond is the condition for selecting variables in the array of summed ones, cannot be null
    * @author renaud.delandtsheer@cetic.be
    * */
  //def sumConstants(vars: Array[Long], cond: SetValue) = SumConstants(vars, cond)

  val sum = oscar.cbls.lib.invariant.numeric.Sum

  /** prod(vars)
    * @param vars is a set of IntVars
    * */
  def prod(vars:Iterable[IntValue]) = Prod(vars:Iterable[IntValue])

  /** left - right
    * where left, right, and output are IntVar*/
  def minus(left:IntValue, right:IntValue) = Minus(left:IntValue, right:IntValue)

  /** left + right
    * where left, right, and output are IntVar*/
  def sum2(left:IntValue, right:IntValue) = Sum2(left:IntValue, right:IntValue)

  /** left * right
    * where left, right, and output are IntVar*/
  def prod2(left:IntValue, right:IntValue) = Prod2(left:IntValue, right:IntValue)

  /**left / right
    * where left, right, and output are IntVar
    * do not set right to zero, as usual... */
  def div(left:IntValue, right:IntValue) = Div(left:IntValue, right:IntValue)

  /**left / right
    * where left, right, and output are IntVar
    * do not set right to zero, as usual... */
  def mod(left:IntValue, right:IntValue) = Mod(left:IntValue, right:IntValue)

  /**abs(v) (absolute value)
    * where output and v are IntVar*/
  def abs(v:IntValue) = Abs(v:IntValue)


  /**
   * This invariant implements the identity function within the min-max range.
   * values lower tham min result to min
   * values higher tham max result to max
   * @author renaud.delandtsheer@cetic.be
   * @param x
   * @param min
   * @param max
   */
  def  bound(x: IntValue, min:Long, max:Long) = Bound(x, min, max)

  /**Maintains output to the smallest value such that
    * output >= from
    * (output - shift) MOD period > zone
    * (output - shift + length) MOD period > zone
    * of course, it is required that length is < period - zone, and exception is thrown otherwise.
    *
    * For instance, suppose that some task can only happen during open day (Mon-Fri),
    * let 'from" being the lowest starting date, and 'length' its duration.
    * the invariant will check that the task can be finished by friday of the week, and if not,
    * will propose the next monday. 'shift' specifies says what is the starting day at zero.
    * zone is the forbidden zone. it starts at the beginning of the cycle.
    *
    * for instance, suppose you represent days starting from zero, and zero is a monday,
    * and you want to round up to the next open day (sa and su are closed day, the correct declaration is:
    * RoundUpModulo(from,duration,7L,2L,5L)
    *
    * @param from the starting date of the task. it can start later.
    * @param duration the duration of the task.
    * @param period the period of the forbidden-allowed pattern
    * @param zone the size of the forbidden zone. it starts at the beginning of the period
    * @param shift the first period starts later than zero. it starts at shift. the duration before its start is allowed.
    */

  def roundUpModulo(from: IntValue, duration: IntValue, period: Long, zone: Long, shift: Long) = RoundUpModulo(from: IntValue, duration: IntValue, period: Long, zone: Long, shift: Long)

  /**Maintains output to the smallest value such that
    * output >= from
    * the interval [output ; output + length] does not overlap with the intervals given in FobiddenZones
    *
    * @param from
    * @param duration
    * @param ForbiddenZones
    */
  def roundUpCustom(from: IntValue, duration: IntValue, ForbiddenZones: List[(Long, Long)]) = RoundUpCustom(from: IntValue, duration: IntValue, ForbiddenZones: List[(Long, Long)])

  /**
   * This invariant implements a step function. Values higher than pivot are mapped to ifval
   * values lower or equal to pivot are mapped to elseval
   * This invariant was suggested by Jean-Noël Monette
   *
   * @param x the IntVar parameter of the invariant
   * @param pivot the pivot value
   * @param thenval the value returned when x > pivot
   * @param elseval the value returned when x <= pivot
   */
  def step(x:IntValue,pivot:Long = 0L,thenval:Long = 1L,elseval:Long = 0L) = Step(x:IntValue,pivot:Long,thenval:Long ,elseval:Long)

  /** sum(i in cond) vars(i)
    * This invariant might modify vars array by cloning some variables to ensure that each variable only appears once.
    * @param vars is a set of IntVars
    * @param cond is the condition for selecting variables in the set of summed ones, cannot be null
    */
  def sumElements(vars: Array[IntValue], cond: SetValue) = SumElements(vars: Array[IntValue], cond: SetValue)

  /** prod(i in cond) vars(i)
    * This invariant might modify vars array by cloning some variables to ensure that each variable only appears once.
    * @param vars is a set of IntVars
    * @param cond is the condition for selecting variables in the set of summed ones.
    */
  def prodElements(vars: Array[IntValue], cond: SetValue) = ProdElements(vars: Array[IntValue], cond: SetValue)

}


/**
 * modeling interface presenting the min-max invariants
 * @author renaud.delandtsheer@cetic.be
 */
trait MinMaxInvariants{

  /** Maintains {i in indices of (vars Inter cond) | vars[i] == max(vars(i in indices of (vars Inter cond))}
    * @param vars is an array of IntVar, which can be bulked
    * @param cond is the condition, supposed fully acceptant if not specified
    * @param default is the value returned when cond is empty
    * update is O(log(n))
    * */
  def argMax(vars: Array[IntValue], cond: SetValue = null,default:Long = Long.MinValue) = ArgMax(vars, cond,default)


  /** Maintains {i in indices of (varss Inter cond) | varss[i] == min(varss(i in indices of (varss Inter cond))}
    * @param varss is an array of IntVar, which can be bulked
    * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
    * @param default is the value returned when cond is empty
    * update is O(log(n))
    * */
  def argMin(varss: Array[IntValue], ccond: SetValue = null, default:Long = Long.MaxValue) = ArgMin(varss, ccond, default)

  /** maintains output = Max(a,b)
    * where output, a, and b are an IntVar
    * use this if you only have two variables to max, otherwise, refer to log iplementations
    * */
  def max2(a: IntValue, b: IntValue) = Max2(a, b)

  /** maintains output = Min(a,b)
    * where output, a, and b are an IntVar
    * use this if you only have two variables to max, otherwise, refer to log iplementations
    * */
  def min2(a: IntValue, b: IntValue) = Min2(a: IntValue, b: IntValue)

  /** Maintains Max(Var(i) | i in cond)
    * @param varss is an array of IntVar, which can be bulked
    * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
    * update is O(log(n))
    * */
  def maxNaive(varss: Array[IntValue], ccond: SetValue = null, default: Long = Long.MinValue) = MaxArray(varss, ccond, default)

  /** Maintains Min(Var(i) | i in cond)
    * @param varss is an array of IntVar, which can be bulked
    * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
    * update is O(log(n))
    * */
  def minNaive(varss: Array[IntValue], ccond: SetValue = null, default: Long = Long.MaxValue) = MinArray(varss, ccond, default)

  /** maintains output = Min(v)
    * where
    * * output is an IntVar
    * * v is an IntSetVar
    * @param default is the default value if v is empty
    * update is O(log(n))
    * */
  def minSet(v: SetValue, default: Long = Long.MaxValue) = MinSet(v, default)

  /** maintains output = Max(v)
    * where
    * * output is an IntVar
    * * v is an IntSetVar
    * @param default is the default value if v is empty
    * update is O(log(n))
    * */
  def maxSet(v: SetValue, default: Long = Long.MinValue) = MaxSet(v, default)


  /**
   * Maintains Max(Var(i) | i in cond)
   * @param varss is an array of IntVar, which can be bulked
   * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
   * update is O(log(n))
   * @author renaud.delandtsheer@cetic.be
   * */
  def maxConstArray(varss: Array[Long], ccond: SetValue, default: Long = Long.MinValue) = MaxConstArray(varss, ccond, default)

  /**
   * Maintains Min(Var(i) | i in cond)
   * @param varss is an array of Long
   * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
   * update is O(log(n))
   * @author renaud.delandtsheer@cetic.be
   * */
  def minConstArray(varss: Array[Long], ccond: SetValue, default: Long = Long.MaxValue) = MinConstArray(varss, ccond, default)

  /**
   * Maintains Max(Var(i) | i in cond)
   * this is a variant that is lazy, and maintains a TODO-list of postponed updates.
   * postponed updates are ones that do not impact on the outout of the invariant.
   * when there is an update, it is first checked against the TODO-list, for cancellation.
   * if the update does not impact the output, it is postponed
   * if it affects the output, it is performed
   * @param varss is an array of IntVar, which can be bulked
   * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
   * @param default the value if ccond is empty
   * @param maxBackLogSize is the maximal number of postponed updates (TODOlist is handled as a FIFO)
   * update is O(log(n)), faster (O(1L) if you do updates and backtracks
   * @author renaud.delandtsheer@cetic.be
   * */
  def maxConstArrayLazy(varss: Array[Long], ccond: SetValue, default: Long = Long.MaxValue, maxBackLogSize:Long = 10L) =
    MaxConstArrayLazy(varss, ccond, default, maxBackLogSize)


  /**
   * Maintains Min(Var(i) | i in cond)
   * this is a variant that is lazy, and maintains a TODO-list of postponed updates.
   * postponed updates are ones that do not impact on the outout of the invariant.
   * when there is an update, it is first checked against the TODO-list, for cancellation.
   * if the update does not impact the output, it is postponed
   * if it affects the output, it is performed
   * @param varss is an array of Long
   * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
   * @param default the value if ccond is empty
   * @param maxBackLogSize is the maximal number of postponed updates (TODOlist is handled as a FIFO)
   * update is O(log(n)), faster (O(1L) if you do updates and backtracks
   * @author renaud.delandtsheer@cetic.be
   * */
  def minConstArrayLazy(varss: Array[Long], ccond: SetValue, default: Long = Long.MaxValue, maxBackLogSize:Long = Long.MaxValue)
  = MinConstArrayLazy(varss, ccond, default, maxBackLogSize)

  /**
    * Maintains Min(constArray(i) | i in condSet), default if condSet is empty
    * This invariant is value-wise, so it will tell the setVar to notify it only about a subset of the variable, and the setVar uses a smart pre-filter mechanism.
    * So this invariant is very efficient when you have a LOT of them, all listening to the same setVar, with different arrays in parameters.
   * @param constArray
   * @param condSet
   * @param default
   * @param maxDiameter is the maximal number of values in condSet that are monitored in the set, must be >=1.
   *                    the actual diameter is kept between 1L and tis value, lazily
   */
  def minConstArrayValueWise(constArray: Array[Long], condSet: SetValue, default: Long, maxDiameter:Long = 2L) =
    new MinConstArrayValueWise(constArray, condSet, default, maxDiameter)
}
