/**
 * *****************************************************************************
 * This file is part of OscaR (Scala in OR).
 *
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 * ****************************************************************************
 */

package oscar.cbls.modeling

import oscar.cbls.invariants.lib.logic.SparseCluster._
import oscar.cbls.invariants.lib.logic.DenseCluster._
import oscar.cbls.invariants.lib.logic._
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.lib.minmax._
import oscar.cbls.invariants.lib.numeric._
import collection.immutable.{SortedSet, SortedMap}
import oscar.cbls.invariants.lib.set._

trait Invariants extends ClusterInvariants
with ComplexLogicInvariants
with AccessInvariants
with MinMaxInvariants
with NumericInvariants
with SetInvariants

trait ClusterInvariants{

  def makeSparseCluster(values:Array[IntVar], clusters: Iterable[Int]):SparseCluster = Cluster.MakeSparse(values, clusters)

  def makeDenseCluster(values:Array[IntVar]):DenseCluster = Cluster.MakeDense(values)

  def makeDenseClusterAssumingMinMax(values:Array[IntVar],themin:Int,themax:Int):DenseCluster = Cluster.MakeDenseAssumingMinMax(values,themin,themax)

  /**maintains a cluster of the indexes of array:  cluster(j) = {i in index of values | values[i] == j}
   * This is considered as a sparse cluster because Cluster is a map and must not cover all possibles values of the values in the array ''values''
   * */
  def sparseCluster(values:Array[IntVar], Clusters:SortedMap[Int,IntSetVar]) = SparseCluster(values:Array[IntVar], Clusters:SortedMap[Int,IntSetVar])

  /**Maintains a cluster of the indexes of array: cluster(j) = {i in index of values | values[i] == j}
   * This is considered as a dense cluster because Cluster is an array and must cover all the possibles values of the values in the array ''values''
   * */
  def denseCluster(values:Array[IntVar], clusters:Array[IntSetVar]) = DenseCluster(values:Array[IntVar], clusters:Array[IntSetVar])

  /**Maintains a count of the indexes of array: count(j) = #{i in index of values | values[i] == j}
   * This is considered as a dense count because counts is an array and must cover all the possibles values of the values in the array ''values''
   * */
  def denseCount(values:Array[IntVar], counts:Array[IntVar]) = DenseCount(values:Array[IntVar], counts:Array[IntVar])

  /**maintains the reverse references. Referencing(i) = {j | Reference(j) includes i}
   * */
  def denseRef(references:Array[IntSetVar], referencing:Array[IntSetVar]) = DenseRef(references:Array[IntSetVar], referencing:Array[IntSetVar])

  /**
   * Maintains a resource usage profile.
   * @param indices the indices of tasks
   * @param start the start time of tasks
   * @param duration the duration of tasks
   * @param amount the amount that tasks use of this resource
   * @param profile the usage profile of the resource maintained to profile(time) <== sum(task.amount | task.start <= time <= t.start+t.duration)
   * @param active the tasks that are active maintained to active(time) <== (task.indices | task.start <= time <= t.start+t.duration)
   */
  def cumulative(indices:Array[Int], start:Array[IntVar], duration:Array[IntVar], amount:Array[IntVar], profile:Array[IntVar], active:Array[IntSetVar])  =
    Cumulative(indices:Array[Int], start:Array[IntVar], duration:Array[IntVar], amount:Array[IntVar], profile:Array[IntVar], active:Array[IntSetVar])


  /** { i in index(values) | cond(values[i] }
   * @param values is an array of IntVar
   * @param cond is a function that selects values to be includes in the output set.
   * This ''cond'' function cannot depend on any IntVar, as updates to these IntVars will not trigger propagation of this invariant.
   */
  def filter(values:Array[IntVar], cond:(Int=>Boolean)) = Filter(values:Array[IntVar], cond:(Int=>Boolean))

  /** {i in index of values | values[i] <= boundary}
   * It is based on two heap data structure, hence updates are log(n) and all updates are allowed
   * @param values an array of intvar
   * @param boundary the boundary for comparison
   */
  def selectLEHeapHeap(values:Array[IntVar], boundary: IntVar) = SelectLEHeapHeap(values:Array[IntVar], boundary: IntVar)


  /**{i \in index of values | values[i] <= boundary}
   * It is based on a queue for the values above the boundary, hence all updates must be accepted by this scheme:
 - SelectLESetQueue does not allow boundary to decrease
 - SelectLESetQueue does not allow elements above boundary to change
 - SelectLESetQueue requires latest variables passing above boundary to be the biggest one
   * @param values: an array of intvar
   * @param boundary: the boundary for comparison
   */
  def selectLESetQueue(values:Array[IntVar], boundary: IntVar) = SelectLESetQueue(values:Array[IntVar], boundary: IntVar)

}

trait ComplexLogicInvariants{

  /**this invariants maintains data structures representing vrp of vehicles.
   * for use in TSP, VRP, etc.
   * arrays start at one until N
   * position 0 is to denote an unrouted node.
   * The nodes from 1 to V are the starting points of vehicles.
   *
   * @param V the number of vrp to consider V>=1 and V<=N
   */
  def routes(V: Int, Next:Array[IntVar]) = Routes.buildRoutes(Next, V)

  /**maintains a sorting of the ''values'' array:
   * @param ReversePerm   i < j => values(ReversePerm(i)) < values(ReversePerm(j))
   * see method GetForwardPerm() for the forward permutation: ReversePerm(ForwardPerm(i)) == i
   * */
  def sort(values:Array[IntVar], ReversePerm:Array[IntVar]) = Sort(values:Array[IntVar], ReversePerm:Array[IntVar])

  /**returns the ForwardPerm for a given array
   * It instantiates an array of the appropriate size and populates it with IntVar.
   */
  def makeSort(values:Array[IntVar]):Sort = Sort.MakeSort(values:Array[IntVar])
}

trait AccessInvariants{
  /** if (ifVar >0) then thenVar else elveVar
   * @param ifVar the condition (IntVar)
   * @param thenVar the returned value if ifVar > 0
   * @param elseVar the returned value if ifVar <= 0
   * */
  def intITE(ifVar:IntVar, thenVar:IntVar, elseVar:IntVar) = IntITE(ifVar, thenVar, elseVar)

  /** inputarray[index]
   * @param inputarray is an array of IntVar
   * @param index is the index accessing the array*/
  def intElement(index:IntVar, inputarray:Array[IntVar]) = IntElement(index:IntVar, inputarray:Array[IntVar])

  /**Union(i in index) (array[i])
   * @param index is an IntSetVar denoting the set of positions in the array to consider
   * @param inputarray is the array of intvar that can be selected by the index
   */
  def intElements(index:IntSetVar, inputarray:Array[IntVar]) = IntElements(index, inputarray)

  /** inputarray[index] on an array of IntSetVar
   * @param inputarray is the array of intsetvar
   * @param index is the index of the array access
   **/
  def intSetElement(index:IntVar, inputarray:Array[IntSetVar]) = IntSetElement(index, inputarray)
}

trait MinMaxInvariants{

  /** Maintains {i in indices of (vars Inter cond) | vars[i] == max(vars(i in indices of (vars Inter cond))}
   * @param vars is an array of IntVar, which can be bulked
   * @param cond is the condition, supposed fully acceptant if not specified
   * @param default is the value returned when cond is empty
   * update is O(log(n))
   * */
  def argMax(vars: Array[IntVar], cond: IntSetVar = null,default:Int = Int.MinValue) = ArgMaxArray(vars, cond,default)


  /** Maintains {i in indices of (varss Inter cond) | varss[i] == min(varss(i in indices of (varss Inter cond))}
   * @param varss is an array of IntVar, which can be bulked
   * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
   * @param default is the value returned when cond is empty
   * update is O(log(n))
   * */
  def argMin(varss: Array[IntVar], ccond: IntSetVar = null, default:Int = Int.MaxValue) = ArgMinArray(varss, ccond, default)

  /** maintains output = Max(a,b)
   * where output, a, and b are an IntVar
   * use this if you only have two variables to max, otherwise, refer to log iplementations
   * */
  def max2(a: IntVar, b: IntVar) = Max2(a, b)

  /** maintains output = Min(a,b)
   * where output, a, and b are an IntVar
   * use this if you only have two variables to max, otherwise, refer to log iplementations
   * */
  def min2(a: IntVar, b: IntVar) = Min2(a: IntVar, b: IntVar)

  /** Maintains Max(Var(i) | i in cond)
   * @param varss is an array of IntVar, which can be bulked
   * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
   * update is O(log(n))
   * */
  def maxArray(varss: Array[IntVar], ccond: IntSetVar = null, default: Int = Int.MinValue) = MaxArray(varss, ccond, default)

  /** Maintains Min(Var(i) | i in cond)
   * @param varss is an array of IntVar, which can be bulked
   * @param ccond is the condition, supposed fully acceptant if not specified (must be specified if varss is bulked)
   * update is O(log(n))
   * */
  def minArray(varss: Array[IntVar], ccond: IntSetVar = null, default: Int = Int.MaxValue) = MinArray(varss, ccond, default)

  /** maintains output = Min(v)
   * where
   * * output is an IntVar
   * * v is an IntSetVar
   * @param default is the default value if v is empty
   * update is O(log(n))
   * */
  def minSet(v: IntSetVar, default: Int = Int.MaxValue) = MinSet(v, default)

  /** maintains output = Max(v)
   * where
   * * output is an IntVar
   * * v is an IntSetVar
   * @param default is the default value if v is empty
   * update is O(log(n))
   * */
  def maxSet(v: IntSetVar, default: Int = Int.MinValue) = new MaxSet(v, default)
}

trait NumericInvariants{
  /** sum(vars)
   * @param vars is an iterable of IntVars
   * */
  def sum(vars:Iterable[IntVar]) = Sum(vars:Iterable[IntVar])

  /** prod(vars)
   * @param vars is a set of IntVars
   * */
  def prod(vars:Iterable[IntVar]) = Prod(vars:Iterable[IntVar])

  /** left - right
   * where left, right, and output are IntVar*/
  def minus(left:IntVar, right:IntVar) = Minus(left:IntVar, right:IntVar)

  /** left + right
   * where left, right, and output are IntVar*/
  def sum2(left:IntVar, right:IntVar) = Sum2(left:IntVar, right:IntVar)

  /** left * right
   * where left, right, and output are IntVar*/
  def prod2(left:IntVar, right:IntVar) = Prod2(left:IntVar, right:IntVar)

  /**left / right
   * where left, right, and output are IntVar
   * do not set right to zero, as usual... */
  def div(left:IntVar, right:IntVar) = Div(left:IntVar, right:IntVar)

  /**left / right
   * where left, right, and output are IntVar
   * do not set right to zero, as usual... */
  def mod(left:IntVar, right:IntVar) = Mod(left:IntVar, right:IntVar)

  /**abs(v) (absolute value)
   * where output and v are IntVar*/
  def abs(v:IntVar) = Abs(v:IntVar)

  /**Maintains output to the smallest value such that
   * output >= from
   * (output - shift) MOD period > zone
   * (output - shift + length) MOD period > zone
   * of course, it is required that length is < period - zone, and exception is thrown otherwise.
   */
  def roundUpModulo(from: IntVar, length: IntVar, period: Int, zone: Int, shift: Int) = RoundUpModulo(from: IntVar, length: IntVar, period: Int, zone: Int, shift: Int)

  def roundUpCustom(from: IntVar, length: IntVar, Zone: List[(Int, Int)]) = RoundUpCustom(from: IntVar, length: IntVar, Zone: List[(Int, Int)])

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
  def step(x:IntVar,pivot:Int = 0,thenval:Int = 1,elseval:Int = 0) = Step(x:IntVar,pivot:Int,thenval:Int ,elseval:Int)

  /** sum(i in cond) vars(i)
   * This invariant might modify vars array by cloning some variables to ensure that each variable only appears once.
   * @param vars is a set of IntVars
   * @param cond is the condition for selecting variables in the set of summed ones, cannot be null
   */
  def sumElements(vars: Array[IntVar], cond: IntSetVar) = SumElements(vars: Array[IntVar], cond: IntSetVar)

  /** prod(i in cond) vars(i)
   * This invariant might modify vars array by cloning some variables to ensure that each variable only appears once.
   * @param vars is a set of IntVars
   * @param cond is the condition for selecting variables in the set of summed ones.
   */
  def prodElements(vars: Array[IntVar], cond: IntSetVar) = ProdElements(vars: Array[IntVar], cond: IntSetVar)

}

trait SetInvariants{
  /** left UNION right
   * @param left is an intvarset
   * @param right is an intvarset
   * */
  def union(left:IntSetVar, right:IntSetVar) = Union(left:IntSetVar, right:IntSetVar)

  /** left INTER right
   * @param left is an intvarset
   * @param right is an intvarset
   * */
  def inter(left:IntSetVar, right:IntSetVar) = Inter(left:IntSetVar, right:IntSetVar)

  /** left MINUS right, the set diff operator
   * @param left is the base set
   * @param right is the set that is removed from left
   * */
  def diff(left:IntSetVar, right:IntSetVar) = Diff(left:IntSetVar, right:IntSetVar)

  /** #(v) (cardinality)
   * @param v is an IntSetVar, the set of integers to count
   */
  def cardinality(v:IntSetVar) = Cardinality(v:IntSetVar)

  /** makes an IntSetVar out of a set of IntVar. If several variables have the same value, the value is present only once in the resulting set
   * @param on is a set of IntVar
   * */
  def makeSet(on:SortedSet[IntVar]) = MakeSet(on:SortedSet[IntVar])

  /** makes a set out of an interval specified by a lower bound and an upper bound. if lb > ub, the set is empty.
   * @param lb is the lower bound of the interval
   * @param ub is the upper bound of the interval
   * */
  def interval(lb:IntVar,ub:IntVar) = Interval(lb:IntVar,ub:IntVar)

  /**maintains the output as any value taken from the intset var parameter.
   * if this set is empty, puts the default value ni output.
   * @param from
   * @param default
   */
  def takeAny(from:IntSetVar,  default:Int) = TakeAny(from:IntSetVar,  default:Int)

  /** Sum(i in on)(fun(i))
   * @param on is the set of integers to add
   * @param fun is an optional function Int -> Int to apply before summing elements. It is expected not to rely on any variable of the model.
   * */
  def setSum(on:IntSetVar, fun:(Int => Int) = ((a:Int) => a)) = SetSum(on, fun)

  /** PRod(i in on)(fun(i))
   * @param on is the set of integers to multiply
   * @param fun is an optional function Int -> Int to apply before multiplying elements. It is expected not to rely on any variable of the model.
   * */
  def setProd(on:IntSetVar, fun:(Int => Int) = ((a:Int) => a)) = SetProd(on, fun)
}
