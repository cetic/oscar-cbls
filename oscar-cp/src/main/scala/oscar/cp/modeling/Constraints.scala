/** *****************************************************************************
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
  * *****************************************************************************/

package oscar.cp.modeling

import java.security.InvalidParameterException

import oscar.cp._
import oscar.cp.constraints._
import oscar.cp.constraints.tables.BasicSmartTableAlgo._
import oscar.cp.constraints.tables.NegativeShortTableAlgo.NegativeShortTableAlgo
import oscar.cp.constraints.tables.NegativeTableAlgo._
import oscar.cp.constraints.tables.ShortTableAlgo._
import oscar.cp.constraints.tables.TableAlgo._
import oscar.cp.constraints.tables.{BasicSmartElement, BasicSmartTableAlgo, NegativeShortTableAlgo, ShortTableAlgo}
import oscar.cp.core.variables.{CPIntVarViewMinus, CPIntVarViewOffset, CPIntVarViewTimes}
import oscar.cp.core.{CPPropagStrength, Constraint}
import oscar.cp.scheduling.constraints.{DisjunctiveWithTransitionTimes, UnaryResource, _}

import scala.collection.mutable.ArrayBuffer

trait Constraints {

  /**
    * @param x the variable on which the view will be created
    * @return a variable in the same store representing: - x
    */
  def opposite(x: CPIntVar): CPIntVar = {
    new CPIntVarViewMinus(x)
  }

  /**
    * @param x the variable on which the view will be created
    * @param d the shift applied to x to obtain the view
    * @return a variable in the same store representing: x - d
    */
  def minus(x: CPIntVar, d: Int): CPIntVar = {
    if (d == 0) x
    else new CPIntVarViewOffset(x, -d)
  }

  /**
    * @param x a variable in the same store as y
    * @param y a variable in the same store as x
    * @return a variable in the same store representing: x - y
    */
  def minus(x: CPIntVar, y: CPIntVar): CPIntVar = {
    val c = CPIntVar(x.min - y.max, x.max - y.min)(x.store)
    x.store.post(new oscar.cp.constraints.BinarySum(c, y, x))
    c
  }

  /**
    * @param x the variable on which the view will be created
    * @param d the shift applied to x to obtain the view
    * @return a variable in the same store representing: x + d
    */
  def plus(x: CPIntVar, d: Int): CPIntVar = {
    if (d == 0) x
    else new CPIntVarViewOffset(x, d)
  }

  /**
    * @param x a variable in the same store as y
    * @param y a variable in the same store as x
    * @return a variable in the same store representing: x + y
    */
  def plus(x: CPIntVar, y: CPIntVar): CPIntVar = {
    if (y.isBound) plus(x, y.value)
    else {
      val c = CPIntVar(x.min + y.min, x.max + y.max)(x.store)
      x.store.post(new oscar.cp.constraints.BinarySum(x, y, c))
      c
    }
  }

  /**
    * @param x the variable on which the view will be created
    * @param c the multiplicand applied to x to obtain the view
    * @return a variable in the same store representing: x * c
    */
  def mul(x: CPIntVar, c: Int): CPIntVar = {
    if (c == 0) CPIntVar(0)(x.store)
    else if (c == 1) x
    else if (c > 0) new CPIntVarViewTimes(x, c)
    else new CPIntVarViewMinus(mul(x, -c))
  }

  /**
    * @param x a variable in the same store as y
    * @param y a variable in the same store as x
    * @return a variable in the same store representing: x * y
    */
  def mul(x: CPIntVar, y: CPIntVar): CPIntVar = {
    val a = x.min
    val b = x.max
    val c = y.min
    val d = y.max
    import oscar.cp.util.NumberUtils
    val t = Array(NumberUtils.safeMul(a, c), NumberUtils.safeMul(a, d), NumberUtils.safeMul(b, c), NumberUtils.safeMul(b, d))
    val z = CPIntVar(t.min, t.max)(x.store)
    x.store.post(new oscar.cp.constraints.MulVar(x, y, z))
    z
  }

  // TODO: general case for multiplication by CPIntVar

  /**
    * @param x the variable on which the view will be created
    * @return a variable in the same store representing: |x|
    */
  def absolute(x: CPIntVar): CPIntVar = {
    val c = CPIntVar(0, Math.max(Math.abs(x.min), Math.abs(x.max)))(x.store)
    x.store.post(new oscar.cp.constraints.Abs(x, c))
    c
  }

  /**
    * Bin-Packing Constraint linking the placement variables of sized items into bins with
    * the total size of the bins
    *
    * @param x with x(i) is the bin where the item i is placed
    * @param w with w(i) is the size of item i
    * @param l with l(j) is the load of bin j
    * @return a binpacking constraint linking the variables in argument such that l[i] == sum_j w[j]*(x[j]==i) for all bins i
    */
  def binPacking(x: IndexedSeq[CPIntVar], w: IndexedSeq[Int], l: IndexedSeq[CPIntVar]): Constraint = {
    new BinPacking(x.toArray, w.toArray, l.toArray)
  }


  /**
    * Bin-Packing Constraint linking the placement variables of sized items into bins with
    * the total size of the bins
    *
    * @param x with x(i) is the bin where the item i is placed
    * @param w with w(i) is the size of item i
    * @param l with l(j) is the load of bin j
    * @param c with c(j) is the cardinality of bin j (number of items)
    * @return a binpacking constraint linking the variables in argument such that l[i] == sum_j w[j]*(x[j]==i) for all bins i and
    */
  def binPackingCardinality(x: IndexedSeq[CPIntVar], w: IndexedSeq[Int], l: IndexedSeq[CPIntVar], c: IndexedSeq[CPIntVar]): Constraint = {
    new BinPackingFlow(x.toArray, w.toArray, l.toArray, c.toArray)
  }

  /**
    * Binary-Knapsack Constraint computing the total weight of items placed into a knapsack
    *
    * @param x with x(i) == 1 if the item is selected, 0 otherwise
    * @param w with w(i) is the weight of item i
    * @param W variable representing the load of the knapsack
    * @return a binary-knapsack constraint linking the variables in argument such that W == sum_j w[j]*x[i]
    */
  def binaryKnapsack(x: IndexedSeq[CPBoolVar], w: IndexedSeq[Int], W: CPIntVar): Constraint = {
    new BinaryKnapsack(x.toArray, w.toArray, W)
  }

  /**
    * Binary-Knapsack Constraint computing the total weight of items placed into a knapsack
    *
    * @param x with x(i) == 1 if the item is selected, 0 otherwise
    * @param w with w(i) is the weight of item i
    * @param W the load of the knapsack
    * @return a binary-knapsack constraint linking the variables in argument such that W == sum_j w[j]*x[i]
    */
  def binaryKnapsack(x: IndexedSeq[CPBoolVar], w: IndexedSeq[Int], W: Int): Constraint = {
    new BinaryKnapsack(x.toArray, w.toArray, CPIntVar(W)(x.head.store))
  }

  /**
    * Binary-Knapsack Constraint computing the total profit of items placed into a capacitated knapsack
    *
    * @param x with x(i) == 1 if the item is selected, 0 otherwise
    * @param p with p(i) >= 0 is the profit you get selecting item i
    * @param w with w(i) > 0 is the weight of item i into the knapsack
    * @param P the total profit of the knapsack
    * @param W the total weight of the knapsack
    * @return a binary-knapsack constraint linking the variables in argument such that P == sum_j p[j]*x[i] and W == sum_j w[j]*x[i]
    */
  def binaryKnapsack(x: IndexedSeq[CPBoolVar], p: IndexedSeq[Int], w: IndexedSeq[Int], P: CPIntVar, W: CPIntVar): Knapsack = {
    new Knapsack(x.toArray, p.toArray, w.toArray, P, W)
  }

  /**
    * The atLeastNValue Constraint counts the number of different values.
    * The available filtering are Weak, Strong.
    * The constraint is called atLeastNValue because the upper-bound of nValue
    * is not used to prune the domains of variables in vars.
    * This constraint is typically used to maximize the number of different values
    * in an objective function.
    * It can be viewed as a soft/relaxed-version of an allDifferent contraint.
    * see: Specific filtering algorithms for over-constrained problems
    * Thierry Petit, Jean-Charles Régin, Christian Bessiere
    *
    * @param vars   an non empty collection of variables
    * @param nValue the number of different values in vars
    * @return a constraint ensuring that nValue is the number of different values in vars
    */
  def atLeastNValue(vars: Iterable[CPIntVar], nValue: CPIntVar): Constraint = {
    new AtLeastNValue(vars.toArray, nValue)
  }

  /**
    * allDifferent Constraint (Available Filtering: Weak, Medium, Strong)
    *
    * @param vars an non empty collection of variables
    * @return a constraint ensure that no value occurs more than once in vars
    */
  def allDifferent(vars: CPIntVar*): Constraint = {
    new AllDifferent(vars: _*)
  }

  /**
    * allDifferent Constraint (Available Filtering: Weak, Medium, Strong)
    *
    * @param vars an non empty collection of variables
    * @return a constraint ensure that no value occurs more than once in vars
    */
  def allDifferent(vars: Iterable[CPIntVar]): Constraint = {
    allDifferent(vars.toArray: _*)
  }

  /**
    * allDifferent Constraint (Available Filtering: Weak, Medium, Strong)
    *
    * @param variables a non empty array of variables
    * @return a constraint ensure that no value occurs more than once in variables
    */
  def allDifferent(variables: Array[CPIntVar]): Constraint = {
    new AllDifferent(variables: _*)
  }

  /**
    * soft version of the allDifferent Constraint (Available Filtering: Weak, Medium, Strong)
    *
    * @param variables  an non empty array of variables
    * @param violations a variable representing the number of violations
    * @return a constraint ensuring that violations is the number of times same values appear in variables
    */
  def softAllDifferent(variables: Array[CPIntVar], violations: CPIntVar): Constraint = {
    new SoftAllDifferent(variables, violations)
  }

  /**
    * notAllEqual Constraint
    *
    * @param variables a non empty array of variables
    * @return a constraint that ensures there is a least two values in 'variables' that are not equal (they are not all equal)
    */
  def notAllEqual(variables: Array[CPIntVar]): Constraint = {
    new NotAllEqual(variables)
  }

  /**
    * minAssignment Constraint (Available Filtering: Medium)
    *
    * @param x       an non empty array of variables
    * @param weights a n x n array (n = x.length)
    * @return a constraint ensure that allDifferent(x) and sum(i) weights(i)(x(i)) <= cost
    */
  def minAssignment(x: Array[CPIntVar], weights: Array[Array[Int]], cost: CPIntVar): MinAssignment = {
    new MinAssignment(x, weights, cost)
  }

  /**
    * Circuit Constraint (Available Filtering: Weak, Strong)
    *
    * @param succ      an array of n variable with domains defined on (0..n-1), where succ[i] represents the city visited after city i
    * @param symmetric should be set to true (default) to have a redundant model on predecessor array, false otherwise
    * @return a constraint enforcing a circuit representation where succ(i) represents
    *         the city visited after city i (no city is visited twice and there is no sub-tours)
    */
  def circuit(succ: Array[CPIntVar], symmetric: Boolean = true): Constraint = {
    if (succ.length < 0) sys.error("no variable.")
    else new Circuit(succ, symmetric)
  }

  /**
    * Ensures that succ represents a valid Hamiltonian circuit (only one tour) of length "cost" <br>
    * Available propagation strengths are Weak, Medium and Strong.
    * Weak = elements + circuit + alldiff (AC)
    * Medium = Weak + minAssignment
    * Strong = Medium + Held&Karp Lower-Bounds
    *
    * @param succ           an array of n variables with domains defined on (0..n-1), where succ[i] represents the city visited after city i
    * @param distMatrixSucc a distance matrix where distMatrix[i][j] is the distance for going from i to j
    * @param cost           variable representing the length of the Hamiltonian circuit
    * @param addPredModel   should be set to true (default) to have a redundant model on predecessor array, false otherwise
    * @return a constraint enforcing a circuit representation where succ(i) represents
    *         the city visited after city i (no city is visited twice and there is no sub-tours)
    *         (cost = sum_i distMatrix(i)(succ(i)))
    */
  def minCircuit(succ: Array[CPIntVar], distMatrixSucc: Array[Array[Int]], cost: CPIntVar, addPredModel: Boolean = true): Constraint = {
    new MinCircuit(succ, distMatrixSucc, cost, addPredModel)
  }

  /**
    * SubCircuit constraint (only one mode of filtering)
    * This constraint enforces `successors` to represent an Hamiltonian circuit on a subset of nodes.
    * A node that is not part of the circuit is its own successor:
    * succ(i) = j means that j is the successor of i and succ(i) = i means that i is not in the circuit.
    *
    * @param successors variables that must represent a sub circuit
    * @return a constraint enforcing `successors` to represent an Hamiltonian circuit on a subset of nodes
    */
  def subCircuit(successors: Array[CPIntVar]): Constraint = {
    SubCircuit(successors)
  }

  /**
    * Lexicographically Less or Equal Constraint
    *
    * @param x an non empty array of variables x1, x2, x3 ... xn
    * @param y an array y1, y2, y3 ... yn (of the same size as x)
    * @return a constraint enforcing x LexLeq y i.e. :
    *         x1 < y1 or (x2,x3,...,xn) LexLeq (y2,y3,...,yn)
    */
  def lexLeq(x: Array[CPIntVar], y: Array[CPIntVar]): Constraint = {
    new LexLeq(x, y)
  }

  /**
    * Element Constraint, indexing an array of integers by a variable
    *
    * @param tab an non empty array n integers
    * @param x   an index variable with domain defined on (0..n-1)
    * @return a variable z linked to tab and x by the relation tab(x) == z
    */
  def element(tab: IndexedSeq[Int], x: CPIntVar, strength: CPPropagStrength = CPPropagStrength.Medium): CPIntVar = {
    val minVal = tab.min
    val maxVal = tab.max
    val z = CPIntVar(minVal, maxVal)(x.store)
    x.store.post(new ElementCst(tab.toArray, x, z), strength)
    z
  }

  /**
    * Element Constraint, indexing an array of integers by a variable
    *
    * @param tab an non empty array n integers
    * @param x   an index variable with domain defined on (0..n-1)
    * @param z   an integer variable
    * @return a constraints such that tab, x and z are linked by the relation tab(x) == z
    */
  def element(tab: IndexedSeq[Int], x: CPIntVar, z: CPIntVar): Constraint = {
    new ElementCst(tab.toArray, x, z)
  }

  /**
    * Element Constraint, indexing an array of integers by a variable
    *
    * @param tab an non empty array n integers
    * @param x   an index variable with domain defined on (0..n-1)
    * @param z   an integer
    * @return a constraints such that tab, x and z are linked by the relation tab(x) == z
    */
  def element(tab: IndexedSeq[Int], x: CPIntVar, z: Int): Constraint = {
    new ElementCst(tab.toArray, x, CPIntVar(z, z)(x.store))
  }

  /**
    * Element 2D Constraint, indexing an integer matrix by two index variables
    *
    * @param matrix rectangle matrix of sizes n x m
    * @param i      the first index variable (line index) with domain defined on (0..n-1)
    * @param j      the second index variable (column index) with domain defined on (0..m-1)
    * @return a variable z linked to the arguments with the relation matrix(i)(j) == z
    */
  def element(matrix: Array[Array[Int]], i: CPIntVar, j: CPIntVar): CPIntVar = {
    val z = CPIntVar(matrix.flatten.min to matrix.flatten.max)(i.store)
    i.store.post(new ElementCst2D(matrix, i, j, z))
    z
  }

  /**
    * Element Constraint, indexing an array of variables by a variable
    *
    * @param tab an non empty array n variables
    * @param x   an index variable with domain defined on (0..n-1)
    * @param l   the desiredpropagation strength
    * @return an integer variable z such that tab, x and z are linked by the relation tab(x) == z
    */
  def elementVar(tab: IndexedSeq[CPIntVar], x: CPIntVar, l: CPPropagStrength = Weak): CPIntVar = {
    val minVal = (for (x <- tab) yield x.getMin).min
    val maxVal = (for (x <- tab) yield x.getMax).max
    val z = CPIntVar(minVal, maxVal)(x.store)
    x.store.add(new ElementVar(tab.toArray, x, z), l)
    z
  }

  /**
    * Element Constraint, indexing an array of variables by a variable
    *
    * @param tab an non empty array of n variables
    * @param x   an index variable with domain defined on (0..n-1)
    * @param z   an integer variable
    * @return a constraints such that tab , x and z are linked by the relation tab(x) == z
    */
  def elementVar(tab: IndexedSeq[CPIntVar], x: CPIntVar, z: CPIntVar): Constraint = {
    new ElementVar(tab.toArray, x, z)
  }

  /**
    * Element Constraint, indexing an array of variables by a variable
    *
    * @param tab an non empty array n variables
    * @param x   an index variable with domain defined on (0..n-1)
    * @param z   an integer
    * @return a constraints such that tab, x and z are linked by the relation tab(x) == z
    */
  def elementVar(tab: IndexedSeq[CPIntVar], x: CPIntVar, z: Int): Constraint = {
    new ElementVar(tab.toArray, x, CPIntVar(z)(x.store))
  }

  /**
    * Element Constraint, indexing an array of variables by a variable
    *
    * @param tab an non empty array n variables
    * @param x   an index variable with domain defined on (0..n-1)
    * @param z   an integer
    * @return a constraints such that tab, x and z are linked by the relation tab(x) == z
    */
  def elementVar(tab: IndexedSeq[CPBoolVar], x: CPIntVar, z: Boolean): Constraint = {
    val z_ = CPBoolVar(z)(x.store)
    new ElementVar(tab.map(_.asInstanceOf[CPIntVar]).toArray, x, z_)
  }

  /**
    * Inverse constraint
    *
    * @param prev an array of `n` integer variables
    * @param next an array of `n` integer variables
    * @return a constraint enforcing for all `i` in `0 until n`:
    *         1. `next(prev(i)) == i`
    *         2. `prev(next(i)) == i`
    */
  def inverse(prev: Array[CPIntVar], next: Array[CPIntVar]): Inverse = new Inverse(prev, next)

  def inverse(prev: Array[CPIntVar]): Array[CPIntVar] = {
    require(prev.length > 0)
    val store = prev(0).store
    val next = Array.fill(prev.length)(CPIntVar(0, prev.length - 1)(store))
    store.add(inverse(prev, next))
    next
  }

  /**
    * Sum Constraint
    *
    * @param vars a non empty array of n variables
    * @param s    a variable representing the sum of vars
    * @return a constraint enforcing vars(0)+vars(1)+...+vars(n) = s
    */
  def sum(vars: Array[CPIntVar], s: CPIntVar): Constraint = {
    if (vars.length == 2) new BinarySum(vars(0), vars(1), s)
    else new Sum(vars, s)
  }

  /**
    * Sum Constraint
    *
    * @param vars a non empty array of n variables
    * @return a variable representing vars(0)+vars(1)+...+vars(n)
    */
  def sum(vars: Iterable[CPIntVar]): CPIntVar = sum(vars.toArray)

  /**
    * Sum Constraint
    *
    * @param vars a non empty array of n variables
    * @return a variable representing vars(0)+vars(1)+...+vars(n)
    */
  def sum(vars: Array[CPIntVar]): CPIntVar = {
    if (vars.length == 0) sys.error("no variables")
    if (vars.length == 1) vars(0)
    else {
      var min = 0
      var max = 0
      var i = vars.length
      while (i > 0) {
        i -= 1
        min += vars(i).min
        max += vars(i).max
      }
      val s = CPIntVar(min, max)(vars(0).store)
      vars(0).store.post(sum(vars, s))
      s
    }
  }

  /**
    * Sum Constraint
    *
    * @param indices the indices on which iterate
    * @param f       a function mapping to a CPIntVar
    * @return a variable representing vars(f(i0))+vars(f(i1))+...+vars(f(in)) with i0, i1...,in the indexes
    */
  def sum[A](indices: Iterable[A])(f: A => CPIntVar): CPIntVar = {
    sum(indices map f)
  }

  /**
    * Sum Constraint
    *
    * @param indices1 a first iterable
    * @param indices2 a second iterable
    * @param f        a function mapping A,B to a variable with A from indexes1 and B from indexes2
    * @return a variable that is the sum of f(A,B) over each (A, B) in (indexes x indexes2)
    */
  def sum[A, B](indices1: Iterable[A], indices2: Iterable[B])(f: (A, B) => CPIntVar): CPIntVar = {
    sum(for (i <- indices1; j <- indices2) yield f(i, j))
  }

  /**
    * Sum Constraint
    *
    * @param n1 size of the first range of i
    * @param n2 size of the second range of j
    * @param f  a function mapping the indices i and j to a variable
    * @return a variable that is the sum of f(i, j) over each (i, j) in (0 until n1 x 0 until n2)
    */
  def sum(n1: Int, n2: Int)(f: (Int, Int) => CPIntVar): CPIntVar = {
    sum(0 until n1, 0 until n2)(f)
  }

  /**
    * Weighted Sum Constraint
    *
    * @param indices an iterable of index values
    * @param f       a function: i => (w_i, x_i) where i is an index from indexes
    * @return a variable S linked with the relation S = sum(i in indexes) (w_i * x_i)
    */
  def weightedSum[A](indices: Iterable[A])(f: A => (Int, CPIntVar)): CPIntVar = {
    val (w, x) = (for (i <- indices) yield f(i)).unzip
    weightedSum(w.toArray, x.toArray)
  }

  /**
    * Weighted Sum Constraint
    *
    * @param indices1 an iterable of index values
    * @param indices2 an iterable of index values
    * @param f        a function: (i,j) => (w_ij, x_ij) where i is an index from indexes1, j is an index from indexes2
    * @return a variable S linked with the relation S = sum(i in indexes1,j in indexes2) (w_ij * x_ij)
    */
  def weightedSum[A, B](indices1: Iterable[A], indices2: Iterable[B])(f: (A, B) => (Int, CPIntVar)): CPIntVar = {
    val (w, x) = (for (i <- indices1; j <- indices2) yield f(i, j)).unzip
    weightedSum(w.toArray, x.toArray)
  }

  /**
    * Weighted Sum Constraint
    *
    * @param w a non empty array of integers representing the weights of the weighted sum
    * @param x a non empty array of variables on which the sum will apply
    * @param y variable to which the weighted sum of w and x must be equal
    * @return y == sum(i)(w_i * x_i)
    */
  def weightedSum(w: Array[Int], x: Array[CPIntVar], y: CPIntVar): Constraint = {
    var i = 0
    while (i < w.length && w(i) == 1) i += 1
    if (i == x.length) sum(x, y)
    else new WeightedSum(w, x, y)
  }

  /**
    * Weighted Sum Constraint
    *
    * @param w a non empty array of integers representing the weights of the weighted sum
    * @param x a non empty array of variables on which the sum will apply
    * @param y integer to which the weighted sum of w and x must be equal
    * @return y==sum(i)(w_i * x_i)
    */
  def weightedSum(w: Array[Int], x: Array[CPIntVar], y: Int): Constraint = {
    weightedSum(w, x, CPIntVar(y)(x(0).store))
  }

  /**
    * Weighted Sum Constraint
    *
    * @param w a non empty array of integers representing the weights of the weighted sum
    * @param x a non empty array of variables on which the sum will apply
    * @return a variable that is equal to sum(i)(w_i * x_i)
    */
  def weightedSum(w: Array[Int], x: Array[CPIntVar]): CPIntVar = {
    val cp = x(0).store
    val m = w.zip(x).map { case (wi, xi) => if (wi < 0) wi * xi.max else wi * xi.min }.sum
    val M = w.zip(x).map { case (wi, xi) => if (wi < 0) wi * xi.min else wi * xi.max }.sum
    val y = CPIntVar(m to M)(cp)
    cp.post(weightedSum(w, x, y))
    y
  }

  /**
    * Weighted Sum Constraint
    *
    * @param w a non empty matrix of integers representing the weights of the weighted sum
    * @param x a non empty matrix of variables on which the sum will apply
    * @return a variable that is equal to sum(ij)(w_ij * x_ij)
    */
  def weightedSum(w: Array[Array[Int]], x: Array[Array[CPIntVar]]): CPIntVar = {
    weightedSum(w.flatten, x.flatten)
  }

  /**
    * Or (logical) Constraint
    *
    * @param vars a non empty collection of n variables
    * @param z    variable representing the result of the or over vars
    * @return an or constraint
    */
  def or(vars: Iterable[CPBoolVar], z: CPBoolVar): Constraint = {
    if (z.isTrue) or(vars)
    else new OrReif(vars.toArray, z)
  }

  /**
    * Or (logical) Constraint
    *
    * @param vars a non empty collection of n variables
    * @return a constraint such that at least one variables in vars must be true
    */
  def or(vars: Iterable[CPBoolVar]): Constraint = or(vars.toArray)

  /**
    * Or (logical) Constraint
    *
    * @param vars a non empty array of n variables
    * @return a constraint such that at least one variables in vars must be true
    */
  def or(vars: Array[CPBoolVar]): Constraint = {
    val nVariables = vars.length
    if (nVariables == 1) vars(0).constraintTrue
    else if (nVariables == 2) new BinaryClause(vars(0), vars(1), "")
    else new Or(vars)
  }

  /**
    * Or (logical) Constraint
    *
    * @param indices a collection of indices on which iterate
    * @param f       a function mapping indices to boolean variables
    * @return a constraint such that at least one variables in vars must be true
    */
  def or[A](indices: Iterable[A])(f: A => CPBoolVar): Constraint = {
    or(for (i <- indices) yield f(i))
  }

  /**
    * Or (logical) Constraint
    *
    * @param vars a non empty collection of n variables
    * @return result of the or over vars
    */
  def isOr(vars: Iterable[CPBoolVar]): CPBoolVar = {
    val z = CPBoolVar()(vars.head.store)
    vars.head.store.add(or(vars, z))
    z
  }

  /**
    * Or (logical) Constraint
    *
    * @param indices a collection of indices on which iterate
    * @param f       a function mapping indices to boolean variables
    * @return z the result of the or over or(f(i))
    */
  def isOr[A](indices: Iterable[A])(f: A => CPBoolVar): CPBoolVar = {
    val x = (for (i <- indices) yield f(i)).toArray
    val z = CPBoolVar()(x(0).store)
    x(0).store.add(or(x, z))
    z
  }


  /**
    * Negative Table Constraints  (constraint given in extension by enumerating invalid valid assignments)
    *
    * @param x                non empty array of variables on which the negative table constraint apply
    * @param impossibleTuples a collection of impossible tuples for variables in x
    * @param algo             the negative table filtering algorithm to be used
    * @return a constraint enforcing that x is not one of the tuples given in tuples
    */
  def negativeTable(x: Array[CPIntVar], impossibleTuples: Array[Array[Int]], algo: NegativeTableAlgo = CompactTableNegative): Constraint = {
    oscar.cp.constraints.tables.negativeTable(x, impossibleTuples, algo)
  }

  /**
    * Negative Short Table Constraints  (constraint given in extension by enumerating invalid valid assignments)
    *
    * Remark : Tuple should not overlap to work!
    *
    * @param x                non empty array of variables on which the negative table constraint apply
    * @param impossibleTuples a collection of impossible tuples for variables in x
    * @param algo             the negative table filtering algorithm to be used
    * @return a constraint enforcing that x is not one of the tuples given in tuples
    */
  def negativeShortTable(x: Array[CPIntVar], impossibleTuples: Array[Array[Int]], star: Int = -1, algo: NegativeShortTableAlgo = NegativeShortTableAlgo.CompactTableNegativeStar): Constraint = {
    oscar.cp.constraints.tables.negativeShortTable(x, impossibleTuples, star, algo)
  }

  /**
    * Table Constraints (constraint given in extension by enumerating valid assignments)
    *
    * @param x              non empty array of variables on which the table constraint apply
    * @param possibleTuples a collection of possible tuples for variables in x
    * @param algo           the table filtering algorithm used
    * @return a constraint enforcing that x is one of the tuples given in tuples
    */
  def table(x: Array[CPIntVar], possibleTuples: Array[Array[Int]], algo: TableAlgo = CompactTable): Constraint = {
    oscar.cp.constraints.tables.table(x, possibleTuples, algo)
  }

  /**
    * Table Constraint for couples (constraint given in extension by enumerating valid assignments)
    *
    * @param x1     first variable
    * @param x2     second variable
    * @param tuples a collection of coulples
    * @return a constraint enforcing that (x1, x2) is one of the couples given in tuples
    */
  def table(x1: CPIntVar, x2: CPIntVar, tuples: Iterable[(Int, Int)]): Constraint = {
    table(Array(x1, x2), tuples.map(t => Array(t._1, t._2)).toArray)
  }

  /**
    * Table Constraint for triples (constraint given in extension by enumerating valid assignments)
    *
    * @param x1     first variable
    * @param x2     second variable
    * @param x3     third variable
    * @param tuples a collection of triples
    * @return a constraint enforcing that (x1, x2, x3) is one of the triples given in tuples
    */
  def table(x1: CPIntVar, x2: CPIntVar, x3: CPIntVar, tuples: Iterable[(Int, Int, Int)]): Constraint = {
    table(Array(x1, x2, x3), tuples.map(t => Array(t._1, t._2, t._3)).toArray)
  }

  /**
    * Table Constraint for quadruples
    *
    * @param x1     first variable
    * @param x2     second variable
    * @param x3     third variable
    * @param x4     fourth variable
    * @param tuples a collection of quadruples
    * @return a constraint enforcing that (x1, x2, x3, x4) is one of the quadruples given in tuples
    */
  def table(x1: CPIntVar, x2: CPIntVar, x3: CPIntVar, x4: CPIntVar, tuples: Iterable[(Int, Int, Int, Int)]): Constraint = {
    table(Array(x1, x2, x3, x4), tuples.map(t => Array(t._1, t._2, t._3, t._4)).toArray)
  }

  /**
    * Table Constraint for quadruples
    *
    * @param x1     first variable
    * @param x2     second variable
    * @param x3     third variable
    * @param x4     fourth variable
    * @param x5     fifth variable
    * @param tuples a collection of five-tuples
    * @return a constraint enforcing that (x1, x2, x3, x4, x5) is one of the five-tuples given in tuples
    */
  def table(x1: CPIntVar, x2: CPIntVar, x3: CPIntVar, x4: CPIntVar, x5: CPIntVar, tuples: Iterable[(Int, Int, Int, Int, Int)]): Constraint = {
    table(Array(x1, x2, x3, x4, x5), tuples.map(t => Array(t._1, t._2, t._3, t._4, t._5)).toArray)
  }

  /**
    * Short Table Constraints (constraint given in extension by enumerating valid assignments)
    *
    * @param x              non empty array of variables on which the table constraint apply
    * @param possibleTuples a collection of possible tuples for variables in x
    * @param algo           the table filtering algorithm used
    * @param star           the value (out of each domains) of the number representing the star
    * @return a constraint enforcing that x is one of the tuples given in tuples
    */
  def shortTable(x: Array[CPIntVar], possibleTuples: Array[Array[Int]], star: Int = -1, algo: ShortTableAlgo = ShortTableAlgo.CompactTableStar): Constraint = {
    oscar.cp.constraints.tables.shortTable(x, possibleTuples, star, algo)
  }

  /**
    * Basic Smart Table Constraints (constraint given in extension by enumerating valid assignments)
    *
    * @param x              non empty array of variables on which the table constraint apply
    * @param possibleTuples a collection of possible tuples for variables in x
    * @param algo           the table filtering algorithm used
    * @return a constraint enforcing that x is one of the tuples given in tuples
    */
  def basicSmartTable(x: Array[CPIntVar], possibleTuples: Array[Array[BasicSmartElement]], algo: BasicSmartTableAlgo = BasicSmartTableAlgo.CompactTableBs): Constraint = {
    oscar.cp.constraints.tables.basicSmartTable(x, possibleTuples, algo)
  }

  /**
    * The modulo constraint ensuring that x % v = y
    *
    * @param x variable on which the modulo applies
    * @param v value applied as modulo
    * @param y variable to which the modulo must be equal
    * @return a constraint enforcing that y is equal to the modulo of x by v (x % v = y)
    */
  def modulo(x: CPIntVar, v: Int, y: CPIntVar): Constraint = {
    new Modulo(x, v, y)
  }

  /**
    * The modulo constraint ensuring that x % v = y
    *
    * @param x variable on which the modulo applies
    * @param v value applied as modulo
    * @param y value to which the modulo must be equal
    * @return a constraint enforcing that y is equal to the modulo of x by v (x % v = y)
    */
  def modulo(x: CPIntVar, v: Int, y: Int): Constraint = {
    new Modulo(x, v, CPIntVar(y)(x.store))
  }

  /**
    * @param x a variable in the same store as y
    * @param v a value in the same store as x
    * @return a variable in the same store representing: x % v
    */
  def modulo(x: CPIntVar, v: Int): CPIntVar = {
    val y = CPIntVar(0, v - 1)(x.store)
    val ok = x.store.post(new oscar.cp.constraints.Modulo(x, v, y))
    y
  }

  /**
    * Among Constraint: n is the number of variables from x in set s.
    *
    * @param n counter variable
    * @param x array of variables
    * @param s set of values
    * @return a constraint enforcing that n = #{ i | x(i) in s }
    */
  def among(n: CPIntVar, x: IndexedSeq[CPIntVar], s: Set[Int]) = {
    new Among(n, x, s)
  }

  /**
    * AtLeast Constraint: at least n variables take their value in s
    *
    * @param n counter variable
    * @param x array of variables
    * @param s set of values
    * @return a constraint enforcing that  #{ i | x(i) in s } >= n
    */
  def atLeast(n: Int, x: IndexedSeq[CPIntVar], s: Set[Int]) = {
    among(CPIntVar(n, x.size)(x.head.store), x, s)
  }

  /**
    * AtLeast Constraint: at least n variables equal to v
    *
    * @param n counter variable
    * @param x array of variables
    * @param v a value
    * @return a constraint enforcing that  #{ i | x(i) = v } >= n
    */
  def atLeast(n: Int, x: IndexedSeq[CPIntVar], v: Int): Constraint = {
    atLeast(n, x, Set(v))
  }

  /**
    * AtMost Constraint: at most n variables take their value in s
    *
    * @param n counter variable
    * @param x array of variables
    * @param s set of values
    * @return a constraint enforcing that  #{ i | x(i) in s } <= n
    */
  def atMost(n: Int, x: IndexedSeq[CPIntVar], s: Set[Int]) = {
    among(CPIntVar(0, n)(x.head.store), x, s)
  }

  /**
    * AtMost Constraint: at least n variables equal to v
    *
    * @param n counter variable
    * @param x array of variables
    * @param v a value
    * @return a constraint enforcing that  #{ i | x(i) = v } <= n
    */
  def atMost(n: Int, x: IndexedSeq[CPIntVar], v: Int): Constraint = atMost(n, x, Set(v))

  /**
    * Count Constraint: n is the number of variables from x equal to y.
    *
    * @param n is a counter variable
    * @param x is a collection of variables
    * @param y is a variable
    * @return a constraint enforcing that n = #{ i | x(i) = y }
    */
  def countEq(n: CPIntVar, x: IndexedSeq[CPIntVar], y: CPIntVar): Constraint = {
    if (y.isBound) countEq(n, x, y.value)
    else new CountSimple(n, x, y)
  }

  /**
    * Count Constraint: n is the number of variables from x equal to v.
    *
    * @param n is a counter variable
    * @param x is a collection of variables
    * @param v is a value
    * @return a constraint enforcing that n = #{ i | x(i) = v }
    */
  def countEq(n: CPIntVar, x: IndexedSeq[CPIntVar], v: Int) = {
    new CountCst(n, x, v)
  }

  /**
    * Count Constraint: n is greater or equal to the number of variables from x equal to y.
    *
    * @param n is a counter variable
    * @param x is an array of variables
    * @param y is a variable
    * @return a constraint enforcing that n >= #{ i | x(i) = y }
    */
  def countGeq(n: CPIntVar, x: IndexedSeq[CPIntVar], y: CPIntVar) = {
    val c = CPIntVar(0 to x.size)(n.store)
    n.store.post(n >= c)
    new Count(c, x, y)
  }

  /**
    * Count Constraint: n is strictly greater than the number of variables from x equal to y.
    *
    * @param n is a counter variable
    * @param x is an array of variables
    * @param y is a variable
    * @return a constraint enforcing that n > #{ i | x(i) = y }
    */
  def countGt(n: CPIntVar, x: IndexedSeq[CPIntVar], y: CPIntVar) = {
    val c = CPIntVar(0 to x.size)(n.store)
    n.store.post(n > c)
    new Count(c, x, y)
  }

  /**
    * Count Constraint: n is less or equal to the number of variables from x equal to y.
    *
    * @param n is a counter variable
    * @param x is an array of variables
    * @param y is a variable
    * @return a constraint enforcing that n <= #{ i | x(i) = y }
    */
  def countLeq(n: CPIntVar, x: IndexedSeq[CPIntVar], y: CPIntVar) = {
    val c = CPIntVar(0 to x.size)(n.store)
    n.store.post(n <= c)
    new Count(c, x, y)
  }

  /**
    * Count Constraint: n is strictly less than the number of variables from x equal to y.
    *
    * @param n is a counter variable
    * @param x is an array of variables
    * @param y is a variable
    * @return a constraint enforcing that n <= #{ i | x(i) = y }
    */
  def countLt(n: CPIntVar, x: IndexedSeq[CPIntVar], y: CPIntVar) = {
    val c = CPIntVar(0 to x.size)(n.store)
    n.store.post(n < c)
    new Count(c, x, y)
  }

  /**
    * Count Constraint: n is different to the number of variables from x equal to y.
    *
    * @param n is a counter variable
    * @param x is an array of variables
    * @param y is a variable
    * @return a constraint enforcing that n != #{ i | x(i) = y }
    */
  def countNeq(n: CPIntVar, x: IndexedSeq[CPIntVar], y: CPIntVar) = {
    val c = CPIntVar(0 to x.size)(n.store)
    n.store.post(n.diff(c))
    new Count(c, x, y)
  }


  /**
    * Global Cardinality Constraint: every value occurs at least min and at most max
    *
    * @param x        an non empty array of variables
    * @param valueMin is the minimum value that is constrained
    * @param cardMin  is the minimum number of occurrences
    * @param cardMax  such that cardMax.size == cardMin.size is the maximum number of occurrences
    * @return a constraint such that each value v in the range  [valueMin..valueMin+cardMin.size-1]
    *         occurs at least cardMin[v-minValue] and at most cardMax[v-minValue] times.
    */
  def gcc(x: Array[CPIntVar], valueMin: Int, cardMin: Array[Int], cardMax: Array[Int]): Constraint = {
    if (cardMin.size != cardMax.size) throw new InvalidParameterException("cardMin.size != cardMax.size")
    new GCC(x, valueMin, cardMin, cardMax)
  }

  /**
    * Global Cardinality Constraint: every value occurs at least min and at most max
    *
    * @param x      an non empty array of variables
    * @param values is the range of constrained values
    * @param min    is the minimum number of occurrences for each value in the range values
    * @param max    is the maximum number of occurrences for each value in the range values
    * @return a constraint such that each value in the range values occurs at least min and at most max times.
    */
  def gcc(x: Array[CPIntVar], values: Range, min: Int, max: Int): Constraint = {
    new GCC(x, values.min, Array.fill(values.size)(min), Array.fill(values.size)(max))
  }

  /**
    * Global Cardinality Constraint: every value v occurs at least min(v) and at most max(v)
    *
    * @param x      an non empty array of variables
    * @param values is the range of constrained values
    * @param min    is the minimum number of occurrences for each value in the range values
    * @param max    is the maximum number of occurrences for each value in the range values
    * @return a constraint such that each value in the range values occurs at least min and at most max times.
    */
  def gcc(x: Array[CPIntVar], values: Range, min: Array[Int], max: Array[Int]): Constraint = {
    new GCC(x, values.min, min, max)
  }

  /**
    * Soft Global Cardinality Constraint = gcc with a violation variable
    *
    * @see Revisiting the Soft Global Cardinality Constraint, Pierre Schaus, Pascal Van Hentenryck, Alessandro Zanarini: CPAIOR 2010
    */
  def softGcc(x: Array[CPIntVar], values: Range, min: Array[Int], max: Array[Int], viol: CPIntVar): SoftGCC = {
    new SoftGCC(x, values.min, min, max, viol)
  }

  /**
    * Global Cardinality Constraint with variable counters
    *
    * @param x               a non empty collection of variables
    * @param valueOccurrence is a collection of pairs (v, o)
    *                        where o is variable representing the number of occurrences of value v
    * @return a constraint such that for each (o,v) in valueOccurrence, o is the number of times the value v appears in x
    */
  def gcc(x: IndexedSeq[CPIntVar], valueOccurrence: Iterable[(Int, CPIntVar)]): Constraint = {
    def freshCard(): CPIntVar = CPIntVar(0, x.size - 1)(x.head.store)

    val sortedValOcc = valueOccurrence.toArray.sortWith((a, b) => a._1 <= b._1)
    val (v0, x0) = sortedValOcc(0)
    var values = Array(v0)
    var cardinalities = Array(x0)
    for (i <- 1 until sortedValOcc.length) {
      val vi_1 = sortedValOcc(i - 1)._1
      val (vi, xi) = sortedValOcc(i)
      for (v <- (vi_1 + 1) until vi) {
        values = values :+ v
        cardinalities = cardinalities :+ freshCard()
      }
      values = values :+ vi
      cardinalities = cardinalities :+ xi
    }
    new GCCVar(x.toArray, values(0), cardinalities)
  }

  /**
    * Global Cardinality Constraint with variable counters
    *
    * @param x               a non empty array of variables
    * @param valueOccurrence is an array of pairs (v,o)
    *                        where o is variable representing the number of occurrences of value v
    * @return a constraint such that for each (o,v) in valueOccurrence, o is the number of times the value v appears in x
    */
  def gcc(x: Array[CPIntVar], valueOccurrence: Array[(Int, CPIntVar)]): Constraint = {
    gcc(x.toIndexedSeq, valueOccurrence.toIterable)
  }

  // regular and automatons

  /**
    * Builds an automaton restricting the number consecutive times the values appear.
    * A stretch is a consecutive number of a same value in vars for instance 112223335 start with a stretch of length 2 of value 1, followed by a stretch of length 3 with value 2,
    * followed by a stretch of length 3 of value 3 followed by a stretch of 1 of value 5.
    *
    * @param vars       an non empty array of variables
    * @param minStretch the minimum stretch length for any value
    * @param maxStretch the maximum stretch length for any value
    * @return an automaton
    */
  def stretchAutomaton(vars: Array[CPIntVar], minStretch: Int, maxStretch: Int): Automaton = {
    stretchAutomaton(vars, minStretch, maxStretch, None)
  }

  /**
    * Builds an automaton restricting the number consecutive times the values appear.
    * A stretch is a consecutive number of a same value in vars for instance 112223335 start with a stretch of length 2 of value 1, followed by a stretch of length 3 with value 2,
    * followed by a stretch of length 3 of value 3 followed by a stretch of 1 of value 5.
    *
    * @param vars        an non empty array of variables
    * @param minStretch  the minimum stretch length for any value
    * @param maxStretch  the maximum stretch length for any value
    * @param transitions collection containing the allowed transitions between consecutive values
    * @return an automaton
    */
  def stretchAutomaton(vars: Array[CPIntVar], minStretch: Int, maxStretch: Int, transitions: Iterable[(Int, Int)]): Automaton = {
    val maxV = vars.map(x => x.getMax).max
    val minV = vars.map(x => x.getMin).min
    if (minV < 0) throw new RuntimeException("warning stretch automaton: some domains with <0 values, only >=0 values can be constrained")

    val minimumStretch = Array.tabulate(maxV + 1)(_ => minStretch)
    val maximumStretch = Array.tabulate(maxV + 1)(_ => maxStretch)

    stretchAutomaton(vars, minimumStretch, maximumStretch, transitions)
  }

  /**
    * Builds an automaton restricting the number consecutive times the values appear.
    * A stretch is a consecutive number of a same value in vars for instance 112223335 start with a stretch of length 2 of value 1, followed by a stretch of length 3 with value 2,
    * followed by a stretch of length 3 of value 3 followed by a stretch of 1 of value 5.
    *
    * @param vars        an non empty array of variables
    * @param minStretch  array containing the minimum stretch length for value corresponding to the index
    * @param maxStretch  array containing the maximum stretch length for value corresponding to the index
    * @param transitions collection containing the allowed transitions between consecutive values
    * @return an automaton
    */
  def stretchAutomaton(vars: Array[CPIntVar], minStretch: Array[Int], maxStretch: Array[Int], transitions: Iterable[(Int, Int)] = None): Automaton = {
    val minV = vars.map(x => x.getMin).min
    if (minV < 0) throw new RuntimeException("warning stretch automaton: some domains with <0 values, only >=0 values can be constrained")

    if (transitions.nonEmpty) {
      var transiFrom = Array[Int]()
      var transiTo = Array[Int]()
      transitions.foreach(t => {
        transiFrom = transiFrom :+ t._1; transiTo = transiTo :+ t._2
      })
      Stretch.getStretchAutomaton(vars, minStretch, maxStretch, transiFrom, transiTo)
    } else {
      Stretch.getStretchAutomaton(vars, minStretch, maxStretch)
    }
  }

  /**
    * Regular Constraint, ensuring that vars accepted by a context free grammar described by given automaton
    *
    * @param vars      an non empty array of variables, with domains belonging to the set of transitions of the automaton
    * @param automaton a deterministic automaton
    * @return a constraint ensuring values in vars respect the automaton provided
    */
  def regular(vars: Array[CPIntVar], automaton: Automaton): Constraint = {
    new Regular(vars, automaton)
  }

  /**
    * Maximum Constraint
    *
    * @param indices collection of indices on which iterate
    * @param f       function mapping each element from indexes to a variable
    * @return a fresh variable z linked to vars by a constraint such that z is the maximum of all variables f(A) for all A in indexes
    */
  def maximum[A](indices: Iterable[A])(f: A => CPIntVar): CPIntVar = {
    maximum(indices map f)
  }

  /**
    * Maximum Constraint
    *
    * @param vars an non empty array of variables
    * @param m    a variables representing the maximum of vars
    * @return a constraint ensuring that m is the maximum of variables in vars
    */
  def maximum(vars: Array[CPIntVar], m: CPIntVar): Constraint = {
    new Maximum(vars.asInstanceOf[Array[CPIntVar]], m)
  }

  /**
    * Maximum Constraint
    *
    * @param vars an non empty array of variables
    * @return a fresh variable m linked to vars by a constraint such that m is the maximum of all variables in vars
    */
  def maximum(vars: Array[CPIntVar]): CPIntVar = {
    val cp = vars(0).store
    val m = CPIntVar(vars.map(_.min).max, vars.map(_.max).max)(cp)
    cp.add(maximum(vars, m))
    m
  }

  /**
    * Maximum Constraint
    *
    * @param vars an non empty collection of variables
    * @return a fresh variable m linked to vars by a constraint such that m is the maximum of all variables in vars
    */
  def maximum(vars: Iterable[CPIntVar]): CPIntVar = {
    val x = vars.toArray
    val cp = x(0).store
    val m = CPIntVar(vars.map(_.min).max, vars.map(_.max).max)(cp)
    cp.add(maximum(x, m))
    m
  }

  /**
    * Minimum Constraint
    *
    * @param indices collection of indices on which iterate
    * @param f       function mapping each element from indexes to a variable
    * @return a fresh variable z linked to vars by a constraint such that z is the minimum of all variables f(A) for all A in indexes
    */
  def minimum[A](indices: Iterable[A])(f: A => CPIntVar): CPIntVar = {
    minimum(indices map f)
  }

  /**
    * Minimum Constraint
    *
    * @param vars an non empty array of variables
    * @param m    a variables representing the maximum of vars
    * @return a constraint ensuring that m is the minimum of variables in vars
    */
  def minimum(vars: Array[CPIntVar], m: CPIntVar): Constraint = {
    new Minimum(vars, m)
  }

  /**
    * Minimum Constraint
    *
    * @param vars an non empty array of variables
    * @return a fresh variable z linked to vars by a constraint such that z is the minimum of all variables in vars
    */
  def minimum(vars: Iterable[CPIntVar]): CPIntVar = {
    val x = vars.toArray
    val cp = x(0).store
    val m = CPIntVar(vars.map(_.min).min, vars.map(_.max).min)(cp)
    cp.add(minimum(x, m))
    m
  }

  /**
    * Constraint enforcing n * sum_i |x[i]-s/n| <= nd and sumi_i x[i] = s <br>
    * Note that this constraint is very similar to spread.
    *
    * @param x  a collection of variables
    * @param s  a value to which the sum over x must be equal
    * @param nd a variable representing the max deviation
    * @return a constraint enforcing that n * sum_i |x[i]-s/n| <= nd and sumi_i x[i] = s
    */
  def deviation(x: Iterable[CPIntVar], s: Int, nd: CPIntVar): Constraint = {
    new Deviation(x.toArray, s, nd)
  }

  /**
    * Constraint enforcing sum_i x[i]ˆ2 <= s2 and sum_i x[i] = s <br>
    * Note that this constraint is very similar deviation.
    *
    * @param x  a collection of variables
    * @param s  a value to which the sum over x must be equal
    * @param s2 a variable representing the max spread
    * @return a constraint enforcing that sum_i x[i]2 <= s2 and sum_i x[i] = s
    */
  def spread(x: Iterable[CPIntVar], s: Int, s2: CPIntVar): Constraint = {
    new Spread(x.toArray, s, s2, true)
  }

  /**
    * Let n = x.size-1 = y.size-1
    * This constraint enforces that x and y are permutations over {0, ... , n}
    * with y(i) giving the position of number i in x. It means that x(y(i)) = i
    * Note that this constraint could be enforced with element constraints but it is less efficient
    * Weak and Strong consistency can be used acting on the filtering of alldifferent constraints
    *
    * @param x a non empty collection of variable
    * @param y a non empty collection of variable of same size as x
    * @return a constraint enforcing that x and y are permutations over {0, ..., n} (i.e. x(y(i)) = i)
    */
  def permutation(x: IndexedSeq[CPIntVar], y: IndexedSeq[CPIntVar]): Constraint = {
    new Permutation(x, y)
  }

  /**
    * Constraints enforcing that variables in s are variables from x sorted in increasing order (i.e. s_i <= s_{i+1})
    * where values in p represent the position/index in x at which values of s are (i.e. x(p_i) = s_i)
    *
    * @param x        a non empty collection of variable
    * @param s        a non empty collection of variable of same size as x
    * @param p        a non empty collection of variable of same size as x
    * @param strictly boolean set to true if the order must be strict
    * @return a constraint enforcing that s is the sorted version of variables in x where x(p_i) = s_i
    */
  def sortedness(x: IndexedSeq[CPIntVar], s: IndexedSeq[CPIntVar], p: IndexedSeq[CPIntVar], strictly: Boolean = false): Array[Constraint] = {
    val cp = x.head.store
    val n = x.size
    val constraints = ArrayBuffer[Constraint]()

    for (i <- 0 until n - 1) {
      constraints.append(elementVar(x, p(i), Strong).leEq(elementVar(x, p(i + 1), Strong)))
      if (strictly) {
        constraints.append(s(i).le(s(i + 1)))
      } else {
        constraints.append(s(i).leEq(s(i + 1)))
      }

    }
    val minx = x.map(_.min).min
    val maxx = x.map(_.max).max
    val mins = s.map(_.min).min
    val maxs = s.map(_.max).max

    for (i <- x.indices) {
      constraints.append(p(i).grEq(0))
      constraints.append(p(i).leEq(n))

      constraints.append(s(i).leEq(maxx))
      constraints.append(s(i).grEq(minx))

      constraints.append(x(i).leEq(maxs))
      constraints.append(x(i).grEq(mins))
    }
    for (i <- 0 until n) {
      constraints.append(elementVar(x, p(i), s(i)))
    }
    constraints.append(allDifferent(p))

    val minVal: Int = x.map(_.min).min
    val maxVal: Int = x.map(_.max).max

    // array of variable occ with domains {0,...,n} that will represent the number of occurrences of each value
    val occ = Array.fill(maxVal - minVal + 1)(CPIntVar(0 to n)(cp))
    constraints.append(gcc(x, (minVal to maxVal).zip(occ)))

    // nbBefore(i) = #{i | x(i) < i } i.e. number of values strictly small than i for i in [minVal .. maxVal]
    val nbBefore = for (i <- minVal to maxVal) yield {
      if (i == minVal) CPIntVar(0)(cp)
      else sum(minVal to i - 1)(j => occ(j))
    }

    for (i <- 0 until n) {
      // there are less than i values smaller than s(i) 
      constraints.append(elementVar(nbBefore, s(i) - minVal).leEq(i))
    }
    constraints.toArray
  }

  /**
    * The StockingCost constraint holds when each item is produced before
    * its due date ($X_i <= d_i$), at most one item is produced at any time
    * on the machine (all the $X_i$ are distinct), and $H$
    * is an upper bound on the total stocking cost (sum_i(d_i - X_i) <= H).
    *
    * This constraint is useful for modeling
    * Production Planning Problem such as Lot Sizing Problems
    *
    * @param X , the variable $X_i$ is the date of production of item $i$ on the machine
    * @param d , the integer $d_i$ is the due-date for item $i$
    * @param H , the variable $H$ is an upper bound on the total number of slots all the items are need in stock.
    * @return a constraint ensuring that every variable in X is lower or equal to corresponding index in d,
    *         all values in X are different and H represents an upper bound of the sum of the differences between
    *         deadlines and variables (sum_i(d_i - X_i) <= H)
    */
  def stockingCost(X: Array[CPIntVar], d: Array[Int], H: CPIntVar) = {
    new StockingCost(X, d, H, 1)
  }


  /**
    * The IDStockingCost constraint holds when each item is produced before
    * its due date ($X_i <= d_i$), the capacity of the machine is respected
    * (i.e. no more than $c$ variables $X_i$ have the same value), and $H$
    * is an upper bound on the total stocking cost ($sum_i((d_i - X_i)*h_i) <= H$).
    *
    * This constraint is the generalization of StockingCost constraint to
    * item dependent stocking cost and useful for modeling
    * Production Planning Problem such as Lot Sizing Problems
    *
    * @param X        , the variable $X_i$ is the date of production of item $i$ on the machine
    * @param deadline , the integer $deadline_i$ is the due-date for item $i$
    * @param h        , the integer $h_i$ is the stocking cost for item $i$
    * @param H        , the variable $H$ is an upper bound on the total number of slots all the items are need in stock.
    * @param cap      , the integer $cap_t$ is the maximum number of items the machine can produce during one time slot $t$ (capacity),
    *                 if an item is produced before its due date, then it must be stocked.
    */
  def stockingCost(X: Array[CPIntVar], deadline: Array[Int], h: Array[Int], H: CPIntVar, cap: Array[Int]) = {
    new IDStockingCost(X, deadline, h, H, cap)
  }

  /**
    * Non overlapping between 2D rectangles
    *
    * @param x  is the x coordinates of the bottom left corner of rectangles
    * @param dx is the length in direction of x of each rectangle
    * @param y  is the y coordinates of the bottom left corner of rectangles
    * @param dy is the length in direction y of each rectangle
    * @return a set of constraints such that posting all of them enforces the non overlapping of rectangles
    */
  def diffn(x: Array[CPIntVar], dx: Array[CPIntVar], y: Array[CPIntVar], dy: Array[CPIntVar]): Iterable[Constraint] = {
    val endx = Array.tabulate(x.length)(i => x(i) + dx(i))
    val endy = Array.tabulate(y.length)(i => y(i) + dy(i))
    val maxX: CPIntVar = maximum(endx)
    val maxY: CPIntVar = maximum(endy)
    val capax = maxX - minimum(x)
    val capay = maxY - minimum(y)
    var cons = Vector[Constraint]()
    for (i <- x.indices;
         j <- i + 1 until x.length) {
      cons = cons :+ new Or(Array((x(i) + dx(i)).isLeEq(x(j)),
        (x(j) + dx(j)).isLeEq(x(i)),
        (y(i) + dy(i)).isLeEq(y(j)),
        (y(j) + dy(j)).isLeEq(y(i)),
        (x(i) + dx(i)).isLeEq(x(j)),
        (x(j) + dx(j)).isLeEq(x(i)),
        (y(i) + dy(i)).isLeEq(y(j)),
        (y(j) + dy(j)).isLeEq(y(i))))
    }
    cons = cons :+ maxCumulativeResource(x, dx, endx, dy, capay)
    cons = cons :+ maxCumulativeResource(y, dy, endy, dx, capax)
    cons
  }

  // scheduling constraints

  /**
    * Unary Resource constraint (also called disjunctive resource): at any time, no two tasks (with required(i) = true) can overlap in time
    *
    * @param starts    the variables representing the start time of the tasks
    * @param durations the variables representing the duration of the tasks
    * @param ends      the variables representing the completion time of the tasks, it is your responsibility to link starts, durations and ends such that start(i) + durations(i) = ends(i)
    * @param required  tells if a task is scheduled on this resource or not, if not this task is not constrained
    * @return a constraint ensuring activities don't overlap in time
    */
  def unaryResource(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], required: Array[CPBoolVar]): UnaryResource = {
    val resource = required.map(_.asInstanceOf[CPIntVar])
    new UnaryResource(starts, durations, ends, resource)
  }

  /**
    * Unary Resource constraint (also called disjunctive resource): at any time, no two tasks (with resources(i) = id) can overlap in time
    *
    * @param starts    the variables representing the start time of the tasks
    * @param durations the variables representing the duration of the tasks
    * @param ends      the variables representing the completion time of the tasks, it is your responsibility to link starts, durations and ends such that start(i) + durations(i) = ends(i)
    * @param resources the variables representing the resource where the task is scheduled
    * @param id        , the resource on which we want to constraint, tasks i such that resources(i) != id are not considered
    * @return a constraint ensuring activities don't overlap in time
    */
  def unaryResource(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], resources: Array[CPIntVar], id: Int) = {
    new UnaryResource(starts, durations, ends, resources, id)
  }

  /**
    * Unary Resource constraint (also called disjunctive resource): at any time, no two tasks can overlap in time
    *
    * @param starts    the variables representing the start time of the tasks
    * @param durations the variables representing the duration of the tasks
    * @param ends      the variables representing the completion time of the tasks, it is your responsibility to link starts, durations and ends such that start(i) + durations(i) = ends(i)
    * @return a constraint ensuring activities don't overlap in time
    */
  def unaryResource(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar]): Constraint = {
    implicit val store = starts(0).store
    val n = starts.length
    val zero = CPIntVar(0)
    val resources = Array.fill(n)(zero)
    unaryResource(starts, durations, ends, resources, 0)
  }

  /**
    * Unary Resource constraint (also called disjunctive resource): at any time, no two tasks can overlap in time
    *
    * @param starts          the variables representing the start time of the tasks
    * @param durations       the variables representing the duration of the tasks
    * @param ends            the variables representing the completion time of the tasks, it is your responsibility to link starts, durations and ends such that start(i) + durations(i) = ends(i)
    * @param types           the integers representing the type of each activity that will be used as entry in the transition times matrix
    * @param transitionTimes matrix of the transition times between the different activities according to their respective type
    * @return a constraint ensuring activities don't overlap in time and that consecutive activities are separated by a transition time corresponding to their respective type
    */
  def unaryResource(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], types: Array[Int], transitionTimes: Array[Array[Int]]): Constraint = {
    val cp = starts(0).store
    for {
      i <- starts.indices
      j <- i + 1 until starts.length
    } {
      cp.add((ends(j) + transitionTimes(types(j))(types(i)) ?<= starts(i)) || (ends(i) + transitionTimes(types(i))(types(j)) ?<= starts(j)))
    }
    unaryResource(starts, durations, ends)
  }

  /**
    * Unary Resource constraint (also called disjunctive resource): at any time, no two tasks can overlap in time
    *
    * @param starts          the variables representing the start time of the tasks
    * @param durations       the variables representing the duration of the tasks
    * @param ends            the variables representing the completion time of the tasks, it is your responsibility to link starts, durations and ends such that start(i) + durations(i) = ends(i)
    * @param transitionTimes matrix of the transition times between the different activities
    * @return a constraint ensuring activities don't overlap in time and that consecutive activities are separated by their respective transition time
    */
  def unaryResource(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], transitionTimes: Array[Array[Int]]) = {
    new DisjunctiveWithTransitionTimes(starts, durations, ends, transitionTimes)
  }

  /**
    * Unary Resource constraint (also called disjunctive resource): at any time, no two tasks can overlap in time
    *
    * @param starts       the variables representing the start time of the tasks
    * @param durations    the variables representing the duration of the tasks
    * @param ends         the variables representing the completion time of the tasks, it is your responsibility to link starts, durations and ends such that start(i) + durations(i) = ends(i)
    * @param familyMatrix matrix of the transition times between the different families
    * @param families     The family associated to each activity
    * @return a constraint ensuring activities don't overlap in time and that consecutive activities are separated by their respective transition time
    */
  def unaryResource(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], familyMatrix: Array[Array[Int]], families: Array[Int]) = {
    new DisjunctiveWithTransitionTimesAndFamilies(starts, durations, ends, familyMatrix, families)
  }

  /**
    * State Resource constraint: at any time, no two tasks needing different states can overlap in time
    *
    * @param starts          the variables representing the start time of the tasks
    * @param durations       the variables representing the duration of the tasks
    * @param ends            the variables representing the completion time of the tasks, it is your responsibility to link starts, durations and ends such that start(i) + durations(i) = ends(i)
    * @param states          the integers representing the states of each activity that will be used as entry in the transition times matrix
    * @param transitionTimes matrix of the transition times between the different activities according to their respective states
    * @return a constraint ensuring activities don't overlap in time if they are not in the same state and that consecutive activities are separated by a transition time corresponding to their respective states
    */
  def stateResource(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], states: Array[Int], transitionTimes: Array[Array[Int]]) = {
    val cp = starts(0).store
    for {
      i <- starts.indices
      j <- i + 1 until starts.length
    } {
      if (states(i) != states(j)) {
        new UnaryResource(Array(starts(i), starts(j)), Array(durations(i), durations(j)), Array(ends(i), ends(j)), Array(CPBoolVar(b = true)(cp), CPBoolVar(b = true)(cp)))
        cp.add((ends(j) + transitionTimes(states(j))(states(i)) ?<= starts(i)) || (ends(i) + transitionTimes(states(i))(states(j)) ?<= starts(j)))
      }
    }
  }

  /**
    * Reservoir resource with specified parameters. For any point of time, the amount of resource in the reservoir must be between its minimal and maximal capacity
    *
    * @param startVars       Variables for task starting times
    * @param durationVars    Variables for task durations
    * @param endVars         Variables for task ending times
    * @param productionVars  Variables for task productions; represents the amounts of the resource produced by tasks
    * @param consumptionVars Variables for task consumptions; represents the amounts of the resource consumed by tasks
    * @param minCapacity     The minimal capacity of the reservoir
    * @param maxCapacity     The maximal capacity of the reservoir
    * @param initialAmount   The initial amount of resource in the reservoir
    * @return a constraint ensuring the level of the reservoir is always between its minCapacity and maxCapacity
    */
  def reservoirResource(startVars: Array[CPIntVar], durationVars: Array[CPIntVar], endVars: Array[CPIntVar], productionVars: Array[CPIntVar], consumptionVars: Array[CPIntVar], minCapacity: Int, maxCapacity: Int, initialAmount: Int) = {
    ReservoirResource(startVars, durationVars, endVars, productionVars, consumptionVars, minCapacity, maxCapacity, initialAmount)
  }

  /**
    * Reservoir resource with specified parameters. For any point of time, the amount of resource in the reservoir must be between its minimal and maximal capacity
    *
    * @param startVars         Variables for task starting times
    * @param durationVars      Variables for task durations
    * @param endVars           Variables for task ending times
    * @param productionVars    Variables for task productions; represents the amounts of the resource produced by tasks
    * @param consumptionVars   Variables for task consumptions; represents the amounts of the resource consumed by tasks
    * @param temporaryProdCons Booleans set to true if corresponding task produces/consumes only during its duration
    * @param minCapacity       The minimal capacity of the reservoir
    * @param maxCapacity       The maximal capacity of the reservoir
    * @param initialAmount     The initial amount of resource in the reservoir
    * @return a constraint ensuring the level of the reservoir is always between its minCapacity and maxCapacity
    */
  def reservoirResource(startVars: Array[CPIntVar], durationVars: Array[CPIntVar], endVars: Array[CPIntVar], productionVars: Array[CPIntVar], consumptionVars: Array[CPIntVar], temporaryProdCons: Array[Boolean], minCapacity: Int, maxCapacity: Int, initialAmount: Int) = {
    ReservoirResource(startVars, durationVars, endVars, productionVars, consumptionVars, temporaryProdCons, minCapacity, maxCapacity, initialAmount)
  }

  /**
    * Discrete Resource constraint with maximum capacity: at any time, the cumulative demands of the tasks executing on the resource id, must be <= than the capacity
    *
    * @param starts    the variables representing the start time of the tasks
    * @param durations the variables representing the duration of the tasks
    * @param ends      the variables representing the completion time of the tasks, it is your responsibility to link starts, durations and ends such that start(i) + durations(i) = ends(i)
    * @param demands   the variables representing how much each task consume of the resource
    * @param resources the variables representing the resource where the task is scheduled
    * @param capacity  the capacity of the resource
    * @param id        , the resource on which we want to constraint the capacity (only tasks i with resources(i) = id are taken into account)
    * @return a constraint enforcing that the load over the resource is always below/at its capacity at any point of time
    */
  def maxCumulativeResource(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int): Constraint = {
    MaxCumulative(starts, durations, ends, demands, resources, capacity, id)
  }

  /**
    * Discrete Resource constraint with maximum capacity: at any time, the cumulative demands of the tasks must be <= than the capacity
    *
    * @param starts    the variables representing the start time of the tasks
    * @param durations the variables representing the duration of the tasks
    * @param ends      the variables representing the completion time of the tasks, it is your responsibility to link starts, durations and ends such that start(i) + durations(i) = ends(i)
    * @param demands   the variables representing how much each task consume of the resource
    * @param capacity  the capacity of the resource
    * @return a constraint enforcing that the load over the resource is always below/at its capacity at any point of time
    */
  def maxCumulativeResource(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], capacity: CPIntVar): Constraint = {
    val cp = starts(0).store
    val resources = Array.fill(starts.length)(CPIntVar(0)(cp))
    maxCumulativeResource(starts, durations, ends, demands, resources, capacity, 0)
  }

  /**
    * Discrete Resource constraint with minimum capacity: at any time where at least one tasks overlaps, the cumulative demands of the tasks executing on the resource id, must be >= than the capacity
    *
    * @param starts    the variables representing the start time of the tasks
    * @param durations the variables representing the duration of the tasks
    * @param ends      the variables representing the completion time of the tasks, it is your responsibility to link starts, durations and ends such that start(i) + durations(i) = ends(i)
    * @param demands   the variables representing how much each task consume of the resource
    * @param resources the variables representing the resource where the task is scheduled
    * @param capacity  the capacity of the resource
    * @param id        , the resource on which we want to constraint the capacity (only tasks i with resources(i) = id are taken into account)
    * @return a constraint enforcing that the load over the resource is always above/at its capacity at any point of time
    */
  def minCumulativeResource(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int): Constraint = {
    new SweepMinCumulative(starts, durations, ends, demands, resources, capacity, id)
  }

  /**
    * Discrete Resource constraint with maximum capacity: at any time where at least one tasks overlaps, the cumulative demands of the tasks must be >= than the capacity
    *
    * @param starts    the variables representing the start time of the tasks
    * @param durations the variables representing the duration of the tasks
    * @param ends      the variables representing the completion time of the tasks, it is your responsibility to link starts, durations and ends such that start(i) + durations(i) = ends(i)
    * @param demands   the variables representing how much each task consume of the resource
    * @param capacity  the capacity of the resource
    * @return a constraint enforcing that the load over the resource is always above/at its capacity at any point of time
    */
  def minCumulativeResource(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], capacity: CPIntVar): Constraint = {
    val cp = starts(0).store
    val resources = Array.fill(starts.length)(CPIntVar(0)(cp))
    minCumulativeResource(starts, durations, ends, demands, resources, capacity, 0)
  }

  /**
    * Constraint x and y to be disjoint (no common values)
    *
    * @param x a set variable
    * @param y a set variable
    * @return a constraint ensuring that x does not contain any value contained in y
    */
  def disjoint(x: CPSetVar, y: CPSetVar): Constraint = {
    new Disjoint(x, y)
  }
}
