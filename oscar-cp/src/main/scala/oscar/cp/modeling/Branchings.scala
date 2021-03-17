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

import oscar.algo.branchings._
import oscar.algo.search.{Branching, BranchingUtils, DiscrepancyBranching}
import oscar.cp.scheduling.search.SetTimesBranching
import oscar.cp.scheduling.search.RankBranching
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPSetVar
import oscar.algo.vars.{IntVarLike, SetVarLike}
import oscar.cp._
import oscar.cp.searches.WeightedDegreeHelper

/**
  * @author Pierre Schaus pschaus@gmail.com
  * @author Renaud Hartert ren.hartert@gmail.com
  */
trait Branchings extends BranchingUtils {
  @inline
  implicit def arrayCPIntToIntLike(orig: Array[CPIntVar]): Array[IntVarLike] = orig.asInstanceOf[Array[IntVarLike]]

  @inline
  implicit def arrayCPSetToSetLike(orig: Array[CPSetVar]): Array[SetVarLike] = orig.asInstanceOf[Array[SetVarLike]]


  /**
    * Binary search with custom variable/value heuristic
    *
    * @example {{{search(binaryIdx(x,i => x(i).size,i => x(i).min))}}}
    *
    * @param variables    Decision variables to branch on
    * @param varHeuristic Given an index in variables, returns an ordered value such that
    *                     the variable with the smallest one is selected first
    * @param valHeuristic Given an index i in variables,
    *                     returns the value in domain of variables(i) to be tried on the left branch,
    *                     this value is removed on the right branch
    * @param orderer
    * @tparam T
    * @return The variable-value heuristic specified by the parameters
    */
  def binaryIdx[T](variables: Array[CPIntVar], varHeuristic: Int => T, valHeuristic: Int => Int)(implicit orderer: T => Ordered[T]): Branching = {
    new BinaryBranching(variables.asInstanceOf[Array[IntVarLike]], varHeuristic, valHeuristic, orderer)
  }

  /**
    * Binary search with custom variable heuristic
    * @example {{{search(binaryIdx(x,i => x(i).size))}}}
    * @param variables    Decision variables to branch on
    * @param varHeuristic Given an index in variables, returns an ordered value such
    *                     that the unbound variable with the smallest one is selected first,
    *                     its min value is then tried on the left branch and removed on the right branch
    * @param orderer
    * @tparam T
    * @return The variable-value heuristic specified by the parameters
    */
  def binaryIdx[T](variables: Array[CPIntVar], varHeuristic: (Int => T))(implicit orderer: T => Ordered[T]): Branching = {
    binaryIdx(variables, varHeuristic, variables(_).min)
  }


  /**
    * Static ordered search, take the first unbound var in variables,
    * try its minimum value on the left branch and remove this value on the right branch
    *
    * @example {{{search(binary(x))}}}
    *
    * @param variables Decision variables to branch on
    * @return The variable-value heuristic specified by the parameters
    */
  def binary(variables: Array[CPIntVar]): Branching = {
    binaryIdx(variables, variables(_).min, variables(_).min)
  }

  /**
    *
    * @param variables Decision variables to branch on
    * @param varHeuris Given a variable in variables, returns a value (such as the domain size) such that
    *                  the unbound variable with the smallest one is selected first
    * @param valHeuris Given the selected variable,
    *                  returns the value in domain of variables(i) to be tried on the left branch,
    *                  this value is removed on the right branch
    * @return The variable-value heuristic specified by the parameters
    */
  def binary(variables: Traversable[CPIntVar], varHeuris: (CPIntVar => Int), valHeuris: (CPIntVar => Int)): Branching = {
    val vars = variables.toArray
    binaryIdx(vars, (i: Int) => varHeuris(vars(i)), (i: Int) => valHeuris(vars(i)))
  }

  /**
    * Variable Heuristic described in
    *
    * ''Reasoning from last conflict (s) in constraint programming,
    * C Lecoutre, L Saïs, S Tabary, V Vidal, 2009''
    *
    * In case a failure occurs after having selected a variable,
    * this variable is tried first until it eventually succeeds
    * and in this case a first-fail (min dom) fall back heuristic is used.
    *
    * @param variables Decision variables to branch on
    * @return The last conflict variable heuristic,
    *         with first fail fallback var heuristic and min-value for the value heuristic.
    */
  def binaryLastConflict(variables: Array[CPIntVar]): Branching = {
    binaryLastConflict(variables, variables(_).size, variables(_).min)
  }

  /**
    * Variable Heuristic described in
    *
    * ''Reasoning from last conflict (s) in constraint programming,
    * C Lecoutre, L Saïs, S Tabary, V Vidal, 2009''
    *
    * In case a failure occurs after having selected a variable,
    * this variable is tried first until it eventually succeeds
    * and in this case a first-fail (min dom) fall back heuristic is used.
    *
    * @param variables    Decision variables to branch on
    * @param varHeuristic Fallback heuristic: given an index in variables, returns an ordered value such that
    *                     the variable with the smallest one is selected first
    * @param orderer
    * @tparam T
    * @return The last conflict  custom variable heuristic,
    *         and min value in domain value heuristic.
    */
  def binaryLastConflict[T](variables: Array[CPIntVar], varHeuristic: (Int => T))(implicit orderer: T => Ordered[T]): Branching = {
    binaryLastConflict(variables, varHeuristic, variables(_).min)(orderer)
  }

  /**
    * Variable Heuristic described in
    *
    * ''Reasoning from last conflict (s) in constraint programming,
    * C Lecoutre, L Saïs, S Tabary, V Vidal, 2009''
    *
    * In case a failure occurs after having selected a variable,
    * this variable is tried first until it eventually succeeds
    * and in this case a first-fail (min dom) fall back heuristic is used.
    *
    * @param variables    Decision variables to branch on
    * @param varHeuristic Fallback heuristic: given an index in variables, returns an ordered value such that
    *                     the variable with the smallest one is selected first
    * @param valHeuristic Given an index i in variables,
    *                     Returns the value in domain of variables(i) to be tried on the left branch,
    *                     this value is removed on the right branch
    * @param orderer
    * @tparam T
    * @return The last conflict variable heuristic,
    *         with custom fallback var heuristic and custom value heuristic.
    */
  def binaryLastConflict[T](variables: Array[CPIntVar], varHeuristic: (Int => T), valHeuristic: (Int => Int))(implicit orderer: T => Ordered[T]): Branching = {
    new BinaryLastConflict[T](variables.asInstanceOf[Array[IntVarLike]], varHeuristic, valHeuristic, orderer)
  }

  /**
    * Variable Heuristic described in
    *
    * ''Reasoning from last conflict (s) in constraint programming,
    * C Lecoutre, L Saïs, S Tabary, V Vidal, 2009''
    *
    * In case a failure occurs after having selected a variable,
    * this variable is tried first until it eventually succeeds
    * and in this case a first-fail (min dom) fall back heuristic is used.
    *
    * The split value is the median of the domain.
    *  - ''= median'' on the left branch
    *  - ''> median'' on the right branch
    *
    * @param variables    Decision variables to branch on
    * @return The last conflict variable heuristic,
    *         with first fail fallback var heuristic and median value split heuristic.
    */
  def splitLastConflictFirstFail(variables: Array[CPIntVar]): Branching = {
    splitLastConflict(variables, variables(_).size, variables(_).min)
  }

  /**
    * Variable Heuristic described in
    *
    * ''Reasoning from last conflict (s) in constraint programming,
    * C Lecoutre, L Saïs, S Tabary, V Vidal, 2009''
    *
    * In case a failure occurs after having selected a variable,
    * this variable is tried first until it eventually succeeds
    * and in this case a first-fail (min dom) fall back heuristic is used.
    *
    * The split value is the median of the domain.
    *  - ''= median'' on the left branch
    *  - ''> median'' on the right branch
    *
    * @param variables    Decision variables to branch on
    * @param varHeuristic Fallback heuristic: given an index in variables, returns a value (e.g. the domain size) such that
    *                     the variable with the smallest one is selected first
    * @return The last conflict variable heuristic,
    *         with first fail fallback var heuristic and median value split heuristic.
    */
  def splitLastConflict(variables: Array[CPIntVar], varHeuristic: (Int => Int)): Branching = {
    splitLastConflict(variables, varHeuristic, variables(_).median)
  }

  /**
    * Variable Heuristic described in
    *
    * ''Reasoning from last conflict (s) in constraint programming,
    * C Lecoutre, L Saïs, S Tabary, V Vidal, 2009''
    *
    * In case a failure occurs after having selected a variable,
    * this variable is tried first until it eventually succeeds
    * and in this case a first-fail (min dom) fall back heuristic is used.
    *
    * @param variables    Decision variables to branch on
    * @param varHeuristic Fallback heuristic: given an index in variables, returns a value (e.g. the domain size) such that
    *                     the variable with the smallest one is selected first
    * @param valHeuristic Given an index i in variables,
    *                     returns a value v in domain of variables(i).
    *                     The constraint ''<= v''  is tried on the left branch,
    *                     and the constraint ''> v'' on the right branch
    * @return The last conflict variable heuristic,
    *         with first fail fallback var heuristic and custom value split heuristic.
    */
  def splitLastConflict(variables: Array[CPIntVar], varHeuristic: (Int => Int), valHeuristic: (Int => Int)): Branching = {
    new SplitLastConflict(variables.asInstanceOf[Array[IntVarLike]], varHeuristic, valHeuristic)
  }

  /**
    * Variable Heuristic called COS described in
    *
    * ''Conflict ordering search for scheduling problems
    * S Gay, R Hartert, C Lecoutre, P Schaus, 2015''
    *
    * Each variable has a time-stamp according to its last failure.
    * Variables are first tried according to the latest time-stamp as variable heuristic.
    * In case none of the unbound variable has yet caused a failure a fallback heuristic is used.
    *
    * @param variables    Decision variables to branch on
    * @param varHeuristic fallback heuristic: given an index in variables, returns an ordered value such that
    *                     the variable with the smallest one is selected first
    * @param valHeuristic given an index i in variables,
    *                     returns the value in domain of variables(i) to be tried on the left branch,
    *                     this value is removed on the right branch
    * @param orderer
    * @tparam T
    * @return The conflict Ordering Search (COS) variable heuristic,
    *         with first fail fallback var heuristic and custom value value heuristic.
    */
  def conflictOrderingSearch[T](variables: Array[CPIntVar], varHeuristic: (Int) => T, valHeuristic: (Int) => Int)(implicit orderer: T => Ordered[T]): Branching = {
    new ConflictOrderingSearch(variables.asInstanceOf[Array[IntVarLike]], varHeuristic, valHeuristic, orderer)
  }


  /**
    * Binary Search on the decision variables vars with fixed static ordering.
    * The next variable to assign is the first unbound variable in vars.
    *
    * @param variables    Decision variables to branch on
    * @param valHeuristic Given an index i in variables,
    *                     returns the value in domain of variables(i) to be tried on the left branch,
    *                     this value is removed on the right branch
    * @return A static variable heuristic with custom value value heuristic
    */
  def binaryStaticIdx(variables: Seq[CPIntVar], valHeuristic: Int => Int): Branching = new BinaryStaticOrderBranching(variables.toArray, valHeuristic)


  /**
    * Binary Search on the decision variables vars with fixed static ordering,
    * and min value in domain value heuristic.
    * The next variable to assign is the first unbound variable in vars.
    *
    * @param variables Decision variables to branch on
    * @return A static variable heuristic with with min-value in domain as value heuristic
    */
  def binaryStatic(variables: Seq[CPIntVar]): Branching = binaryStaticIdx(variables, i => variables(i).min)

  /**
    * Binary First Fail (min dom size) on the decision variables vars with custom value heuristic
    *
    * @param variables Decision variables to branch on
    * @param valHeuris Given an index i in variables, returns the value v to try
    *                  on left branch for the chosen variable, this value is removed on the right branch
    * @return A first fail variable heuristic with custom value heuristic
    */
  def binaryFirstFailIdx(variables: Seq[CPIntVar], valHeuris: (Int => Int)): Branching = {
    val vars = variables.toArray
    binaryIdx(vars, i => (vars(i).size, i), valHeuris)
  }

  /**
    * Binary First Fail (min dom size) on the decision variables vars with
    * min value in domain as value heuritic
    *
    * @param variables Decision variables to branch on
    * @return A first fail variable heuristic with min value in domain value heuristic
    */
  def binaryFirstFail(variables: Seq[CPIntVar]): Branching = {
    val vars = variables.toArray
    binaryFirstFailIdx(vars, vars(_).min)
  }

  /**
    * Binary First Fail (min dom size) on the decision variables vars with custom value heuristic
    *
    * @param variables Decision variables to branch on
    * @param valHeuris Given an variable in variables, returns the value v to try
    *                  on left branch for the chosen variable, this value is removed on the right branch
    * @return A first fail variable heuristic with custom value heuristic
    */
  def binaryFirstFail(variables: Seq[CPIntVar], valHeuris: (CPIntVar => Int)): Branching = {
    val vars = variables.toArray
    binaryFirstFailIdx(vars, i => valHeuris(vars(i)))
  }


  /**
    * Select first the unbound variable with the max number of propagator attached (max-degree heuristic)
    *
    * @param variables Decision variables to branch on
    * @return A max-degree variable heuristic with min value in domain value heuristic
    */
  def binaryMaxDegree(variables: Seq[CPIntVar]): Branching = {
    val vars = variables.toArray
    binaryIdx(vars, -vars(_).constraintDegree, vars(_).min)
  }

  /**
    * Heuristic Decribed in:
    *
    * ''Boosting systematic search by weighting constraints
    * Frédéric Boussemart, Fred Hemery, Christophe Lecoutre, Lakhdar Sais, 2004''
    *
    * Binary Search based on the weighted degree of each variable,
    * the variable with the greater degree being selected first.
    * The weighted degree of a var is the number of times a constraint
    * to which it is linked has been involved in a failure.
    *
    * @param variables  Decision variables to branch on
    * @param valHeuris  Given an variable in variables, returns the value v to try
    *                   on left branch for the chosen variable, this value is removed on the right branch
    * @param decayRatio is applied to the weight of each variable at each failure
    * @return A weighted-degree variable heuristic with custom value heuristic
    */
  def binaryMaxWeightedDegree(variables: Seq[CPIntVar], valHeuris: (CPIntVar => Int), decayRatio: Double): Branching = {
    val vars = variables.toArray
    val helper = new WeightedDegreeHelper(vars.head.store, vars, decayRatio)

    //TODO find a better way to convert the double to an Int
    binaryIdx(vars, i => -(helper.getWeightedDegree(vars(i)) * 1000).round.toInt, i => valHeuris(vars(i)))
  }

  /**
    * Heuristic Decribed in:
    *
    * ''Boosting systematic search by weighting constraints
    * Frédéric Boussemart, Fred Hemery, Christophe Lecoutre, Lakhdar Sais, 2004''
    *
    * Binary Search based on the weighted degree of each variable,
    * the unbound variable with the greater degree being selected first.
    * The weighted degree of a var is the number of times a constraint
    * to which it is linked has been involved in a failure.
    *
    * @param variables  Decision variables to branch on
    * @param decayRatio is applied to the weight of each variable at each failure
    * @return A weighted-degree variable heuristic with a min value in domain value heuristic
    */
  def binaryMaxWeightedDegree(variables: Seq[CPIntVar], decayRatio: Double = 0.99): Branching = {
    binaryMaxWeightedDegree(variables, x => x.min, decayRatio)
  }


  /**
    * Heuristic Decribed in:
    *
    * ''Boosting systematic search by weighting constraints
    * Frédéric Boussemart, Fred Hemery, Christophe Lecoutre, Lakhdar Sais, 2004''
    *
    * Binary Search based on the weighted degree of each variable,
    * the unbound variable with the smallest ratio (domain-size/weighted-degree) selected first.
    * The weighted degree of a var is the number of times a constraint
    * to which it is linked has been involved in a failure.
    *
    * @param variables  Decision variables to branch on
    * @param decayRatio is applied to the weight of each variable at each failure
    * @param valHeuris  Given an variable in variables, returns the value v to try
    *                   on left branch for the chosen variable, this value is removed on the right branch
    * @return A min dom over weighted-degree variable heuristic with a custom value heuristic
    */
  def binaryMinDomOnWeightedDegree(variables: Seq[CPIntVar], valHeuris: (CPIntVar => Int), decayRatio: Double): Branching = {
    val vars = variables.toArray
    val helper = new WeightedDegreeHelper(vars.head.store, vars, decayRatio)

    //TODO find a better way to convert the double to an Int
    binaryIdx(vars, i => (helper.getDomOnWeightedDegree(vars(i)) * 1000).round.toInt, i => valHeuris(vars(i)))
  }


  /**
    * Heuristic Decribed in:
    *
    * ''Boosting systematic search by weighting constraints
    * Frédéric Boussemart, Fred Hemery, Christophe Lecoutre, Lakhdar Sais, 2004''
    *
    * Binary Search based on the weighted degree of each variable,
    * the unbound variable with the smallest ratio (domain-size/weighted-degree) selected first.
    * The weighted degree of a var is the number of times a constraint
    * to which it is linked has been involved in a failure.
    *
    * @param variables  Decision variables to branch on
    * @param decayRatio is applied to the weight of each variable at each failure
    * @return A min dom over weighted-degree variable heuristic with a min-value in domain value heuristic
    */
  def binaryMinDomOnWeightedDegree(variables: Seq[CPIntVar], decayRatio: Double = 0.99): Branching = {
    binaryMinDomOnWeightedDegree(variables, x => x.min, decayRatio)
  }

  /**
    * Binary search on the decision variables vars, splitting the domain of the selected variable on the
    * median of the values (left : <= median, right : > median)
    */


  /**
    * Binary Split search with custom variable/value heuristic
    *
    * @param variables    Decision variables to branch on
    * @param varHeuristic Given an index in variables, returns an ordered value such that
    *                     the variable with the smallest one is selected first
    * @param valHeuristic Given an index i in variables,
    *                     returns a value v, the constraint ''<= v'' is tried on the left branch,
    *                     and ''> v'' is tried on the right branch
    * @param orderer
    * @tparam T
    * @return The variable-value-split heuristic specified by the parameters
    */
  def binarySplitIdx[T](variables: Seq[CPIntVar], varHeuristic: (Int => T), valHeuristic: (Int => Int))(implicit orderer: T => Ordered[T]): Branching = {
    val xa = variables.toArray.asInstanceOf[Array[IntVarLike]]
    new BinaryDomainSplitBranching(xa, varHeuristic, valHeuristic, orderer)
  }

  /**
    * Binary Split search with custom variable heuristic
    * and (min+max)/2 split value heuristic.
    *
    * @param variables    Decision variables to branch on
    * @param varHeuristic Given an index in variables, returns an ordered value such that
    *                     the variable with the smallest one is selected first
    * @param orderer
    * @tparam T
    * @return The variable-value-split heuristic specified by the parameters
    */
  def binarySplitIdx[T](variables: Seq[CPIntVar], varHeuristic: (Int => T))(implicit orderer: T => Ordered[T]): Branching = {
    val xa = variables.toArray.asInstanceOf[Array[IntVarLike]]
    new BinaryDomainSplitBranching(xa, varHeuristic, orderer)
  }

  /**
    * Binary Split search with custom variable/value heuristic
    *
    * @param variables    Decision variables to branch on
    * @param varHeuristic Given a variable in variables, returns a value such that
    *                     the variable with the smallest one is selected first
    * @param valHeuristic Given an index i in variables,
    *                     returns a value v, the constraint ''<= v'' is tried on the left branch,
    *                     and ''> v'' is tried on the right branch
    * @return The variable-value-split heuristic specified by the parameters
    */
  def binarySplit(variables: Seq[CPIntVar], varHeuristic: (CPIntVar => Int), valHeuristic: (CPIntVar => Int)): Branching = {
    binarySplitIdx(variables, i => varHeuristic(variables(i)), i => valHeuristic(variables(i)))
  }

  /**
    * Binary Split search with custom variable heuristic
    * and (min+max)/2 split value heuristic.
    *
    * @param variables    Decision variables to branch on
    * @param varHeuristic Given a variable in variables, returns a value such that
    *                     the variable with the smallest one is selected first
    * @return The variable-value-split heuristic specified by the parameters
    */
  def binarySplit(variables: Seq[CPIntVar], varHeuristic: (CPIntVar => Int)): Branching = {
    binarySplitIdx(variables, i => varHeuristic(variables(i)))
  }

  /**
    * Binary Split search with static variable heuristic as
    * specified in the order in variables
    * and (min+max)/2 split value heuristic.
    *
    * @param variables Decision variables to branch on
    * @return The variable-value-split heuristic specified by the parameters
    */
  def binarySplit(variables: Seq[CPIntVar]): Branching = {
    binarySplitIdx(variables, i => i)
  }

  /**
    * Binary Search on a set variable
    * forcing an arbitrary value to be part of the set the left branch,
    * and removing this value from the set on the right branch until the variable is bound
    */
  def binary(x: CPSetVar): Branching = {
    new BinarySetBranching(x)
  }

  /**
    * Represents a dependency in the search between a boolean selection variable and other optional variables.
    * The optional variables have to be bound only if the selection variable is set to true.
    */
  type CPOptionalSelection = oscar.algo.search.OptionalSelection[CPBoolVar, CPIntVar]
  final val CPOptionalSelection = oscar.algo.search.OptionalSelection

  /**
    * Branching that maximizes optional selections:
    * When branching on a selection variable, it creates two child nodes, left=true, right=false.
    * On the true (left) branch, all the optional variables must be assigned before attempting a next selection.
    * On the false (right) branch, the optional variables are not branched on at all.
    *
    * @param vars The OptionalSelection objects linking the boolean selection variables with their optional variables
    * @param selectionBranching The branching to be used on the selection variables
    */
  def maxSelection(vars: Seq[CPOptionalSelection], selectionBranching: Branching): Branching = {
    new MaxSelectionBranching(vars, selectionBranching)
  }

  /**
    * Set times heuristic (for discrete resources)
    * This heursitic was described in
    *
    * ''Time- versus-capacity compromises in project scheduling. (Le Pape et al.). 1994.''
    */
  def setTimes(starts: IndexedSeq[CPIntVar], durations: IndexedSeq[CPIntVar], ends: IndexedSeq[CPIntVar], tieBreaker: Int => Int = (i: Int) => i): Branching = new SetTimesBranching(starts, durations, ends, tieBreaker)

  /**
    * Rank heuristic (for unary resources)
    * Try to find a total order on the activities
    */
  def rank[T](starts: IndexedSeq[CPIntVar], durations: IndexedSeq[CPIntVar], ends: IndexedSeq[CPIntVar], by: Int => T)(implicit orderer: T => Ordered[T]): Branching = new RankBranching(starts, durations, ends, by)

  def rank(starts: IndexedSeq[CPIntVar], durations: IndexedSeq[CPIntVar], ends: IndexedSeq[CPIntVar]): Branching = {
    rank(starts, durations, ends, (i: Int) => ends(i).max)
  }


  /**
    * Impose a limit on the number of right decisions (called discrapancy).
    * The left-most decision has a discrepancy 0, the second left-most 1, etc.
    * Each node with a total accumulated discrepancy ''> maxDiscrepency'' on the ancestor decisions
    * is pruned.
    *
    * @example {{{search(discrepency(binaryFirstFail(queens),3))}}}
    *
    * @param branching      The search strategy on which the discrepancy limit should be applied
    * @param maxDiscrepancy The discrepancy limit
    * @return Same branching behavior as as branching
    *         but with the nodes having a discrepancy ''> maxDiscrepancy'' cut-off
    */
  def discrepancy(branching: Branching, maxDiscrepancy: Int): Branching = {
    new DiscrepancyBranching(branching, maxDiscrepancy)
  }

  /**
    * {{{x.size}}}
    *
    * @example  {{{search(binary(variables,minDom,minVal))}}}
    *
    * @param x
    * @return {{{x.size}}}
    */
  def minDom(x: CPIntVar): Int = x.size


  /**
    * {{{ i => x(i).size }}}
    *
    * @example  {{{search(binaryIdx(x,minDom(x),minVal(x)))}}}
    *
    * @param x
    * @return {{{ i => x(i).size }}}
    */
  def minDom(x: Array[CPIntVar]): Int => Int = i => x(i).size

  /**
    * {{{x.max-x.min}}}
    *
    * @example  {{{search(binary(variables,minRegret,minVal))}}}
    *
    * @param x
    * @return {{{x.max - x.min}}}
    */
  def minRegret(x: CPIntVar): Int = x.max - x.min

  /**
    * {{{ i => x(i).max-x(i).min }}}
    *
    * @example  {{{search(binaryIdx(x,minRegret(x),minVal(x)))}}}
    *
    * @param x
    * @return {{{ i => x(i).max-x(i).min }}}
    */
  def minRegret(x: Array[CPIntVar]): Int => Int = i => x(i).max - x(i).min

  /**
    * {{{(x.size, -x.constraintDegree)}}}
    *
    * @example  {{{search(binary(variables, minDomMaxDegree,minVal))}}}
    *
    * @param x
    * @return {{{(x.size, -x.constraintDegree)}}}
    */
  def minDomMaxDegree(x: CPIntVar): (Int, Int) = (x.size, -x.constraintDegree)

  /**
    * {{{ (i => (x(i).size, -x(i).constraintDegree)) }}}
    *
    * @example  {{{search(binaryIdx(x,minDomMaxDegree(x),minVal(x)))}}}
    *
    * @param x
    * @return {{{ (i => (x(i).size, -x(i).constraintDegree)) }}}
    */
  def minDomMaxDegree(x: Array[CPIntVar]): Int => (Int, Int) = (i => (x(i).size, -x(i).constraintDegree))

  /**
    * {{{-x.constraintDegree)}}}
    *
    * @example  {{{search(binary(variables,maxDegree,minVal))}}}
    *
    * @param x
    * @return {{{-x.constraintDegree}}}
    */
  def maxDegree(x: CPIntVar): Int = -x.constraintDegree

  /**
    * {{{i => -x(i).constraintDegree }}}
    *
    * @example  {{{search(binaryIdx(x,maxDegree(x),minVal(x)))}}}
    *
    * @param x
    * @return {{{ (i => -x(i).constraintDegree }}}
    */
  def maxDegree(x: Array[CPIntVar]) = (i: Int) => -x(i).constraintDegree


  /**
    * {{{x.min}}}
    *
    * @example  {{{search(binary(variables,minDom,minVal))}}}
    *
    * @param x
    * @return {{{x.min}}}
    */
  def minVal(x: CPIntVar): Int = x.min

  /**
    * {{{ i => x(i).min }}}
    *
    * @example  {{{search(binaryIdx(x,minDom(x),minVal(x)))}}}
    *
    * @param x
    * @return {{{ i => x(i).min }}}
    */
  def minVal(x: Array[CPIntVar]) = (i: Int) => x(i).min

  /**
    * {{{x.max}}}
    *
    * @example  {{{search(binary(variables,minDom,maxVal))}}}
    *
    * @param x
    * @return {{{x.max}}}
    */
  def maxVal(x: CPIntVar): Int = x.max

  /**
    * {{{ i => x(i).max }}}
    *
    * @example  {{{search(binaryIdx(x,minDom(x),maxVal(x)))}}}
    *
    * @param x
    * @return {{{ i => x(i).max }}}
    */
  def maxVal(x: Array[CPIntVar]) = (i: Int) => x(i).max

  /**
    * @author Pierre Schaus pschaus@gmail.com
    * @param x
    * @param fallBackValHeuristic
    */
  class ValueHeuristicLearner(x: Array[CPIntVar], fallBackValHeuristic: (Int => Int)) {
    private[this] val lastValues = Array.fill(x.length)(Int.MinValue)

    x(0).store.onPush {
      for (i <- 0 until x.length) {
        if (x(i).isBound) {
          lastValues(i) = x(i).min
        }
      }
    }

    def valueHeuristic(i: Int): Int = {
      if (x(i).hasValue(lastValues(i))) {
        lastValues(i)
      } else {
        fallBackValHeuristic(i)
      }
    }
  }

  /**
    * Value Heuristic wrapper that will try to learn a successful heuristic
    * When a value succeeds, it is recorded and first attempted
    * whenever it is possible, otherwise it uses the default value heuristic
    *
    * @example {{{conflictOrderingSearch(x, i => (x(i).min, i), learnValueHeuristic(x, i => x(i).min))}}}
    *
    * @param variables            the variables on which the value heuristic is applied
    * @param fallBackValHeuristic i => v where i is the variable index, v the value in the domain of x(i)
    * @return a value heuristic i => v where i is the variable index, v is the value in the domain of x(i)
    */
  def learnValueHeuristic(variables: Array[CPIntVar], fallBackValHeuristic: (Int => Int)): (Int => Int) = {
    new ValueHeuristicLearner(variables, fallBackValHeuristic).valueHeuristic
  }
}
