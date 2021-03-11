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
package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.{CPPropagStrength, Constraint}

/**
 * Cardinality constraint on prefixes of a variable array.
 * See https://github.com/vlecomte/prefixcc-tech-report/blob/master/report.pdf for a technical report of the problem and
 * an explanation of the implementation.
 *
 * @param X The variables to be constrained
 * @param minVal The first value to be constrained: they are consecutive and their number is determined by the size of
 *               the bound lists. WARNING: the algorithm assumes that the variables only contain values in that
 *               interval!
 * @param lowerLists The lists of lower bounds for each value; for example (5,2) will mean that there has to be at least
 *                   two occurrences of the value in the first five variables
 * @param upperLists The lists of upper bounds for each value; for example (6,3) will mean that there has to be at most
 *                   three occurrences of the value in the first six variables
 * @author Victor Lecomte, Cyrille, Pierre
 */
class NestedGCCDecompFWC(X: Array[CPIntVar], minVal: Int, lowerLists: Array[Array[(Int, Int)]],
                  upperLists: Array[Array[(Int, Int)]])
  extends Constraint(X(0).store, "PrefixCCSegment") {

  override def associatedVars(): Iterable[CPVar] = X

  // Handy structures for memorization.
  // They allow to have common code for lower bound and upper bound treatment.

  /**
   * Main memorization structure for lower/upper bounds.
   * Mainly it keeps the state of the segments between bounds and how many more variables can be assigned or removed
   * before some actions have to be triggered, but it is also used in the initialization process.
   */
  private class SegmentStructure() {
    /** Used in the initialization process, is used to store the best known bound for every prefix. */
    var full: Array[Int] = null

    // The filtered bounds
    /** Ending point of the prefix for this bound */
    val boundIdx = Array.ofDim[Int](nVariables + 1)
    /** Value of this bound (i.e. minimal or maximal number of occurrences) */
    val boundVal = Array.ofDim[Int](nVariables + 1)
    /** Number of bounds */
    var nBounds = 0
    /** Number of intervals between the bounds */
    var nIntervals = 0
    /** Ending point of the largest prefix that is bound */
    var lastIdx = 0

  }



  /**
   * Temporary structure for the above list of unbound variables, used in the initialization process
   * @param size The number of variables this has to cover (will not always be [[nVariables]])
   */
  private class TempList(size: Int) {
    var first = size
    var last = -1
    val prev = Array.fill(size)(-1)
    val next = Array.fill(size)(size)
    val contains = Array.fill(size)(false)
  }

  /** Number of variables */
  private[this] val nVariables = X.length
  /** Number of variables that are actually constrained */
  private[this] var nRelevantVariables = 0
  /** Number of values for which we are given bounds */
  private[this] val nValues = lowerLists.length
  /** Largest value */
  private[this] val maxVal = minVal + nValues - 1

  /** Memorization structure for the lower bounds */
  private[this] val lower = Array.fill(nValues)(new SegmentStructure())
  /** Memorization structure for the upper bounds */
  private[this] val upper = Array.fill(nValues)(new SegmentStructure())



  // ============
  // INIT METHODS
  // ============

  override def setup(l: CPPropagStrength): Unit = {

    val feasibleLower = (index: Int, value: Int) => value <= index
    val feasibleUpper = (index: Int, value: Int) => value >= 0

    var vi = nValues
    while (vi > 0) {
      vi -= 1

      lower(vi).full = Array.tabulate(nVariables + 1)(i => 0)
      upper(vi).full = Array.tabulate(nVariables + 1)(i => i)

      readArguments(lower(vi), lowerLists(vi), feasibleLower)
      readArguments(upper(vi), upperLists(vi), feasibleUpper)
    }

    fillBounds()

    testAndDeduceBounds()


    filterBounds()


    val releventIdx: Set[Int] = (for (v <- 0 until nValues) yield {
      upper(v).boundIdx.take(upper(v).nBounds) ++ lower(v).boundIdx.take(lower(v).nBounds)
    }).flatten.toSet

    for (t <- releventIdx) {
      val uppers = Array.tabulate(nValues) (v => upper(v).full(t))
      val lowers = Array.tabulate(nValues) (v => lower(v).full(t))
      if (t > 0)
        s.post(new GCCFWC(X.take(t),minVal,lowers,uppers)) ;
    }
  }

  /**
   * Copies the bounds given into the structure or fails if the bounds are unfeasible
   * @param st The structure into which to copy the bounds
   * @param boundList The given list of bounds
   * @param feasible The feasibility criterion in terms of index and value of the bound
   */
  private def readArguments(st: SegmentStructure, boundList: Array[(Int, Int)],
                            feasible: (Int, Int) => Boolean): Unit = {
    var bound = boundList.length
    while (bound > 0) {
      bound -= 1
      val (index, value) = boundList(bound)
      if (index < 0 || index > nVariables) {
        throw new IllegalArgumentException("Bound cutoff out of range: " + index)
      } else if (!feasible(index, value)) {
        throw Inconsistency
      }
      st.full(index) = value
    }
  }

  // ---------
  // Filtering
  // ---------

  /**
   * Filters the bounds given in the input to keep only a minimal set of them that gives the same information.
   */
  private def filterBounds(): Unit = {
    nRelevantVariables = 0

    val filterFlat = (prevIdx: Int, prevVal: Int, nextIdx: Int, nextVal: Int) => nextVal > prevVal
    val filterSlope = (prevIdx: Int, prevVal: Int, nextIdx: Int, nextVal: Int) => nextVal - prevVal < nextIdx - prevIdx

    var vi = nValues
    while (vi > 0) {
      vi -= 1

      filterGeneric(lower(vi), filterSlope, filterFlat)
      filterGeneric(upper(vi), filterFlat, filterSlope)
      nRelevantVariables = nRelevantVariables max lower(vi).lastIdx max upper(vi).lastIdx
    }
  }

  /**
   * Filters bounds according to filtering criteria
   * @param st The structure into which the filtered bounds are to be put
   * @param prevFilter Returns true if the previous bound gives additional information wrt the next one
   * @param nextFilter Returns true if the next bound gives additional information wrt the previous one
   */
  private def filterGeneric(st: SegmentStructure,
                            prevFilter: (Int, Int, Int, Int) => Boolean,
                            nextFilter: (Int, Int, Int, Int) => Boolean): Unit = {
    import st._

    // Adding lower and upper bound 0 at 0, for convenience.
    boundIdx(0) = 0
    boundVal(0) = 0
    nBounds = 1

    // Convenience macros that give the last bound
    def lastIdx = boundIdx(nBounds - 1)
    def lastVal = boundVal(nBounds - 1)

    var i = 1
    while (i <= nVariables) {
      // If this new bound gives additional information
      if (nextFilter(lastIdx, lastVal, i, full(i))) {
        // Remove every previous bound that does not give additional information
        while (nBounds > 1 && !prevFilter(lastIdx, lastVal, i, full(i))) {
          nBounds -= 1
        }
        // Add the bound to the structure
        boundIdx(nBounds) = i
        boundVal(nBounds) = full(i)
        nBounds += 1
      }
      i += 1
    }

    nIntervals = nBounds - 1
    st.lastIdx = lastIdx
  }

  // -------
  // Filling
  // -------

  /**
   * Fills holes in the known bounds by deducing values on the left or the right of given bounds.
   * For example, if there is a maximum of 3 occurrences in the interval [0,5[, there will be a maximum of 3 in [0,4[
   * and of 4 in [0,6[.
   */
  private def fillBounds(): Unit = {
    var vi = nValues
    while (vi > 0) {
      vi -= 1

      var i = 0
      while (i < nVariables) {
        i += 1
        lower(vi).full(i) = lower(vi).full(i) max lower(vi).full(i - 1)
        upper(vi).full(i) = upper(vi).full(i) min (upper(vi).full(i - 1) + 1)
      }
      while (i > 0) {
        i -= 1
        lower(vi).full(i) = lower(vi).full(i) max (lower(vi).full(i + 1) - 1)
        upper(vi).full(i) = upper(vi).full(i) min upper(vi).full(i + 1)
      }
    }
  }

  // ---------
  // Deduction
  // ---------

  /**
   * Does some basic tests on the bounds and deduces bounds based on the bounds for other values
   */
  private def testAndDeduceBounds(): Unit = {
    var i = nVariables
    while (i > 0) {
      // Compute the sums
      var lowerSum = 0
      var upperSum = 0
      var vi = nValues
      while (vi > 0) {
        vi -= 1
        // The lower bound cannot be higher than the upper bound
        if (lower(vi).full(i) > upper(vi).full(i)) throw Inconsistency
        lowerSum += lower(vi).full(i)
        upperSum += upper(vi).full(i)
      }
      // Test the sums
      if (lowerSum > i || upperSum < i) throw Inconsistency
      // Deduce some bounds
      vi = nValues
      while (vi > 0) {
        vi -= 1
        // The lower bound will be at least the number of variables minus the sum of all other upper bounds
        lower(vi).full(i) = lower(vi).full(i) max (i - upperSum + upper(vi).full(i))
        // The upper bound will be at most the number of variables minus the sum of all other lower bounds
        upper(vi).full(i) = upper(vi).full(i) min (i - lowerSum + lower(vi).full(i))
      }
      i -= 1
    }
  }
}