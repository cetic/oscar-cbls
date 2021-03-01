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
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.delta.DeltaIntVar
import oscar.cp.core.{CPPropagStrength, Constraint}
import oscar.cp.core.variables.{CPIntVar, CPVar}

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
 * @author Victor Lecomte
 */
class NestedGCCFWC(X: Array[CPIntVar], minVal: Int, lowerLists: Array[Array[(Int, Int)]],
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

    // The segment structure itself
    /** The segment a variable belongs to */
    var intervalOf: Array[Int] = null
    /** The parent of an interval, if it has been merged with others */
    var parentRev: Array[ReversibleInt] = null
    /** The number of unbound removals before the segment reaches its critical point */
    var untilCriticalRev: Array[ReversibleInt] = null
    /** The previous segment (the interval on the left) */
    var prevRev: Array[ReversibleInt] = null
    /** The endpoint of the segment */
    var rightLimitRev: Array[ReversibleInt] = null
  }

  /**
   * Temporary structure for the reversible parts of the segment structure, used in the initialization process
   */
  private class TempStructure() {
    var parent: Array[Int] = null
    var untilCritical: Array[Int] = null
    var rightLimit: Array[Int] = null
  }

  /**
   * Doubly-linked list of unbound variables for a value, used when pruning
   */
  private class UnboundList() {
    /** Points to the first unbound variable for that value */
    var firstRev: ReversibleInt = null
    /** Points to the previous one */
    var prevRev: Array[ReversibleInt] = null
    /** Points to the next one */
    var nextRev: Array[ReversibleInt] = null
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
  /** Lists of unbound variables for the values */
  private[this] val unbound = Array.fill(nValues)(new UnboundList())

  /** Change buffer used when copying value removals from a delta */
  private[this] var changeBuffer: Array[Int] = null


  private[this] val Remove = 0
  private[this] val Assign = 1
  /*
  private[this] object DomOperation extends Enumeration {
    type DomOperation = Value
    val Remove, Assign = Value
  }
  import DomOperation._
  */

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

    initAndCheck()
    //Nothing more to do here, everything in closures or other constraints
    this.deactivate()
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

  // -------------------------
  // Initial counts and checks
  // -------------------------

  /**
   * Initializes the memorization structures and performs first checks
   */
  private def initAndCheck(): Unit = {

    val lowerTmp = Array.fill(nValues)(new TempStructure())
    val upperTmp = Array.fill(nValues)(new TempStructure())

    val criticalInitLower = (prevVal: Int, nextVal: Int) => prevVal - nextVal
    val criticalInitUpper = (prevVal: Int, nextVal: Int) => nextVal - prevVal

    // Give initial values to the structures
    var vi = nValues
    while (vi > 0) {
      vi -= 1
      initTemp(lower(vi), lowerTmp(vi), criticalInitLower)
      initTemp(upper(vi), upperTmp(vi), criticalInitUpper)
    }

    // Adapt the size of the buffer
    var bufferSize = 0
    var i = nRelevantVariables
    while (i > 0) {
      i -= 1
      bufferSize = bufferSize max X(i).size
    }
    changeBuffer = Array.ofDim[Int](bufferSize)

    // Create the linked list of unbound variables
    val unboundTmp = Array.fill(nValues)(new TempList(nRelevantVariables))
    initialCount(lowerTmp, upperTmp, unboundTmp)

    // Initial checks
    vi = nValues
    while (vi > 0) {
      vi -= 1
      val v = vi + minVal

      initialCheck(lower(vi), lowerTmp(vi), unboundTmp(vi), x => x.assign(v))
      initialCheck(upper(vi), upperTmp(vi), unboundTmp(vi), x => x.removeValue(v))
    }

    // Copy the temporary values into the reversible arrays
    vi = nValues
    while (vi > 0) {
      vi -= 1

      copyToRev(lower(vi), lowerTmp(vi))
      copyToRev(upper(vi), upperTmp(vi))
      copyListToRev(unbound(vi), unboundTmp(vi))
    }
  }

  /**
   * Performs the initial counting of bound and unbound variables for all values
   * @param lowerTmp The structure for the lower bounds
   * @param upperTmp The structure for the upper bounds
   * @param unboundTmp The list of unbound variables for each value
   */
  private def initialCount(lowerTmp: Array[TempStructure], upperTmp: Array[TempStructure],
                           unboundTmp: Array[TempList]): Unit = {
    var i = 0
    while (i < nRelevantVariables) {
      val x = X(i)

      if (x.isBound) {
        val v = x.min
        if (minVal <= v && v <= maxVal) {
          // The variable is bound to this value
          val vi = v - minVal
          if (i < lower(vi).lastIdx) {
            lowerTmp(vi).untilCritical(lower(vi).intervalOf(i)) += 1
          }
          if (i < upper(vi).lastIdx) {
            upperTmp(vi).untilCritical(upper(vi).intervalOf(i)) -= 1
          }
        }
      } else {
        var c = x.fillArray(changeBuffer)
        while (c > 0) {
          c -= 1
          val v = changeBuffer(c)
          if (minVal <= v && v <= maxVal) {
            // The variable has this value but is not bound to it
            val vi = v - minVal
            if (i < lower(vi).lastIdx) {
              lowerTmp(vi).untilCritical(lower(vi).intervalOf(i)) += 1
            }

            // Fill the linked list of unbound variables
            val list = unboundTmp(vi)
            list.contains(i) = true
            if (list.first == nRelevantVariables) {
              list.first = i
            } else {
              list.next(list.last) = i
              list.prev(i) = list.last
            }
            list.last = i
          }
        }
      }

      // Register before the first check loop so that we receive information on what we changed there
      x.callOnChangesIdx(i, delta => whenDomainChanges(delta, x))
      i += 1
    }
  }

  /**
   * Performs the initial checks on the feasibility of lower or upper bounds of a value, and assigns or removes values
   * from variables if necessary
   * @param st The segment structure (for fixed information)
   * @param tmp The temporary structure to work on
   * @param list The unbound list
   * @param action The action to perform when reaching the critical value on a leftmost interval
   */
  private def initialCheck(st: SegmentStructure, tmp: TempStructure, list: TempList,
                           action: CPIntVar => Unit): Unit = {
    import st._

    // Merge as much as possible, intentionally backwards!
    var inter = nIntervals
    while (inter > 1) {
      inter -= 1

      if (tmp.untilCritical(inter) <= 0) {
        tmp.parent(inter) = inter - 1
        tmp.untilCritical(inter - 1) += tmp.untilCritical(inter)
        tmp.rightLimit(inter - 1) = tmp.rightLimit(inter)
      }
    }

    if (nIntervals > 0) {
      // If some of the leftmost constraints are already decided
      if (tmp.untilCritical(0) < 0) throw Inconsistency
      else if (tmp.untilCritical(0) == 0) {
        // Try to assign or remove the unbound
        var i = list.first
        while (i < tmp.rightLimit(0)) {
          action(X(i))
          i = list.next(i)
        }

        // If this is the only interval remaining
        if (tmp.rightLimit(0) == lastIdx) {
          tmp.parent(0) = -1
        }
        // Otherwise, merge it to the right
        else {
          val next = intervalOf(tmp.rightLimit(0))
          tmp.parent(next) = 0
          tmp.untilCritical(0) = tmp.untilCritical(next)
          tmp.rightLimit(0) = tmp.rightLimit(next)
        }
      }
    }

    // Redirect variables
    inter = nIntervals
    var i = lastIdx
    while (inter > 0) {
      inter -= 1
      if (tmp.parent(inter) == inter) {
        while (i > boundIdx(inter)) {
          i -= 1
          intervalOf(i) = inter
        }
      }
    }
  }

  // ------------------------------
  // Temporary structure management
  // ------------------------------

  /**
   * Initializes a temporary segment structure
   * @param st The reversible segment structure
   * @param tmp The temporary segment structure
   * @param criticalInit Gives the initial value for untilCritical
   */
  private def initTemp(st: SegmentStructure, tmp: TempStructure, criticalInit: (Int, Int) => Int): Unit = {
    import st._

    intervalOf = Array.ofDim[Int](lastIdx)
    tmp.parent = Array.tabulate(nIntervals)(inter => inter)
    tmp.untilCritical = Array.tabulate(nIntervals)(inter => criticalInit(boundVal(inter), boundVal(inter + 1)))
    tmp.rightLimit = Array.tabulate(nIntervals)(inter => boundIdx(inter + 1))

    var inter = nIntervals
    var i = lastIdx
    while (inter > 0) {
      inter -= 1
      while (i > boundIdx(inter)) {
        i -= 1
        intervalOf(i) = inter
      }
    }
  }

  /**
   * Copies a temporary segment structure to the reversible one
   * @param st The reversible segment structure
   * @param tmp The temporary segment structure
   */
  private def copyToRev(st: SegmentStructure, tmp: TempStructure): Unit = {
    import st._

    // Initialize the reversible arrays
    parentRev = Array.ofDim[ReversibleInt](nIntervals)
    untilCriticalRev = Array.ofDim[ReversibleInt](nIntervals)
    prevRev = Array.ofDim[ReversibleInt](nIntervals)
    rightLimitRev = Array.ofDim[ReversibleInt](nIntervals)

    // Copy the temporary structures into them
    var inter = 0
    var prevInter = -1
    while (inter < nIntervals) {
      if (tmp.parent(inter) == inter) {
        parentRev(inter) = new ReversibleInt(s, tmp.parent(inter))
        untilCriticalRev(inter) = new ReversibleInt(s, tmp.untilCritical(inter))
        prevRev(inter) = new ReversibleInt(s, prevInter)
        rightLimitRev(inter) = new ReversibleInt(s, tmp.rightLimit(inter))
        prevInter = inter
      }
      inter += 1
    }
  }

  /**
   * Copies a temporary unbound variable list into a reversible one
   * @param list The reversible unbound list
   * @param tmpList The temporary unbound list
   */
  private def copyListToRev(list: UnboundList, tmpList: TempList): Unit = {
    // Copy the temporary unbound list into the reversible one
    list.firstRev = new ReversibleInt(s, tmpList.first)
    list.prevRev = Array.tabulate(nRelevantVariables)(i =>
      if (tmpList.contains(i)) new ReversibleInt(s, tmpList.prev(i))
      else null
    )
    list.nextRev = Array.tabulate(nRelevantVariables)(i =>
      if (tmpList.contains(i)) new ReversibleInt(s, tmpList.next(i))
      else null
    )
  }


  // ==============
  // UPDATE METHODS
  // ==============

  /**
   * Updates the structures and prunes according to the changes made on the variable
   * @param delta The values that were removed
   * @param x The variable they were removed from
   */
  @inline private def whenDomainChanges(delta: DeltaIntVar, x: CPIntVar): Boolean = {
    val i = delta.id

    // Treat the value removals
    var c = delta.fillArray(changeBuffer)
    while (c > 0) {
      c -= 1
      val v = changeBuffer(c)
      // If the value removed is one we track
      if (minVal <= v && v <= maxVal) {
        val vi = v - minVal
        onUpdate_(i, lower(vi), unbound(vi), otherVar => otherVar.assign(v))
      }
    }

    if (x.isBound) {
      val v = x.min
      // If the value removed is one we track
      if (minVal <= v && v <= maxVal) {
        val vi = v - minVal
        onUpdate_(i, upper(vi), unbound(vi), otherVar => otherVar.removeValue(v))
      }
    }

    false
  }

  /**
   * Generic update when the critical value of a segment decreases, merges segments if needed
   * @param i The index of the variable
   * @param st The segment structure
   * @param list The unbound list
   * @param action The action to be performed when the critical value of a leftmost segment is reached
   */

  @inline private def onUpdate(i: Int, st: SegmentStructure, list: UnboundList,
                               domOp: Int, v: Int): Unit = {
    import st._

    // If this variable is in one of the segments
    if (i < lastIdx) {
      removeUnbound(list, i)

      // Find the segment this variable is in
      val inter = findParent(parentRev, intervalOf(i))
      if (inter != -1) {
        val untilCritical = untilCriticalRev(inter).decr()

        // If the critical value is reached
        if (untilCritical == 0) {
          val directPrev = prevRev(inter).value

          // If this is the leftmost segment, we eliminate unbound and merge with the next interval
          if (directPrev == -1) {

            // Assign or remove every unbound
            val middleLimit = rightLimitRev(inter).value
            var unboundI = list.firstRev.value
            while (unboundI < middleLimit) {
              if (domOp == Remove) {
                X(unboundI).removeValue(v)
              } else { // assign
                X(unboundI).assign(v)
              }
              unboundI = list.nextRev(unboundI).value
            }

            // Merge with next if possible
            if (middleLimit != lastIdx) {
              val next = findParent(parentRev, intervalOf(middleLimit))

              // Compute limits to guess the preferable merge side
              val rightLimit = rightLimitRev(next).value

              // Left merge
              if (2 * middleLimit > rightLimit) {
                parentRev(next).setValue(inter)
                untilCriticalRev(inter).setValue(untilCriticalRev(next).value)
                rightLimitRev(inter).setValue(rightLimit)
              }
              // Right merge
              else {
                parentRev(inter).setValue(next)
                prevRev(next).setValue(-1)
              }
            }
          }

          // Otherwise, we merge with the previous interval
          else {
            val prev = findParent(parentRev, directPrev)

            // Compute limits to guess the preferable merge side
            val prevPrev = prevRev(prev).value
            val leftLimit = boundIdx(prevPrev + 1)
            val middleLimit = boundIdx(directPrev + 1)
            val rightLimit = rightLimitRev(inter).value

            // Left merge
            if (2 * middleLimit >= leftLimit + rightLimit) {
              parentRev(inter).setValue(prev)
              rightLimitRev(prev).setValue(rightLimit)
            }
            // Right merge
            else {
              parentRev(prev).setValue(inter)
              untilCriticalRev(inter).setValue(untilCriticalRev(prev).value)
              prevRev(inter).setValue(prevPrev)
            }
          }
        }
      }
    }
  }


  @inline private def onUpdate_(i: Int, st: SegmentStructure, list: UnboundList,
                               action: CPIntVar => Unit): Unit = {
    import st._

    // If this variable is in one of the segments
    if (i < lastIdx) {
      removeUnbound(list, i)

      // Find the segment this variable is in
      val inter = findParent(parentRev, intervalOf(i))
      if (inter != -1) {
        val untilCritical = untilCriticalRev(inter).decr()

        // If the critical value is reached
        if (untilCritical == 0) {
          val directPrev = prevRev(inter).value

          // If this is the leftmost segment, we eliminate unbound and merge with the next interval
          if (directPrev == -1) {

            // Assign or remove every unbound
            val middleLimit = rightLimitRev(inter).value
            var unboundI = list.firstRev.value
            while (unboundI < middleLimit) {
              action(X(unboundI))
              unboundI = list.nextRev(unboundI).value
            }

            // Mark right neighbor if it exists as new leftmost segment
            if (middleLimit != lastIdx) {
              val next = findParent(parentRev, intervalOf(middleLimit))
              prevRev(next).setValue(-1)
            }
          }

          // Otherwise, we merge with the previous interval
          else {
            val prev = findParent(parentRev, directPrev)

            // Compute limits to guess the preferable merge side
            val prevPrev = prevRev(prev).value
            val leftLimit = boundIdx(prevPrev + 1)
            val middleLimit = boundIdx(directPrev + 1)
            val rightLimit = rightLimitRev(inter).value

            // Left merge
            if (2 * middleLimit >= leftLimit + rightLimit) {
              parentRev(inter).setValue(prev)
              rightLimitRev(prev).setValue(rightLimit)
            }
            // Right merge
            else {
              parentRev(prev).setValue(inter)
              untilCriticalRev(inter).setValue(untilCriticalRev(prev).value)
              prevRev(inter).setValue(prevPrev)
            }
          }
        }
      }
    }
  }

  /**
   * Removes a variable from the list of unbound variables of a value
   * @param list The unbound variable list
   * @param i The index of the variable
   */
  @inline private def removeUnbound(list: UnboundList, i: Int): Unit = {
    import list._
    val prev = prevRev(i).value
    val next = nextRev(i).value

    if (prev == -1) {
      firstRev.setValue(next)
    } else {
      nextRev(prev).setValue(next)
    }
    if (next != nRelevantVariables) {
      prevRev(next).setValue(prev)
    }
  }

  /**
   * Finds the parent of a segment
   * @param parentTable The table of direct parents
   * @param child The segment whose parent we want to find
   * @return The id of the parent segment
   */
  @inline private def findParent(parentTable: Array[ReversibleInt], child: Int): Int = {
    val parent = parentTable(child).value
    if (parent == child || parent == -1) return parent
    findParent(parentTable, parent)
  }
}