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
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.{CPPropagStrength, Constraint}

/**
 * Cardinality constraint on prefixes of a variable array
 * @param X The variables to be constrained
 * @param minVal The first value to be constrained; they are consecutive and their number is determined by the size of
 *               the bound lists; WARNING: at the moment the algorithm assumes that the variables only contain values
 *               in that interval...
 * @param lowerLists The lists of lower bounds for each value; for example (5,2) will mean that there has to be at least
 *                   two occurences of the value in the first five variables
 * @param upperLists The lists of upper bounds for each value; for example (6,3) will mean that there has to be at most
 *                   three occurrences of the value in the first six variables
 * @author Victor Lecomte
 */
class PrefixCCEmbedded(X: Array[CPIntVar], minVal: Int, lowerLists: Array[Array[(Int, Int)]],
                   upperLists: Array[Array[(Int, Int)]])
  extends Constraint(X(0).store, "PrefixCCEmbedded") {

  override def associatedVars(): Iterable[CPVar] = X

  // Handy structures for memorization.
  // They allow to have common code for lower bound and upper bound treatment.

  /**
   * Main memorization structure for lower/upper bounds.
   * Mainly it keeps the state of the segments between bounds and how many more variables can be assigned or removed
   * before some actions have to be triggered, but it is also used in the initialization process.
   */
  private class BoundsStructure() {
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

    val fwcFor = Array.ofDim[Array[FWCStructure]](nVariables)
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
  private[this] val lower = Array.fill(nValues)(new BoundsStructure())
  /** Memorization structure for the upper bounds */
  private[this] val upper = Array.fill(nValues)(new BoundsStructure())
  /** Lists of unbound variables for the values */
  private[this] val unbound = Array.fill(nValues)(new UnboundList())

  private class FWCStructure(val boundIdx: Int, val boundVal: Int, untilCritical: Int,
                             val list: UnboundList, val action: CPIntVar => Unit) {
    val untilCriticalRev = new ReversibleInt(s, untilCritical)

    def update(): Unit = {
      if (untilCriticalRev.decr() == 0) {
        var unboundI = list.firstRev.value
        while (unboundI < boundIdx) {
          action(X(unboundI))
          unboundI = list.nextRev(unboundI).value
        }
      }
    }
  }

  /** Change buffer used when copying value removals from a delta */
  private[this] var changeBuffer: Array[Int] = null

  private def printStatus(): Unit = {
    for ((x,i) <- X.zipWithIndex) {
      println(s"$i: ${x.toArray.mkString(" ")}")
    }
    println()
  }


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
  }

  /**
   * Copies the bounds given into the structure or fails if the bounds are unfeasible
   * @param st The structure into which to copy the bounds
   * @param boundList The given list of bounds
   * @param feasible The feasibility criteria in terms of index and value of the bound
   */
  private def readArguments(st: BoundsStructure, boundList: Array[(Int, Int)],
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
  private def filterGeneric(st: BoundsStructure,
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

    // Create the linked list of unbound variables
    val unboundTmp = Array.fill(nValues)(new TempList(nRelevantVariables))

    // Temporary tables for mandatory and possible
    val nMandatoryTmp = Array.ofDim[Int](nValues, nRelevantVariables + 1)
    val nPossibleTmp = Array.ofDim[Int](nValues, nRelevantVariables + 1)
    var vi = nValues
    while (vi > 0) {
      vi -= 1
      nMandatoryTmp(vi)(0) = 0
      nPossibleTmp(vi)(0) = 0
    }

    // Initial counting (the rest is done in the update functions)
    var bufferSize = 0
    var i = 0
    while (i < nRelevantVariables) {
      val x = X(i)

      // Count the number of variables that have the value
      var vi = nValues
      while (vi > 0) {
        vi -= 1
        nMandatoryTmp(vi)(i + 1) = nMandatoryTmp(vi)(i)
        nPossibleTmp(vi)(i + 1) = nPossibleTmp(vi)(i)
        val v = vi + minVal

        if (x.hasValue(v)) {
          nPossibleTmp(vi)(i + 1) += 1

          if (x.isBound) {
            nMandatoryTmp(vi)(i + 1) += 1
          } else {
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

      // Adapt the change buffer size
      bufferSize = bufferSize max x.size

      // Register before the first check loop so that we receive information on what we changed there
      x.callOnChangesIdx(i, delta => whenDomainChanges(delta, x))

      i += 1
    }

    changeBuffer = Array.ofDim(bufferSize)

    // First check loop
    vi = nValues
    while (vi > 0) {
      vi -= 1
      val v = vi + minVal
      val tmpList = unboundTmp(vi)
      //println(s"value $vi:")

      val criticalLower = (inter: Int) => nPossibleTmp(vi)(lower(vi).boundIdx(inter)) - lower(vi).boundVal(inter)
      val criticalUpper = (inter: Int) => upper(vi).boundVal(inter) - nMandatoryTmp(vi)(upper(vi).boundIdx(inter))

      initFwc(lower(vi), tmpList, unbound(vi), criticalLower, _.assign(v))
      initFwc(upper(vi), tmpList, unbound(vi), criticalUpper, _.removeValue(v))
    }

    // Copy the temporary values into the reversible arrays
    vi = nValues
    while (vi > 0) {
      vi -= 1
      copyListToRev(unbound(vi), unboundTmp(vi))
    }
  }

  private def initFwc(st: BoundsStructure, tmpList: TempList, list: UnboundList, untilCriticalIn: Int => Int,
                      action: CPIntVar => Unit): Unit = {
    import st._

    // Create the arrays
    var bound = nBounds
    var i = nRelevantVariables
    while (bound > 0) {
      bound -= 1
      while (i > boundIdx(bound)) {
        i -= 1
        fwcFor(i) = Array.ofDim(nIntervals - bound)
      }
    }

    // Create the FWC structures
    bound = nIntervals
    while (bound > 0) {
      //println(s"bound ${boundVal(bound)} at ${boundIdx(bound)}")
      val thisIdx = boundIdx(bound)
      val untilCritical = untilCriticalIn(bound)
      //println(s"until critical: $untilCritical")

      if (untilCritical < 0) {
        throw Inconsistency
      } else if (untilCritical == 0) {
        var unboundI = tmpList.first
        while (unboundI < thisIdx) {
          action(X(unboundI))
          unboundI = tmpList.next(unboundI)
        }
      }

      val fwcStructure = new FWCStructure(thisIdx, boundVal(bound), untilCritical, list, action)

      var i = thisIdx
      while (i > 0) {
        i -= 1
        fwcFor(i)(nIntervals - bound) = fwcStructure
      }
      bound -= 1
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

    //println(s"updating $i")
    //printStatus()

    // Treat the value removals
    var c = delta.fillArray(changeBuffer)
    while (c > 0) {
      c -= 1
      val v = changeBuffer(c)
      // If the value removed is one we track
      if (minVal <= v && v <= maxVal) {
        val vi = v - minVal
        removeUnbound(unbound(vi), i)

        val lowerBounds = lower(vi).fwcFor(i)
        var boundI = lowerBounds.length
        while (boundI > 0) {
          boundI -= 1
          lowerBounds(boundI).update()
        }
      }
    }

    if (x.isBound) {
      val v = x.min
      // If the value removed is one we track
      if (minVal <= v && v <= maxVal) {
        val vi = v - minVal
        removeUnbound(unbound(vi), i)

        val upperBounds = upper(vi).fwcFor(i)
        var boundI = upperBounds.length
        while (boundI > 0) {
          boundI -= 1
          //println("upper update")
          upperBounds(boundI).update()
        }
      }
    }
    false
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
}