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

import oscar.algo.reversible._
import oscar.cp.core._
import oscar.cp.core.delta._
import oscar.cp.core.variables._

/**
 * Global Cardinality Constraint with violation.
 * Only the maximum of the violation variable is taken into account, and only its minimum is updated.
 *
 * @param X The variables to constraint (at least one)
 * @param minVal The smallest value in the interval; its size is determined by the size of lower and upper
 * @param lower The lower bounds for the occurrences of the values of the interval, in order
 * @param upper The upper bounds for the occurrences of the values of the interval, in order
 * @param viol The violation of the constraints, i.e. the sum of the distances under lower bounds and over upper bounds
 * @see SoftGCC, the AC version of this constraint.
 *
 * @author Victor Lecomte
 */
class SoftGCCFWC(X: Array[CPIntVar], minVal: Int, lower: Array[Int], upper: Array[Int], viol: CPIntVar)
  extends Constraint(X(0).store, "SoftGCCFWC") {

  override def associatedVars(): Iterable[CPVar] = X ++ Array(viol)

  idempotent = false

  private[this] val nValues = lower.length
  private[this] val maxVal = minVal + nValues - 1
  private[this] val nVariables = X.length

  // MEMORIZATION STRUCTURE:
  // The number of variables that are bound to the value
  private[this] var nMandatoryRev: Array[ReversibleInt] = null
  // The number of variables that have the value
  private[this] var nPossibleRev: Array[ReversibleInt] = null

  // The sparse set to memorize the variables having a value that is unbound
  private[this] val unboundSet = Array.ofDim[Int](nValues, nVariables)
  private[this] val unboundIndex = Array.ofDim[Int](nValues, nVariables)

  // The best-known minimal/effective violation right now
  private[this] var minViolRev: ReversibleInt = null

  // Whether we reached the max violation and we should prune
  private[this] var atMaxViolRev: ReversibleBoolean = null

  // Values for which the number of possible variables is under the lower bound
  private[this] var nValuesInLackRev: ReversibleInt = null
  private[this] val inLack = Array.ofDim[Int](nValues)
  // Values for which the number of mandatory variables is over the upper bound
  private[this] var nValuesInExcessRev: ReversibleInt = null
  private[this] val inExcess = Array.ofDim[Int](nValues)

  // Change buffer to load the deltas
  private[this] var changeBuffer: Array[Int] = null

  override def setup(l: CPPropagStrength): Unit = {

    // Temporary variables to avoid using reversible variables too much
    val nMandatory = new Array[Int](nValues)
    val nPossible = new Array[Int](nValues)
    var minViol = 0
    var nValuesInLack = 0
    var nValuesInExcess = 0

    // Initial counting (the rest is done in the update functions)
    var bufferSize = 0
    var i = nVariables
    while (i > 0) {
      i -= 1
      val x = X(i)

      // Count the number of variables that are bound to the value
      if (x.isBound) {
        val v = x.min
        if (minVal <= v && v <= maxVal) {
          val vi = v - minVal
          nMandatory(vi) += 1
        }
      }
      // Count the number of variables that have the value
      var vi = nValues
      while (vi > 0) {
        vi -= 1
        if (x.hasValue(vi + minVal)) {
          nPossible(vi) += 1

          // If x is unbound, add it to the value's sparse set
          if (!x.isBound) {
            val index = nPossible(vi) - nMandatory(vi) - 1
            unboundSet(vi)(index) = i
            unboundIndex(vi)(i) = index
          }
        }
      }

      // Adapt the change buffer size
      if (bufferSize < x.size) {
        bufferSize = x.size
      }

      // Register before the first check loop so that we receive information on what we changed there
      x.callOnChangesIdx(i, delta => whenDomainChanges(delta, x))
    }

    // First violation counting
    var vi = nValues
    while (vi > 0) {
      vi -= 1

      // Ensure that lower <= upper
      if (lower(vi) > upper(vi)) {
        minViol += lower(vi) - upper(vi)
        val tmp = lower(vi)
        lower(vi) = upper(vi)
        upper(vi) = tmp
      }

      // Count and remember effective violations
      if (nPossible(vi) <= lower(vi)) {
        inLack(nValuesInLack) = vi
        nValuesInLack += 1
        minViol += lower(vi) - nPossible(vi)
      }
      if (nMandatory(vi) >= upper(vi)) {
        inExcess(nValuesInExcess) = vi
        nValuesInExcess += 1
        minViol += nMandatory(vi) - upper(vi)
      }
    }

    // First violation check
    viol.updateMin(minViol)

    // Initialize the memorization structure
    implicit val context = s
    nMandatoryRev = Array.tabulate(nValues)(vi => ReversibleInt(nMandatory(vi)))
    nPossibleRev = Array.tabulate(nValues)(vi => ReversibleInt(nPossible(vi)))
    minViolRev = ReversibleInt(minViol)
    atMaxViolRev = new ReversibleBoolean(s, false)
    nValuesInLackRev = ReversibleInt(nValuesInLack)
    nValuesInExcessRev = ReversibleInt(nValuesInExcess)
    
    // If we have reached the maximal violation
    if (minViol == viol.max) {
      whenMaxViolReached()
    }

    // Detect when the maximal violation is externally reduced
    viol.filterWhenBind(idempot = true) {
      if (!atMaxViolRev.value && minViolRev.value == viol.max) {
        whenMaxViolReached()
        false
      } else {
        true
      }
    }

    // Create the buffer
    changeBuffer = new Array(bufferSize)

    // Everything is up to the closures now
    this.deactivate()
  }

  /**
   * Propagates when the domain of a variable in [[X]] changes.
   * @param delta The domain changes
   * @param x The variable that changed
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
        val nPossible = nPossibleRev(vi).decr()
        val nMandatory = nMandatoryRev(vi).value
        val nUnbound = nPossible - nMandatory

        removeUnbound(i, vi, nUnbound)

        // If too few variables have the value
        if (nPossible <= lower(vi)) {
          // If we cannot violate anymore
          if (atMaxViolRev.value) {
            // Assign the unbound
            eliminateUnbound(vi, nUnbound, _.assign(v))
          } else {
            // If we only reach the limit, remember this value
            if (nPossible == lower(vi)) {
              inLack(nValuesInLackRev.incr() - 1) = vi
            }
            // If we go past the limit, increase the minimal violation
            else
              increaseMinViol()
          }
        }
      }
    }

    // Treat the value assignments
    if (x.isBound) {
      val v = x.min
      // If the value assigned is one we track
      if (minVal <= v && v <= maxVal) {
        val vi = v - minVal
        val nPossible = nPossibleRev(vi).value
        val nMandatory = nMandatoryRev(vi).incr()
        val nUnbound = nPossible - nMandatory

        removeUnbound(i, vi, nUnbound)

        // If too many variables are bound to the value
        if (nMandatory >= upper(vi)) {
          // If we cannot violate anymore
          if (atMaxViolRev.value) {
            // Remove the value from the unbound
            eliminateUnbound(vi, nUnbound, _.removeValue(v))
          }
          else {
            // If we only reach the limit, remember this value
            if (nMandatory == upper(vi)) {
              inExcess(nValuesInExcessRev.incr() - 1) = vi
            }
            // If we go past the limit, increase the minimal violation
            else
              increaseMinViol()
          }
        }
      }
    }
    false
  }

  /**
   * Increases the minimal violation locally and in [[viol]], prunes if it reaches the maximum
   */
  @inline def increaseMinViol(): Unit = {
    val minViol = minViolRev.incr()
    if (minViol == viol.max) {
      whenMaxViolReached()
      viol.assign(minViol)
    }
    else
      viol.updateMin(minViol)
  }

  /**
   * Prunes everything that waited until the maximum violation was reached
   */
  @inline def whenMaxViolReached(): Unit = {
    atMaxViolRev.setTrue()

    var c = nValuesInLackRev.value
    while (c > 0) {
      c -= 1
      val vi = inLack(c)
      val v = vi + minVal
      eliminateUnbound(vi, nPossibleRev(vi).value - nMandatoryRev(vi).value, _.assign(v))
    }
    c = nValuesInExcessRev.value
    while (c > 0) {
      c -= 1
      val vi = inExcess(c)
      val v = vi + minVal
      eliminateUnbound(vi, nPossibleRev(vi).value - nMandatoryRev(vi).value, _.removeValue(v))
    }
  }

  /**
   * Removes a variable from the unbound sparse set of a value
   * @param i The index of the variable to remove
   * @param vi The index of the value
   * @param last The index of the element that will replace i
   */
  @inline private def removeUnbound(i: Int, vi: Int, last: Int): Unit = {
    val thisUnboundSet = unboundSet(vi)
    val thisUnboundIndex = unboundIndex(vi)
    val atLast = thisUnboundSet(last)
    val index = thisUnboundIndex(i)

    thisUnboundSet(index) = atLast
    thisUnboundIndex(atLast) = index
    thisUnboundSet(last) = i
    thisUnboundIndex(i) = last
  }

  /**
   * Eliminates applies an action the unbound variables of a value
   * @param vi The index of the value
   * @param nUnbound The size of the unbound sparse set
   * @param action The action to be performed
   */
  @inline private def eliminateUnbound(vi: Int, nUnbound: Int, action: CPIntVar => Unit): Unit = {
    val thisUnboundSet = unboundSet(vi)
    var i = nUnbound
    while (i > 0) {
      i -= 1
      action(X(thisUnboundSet(i)))
    }
  }
}