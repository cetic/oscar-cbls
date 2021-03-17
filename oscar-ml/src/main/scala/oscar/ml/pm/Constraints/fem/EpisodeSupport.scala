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

package oscar.ml.pm.Constraints.fem

import java.util

import oscar.algo.Inconsistency
import oscar.algo.array.ArrayStackInt
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core._
import oscar.cp.core.variables._
import oscar.ml.pm.utils.{Dataset, DatasetUtils}

/**
 *
 * EpisodeSupport : for FEM problem
 * It is the adaptation of PPIC constraints for mining frequent episodes in a long sequence.
 * This constraint generate all available solution given such parameters
 *
 * @author John Aoga (johnaoga@gmail.com) and Quentin Cappart (quentin.cappart@uclouvain.be) and Pierre Schaus (pschaus@gmail.com)
 *         Relevant paper: ...
 *
 *         PART OF SOLVER OSCAR (https://bitbucket.org/oscarlib/oscar/wiki/Home)
 * @param P      , is an episode pattern where $P_i$ is the item in position $i$ in $P$
 * @param data   [Long Sequence], is the sequence that will be mined.
 *               EX : [a b c a a b]
 * @param minsup is a threshold support, item must appear in at least minsup sequences $support(item)>=minsup$
 *
 */

final class EpisodeSupport(val P: Array[CPIntVar], val minsup: Int, val data: Dataset) extends Constraint(P(0).store, "EpisodeSupport") {


  /// Initializing other inputs variables namely precomputed data structures
  val LS: Array[Int] = data.getData(0)
  val nItems: Int = data.nbItem

  /**
   * lastPosOfItem is the last real position of a symbol in a sequence, if 0 it is not present
   * e.g. : {a : 4, b : 5, c : 2} (index from 0)
   */
  val lastPosOfItem: Array[Int] = DatasetUtils.getItemLastPosBySequence(data)(0).map(_ - 1)

  idempotent = true

  private[this] val lastPosList: Array[(Int, Int)] = lastPosOfItem.zipWithIndex.sortBy(-_._1)
  private[this] val (lastPosListPos: Array[Int], lastPosListItem: Array[Int]) = lastPosList.unzip


  private[this] val epsilon = 0 //this is for empty item
  private[this] val lenLS = LS.size
  private[this] val patternSeq = P.clone()
  private[this] val lenPatternSeq = P.length

  /// Representation of pseudo-projected-database
  private[this] var innerTrailSize = lenLS * 5
  private[this] var psdbPosInSeq = Array.tabulate(innerTrailSize)(i => i) //position of prefix in this sid

  private[this] val psdbStart = new ReversibleInt(s, 0) //current size of trail
  private[this] val psdbSize = new ReversibleInt(s, lenLS) //current position in trail

  /// When InnerTrail is full, it allows to double size of trail
  @inline private def growInnerTrail(): Unit = {
    val newPsdbPosInSeq = new Array[Int](innerTrailSize * 2)
    System.arraycopy(psdbPosInSeq, 0, newPsdbPosInSeq, 0, innerTrailSize)
    psdbPosInSeq = newPsdbPosInSeq
    innerTrailSize *= 2
  }

  /// Support counter contain support for each item, it is reversible for efficient backtrack
  private[this] val supportCounter = Array.ofDim[Int](nItems + 1)
  var curPrefixSupport: Int = 0

  /// Current position in P $P_i = P[curPosInP.value]$
  private[this] val curPosInP = new ReversibleInt(s, 0)

  private[this] val matchedPos = new ArrayStackInt()

  /// Check if pruning is done successfully
  private[this] var pruneSuccess = true

  /**
   * Entry in constraint, function for all init
   *
   * @param l, represents the strength of the propagation
   */
  final override def setup(l: CPPropagStrength): Unit = {
    assert(minsup > 0)
    propagate()
    var i = patternSeq.length
    while (i > 0) {
      i -= 1
      patternSeq(i).callPropagateWhenBind(this)
    }
  }

  /**
   * Propagate
   */
  final override def propagate(): Unit = {
    var v = curPosInP.value
    if (P(v).isBoundTo(epsilon)) {
      if (!P(v - 1).isBoundTo(epsilon)) {
        enforceEpsilonFrom(v)
      }
      this.deactivate()
      return
    }
    while (v < P.length && P(v).isBound && P(v).min != epsilon) {
      if (!filterPrefixProjection(P(v).getMin)) {
        throw Inconsistency
      }
      curPosInP.incr()
      v = curPosInP.value
    }
    if (v > 0 && v < P.length && P(v).isBoundTo(epsilon)) {
      enforceEpsilonFrom(v)
    }
  }


  /**
   * when $P_i = epsilon$, then $P_i+1 = epsilon$
   *
   * @param i current position in P
   */
  def enforceEpsilonFrom(i: Int): Unit = {
    var j = i
    while (j < lenPatternSeq) {
      P(j).assign(epsilon)
      j += 1
    }
  }

  /**
   * P[curPosInP.value] has just been bound to "prefix"
   * all the indices before (< currPosInP) are already bound
   *
   * if prefix is not epsilon we can compute next pseudo-projected-database
   * with projectSDB function
   *
   * @param prefix
   * @return the Boolean is to say if current prefix is a solution or not
   */
  private def filterPrefixProjection(prefix: Int): Boolean = {
    val i = curPosInP.value + 1
    if (i >= 2 && prefix == epsilon) {
      return true
    } else {

      val sup = projectSDB(prefix)
      if (sup < minsup) {
        return false
      } else {
        pruneSuccess = true
        ///Prune next position pattern P domain if it exists unfrequent items
        prune(i)
        return pruneSuccess
      }
    }
  }

  val dom = Array.ofDim[Int](nItems)

  /**
   * pruning strategy
   *
   * @param i current position in P
   */
  private def prune(i: Int): Unit = {
    val j = i
    if (j >= lenPatternSeq) return
    var k = 0
    val len = P(j).fillArray(dom)
    while (k < len) {
      val item = dom(k)
      if (item != epsilon && supportCounter(item) < minsup) {
        P(j).removeValue(item)
      }
      k += 1
    }
  }


  /**
   * Computing of next pseudo projected database
   *
   * @param prefix
   * @return
   */
  def projectSDB(prefix: Int): Int = {
    matchedPos.clear()
    val startInit = psdbStart.value
    val sizeInit = psdbSize.value
    //Count sequences validated for next step
    curPrefixSupport = 0
    //allow to predict failed sid (sequence) and remove it
    //reset support to 0
    util.Arrays.fill(supportCounter, 0)

    var i = startInit
    var j = startInit + sizeInit

    var break = false // Break the main loop when the prefix is not detected in the current sequence (dominance rule)

    // Tias optim: nbAdded < nbAddedTarget
    // because we know how many need to be added so we can stop when this target is reached

    var previousPos = -1 // caching previous matched position

    while (i < startInit + sizeInit && !break) {
      var pos = psdbPosInSeq(i)
      if (curPosInP.value > 0 || prefix == LS(pos)) {
        // here we know at least that prefix is present in the sequence
        // search for next value "prefix" in the sequence starting from
        if (lastPosOfItem(prefix) < pos) {
          break = true
        } else {
          // we are sure prefix next position is available and so we add the sequence in the new projected data base

          // find next position of prefix
          if (previousPos < pos) {
            while (pos < lenLS && prefix != LS(pos)) {
              pos += 1
            }
            previousPos = pos
          } else {
            pos = previousPos
          }
          matchedPos.append(pos + 1)
          //update pseudo projected database
          psdbPosInSeq(j) = pos + 1
          j += 1
          if (j >= innerTrailSize) growInnerTrail()
          curPrefixSupport += 1
        }
      }
      i += 1
    } // end main while loop
    // compute  supports
    var k = 0
    while (k < nItems && !matchedPos.isEmpty) {
      while (!matchedPos.isEmpty && matchedPos.top > lastPosListPos(k)) {
        matchedPos.pop()
      }
      supportCounter(lastPosListItem(k)) += matchedPos.size
      k += 1
    }
    // update trail
    psdbStart.value = startInit + sizeInit
    psdbSize.value = curPrefixSupport
    curPrefixSupport
  }

  override def associatedVars(): Iterable[CPVar] = P
}
