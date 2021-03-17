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

package oscar.ml.pm.Constraints.fim

import oscar.algo.reversible._
import oscar.cp._
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.CPVar
import oscar.ml.pm.utils.{Dataset, ReversibleSparseBitSet2}

/**
 *
 * CoverSize with Support as a CP variable (same for all contraints end with Var)
 *
 * @author John Aoga johnaoga@gmail.com
 * @author Pierre Schaus pschaus@gmail.com
 *         Relevant paper: Compact table and http://becool.info.ucl.ac.be/biblio/coversize-global-constraint-frequency-based-itemset-mining
 *
 *         PART OF SOLVER OSCAR (https://bitbucket.org/oscarlib/oscar/wiki/Home)
 */

class CoverSize(val I: Array[CPBoolVar], Sup: CPIntVar, data: Dataset) extends Constraint(I(0).store, "CoverSize") {
  override def associatedVars(): Iterable[CPVar] = I ++ Array(Sup)

  /// Initializing Variables
  val nItems: Int = data.nbItem
  val nTrans: Int = data.nbTrans
  val TDB: Array[Set[Int]] = data.intoVertical()

  //idempotent = true

  //init coverage
  private[this] val coverage = new ReversibleSparseBitSet2(s, nTrans, 0 until nTrans)

  ///Create matrix B (nItems x nTrans) (i = item, j = transaction)
  //Is such that columns(i) is the coverage of item i.
  private[this] val columns = Array.tabulate(nItems) { x => new coverage.BitSet(TDB(x)) }

  ///contains all the unbound variables that are not in the closure of the current itemset.
  //closure => freq(I(D)U{i}) = freq(I(D))
  private[this] val unboundNotInClosure = Array.tabulate(I.length)(i => i)
  private[this] val nUnboundNotInClosure = new ReversibleInt(s, I.length)

  private[this] val updateSupLB = new ReversibleBoolean(s, true)
  private[this] var supLB = 0

  private[this] val alwaysFilter = Array.ofDim[Boolean](nItems)

  /**
   *
   * @param l
   * @return CPOutcome state
   */
  override def setup(l: CPPropagStrength): Unit = {

    for (i <- 0 until nItems; if !I(i).isBound) {
      I(i).callPropagateWhenBind(this)

    }
    /*val nU = nUnboundNotInClosure.value
    var i = nU
    while (i > 0) {
      i -= 1
      val idx = unboundNotInClosure(i)
      alwaysFilter(idx) = coverage.filter(columns, unboundNotInClosure, nU, unboundNotInClosure(idx))
    }*/

    if (!Sup.isBound) Sup.callPropagateWhenBoundsChange(this)

    propagate()
  }

  /**
   *
   * @return CPOutcome state
   */
  override def propagate(): Unit = {

    coverage.clearCollected()

    var coverChanged = false

    // update the coverage
    var nU = nUnboundNotInClosure.value
    var i = nU
    while (i > 0) {
      i -= 1
      val idx = unboundNotInClosure(i)
      if (I(idx).isBound) {
        nU = removeItem(i, nU, idx)
        //when bound to 1, then idx in coverage, make intersection
        if (I(idx).min == 1) {
          coverChanged |= coverage.intersectWith(columns(idx))
        } else {
          // need to recompute upper bound only if one item is newly set to 0
          updateSupLB.value = true
        }
      }
    }

    // remove items that if included induce a too small coverage
    i = nU
    while (i > 0) {
      i -= 1
      val idx = unboundNotInClosure(i)
      //cond1: frequency condition
      if (coverage.intersectCount(columns(idx)) < Sup.min) {
        nU = removeItem(i, nU, idx)
        I(idx).assignFalse()
        updateSupLB.value = true
      }
    }

    val supUB = coverage.count()
    Sup.updateMax(supUB)

    // update the Sup.min by intersect all the remaining unbound items
    //if (updateSupLB.value) {
    supLB = coverage.intersectCountAll(columns, unboundNotInClosure, nU)
    Sup.updateMin(supLB)
    updateSupLB.value = false
    //}


    if (supUB == Sup.min) {
      // remove all unbound items that are not super sets of coverage
      // because these would necessarily decrease the coverage
      i = nU
      while (i > 0) {
        i -= 1
        val idx = unboundNotInClosure(i)
        if (!coverage.isSubSetOf(columns(idx))) {
          nU = removeItem(i, nU, idx)
          I(idx).assignFalse()
          updateSupLB.value = true
        }
      }
    }


    /*if (supLB == Sup.max) {
      // include all items that are strictly necessary to reach the lower-bound
      i = nU
      while (i > 0) {
        i -= 1
        val idx = unboundNotInClosure(i)
        if (alwaysFilter(idx) || coverage.filter(columns, unboundNotInClosure, nU, idx)) {
          // some transactions are uniquely set in idx
          // can be much faster, if I is present in some unique transaction
          I(idx).assignTrue()
        }
      }
    }*/
    nUnboundNotInClosure.value = nU

  }


  /**
   *
   * @param item
   * @param nU    the number of not unbound item which are not in the current closure
   * @param index the index of current item
   * @return
   */
  def removeItem(item: Int, nU: Int, index: Int): Int = {
    val lastU = nU - 1
    unboundNotInClosure(item) = unboundNotInClosure(lastU)
    unboundNotInClosure(lastU) = index
    lastU
  }

}



