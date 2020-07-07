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

import oscar.algo.Inconsistency
import oscar.algo.reversible._
import oscar.cp._
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.CPVar
import oscar.ml.pm.utils.{Dataset, ReversibleSparseBitSet2}


/**
 *
 * ClosedFIM : for closed Frequent Itemset Mining  problem
 * WARNING: When using delta != 0, the exact solution is not a guarantee
 *
 * @author John Aoga johnaoga@gmail.com
 * @author Pierre Schaus pschaus@gmail.com
 *         Relevant paper: Compact table and http://becool.info.ucl.ac.be/biblio/coversize-global-constraint-frequency-based-itemset-mining
 *
 *         PART OF SOLVER OSCAR (https://bitbucket.org/oscarlib/oscar/wiki/Home)
 */
class ClosedFIM(val I: Array[CPBoolVar], val frequency: Int, val data: Dataset) extends Constraint(I(0).store, "ClosedFrequentCoverage") {
  override def associatedVars(): Iterable[CPVar] = I

  /// Initializing Input
  val nItems: Int = data.nbItem
  val nTrans: Int = data.nbTrans
  val TDB: Array[Set[Int]] = data.intoVertical()
  val delta = 0

  /// Declaring/Initializing other variables
  idempotent = true

  //init coverage
  private[this] val coverage = new ReversibleSparseBitSet2(s, nTrans, 0 until nTrans)

  ///Create matrix B (nItems x nTrans) (i = item, j = transaction)
  //Is such that columns(i) is the coverage of item i.
  private[this] val columns = Array.tabulate(nItems) { x => new coverage.BitSet(TDB(x)) }

  ///contains all the unbound variables that are not in the closure of the current itemset.
  //closure => freq(I(D)U{i}) = freq(I(D))
  private[this] val unboundNotInClosureIndices = Array.tabulate(I.length)(i => i)
  private[this] val revUnboundNotInClosure = new ReversibleInt(s, I.length)

  /**
   *
   * @param l
   * @return CPOutcome state
   */
  override def setup(l: CPPropagStrength): Unit = {

    for (i <- 0 until nItems; if !I(i).isBound) {
      I(i).callPropagateWhenBind(this)
    }

    propagate()
  }

  /**
   *
   * @return CPOutcome state
   */
  override def propagate(): Unit = {

    coverage.clearCollected()

    var coverChanged = false

    var nU = revUnboundNotInClosure.value
    var i = nU
    while (i > 0) {
      i -= 1
      val idx = unboundNotInClosureIndices(i)
      if (I(idx).isTrue) {
        coverChanged |= coverage.intersectWith(columns(idx))
        nU = removeItem(i, nU, idx)
      }
    }

    val cardinality = coverage.count()

    //failure of frequency rule
    if (cardinality < frequency)
      throw Inconsistency

    //pruning1 (cardinality >= frequency) :
    // avoids to add items that will not allow the itemset
    // to meet the frequency threshold
    prune(nU, cardinality)

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
    unboundNotInClosureIndices(item) = unboundNotInClosureIndices(lastU)
    unboundNotInClosureIndices(lastU) = index
    lastU
  }

  /**
   *
   * @param nUbound     the number of not unbound item which are not in the current closure
   * @param cardinality the frequency (support) of current itemset
   * @return
   */
  def prune(nUbound: Int, cardinality: Int): Unit = {
    var nU = nUbound
    var i = nU
    while (i > 0) {
      i -= 1
      val idx = unboundNotInClosureIndices(i)
      val cardIdx = coverage.intersectCount(columns(idx), frequency)

      if (I(idx).isFalse) { //I_i == 0
        if (cardinality - cardIdx <= delta) throw Inconsistency
        else if (cardIdx < frequency) nU = removeItem(i, nU, idx)
      } else { //I_i == U

        if (cardIdx < frequency) {
          nU = removeItem(i, nU, idx)
          //enforced to zero, this item will not be taken into account anymore
          I(idx).assignFalse()

        } else if (cardinality - cardIdx <= delta) { //condition 2 : cardIdx = cardinality => freq(I(U)) = freq(I(U)+idx) => closure
          nU = removeItem(i, nU, idx)
          //enforced to one, this item will not be taken into account anymore
          I(idx).assignTrue()
        }
      }
    }
    revUnboundNotInClosure.value = nU
  }

}

