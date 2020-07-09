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
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.{CPPropagStrength, Constraint}
import oscar.ml.pm.utils.{Dataset, DatasetUtils, TimeOption}

/**
 *
 * EpisodeSupportT : for FEM problem with timed database
 * It is the adaptation of PPICt constraints for mining frequent episodes in a long sequence with time consideration.
 * This constraint generate all available solution given such parameters
 *
 * @author John Aoga (johnaoga@gmail.com) and Quentin Cappart (quentin.cappart@uclouvain.be) and Pierre Schaus (pschaus@gmail.com)
 *         Relevant paper: ...
 *
 *         PART OF SOLVER OSCAR (https://bitbucket.org/oscarlib/oscar/wiki/Home)
 * @param P            , is an episode pattern where $P_i$ is the item in position $i$ in $P$
 * @param data         [Long Sequence], is the sequence that will be mined.
 *                     EX : [ (a,1) (b,2) (b,3) (c,6) (a,8) (c,9) (b,14)]
 * @param minsup       is a threshold support, item must appear in at least minsup sequences $support(item)>=minsup$
 * @param timeThresold are time restriction thressholds
 *
 */

final class EpisodeSupportT(val P: Array[CPIntVar], val minsup: Int, val data: Dataset, val timeThresold: TimeOption) extends Constraint(P(0).store, "EpisodeSupportT") {

  idempotent = true

  /// Initializing other input variables
  private[this] val epsilon = 0 //this is for empty item
  private[this] val lenSeq = data.nbTrans
  private[this] val nItems: Int = data.nbItem
  private[this] val minspan: Int = timeThresold.minspan
  private[this] val maxspan: Int = timeThresold.maxspan
  private[this] val mingap: Int = timeThresold.mingap
  private[this] val maxgap: Int = timeThresold.maxgap
  private[this] val patternSeq = P.clone()
  private[this] val lenPatternSeq = P.length

  /* Array of symbols and array of timestamp */
  private[this] val seq_symb: Array[Int] = data.getData(0)
  private[this] val seq_time: Array[Int] = data.getTime(0)
  private[this] val lastPosOfItem: Array[Array[Int]] = DatasetUtils.getLSNextPosGap(data, maxgap)

  private[this] val lastPosLists: Array[Array[(Int, Int)]] = lastPosOfItem.map(x => x.zipWithIndex.sortBy(-_._1))
  private[this] val lastPosListsPos: Array[Array[Int]] = Array.ofDim[Array[Int]](lenSeq)
  private[this] val lastPosListsItem: Array[Array[Int]] = Array.ofDim[Array[Int]](lenSeq)

  private[this] val tempsss: Array[Array[Int]] = Array.ofDim[Array[Int]](lenSeq)
  for (j <- 0 until lenSeq) {
    val a = Array.ofDim[Int](nItems)
    for (i <- 0 until nItems) {
      a(lastPosLists(j)(i) _2) = lastPosLists(j)(i) _1
    }
    tempsss(j) = a
  }

  for (i <- 0 until lenSeq) {
    val (a: Array[Int], b: Array[Int]) = lastPosLists(i).unzip
    lastPosListsPos(i) = a
    lastPosListsItem(i) = b
  }

  ///representation of pseudo-projected-database
  private[this] var innerTrailSize = lenSeq * 5
  private[this] var nItemMaxPerSeq = 10
  private[this] var startv = Array.tabulate(innerTrailSize)(i => i) //the N° of Sequence ( = pos in the seq)
  private[this] var esize = Array.tabulate(innerTrailSize)(i => 1)
  private[this] var embs = Array.tabulate(innerTrailSize)(i => Array.ofDim[Int](nItemMaxPerSeq)) //all end positions of prefix in this part of the seq
  private[this] val rPhi = new ReversibleInt(s, 0) //current size of trail
  private[this] val rVarphi = new ReversibleInt(s, lenSeq) //current position in trail

  ///when InnerTrail is full, it allows to double size of trail
  @inline private def growInnerTrail(): Unit = {
    val newSids = new Array[Int](innerTrailSize * 2)
    val newEmbsEnd = new Array[Array[Int]](innerTrailSize * 2)
    val newEmbSize = new Array[Int](innerTrailSize * 2)

    System.arraycopy(startv, 0, newSids, 0, innerTrailSize)
    System.arraycopy(embs, 0, newEmbsEnd, 0, innerTrailSize)
    System.arraycopy(esize, 0, newEmbSize, 0, innerTrailSize)

    startv = newSids
    embs = newEmbsEnd
    esize = newEmbSize

    innerTrailSize *= 2
  }

  @inline private def growAllOccSize(j: Int, curSize: Int, sid: Int): Unit = {
    val endAllocc = Array.ofDim[Int](curSize * 2)

    System.arraycopy(embs(j), 0, endAllocc, 0, curSize)

    embs(j) = endAllocc

  }

  ///support counter contain support for each item, it is reversible for efficient backtrack
  private[this] val freq = Array.ofDim[Int](nItems + 1)
  private[this] val projFreq = Array.ofDim[Int](nItems + 1)
  var curPrefixSupport: Int = 0
  var curMinSpanSupport: Int = 0

  ///current position in P $P_i = P[curPosInP.value]$
  private[this] val rPsi = new ReversibleInt(s, 0)

  ///check if pruning is done successfully
  private[this] var pruneSuccess = true

  ///precomputed position for mingap
  private[this] var seqMinTime = seq_time.map(e => e + mingap)
  private[this] val nextPosGap = Array.tabulate(lenSeq)(e => lenSeq)
  private[this] var i1 = 0
  private[this] var i2 = 0
  while (i1 < lenSeq) {
    if (seqMinTime(i2) <= seq_time(i1)) {
      nextPosGap(i2) = i1
      i2 += 1
    } else i1 += 1
  }

  ///precomputed position for maxgap
  seqMinTime = seq_time.map(e => e + maxgap)
  private[this] val nextPosMaxGap = Array.tabulate(lenSeq)(e => lenSeq)
  i1 = 0
  i2 = 0
  while (i1 < lenSeq) {
    if (seqMinTime(i2) > seq_time(i1)) i1 += 1
    else {
      nextPosMaxGap(i2) = if (seqMinTime(i2) == seq_time(i1)) i1 else i1 - 1
      i2 += 1
    }
  }

  ///visitedItem
  private[this] val tempFreqBool = Array.fill[Boolean](nItems)(false)

  ///remain val in the domain
  private[this] val validItems = Array.tabulate(nItems)(i => i)
  private[this] val posOfItems = Array.tabulate(nItems)(i => i)
  private[this] val rValidItems = new ReversibleInt(s, nItems)


  //precomputed all sups at each pos
  private[this] val allSups: Array[Array[Int]] = Array.ofDim(lenSeq, nItems)
  i1 = 0
  private[this] var mylim = rValidItems.value
  while (i1 < lenSeq) {
    i2 = 1
    while (i2 < nItems) {
      if (seq_symb(i1) == i2) allSups(i1)(i2) = if (i1 <= 0) 1 else allSups(i1 - 1)(i2) + 1
      else allSups(i1)(i2) = if (i1 == 0) 0 else allSups(i1 - 1)(i2)

      /*if (i1 == lenSeq-1 && allSups(i1)(i2) < minsup) {
        P(0).removeValue(i2)
        mylim = removeItem(i2,mylim)
      }*/

      i2 += 1
    }
    i1 += 1
  }
  rValidItems.value = mylim

  private[this] val isVisited = Array.fill[Boolean](nItems)(false)

  //override def associatedVars(): Iterable[CPVar] = P

  /**
   * Entry in constraint, function for all init
   *
   * @param l
   * @return The outcome of the first propagation and consistency check
   */
  final override def setup(l: CPPropagStrength): Unit = {
    propagate()
    var i = patternSeq.length
    while (i > 0) {
      i -= 1
      patternSeq(i).callPropagateWhenBind(this)
    }
  }


  /**
   * propagate
   *
   * @return the outcome i.e. Failure, Success or Suspend
   */
  final override def propagate(): Unit = {
    var v = rPsi.value

    if (P(v).isBoundTo(epsilon)) {
      if (!P(v - 1).isBoundTo(epsilon)) {
        enforceEpsilonFrom(v)
      }

      checkSpan() //forspan
      this.deactivate() // Correspond to a success
      return
    }

    while (v < P.length && P(v).isBound && P(v).min != epsilon) {
      if (!filterPrefixProjection(P(v).getMin)) throw Inconsistency
      rPsi.incr()
      v = rPsi.value
    }

    if (v > 0 && v < P.length && P(v).isBoundTo(epsilon)) {
      enforceEpsilonFrom(v)
      //??// checkSpan()//forspan
    }


    if (P.last.isBound) {
      checkSpan() //forspan
    }

  }

  //forspan
  def checkSpan(): Unit = {
    if (curMinSpanSupport < minsup) {
      throw Inconsistency
    }
    else if (curMinSpanSupport < curPrefixSupport) curPrefixSupport = curMinSpanSupport
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

    val i = rPsi.value + 1
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

  ///initialisation of domain
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
    var lim = rValidItems.value
    val len = P(j).fillArray(dom)
    while (k < len) {
      val item = dom(k)
      if (item != epsilon) {
        if (posOfItems(item) >= lim || freq(item) < minsup) {
          P(j).removeValue(item)
          lim = removeItem(item, lim)
        }
      }
      k += 1
    }
    rValidItems.value = lim
  }


  def updateSupport(u: Int, curPosInSid: Int): Int = {
    //update freq based on the next extenxion windows
    val l = nextPosGap(u)
    val endl = nextPosMaxGap(u)

    var i = rValidItems.value
    while (i > 0) {
      i -= 1
      val item = validItems(i)
      if (l < lenSeq) {
        val deb = if (l == 0) 0 else allSups(l - 1)(item)
        val fin = if (endl >= lenSeq) allSups(lenSeq - 1)(item) else allSups(endl)(item)
        tempFreqBool(item) |= fin > deb
      }
    }
    endl
  }

  /**
   * Computing of next pseudo projected database
   *
   * @param prefix
   * @return
   */
  private def projectSDB(prefix: Int): Int = {

    val phi = rPhi.value
    var varphi = rVarphi.value
    val psi = rPsi.value

    //Count sequences validated for next step
    curPrefixSupport = 0
    curMinSpanSupport = 0 //forspan

    //reset support to 0
    util.Arrays.fill(freq, 0)

    if (psi == 0) {
      var pos = 0
      var sup = 0
      //val pLast = lastPosOfItem(0)(prefix) + 1 // to replace the lenSeq
      while (pos < lenSeq) {
        if (seq_symb(pos) == prefix) {

          startv(sup) = pos
          esize(sup) = 1
          embs(sup)(0) = pos
          sup += 1

          //update freq based on the next extenxion windows
          var l = nextPosGap(pos)
          val endl = nextPosMaxGap(pos)

          var idx = rValidItems.value
          while (idx > 1) {
            idx -= 1
            val item = validItems(idx)
            if (l < lenSeq) {
              val deb = if (l == 0) 0 else allSups(l - 1)(item)
              val fin = if (endl >= lenSeq) allSups(lenSeq - 1)(item) else allSups(endl)(item)
              freq(item) += (if (fin > deb) 1 else 0)
            }
          }
        }
        pos += 1
      }
      curPrefixSupport = sup
      curMinSpanSupport = sup //forspan
      varphi = 0
    } else {
      var c = phi
      var j = phi + varphi
      while (c < phi + varphi) {

        val pos = startv(c)
        val pLast = lastPosOfItem(pos)(prefix)

        if (embs(c)(0) <= pLast) {
          //initialization
          var nEmb = 0
          java.util.Arrays.fill(tempFreqBool, false)
          var break = false
          var curPosInSid = 0
          var k = 0
          val nExtensionWindow = esize(c)

          var canMinSpanSupIncr = false //forspan
          //find prefix positions in extension windows
          while (!break && k < nExtensionWindow && embs(c)(k) <= pLast) {

            val endPatternPos = embs(c)(k)

            var maxTime = Math.min(seq_time(endPatternPos) + maxgap, seq_time(pos) + maxspan) //TODO the second part of the max could be lastpos[sidpos][seq_symb(sidpos)]

            var u = nextPosGap(endPatternPos)
            while (!break && u <= pLast && seq_time(u) <= maxTime) {
              if (prefix == seq_symb(u)) { //new embedding

                if (nEmb >= embs(j).length) growAllOccSize(j, nEmb, pos)

                embs(j)(nEmb) = u
                nEmb += 1
                //forspan
                if (!canMinSpanSupIncr && seq_time(u) - seq_time(pos) >= minspan) {
                  canMinSpanSupIncr = true
                  curMinSpanSupport += 1
                }
                // count the supports and find the end of extension window
                curPosInSid = updateSupport(u, curPosInSid)
                if (curPosInSid > lenSeq - 1) break = true

              }
              u += 1
            }
            k += 1
          }

          //update freq based on the next extenxion windows
          var idx = rValidItems.value
          while (idx > 1) {
            idx -= 1
            val item = validItems(idx)
            freq(item) += (if (tempFreqBool(item)) 1 else 0)
          }

          if (nEmb > 0) {
            // current sequence is part of the projected database
            startv(j) = pos
            esize(j) = nEmb
            if (j + 1 >= innerTrailSize) growInnerTrail()
            curPrefixSupport += 1
            j += 1
          }
        }
        c += 1
      }
    }

    rPhi.value = phi + varphi
    rVarphi.value = curPrefixSupport

    curPrefixSupport

  }

  def removeItem(item: Int, limit: Int): Int = {
    val curPos = posOfItems(item)

    if (curPos >= limit) return limit

    val newlimit = limit - 1

    if (newlimit < 0) return 0

    posOfItems(item) = newlimit
    posOfItems(validItems(newlimit)) = curPos

    validItems(curPos) = validItems(newlimit)
    validItems(newlimit) = item

    newlimit
  }

  override def associatedVars(): Iterable[CPVar] = P
}
