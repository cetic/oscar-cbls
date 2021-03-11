package oscar.ml.pm.Constraints.spm

import oscar.algo.Inconsistency
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core._
import oscar.cp.core.variables._
import oscar.ml.pm.utils.{Dataset, DatasetUtils, TestHelpers, TimeOption}


/**
 * PPICt [Constraint Programming & Sequential Pattern Mining with Prefix projection method and time constraints]
 * is the CP version of PPIC constraint supporting time constraints.
 *
 * This constraint generate all available solution given such parameters
 *
 * @param P            , is pattern where $P_i$ is the item in position $i$ in $P$
 * @param data         , [sequence database] it is a set of sequences. Each line $SDB_i$ or $t_i$ represent a sequence: (item, time)
 *                     s1 (a, 2)(b, 5)(d, 6)(c, 10)(b, 11)
 *                     s2 (b, 1)(a, 2)(a, 9)(d, 12)(c, 15)(a, 18)(b, 24)
 *                     s3 (a, 2)(b, 4)(d, 6)(d, 8)(b, 10)(e, 12)(c, 14)
 *                     s4 (a, 1)(c, 2)(c, 3)(b, 4)
 * @param minsup       is a threshold support, item must appear in at least minsup sequences $support(item)>=minsup$
 * @param timeThresold (minspan: Int, maxspan: Int, mingap: Int, maxgap: Int) represents time constraints
 * @author John Aoga (johnaoga@gmail.com) and Pierre Schaus (pschaus@gmail.com)
 *
 *         Aoga, J.O.R., Guns, T. & Schaus, P. Mining Time-constrained Sequential Patterns with Constraint Programming.
 *         Constraints 22, 548–570 (2017). https://doi.org/10.1007/s10601-017-9272-3
 */

class PPICt(val P: Array[CPIntVar], val minsup: Int, val data: Dataset, val timeThresold: TimeOption) extends Constraint(P(0).store, "PPICt") {

  idempotent = true

  private[this] val SDB: Array[Array[Int]] = data.getData
  private[this] val SDBtime: Array[Array[Int]] = data.getTime

  /// Initializing other input variables
  private[this] val epsilon = 0 //this is for empty item
  private[this] val lenSDB = SDB.length
  private[this] val nItems: Int = data.nbItem
  private[this] val minspan: Int = timeThresold.minspan
  private[this] val maxspan: Int = timeThresold.maxspan
  private[this] val mingap: Int = if (timeThresold.mingap == 0) 1 else timeThresold.mingap
  private[this] val maxgap: Int = timeThresold.maxgap
  private[this] val patternSeq = P.clone()
  private[this] val lenPatternSeq = P.length

  // Precomputed data structures
  /**
   * lastPositionMap is the last real position of an item in a sequence, if 0 it is not present
   * (index from 1)
   *        a, b, c, d, e,
   * s1: 0, 1, 5, 4, 3, 0, 0
   * s2: 0, 6, 7, 5, 4, 0, 0
   * s3: 0, 1, 5, 7, 4, 6, 0
   * s4: 0, 1, 4, 3, 0, 0, 0
   */
  private[this] val lastPositionMap: Array[Array[Int]] = DatasetUtils.getItemLastPosBySequence(data)

  /**
   * nextPosGap is the first possible position of an item in a sequence taking into account the mingap, if > the sequence length it is not present
   * (index from 1)
   *    p1,p2,p3,p4,p5,p6,p7
   * s1: 1, 3, 3, 6, 6
   * s2: 2, 2, 3, 4, 5, 6, 8
   * s3: 2, 3, 4, 5, 6, 8, 8
   * s4: 3, 5, 5, 5
   */
  private[this] val nextPosGap: Array[Array[Int]] = DatasetUtils.getSDBNextPosGap(data, mingap)

  /**
   * itemsSupport: is the initial support (number of sequences where a item is appeared) of all items
   * (eps.: 4 +:) a : 4, b : 4, c : 4, d : 3, e : 1
   */
  private[this] val itemsSupport: Array[Int] = lenSDB +: DatasetUtils.getSDBSupport(data)

  /**
   * nItemMaxPerSeq the longuest itemset: 5 ( <= nItems)
   */
  private[this] val nItemMaxPerSeq: Int = DatasetUtils.getNItemMaxPerSeq(data)

  // Initialisation of domain
  private[this] val dom = Array.ofDim[Int](nItems + 1)

  /**
   * Building the backtracking-aware data structure
   */
  // Pointer to backtracking-aware data structure (phi and varphi)
  private[this] val psdbStart = new ReversibleInt(s, 0)
  private[this] val psdbSize = new ReversibleInt(s, 0)

  // Current position in P $P_i = P[curPosInP.value]$
  private[this] val curPosInP = new ReversibleInt(s, 0)

  // Representation of pseudo-projected-database
  // Current size of the trail
  private[this] var innerTrailSize = lenSDB * 5

  // Current position in trail
  private[this] var sids = Array.tabulate(innerTrailSize)(i => i)
  private[this] var embSize = Array.tabulate(innerTrailSize)(i => 1)
  private[this] var embsEnd = Array.tabulate(innerTrailSize)(i => Array.ofDim[Int](nItemMaxPerSeq))
  private[this] var embsFirst = Array.tabulate(innerTrailSize)(i => Array.ofDim[Int](nItemMaxPerSeq))


  /**
   * Extra init
   */
  // Number of Sequence (sid)
  private[this] val realNItem = itemsSupport.length
  private[this] val supportCounter: Array[Int] = itemsSupport

  /// Visited Item variable
  private[this] val visitedItem = Array.fill[Boolean](realNItem)(false)

  ///
  var curPrefixSupport: Int = 0

  /**
   * Entry in constraint, function for all init
   *
   * @param l , represents the strength of the propagation
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
    var i = curPosInP.value
    while (i < P.length && P(i).isBound && P(i).min != epsilon) {
      val sup = projectSDB(P(i).min)
      if (sup < minsup) throw Inconsistency
      curPosInP.incr()
      i += 1
    }

    if (i > 0 && i < P.length && P(i).isBoundTo(epsilon)) {
      //TODO check minspan
      while (i < lenPatternSeq) {
        P(i).assign(epsilon)
        i += 1
      }
    }
  }

  /**
   * Pruning strategy
   *
   * @param nextPosInP next (considered unbound) position in P
   */
  private def prune(nextPosInP: Int): Unit = {
    if (nextPosInP >= lenPatternSeq) return

    // Remove all infrequent symbols from the next domain
    var k = P(nextPosInP).fillArray(dom)
    while (k > 0) {
      k -= 1
      val item = dom(k)
      if (item != epsilon && supportCounter(item) < minsup) {
        P(nextPosInP).removeValue(item)
      }
    }
  }


  /**
   * Computing of next pseudo projected database
   *
   * @param prefix the item bound at current position
   * @return the support of the current prefix
   */
  def projectSDB(prefix: Int): Int = {

    val startInit = psdbStart.value
    val sizeInit = psdbSize.value
    val l = curPosInP.value

    // Prefix support
    var sup = 0

    // Reset support to 0
    java.util.Arrays.fill(supportCounter, 0)

    var j = 0

    if (l == 0) {
      var sid = lenSDB
      while (sid > 0) {
        sid -= 1

        // Initialization
        var nEmb = 0
        val seqs = SDB(sid)
        val lSeqs = seqs.length
        java.util.Arrays.fill(visitedItem, false)
        var break = false
        var curPosInSid = 0
        var pos = 0
        val pLast = lastPositionMap(sid)(prefix)

        // Find prefix positions in all sequences
        while (!break && pos < pLast) {
          if (prefix == seqs(pos)) { // new match!
            if (nEmb >= embsEnd(j).length) growAllOccSize(j, nEmb, sid)

            embsFirst(j)(nEmb) = pos
            embsEnd(j)(nEmb) = pos
            nEmb += 1

            // Compute the supports and find the end of sequence
            curPosInSid = updateSupport(pos, pos, sid, lSeqs, seqs, curPosInSid)
            if (curPosInSid > lSeqs - 1) break = true

          }
          pos += 1
        }
        if (nEmb > 0) { // Current sequence is part of the projected database
          sids(j) = sid
          embSize(j) = nEmb
          if (j + 1 >= innerTrailSize) growInnerTrail()
          sup += 1
          j += 1
        }
      }
    } else {
      var c = startInit
      j = startInit + sizeInit
      while (c < startInit + sizeInit) {

        val sid = sids(c)
        val pLast = lastPositionMap(sid)(prefix)

        if (embsEnd(c)(0) < pLast) {
          // Initialization
          var nEmb = 0
          val seqs = SDB(sid)
          val lSeqs = seqs.length
          java.util.Arrays.fill(visitedItem, false)
          var break = false
          var curPosInSid = 0
          var k = 0
          val nExtensionWindow = embSize(c)

          // Find prefix positions in extension windows
          while (!break && k < nExtensionWindow && embsEnd(c)(k) < pLast) {
            val firstPatternPos = embsFirst(c)(k)
            val endPatternPos = embsEnd(c)(k)

            var maxTime = Math.min(SDBtime(sid)(endPatternPos) + maxgap, SDBtime(sid)(firstPatternPos) + maxspan) //v
            if (SDBtime(sid)(endPatternPos) - SDBtime(sid)(firstPatternPos) > mingap && k + 1 < nExtensionWindow) {
              maxTime = Math.min(maxTime, SDBtime(sid)(embsEnd(c)(k + 1)))
            }

            var u = nextPosGap(sid)(endPatternPos)
            while (!break && u < pLast && SDBtime(sid)(u) <= maxTime) {
              if (prefix == seqs(u)) { // New embedding
                if (nEmb >= embsEnd(j).length) growAllOccSize(j, nEmb, sid)
                embsFirst(j)(nEmb) = firstPatternPos
                embsEnd(j)(nEmb) = u
                nEmb += 1

                // Compute the supports and find the end of extension window
                curPosInSid = updateSupport(firstPatternPos, u, sid, lSeqs, seqs, curPosInSid)
                if (curPosInSid > lSeqs - 1) break = true

              }
              u += 1
            }
            k += 1
          }
          if (nEmb > 0) { // Current sequence is part of the projected database
            sids(j) = sid
            embSize(j) = nEmb
            if (j + 1 >= innerTrailSize) growInnerTrail()
            sup += 1
            j += 1
          }
        }
        c += 1
      }
    }

    psdbStart.value = startInit + sizeInit
    psdbSize.value = sup
    //print(sup)
    curPrefixSupport = sup
    sup

  }

  /**
   * When InnerTrail is full, this function allows doubling the size of the trail
   */
  @inline private def growInnerTrail(): Unit = {
    val newSids = new Array[Int](innerTrailSize * 2)
    val newEmbsFirst = new Array[Array[Int]](innerTrailSize * 2)
    val newEmbsEnd = new Array[Array[Int]](innerTrailSize * 2)
    val newEmbSize = new Array[Int](innerTrailSize * 2)

    System.arraycopy(sids, 0, newSids, 0, innerTrailSize)
    System.arraycopy(embsFirst, 0, newEmbsFirst, 0, innerTrailSize)
    System.arraycopy(embsEnd, 0, newEmbsEnd, 0, innerTrailSize)
    System.arraycopy(embSize, 0, newEmbSize, 0, innerTrailSize)

    sids = newSids
    embsFirst = newEmbsFirst
    embsEnd = newEmbsEnd
    embSize = newEmbSize

    innerTrailSize *= 2
  }

  /**
   *
   * @param j
   * @param curSize
   * @param sid
   */
  @inline private def growAllOccSize(j: Int, curSize: Int, sid: Int): Unit = {
    val firstAllocc = Array.ofDim[Int](curSize * 2)
    val endAllocc = Array.ofDim[Int](curSize * 2)

    System.arraycopy(embsFirst(j), 0, firstAllocc, 0, curSize)
    System.arraycopy(embsEnd(j), 0, endAllocc, 0, curSize)

    embsFirst(j) = firstAllocc
    embsEnd(j) = endAllocc

  }

  /**
   * Update support for each item using
   *
   * @param firstPatternPos
   * @param endPatternPos
   * @param sid
   * @param lSeqs
   * @param seqs
   * @param lastExtensionEnd
   * @return
   */
  def updateSupport(firstPatternPos: Int, endPatternPos: Int, sid: Int, lSeqs: Int, seqs: Array[Int], lastExtensionEnd: Int): Int = {
    var u = Math.max(nextPosGap(sid)(endPatternPos), lastExtensionEnd)
    val maxTime = Math.min(SDBtime(sid)(endPatternPos) + maxgap, SDBtime(sid)(firstPatternPos) + maxspan)

    while (u < lSeqs && SDBtime(sid)(u) <= maxTime) {
      val item = seqs(u)

      if (!visitedItem(item)) {
        supportCounter(item) += 1
        visitedItem(item) = true
      }
      u += 1
    }
    u
  }

  override def associatedVars(): Iterable[CPVar] = P
}