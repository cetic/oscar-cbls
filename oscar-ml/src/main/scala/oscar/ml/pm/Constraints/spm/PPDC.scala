package oscar.ml.pm.Constraints.spm

import oscar.algo.Inconsistency
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core._
import oscar.cp.core.variables._
import oscar.ml.pm.utils.{Dataset, DatasetUtils, TestHelpers}

/**
 * PPDC [Constraint Programming & Sequential Pattern Mining with Prefix projection method]
 * is the CP version of Prefix projection method of Sequential Pattern Mining (with several improvements)
 * which is based on projected database (We use here pseudo-projected-database \{ (sid, pos) \}).
 *
 * This constraint generate all available solution given such parameters
 *
 * @param P      , is pattern where $P_i$ is the item in position $i$ in $P$
 * @param data   , [sequence database] it is a set of sequences. Each line $SDB_i$ or $t_i$ represent a sequence
 *               s1 abcbc
 *               s2 babc
 *               s3 ab
 *               s4 bcd
 * @param minsup is a threshold support, item must appear in at least minsup sequences $support(item)>=minsup$
 * @author John Aoga (johnaoga@gmail.com) and Pierre Schaus (pschaus@gmail.com)
 *
 * AOGA, John OR, GUNS, Tias, et SCHAUS, Pierre. An efficient algorithm for mining frequent sequence with constraint programming.
 * In : Joint European Conference on Machine Learning and Knowledge Discovery in Databases. Springer, Cham, 2016. p. 315-330.
 *
 * The main difference with PPIC is how the support is counted.
 * You can find a further explanation in my dissertation (page 102) - http://hdl.handle.net/2078.1/218062
 */

class PPDC(val P: Array[CPIntVar], val minsup: Int, val data: Dataset) extends Constraint(P(0).store, "PPDC") {

  System.err.println("PPDC!")

  idempotent = true

  /// Initializing the other input variables (precomputed data structures)
  private[this] val SDB: Array[Array[Int]] = data.getData

  private[this] val epsilon = 0 //this is for empty item
  private[this] val lenSDB = SDB.length
  private[this] val nItems: Int = data.nbItem
  private[this] val patternSeq = P.clone()
  private[this] val lenPatternSeq = P.length
  private[this] val dom = Array.ofDim[Int](nItems)

  // precomputed data structures

  /**
   * lastPositionMap is the last real position of an item in a sequence, if 0 it is not present
   * (index from 1)
   *        a, b, c,
   * s1: 0, 1, 4, 5, 0
   * s2: 0, 2, 3, 4, 0
   * s3: 0, 1, 2, 0, 0
   * s4: 0, 0, 1, 2, 0
   */
  private[this] val lastPositionMap: Array[Array[Int]] = DatasetUtils.getItemLastPosBySequence(data)

  /**
   * lastPositionList is an ordered list of last postions of items in a given sequence
   * (index from 1)
   *    p1,p2,p3,
   * s1: 5, 4, 1
   * s2: 4, 3, 2
   * s3: 2, 1
   * s4: 2, 1
   */
  private[this] val lastPositionList: Array[Array[Int]] = DatasetUtils.getSDBLastPos(data, lastPositionMap)
  //--//TestHelpers.printMat(lastPositionList)

  /**
   * itemsSupport: is the initial support (number of sequences where a item is appeared) of all items
   * (eps.: 4 +:) a : 3, b : 4, c : 3
   */
  private[this] val itemsSupport: Array[Int] = lenSDB +: DatasetUtils.getSDBSupport(data)

  ///representation of pseudo-projected-database
  private[this] var innerTrailSize = lenSDB * 5
  private[this] var psdbSeqId = Array.tabulate(innerTrailSize)(i => i) //the N° of Sequence (sid)
  private[this] var psdbPosInSeq = Array.tabulate(innerTrailSize)(i => 0) //position of prefix in this sid
  private[this] val psdbStart = new ReversibleInt(s, 0) //current size of trail
  private[this] val psdbSize = new ReversibleInt(s, lenSDB) //current position in trail

  ///when InnerTrail is full, it allows to double size of trail
  @inline private def growInnerTrail(): Unit = {
    val newPsdbSeqId = new Array[Int](innerTrailSize * 2)
    val newPsdbPosInSeq = new Array[Int](innerTrailSize * 2)
    System.arraycopy(psdbSeqId, 0, newPsdbSeqId, 0, innerTrailSize)
    System.arraycopy(psdbPosInSeq, 0, newPsdbPosInSeq, 0, innerTrailSize)
    psdbSeqId = newPsdbSeqId
    psdbPosInSeq = newPsdbPosInSeq
    innerTrailSize *= 2
  }

  ///support counter contain support for each item, it is reversible for efficient backtrack
  val supportCounter: Array[ReversibleInt] = Array.tabulate(itemsSupport.length)(e => new ReversibleInt(s, itemsSupport(e)))
  var curPrefixSupport: Int = 0

  ///current position in P $P_i = P[curPosInP.value]$
  private[this] val curPosInP = new ReversibleInt(s, 0)

  /**
   * Entry in constraint, function for all init
   *
   * @param l, represents the strength of the propagation
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

    var v = curPosInP.value
    if (P(v).isBoundTo(epsilon)) {
      if (!P(v - 1).isBoundTo(epsilon))
        enforceEpsilonFrom(v)
    } else {
      while (v < P.length && P(v).isBound && P(v).min != epsilon) {
        if (!filterPrefixProjection(P(v).getMin)) throw Inconsistency
        curPosInP.incr()
        v = curPosInP.value
      }

      if (v > 0 && v < lenPatternSeq && P(v).isBoundTo(epsilon)) {
        enforceEpsilonFrom(v)
      }
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
      true
    } else {
      val sup = projectSDB(prefix)
      if (sup < minsup) {
        false
      } else {
        prune(i)
        true
      }
    }
  }

  /**
   * Pruning strategy
   * Prune next position pattern P domain if it exists infrequent items
   *
   * @param i current position in P
   */
  private def prune(i: Int): Unit = {
    if (i >= lenPatternSeq) return
    var k = 0
    val len = P(i).fillArray(dom)
    while (k < len) {
      val item = dom(k)
      if (item != epsilon && supportCounter(item) < minsup) {
        P(i).removeValue(item)
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

    val startInit = psdbStart.value
    val sizeInit = psdbSize.value

    // Count sequences validated for next step
    curPrefixSupport = 0

    var i = startInit
    var j = startInit + sizeInit

    while (i < startInit + sizeInit) {

      val sid = psdbSeqId(i)
      val ti = SDB(sid)
      val lti = ti.length
      val start = psdbPosInSeq(i)
      var pos = start

      if (lastPositionMap(sid)(prefix) != 0) {
        // We know at least that prefix is present in sequence sid

        // Search for next value "prefix" in the sequence starting from
        if (lastPositionMap(sid)(prefix) - 1 >= pos) {
          // Prefix next position is available,
          // we can thus add the sequence in the new projected data base

          // Find next position of prefix
          while (pos < lti && prefix != ti(pos)) {
            val item = ti(pos)
            updateSupportCounter(item, sid, pos)
            pos += 1
          }

          // Check if this prefix will still available
          updateSupportCounter(prefix, sid, pos)

          // Update pseudo projected database and support
          psdbSeqId(j) = sid
          psdbPosInSeq(j) = pos + 1
          j += 1
          if (j >= innerTrailSize) growInnerTrail()

          curPrefixSupport += 1

        } else {
          removeAllItemsInSid(sid, pos, lti, ti)
        }
      } else {
        removeAllItemsInSid(sid, pos, lti, ti)
      }

      i += 1
    }

    psdbStart.value = startInit + sizeInit
    psdbSize.value = curPrefixSupport

    curPrefixSupport


  }

  /**
   * Decrease the support of an item if it is removed
   *
   * @param item
   * @param sid
   * @param pos
   */
  def updateSupportCounter(item: Int, sid: Int, pos: Int): Unit = {
    if (lastPositionMap(sid)(item) - 1 <= pos) {
      // The the support of an item doesn't need to be exact when it is below the threshold
      // because at that point this item is not interesting anymore.
      // (This optimization can save some useless trailing operations)
      supportCounter(item).decr()
    }
  }

  /**
   * remove all item in this sequence by decreasing supportCounter *
   *
   * @param sid
   * @param initPos
   * @param lenOfSequence
   * @param sequence
   */
  def removeAllItemsInSid(sid: Int, initPos: Int, lenOfSequence: Int, sequence: Array[Int]): Unit = {
    var pos = initPos
    if (pos < lenOfSequence) {
      updateSupportCounter(sequence(pos), sid, pos)
    }

    val tiLast = lastPositionList(sid)

    //--//println(tiLast.mkString("-")+" pos = "+pos)
    var c = 0
    while ( c < tiLast.length && tiLast(c)-1 > pos ) {
      //////print(tiLast(c)+" ")
      supportCounter ( sequence(tiLast(c)-1) ).decr()
      c += 1
    }

    /*while (!break && pos < lenOfSequence) {
      val last = tiLast(pos)
      if (last == 0) {
        break = true
      } else {
        val item = sequence(last - 1)
        supportCounter(item).decr()
        pos = last - 1
      }
    }*/
  }

  override def associatedVars(): Iterable[CPVar] = P
}