package oscar.ml.pm.Constraints.spm

import oscar.algo.Inconsistency
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core._
import oscar.cp.core.variables._
import oscar.ml.pm.utils.{Dataset, DatasetUtils}

/**
 * PPIC [Constraint Programming & Sequential Pattern Mining with Prefix projection method]
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
 */

class PPIC(val P: Array[CPIntVar], val minsup: Int, val data: Dataset) extends Constraint(P(0).store, "PPIC") {

  idempotent = true

  /// Initializing the other input variables (precomputed data structures)
  private[this] val SDB: Array[Array[Int]] = data.getData

  private[this] val epsilon = 0 //this is for empty item
  private[this] val lenSDB = SDB.size
  private[this] val nItems: Int = data.nbItem
  private[this] val patternSeq = P.clone()
  private[this] val lenPatternSeq = P.length

  // precomputed data structures
  private[this] val SdbOfLastPos: Array[Array[Int]] = DatasetUtils.getSDBLastPos(data)
  private[this] val firstPosOfItemBySequence: Array[Array[Int]] = DatasetUtils.getItemFirstPosBySequence(data)

  /**
   * lastPosOfItemBySequence: is the last real position of an item in a sequence, if 0 it is not present
   * s1  s2  s3  s4
   * a  1   2   1   0
   * b  4   3   2   1
   * c  5   4   0   2
   * d  0   0   0   3
   */
  private[this] val lastPosOfItemBySequence: Array[Array[Int]] = DatasetUtils.getItemLastPosBySequence(data)

  /**
   * itemsSupport: is the initial support (number of sequences where a item is appeared) of all items
   * a : 3, b : 4, c : 3, d : 1
   */
  private[this] val itemsSupport: Array[Int] = DatasetUtils.getSDBSupport(data)

  ///representation of pseudo-projected-database
  private[this] var innerTrailSize = lenSDB * 5
  private[this] var psdbSeqId = Array.tabulate(innerTrailSize)(i => i) //the N° of Sequence (sid)
  private[this] var psdbPosInSeq = Array.tabulate(innerTrailSize)(i => -1) //position of prefix in this sid
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
  private[this] var supportCounter: Array[Int] = Array.ofDim(nItems)
  System.arraycopy(itemsSupport, 0, supportCounter, 0, nItems)
  var curPrefixSupport: Int = 0

  ///current position in P $P_i = P[curPosInP.value]$
  private[this] val curPosInP = new ReversibleInt(s, 0)

  ///check if pruning is done successfully
  private[this] var pruneSuccess = true

  ///init to zero array
  ////private[this] val MAX_SEQ_SIZE = 1000
  private[this] val MAX_INITARRAY_SIZE: Int = Array[Int](lenPatternSeq + 1, nItems).max
  private[this] val initArray = Array.fill[Int](MAX_INITARRAY_SIZE)(0)

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
    var v = curPosInP.value

    if (P(v).isBoundTo(epsilon)) {
      if (!P(v - 1).isBoundTo(epsilon)) {
        enforceEpsilonFrom(v)
      }
    } else {

      while (v < P.length && P(v).isBound && P(v).min != epsilon) {

        if (!filterPrefixProjection(P(v).getMin)) throw Inconsistency

        curPosInP.incr()
        v = curPosInP.value
      }

      if (v > 0 && v < P.length && P(v).isBoundTo(epsilon)) {
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
        pruneSuccess = true
        ///Prune next position pattern P domain if it exists unfrequent items
        prune(i)

        pruneSuccess
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
  private def projectSDB(prefix: Int): Int = {
    val startInit = psdbStart.value
    val sizeInit = psdbSize.value

    //Count sequences validated for next step
    curPrefixSupport = 0

    //allow to predict failed sid (sequence) and remove it
    val nbAddedTarget = itemsSupport(prefix)

    //reset support to 0
    System.arraycopy(initArray, 0, supportCounter, 0, nItems)

    var i = startInit
    var j = startInit + sizeInit
    var nbAdded = 0
    //////println("startInit = "+startInit+" sizeInit = "+sizeInit)
    // Tias optim: nbAdded < nbAddedTarget
    // because we know how many need to be added so we can stop when this target is reached
    while (i < startInit + sizeInit && nbAdded < nbAddedTarget) {

      val sid = psdbSeqId(i)

      val ti = SDB(sid)
      val lti = ti.length
      val start = psdbPosInSeq(i)

      var pos = start

      if (lastPosOfItemBySequence(prefix)(sid) != 0) {
        // here we know at least that prefix is present in sequence sid

        // search for next value "prefix" in the sequence starting from
        if (lastPosOfItemBySequence(prefix)(sid) - 1 >= pos) {
          // we are sure prefix next position is available and so we add the sequence in the new projected data base

          nbAdded += 1

          // find next position of prefix
          if (start == -1) {
            pos = firstPosOfItemBySequence(prefix)(sid) - 1
          } else {
            while (pos < lti && prefix != ti(pos)) {
              pos += 1
            }
          }

          //update pseudo projected database and support
          psdbSeqId(j) = sid
          psdbPosInSeq(j) = pos + 1
          j += 1
          if (j >= innerTrailSize) growInnerTrail()

          curPrefixSupport += 1

          //recompute support
          var break = false
          val tiLast = SdbOfLastPos(sid)

          while (!break && pos < lti) {
            val last = tiLast(pos)

            if (last == 0) {
              break = true
            } else {
              val item = ti(last - 1)
              supportCounter(item) += 1
              pos = last - 1
            }

          }

        }
      }

      i += 1
    }

    psdbStart.value = startInit + sizeInit
    psdbSize.value = curPrefixSupport

    curPrefixSupport

  }

  override def associatedVars(): Iterable[CPVar] = P
}