package oscar.ml.spm


import oscar.algo.Inconsistency
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core._
import oscar.cp.core.variables._

/**
  * PPIC [Constraint Programming & Sequential Pattern Mining with Prefix projection method]
  * is the CP version of Prefix projection method of Sequential Pattern Mining (with several improvements)
  * which is based on projected database (We use here pseudo-projected-database \{ (sid, pos) \}).
  *
  * This constraint generate all available solution given such parameters
  *
  * @param P             , is pattern where $P_i$ is the item in position $i$ in $P$
  * @param SDB           , [sequence database] it is a set of sequences. Each line $SDB_i$ or $t_i$ represent a sequence
  * @param minsup        is a threshold support, item must appear in at least minsup sequences $support(item)>=minsup$
  * @author John Aoga (johnaoga@gmail.com) and Pierre Schaus (pschaus@gmail.com)
  *
  * AOGA, John OR, GUNS, Tias, et SCHAUS, Pierre. An efficient algorithm for mining frequent sequence with constraint programming.
  * In : Joint European Conference on Machine Learning and Knowledge Discovery in Databases. Springer, Cham, 2016. p. 315-330.
  */
class PPIC(val P: Array[CPIntVar], val SDB: Array[Array[Int]], val minsup: Int) extends Constraint(P(0).store, "PPIC") {

  // TODO: check the parameters

  // we assume that the items are between 1 and nItems

  val nItems = (SDB.map(_.max)).max
  val nSeq = SDB.length


  // precomputed data-structes (see Table1 in the paper).

  val lastPosMap = Array.ofDim[Int](nSeq,nItems + 1)
  val firstPosMap = Array.ofDim[Int](nSeq,nItems + 1)
  val itemsSupport = Array.ofDim[Int](nItems + 1)
  val lastPosList = Array.ofDim[Array[Int]](nSeq)
  preprocessMaps()

  idempotent = true

  private[this] val epsilon = 0 //this is for empty item
  private[this] val lenSDB = SDB.size
  private[this] val patternSeq = P.clone()
  private[this] val lenPatternSeq = P.length

  ///representation of pseudo-projected-database
  private[this] var innerTrailSize = lenSDB * 5
  private[this] var psdbSeqId = Array.tabulate(innerTrailSize)(i => i) //the NÂ° of Sequence (sid)
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
  private[this] var supportCounter = itemsSupport
  var curPrefixSupport: Int = 0

  ///current position in P $P_i = P[curPosInP.value]$
  private[this] val curPosInP = new ReversibleInt(s, 0)

  def preprocessMaps(): Unit = {
    var sid = 0
    while (sid < nSeq) {
      var j = 0
      val len = SDB(sid).length
      val visitedItem = Array.ofDim[Boolean](nItems + 1)
      while (j < len) {
        val item = SDB(sid)(j)
        if (lastPosMap(sid)(item) < j + 1) {
          lastPosMap(sid)(item) = j + 1
        }
        j += 1
        if (!visitedItem(item)) {
          firstPosMap(sid)(item) = j + 1
          itemsSupport(item) += 1
          visitedItem(item) = true
        }
      }
      sid += 1
    }
    sid = 0
    while (sid < nSeq) {
      var j = 0
      val len = SDB(sid).length
      val tempLastNext = Array.ofDim[Int](len)
      while (j < len) {
        val tab = lastPosMap(sid).filter(p => p != 0 && p > j + 1)
        if (!tab.isEmpty) {
          tempLastNext(j) = tab.min
        }
        j += 1
      }
      lastPosList(sid) = tempLastNext
      sid += 1
    }
  }


  override def associatedVars(): Iterable[CPVar] = P


  /**
    * Entry in constraint, function for all init
    *
    * @param l
    * @return The outcome of the first propagation and consistency check
    */
  final override def setup(l: CPPropagStrength) = {
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
  final override def propagate() = {
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


  // enforces P[i,..] = epsillon
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
        false
      } else {
        prune(i)
        true
      }
    }
  }

  // Remove all un-frequent items from P(i)
  val dom = Array.ofDim[Int](nItems)
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

    // Count sequences validated for next step
    curPrefixSupport = 0

    // Allow to predict failed sid (sequence) and remove it
    val nbAddedTarget = itemsSupport(prefix)

    // Reset support to 0
    supportCounter = Array.fill[Int](nItems)(0)

    var i = startInit
    var j = startInit + sizeInit
    var nbAdded = 0

    // nbAdded < nbAddedTarget is an optimization since the number of items to be added is known
    while (i < startInit + sizeInit && nbAdded < nbAddedTarget) {

      val sid = psdbSeqId(i)
      val ti = SDB(sid)
      val lti = ti.length
      val start = psdbPosInSeq(i)
      var pos = start

      if (lastPosMap(sid)(prefix) != 0) {
        // We know at least that prefix is present in sequence sid

        // search for next value "prefix" in the sequence starting from
        if (lastPosMap(sid)(prefix) - 1 >= pos) {
          //  Prefix next position is available,
          //  we can thus add the sequence in the new projected data base

          nbAdded += 1

          // Find next position of prefix
          if (start == -1) {
            pos = firstPosMap(sid)(prefix) - 1
          } else {
            while (pos < lti && prefix != ti(pos)) {
              pos += 1
            }
          }

          // Update pseudo projected database and support
          psdbSeqId(j) = sid
          psdbPosInSeq(j) = pos + 1
          j += 1
          if (j >= innerTrailSize) growInnerTrail()

          curPrefixSupport += 1

          // Recompute support
          var break = false
          val tiLast = lastPosList(sid)

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

    return curPrefixSupport

  }

}


