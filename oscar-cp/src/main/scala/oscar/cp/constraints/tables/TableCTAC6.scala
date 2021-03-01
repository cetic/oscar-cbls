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

package oscar.cp.constraints.tables


import oscar.algo.Inconsistency
import oscar.algo.reversible.{ReversibleBoolean, ReversibleInt, TrailEntry}
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.{CPPropagStrength, CPStore, Constraint}

import scala.collection.mutable.ArrayBuffer
import oscar.cp.core.delta.DeltaIntVar


/**
 * Implementation of the Compact Table algorithm (CT) for the table constraint.
 * @param X the variables restricted by the constraint.
 * @param table the list of tuples composing the table.
 * @author Jordan Demeulenaere j.demeulenaere1@gmail.com
 * @author Pierre Schaus pschaus@gmail.com
 */

/* Trailable entry to restore the value of the ith Long of the valid tuples */
final class TableCTAC6LongTrailEntry(table: TableCTAC6, i: Int, value: Long) extends TrailEntry {
  @inline override def restore(): Unit = table.restore(i, value)
}

final class TableCTAC6(X: Array[CPIntVar], table: Array[Array[Int]]) extends Constraint(X(0).store, "TableCTAC6") {

  override def associatedVars(): Iterable[CPVar] = X

  /* Setting idempotency & lower priority for propagate() */
  idempotent = true
  priorityL2 = CPStore.MaxPriorityL2 - 1

  /* Basic information */
  private[this] val arity = X.length
  private[this] val nbTuples = table.length
  private[this] val spans = Array.tabulate(arity)(i => X(i).max - X(i).min + 1)
  private[this] val originalMins = Array.tabulate(arity)(i => X(i).min)
  private[this] val store = X(0).store
  private[this] val maxDomain = X.map(_.size).max
  private[this] val domainArray = new Array[Int](maxDomain)
  private[this] var domainArraySize = 0

  /* Bitsets and other arrays */
  private[this] var nbLongs = 0
  private[this] val validTuplesBuffer = ArrayBuffer[Int]()
  private[this] var validTuples: Array[Long] = null
  private[this] val masks = Array.tabulate(arity)(i => new Array[Array[Long]](spans(i)))
  private[this] var lastMagics: Array[Long] = null
  private[this] var tempMask: Array[Long] = null

  /* We keep the valid longs in a sparse set */
  private[this] var validLongs: Array[Int] = null
  private[this] val nValidLongsRev = new ReversibleInt(s, 0)
  private[this] var nValidLongs: Int = 0


  // sparseSet for valid Longs
  private[this] var nValidLongsToCheck: Int = 0
  private[this] var validLongstoCheck: Array[Int] = null
  private[this] var validLongstoCheckMap: Array[Int] = null

  @inline private def initValidLongsToCheckSparseSet(n: Int): Unit = {
    validLongstoCheck = Array.tabulate(n)(i => i)
    validLongstoCheckMap = Array.tabulate(n)(i => i)
  }

  @inline private def insertValidLongToCheck(v: Int): Unit = {
    if (validLongstoCheckMap(v) >= nValidLongsToCheck) {
      val i1 = validLongstoCheckMap(v)
      val v2 = validLongstoCheck(nValidLongsToCheck)
      validLongstoCheck(nValidLongsToCheck) = v
      validLongstoCheckMap(v) = nValidLongsToCheck
      validLongstoCheck(i1) = v2
      validLongstoCheckMap(v2) = i1
      nValidLongsToCheck += 1
    }
  }

  /* Structures for the improvements */
  private[this] var touchedVar = -1
  private[this] val needPropagate = new ReversibleBoolean(store, false)

  var deltas: Array[DeltaIntVar] = new Array[DeltaIntVar](arity)

  /* Inverse Last Supports Lists */

  class NodeSupport(val varIndex: Int, val valIndex: Int, var wordIndex: Int,
                    var prev: NodeSupport, var next: NodeSupport, var inList: Boolean = false) extends TrailEntry {

    override def toString(): String = {
      s"($varIndex,$valIndex)"
      //"x"+varIndex+" v:"+valIndex
    }


    /* restore the support in its reverse support list */
    @inline override def restore(): Unit = {
      // and add it as first element in its new list
      prev = null
      val newNext = inverseLastSupports(wordIndex)
      next = newNext
      if (newNext != null) newNext.prev = this
      inverseLastSupports(wordIndex) = this
      inList = true
    }

    /* remove it from the reverse support list where it is in a reversible way */
    @inline def reversibleRemove() = {
      //println("remove:"+this)
      if (wordIndex >= 0) { // check if the node is already in a list
        // remove the node from its current list
        if (prev == null) { // is the first one
          inverseLastSupports(wordIndex) = next
          if (next != null) {
            next.prev = null
          }
        } else { // node is in the middle or the last
          prev.next = next
          if (next != null) { // not the last
            next.prev = prev
          }
        }
      }
      next = null
      prev = null
      inList = false
      store.trail(this)
    }


  }
  private[this] val lastSupports = Array.tabulate(arity)(i => Array.tabulate(spans(i))(j => new NodeSupport(i,j,-1,null,null)))
  private[this] var inverseLastSupports: Array[NodeSupport] = null // first node is a dummy node


  def printInverseLastSupportsLists(): Unit = {
    for (i <- 0 until nbLongs) {

      var curr = inverseLastSupports(i)

      println("list of word"+i)
      while (curr != null) {
        print(s"$curr,")
        curr = curr.next

      }
      print("\n")

    }
  }


  /* Move the corresponding node in the appropriate list */
  @inline private def updateWordSupport(variableIndex: Int, valueIndex: Int, wordIndex: Int): Unit = {

    val node: NodeSupport = lastSupports(variableIndex)(valueIndex)
    val oldWord = node.wordIndex
    node.wordIndex = wordIndex

    // remove the node from its current list
    if (oldWord >= 0) { // check if the node is already in a list
      if (node.prev == null) { // node is the first one
        inverseLastSupports(oldWord) = node.next
        if (node.next != null) {
          node.next.prev = null
        }
      } else { // node is in the middle or the last
        node.prev.next = node.next
        if (node.next != null) { // not the last
          node.next.prev = node.prev
        }
      }
    }

    // and add it as first element in its new list
    node.prev = null
    node.next = null
    val newNext = inverseLastSupports(wordIndex)
    node.next = newNext
    if (newNext != null) newNext.prev = node
    inverseLastSupports(wordIndex) = node
    node.inList = true

  }

  def isInfinite(): Boolean = {
    for (w <- 0 until nbLongs) {
      var curr = inverseLastSupports(w)
      var i = 0
      while (curr != null) {
        curr = curr.next
        i += 1
        if (i > 10000) return true
      }

    }
    return false
  }

  def noSelfLoops(): Boolean = {
    lastSupports.forall(_.forall(n => n.next != n && n.prev != n))
  }



  override def setup(l: CPPropagStrength): Unit = {

    /* Retrieve the current valid tuples */
    fillValidTuples()

    /* Compute the masks for each (x,a) pair */
    fillMasksAndComputeSupports()

    /* Remove values not supported by any tuple */
    removeUnsupportedValues()

    /* Call propagate() and update(x, delta) when domains change */
    var i = 0
    while (i < arity) {
      val x = X(i)
      deltas(i) = x.callPropagateOnChangesWithDelta(this)
      i += 1
    }
  }


  /*
  @inline private def updateDelta(varIndex: Int, delta: DeltaIntVar): Unit = {
    //println("delta size:"+delta.size)
    val intVar = X(varIndex)//delta.variable

    // Cache reversible values
    nValidLongs = nValidLongsRev.value

    var changed = false
    val originalMin = originalMins(varIndex)

    clearTempMask()
    domainArraySize = delta.fillArray(domainArray)
    var i = 0
    while (i < domainArraySize) {
      val valueIndex = domainArray(i) - originalMin
      lastSupports(varIndex)(valueIndex).reversibleRemove()
      orTempMask(varIndex, valueIndex)

      i += 1
    }
    changed = substractTempMaskFromValid()

    /* If validTuples has changed, we need to perform a consistency check by propagate() */
    if (changed) {
      /* Failure if there are no more valid tuples */
      if (nValidLongs == 0) {
        return Failure
      }

      /* We check if x was the only modified variable since last propagate() */
      if (touchedVar == -1 || touchedVar == varIndex) touchedVar = varIndex
      else touchedVar = -2

      needPropagate.setTrue()
    }

    // Trail reversibles
    nValidLongsRev.value = nValidLongs

    Suspend
  }*/

  /**
   * Invalidates tuples by handling delta, the set of values removed from D(x) since the last call to this function
   * @param varIndex the index of x in the array of variables.
   * @param delta the set of values removed since the last call.
   * @return the outcome i.e. Failure or Success.
   */
  @inline private def updateDelta(varIndex: Int, delta: DeltaIntVar): Unit = {
    //println("delta size:"+delta.size)
    val intVar = X(varIndex)//delta.variable

    // Cache reversible values
    nValidLongs = nValidLongsRev.value

    var changed = false
    val originalMin = originalMins(varIndex)
    val varSize = intVar.size
    val varMin = intVar.min


    /* Update the value of validTuples by considering D(x) or delta */
    if (varSize == 1) {
      /* The variable is assigned */
      setTempMask(varIndex, varMin - originalMin)
      changed = andTempMaskWithValid()
    } else {
      clearTempMask()
      if (delta.size < varSize /*never do the reset*/ ) {
        /* Use delta to update validTuples */
        domainArraySize = delta.fillArray(domainArray)
        var i = 0
        while (i < domainArraySize) {
          val valueIndex = domainArray(i) - originalMin
          // remove from the support list and trail this removal
          lastSupports(varIndex)(valueIndex).reversibleRemove()
          orTempMask(varIndex, valueIndex)
          i += 1
        }
        changed = substractTempMaskFromValid()
      } else {
        /* Use domain to update validTuples */
        domainArraySize = intVar.fillArray(domainArray)
        var i = 0
        while (i < domainArraySize) {
          orTempMask(varIndex, domainArray(i) - originalMin)
          i += 1
        }
        changed = andTempMaskWithValid()
      }

    }

    /* If validTuples has changed, we need to perform a consistency check by propagate() */
    if (changed) {
      /* Failure if there are no more valid tuples */
      if (nValidLongs == 0) {
        throw Inconsistency
      }

      /* We check if x was the only modified variable since last propagate() */
      if (touchedVar == -1 || touchedVar == varIndex) touchedVar = varIndex
      else touchedVar = -2

      needPropagate.setTrue()
    }

    // Trail reversibles
    nValidLongsRev.value = nValidLongs
  }


  /**
   * Perform a consistency check : for each variable value pair (x,a), we check if a has at least one valid support.
   * Unsupported values are removed.
   * @return the outcome i.e. Failure or Success.
   */
  var nCheck = 0
  override def propagate(): Unit = {
    //println("propagate"+scala.util.Random.nextInt(10))

    // Cache reversible values
    val nValidLongsTmp = nValidLongsRev.value
    nValidLongs = nValidLongsTmp


    // reset
    needPropagate.setFalse()
    //toCheck.clear()
    nValidLongsToCheck = 0
    var i = 0

    // do the update delta
    i = 0
    while (i < arity) {
      if (deltas(i).size > 0) {
        updateDelta(i,deltas(i))
      }
      i += 1
    }

  /*
    var cpt = 0
    i =  nValidLongsTmp
    while (i > 0) {
      i -= 1
      val offset = validLongs(i)
      if (supportsAddedToCheck(offset)) cpt += 1
    }
    println(cpt+"/"+nValidLongsTmp)
*/

    // No need for the check if validTuples has not changed
    if (!needPropagate.value) {
      return
    }



    if (touchedVar == -2) {
      touchedVar = -1
    }

    /* For each variable value (x,a), check if a is supported. Unsupported values are removed from their respective
     * domains */
    //println(toCheck.size)


    i = 0
    while (i < nValidLongsToCheck) {

        val offset = validLongstoCheck(i)
        var curr = inverseLastSupports(offset)

        while (curr != null) {
          val node = curr
          curr = curr.next
          val varIdx = node.varIndex
          val valIdx = node.valIndex
          nCheck += 1

          if (!X(varIdx).hasValue(valIdx+originalMins(varIdx))) {
            //println("problem")
            lastSupports(varIdx)(valIdx).reversibleRemove()
          }
          else if (/*X(varIdx).hasValue(valIdx+originalMins(varIdx)) &&*/ !supported(varIdx, valIdx)) {
            X(varIdx).removeValue(valIdx + originalMins(varIdx))
          }
        }
        i += 1
    }


  /*
    var varIndex = 0
    while (varIndex < arity) {
      domainArraySize = X(varIndex).fillArray(domainArray)
      var i = 0
      var value = 0
      while (i < domainArraySize) {
        value = domainArray(i)
        if (!supported(varIndex, value - originalMins(varIndex))) {
          X(varIndex).removeValue(value)
        }
        i += 1
      }
      varIndex += 1
    }*/

    // Trail reversibles
    nValidLongsRev.value = nValidLongs
  }






  /* ----- Functions used during propagation ----- */

  /**
   * Check whether a variable value (x,a) is supported by the current validTuples.
   * @param varIndex the index of the variable.
   * @param valueIndex the index of the value (i.e. value - originalMin(x)).
   * @return true if (x,a) is supported, false otherwise.
   */
  @inline private final def supported(varIndex: Int, valueIndex: Int): Boolean = {
    /* We check if the last support is still a support */
    val mask = masks(varIndex)(valueIndex)
    val support = lastSupports(varIndex)(valueIndex).wordIndex
    if ((mask(support) & validTuples(support)) != 0) {
      return true
    }


    /* We check the equations
     *         mask(x,a) & validTuples != 0
     * only for the relevant parts of the bitsets. */
    var i = 0 //nValidLongs
    while (i < nValidLongs) {
      val offset = validLongs(i)
      if ((mask(offset) & validTuples(offset)) != 0) {
        /* We found a new support, we update it */
        updateWordSupport(varIndex,valueIndex,offset)

        return true
      }
      i += 1
    }

    // remove from the support list and trail this removal
    lastSupports(varIndex)(valueIndex).reversibleRemove()


    false
  }

  /**
   * Set the bitset tempMask to be the same as mask(x,a).
   * @param varIndex the index of x.
   * @param valueIndex the index of a (i.e. a - originalMin(x)).
   */
  @inline private def setTempMask(varIndex: Int, valueIndex: Int): Unit = {
    var i = nbLongs
    while (i > 0) {
      i -= 1
      tempMask(i) = masks(varIndex)(valueIndex)(i)
    }
  }

  /**
   * Clear the tempMask.
   */
  @inline private def clearTempMask(): Unit = {
    var i = nbLongs
    while (i > 0) {
      i -= 1
      tempMask(i) = 0L
    }
  }

  /**
   * Apply the OR bitwise operation between tempMask and mask(x,a), and store the result in tempMask :
   *      tempMask = tempMask |= mask(x,a)
   * @param varIndex the index of x.
   * @param valueIndex the index of a (i.e. a - originalMin(x)).
   */
  @inline private def orTempMask(varIndex: Int, valueIndex: Int): Unit = {
    val mask = masks(varIndex)(valueIndex)
    var i = nValidLongs
    while (i > 0) {
      i -= 1
      val offset = validLongs(i)
      tempMask(offset) |= mask(offset)
    }
  }

  /**
   * Apply the AND bitwise operation between validTuples and tempmask, and store the result in validTuples :
   *      validTuples = validTuples & tempMask
   * @return true if validTuples has changed, false otherwise.
   */
  @inline private def andTempMaskWithValid(): Boolean = {
    var changed = false
    var i = nValidLongs
    while (i > 0) {
      i -= 1
      val offset = validLongs(i)
      val tempMaskOffset = tempMask(offset)
      if ((~tempMaskOffset & validTuples(offset)) != 0) {
        andValidTuples(i, offset, tempMaskOffset)
        insertValidLongToCheck(offset)
        changed = true
      }
    }
    changed
  }

  /**
   * Set to 0 each bit in validTuples that are set in tempMask, i.e. :
   *      validTuples = validTuples & (~tempMask)
   * @return true if validTuples has changed, false otherwise.
   */
  @inline private def substractTempMaskFromValid(): Boolean = {
    var changed = false
    var i = nValidLongs
    while (i > 0) {
      i -= 1
      val offset = validLongs(i)
      val tempMaskOffset = tempMask(offset)
      if ((tempMaskOffset & validTuples(offset)) != 0) {
        andValidTuples(i, offset, ~tempMaskOffset)
        insertValidLongToCheck(offset)
        changed = true
      }
    }
    changed
  }

  /**
   * Apply the AND bitwise operation with validTuples[offset] and another Long :
   *      validTuples[offset] = validTuples[offset] & mask
   * Trail the value of validTuples[offset] before if it is the first time that it is changed in this search node.
   * @param offset the index of the Long in validTuples to change.
   * @param mask the mask to apply.
   */
  @inline private def andValidTuples(position: Int, offset: Int, mask: Long): Unit = {

    val storeMagic = store.magic
    if (lastMagics(offset) != storeMagic) {
      lastMagics(offset) = storeMagic
      trail(offset)
    }

    val newLong = validTuples(offset) & mask
    validTuples(offset) = newLong

    // Remove the long from the set if
    if (newLong == 0) {
      nValidLongs -= 1
      validLongs(position) = validLongs(nValidLongs)
      validLongs(nValidLongs) = offset
    }
  }

  /**
   * Trail the value of validTuples[offset].
   * @param offset the index of the Long to trail.
   */
  @inline private def trail(offset: Int): Unit = {
    val trailEntry = new TableCTAC6LongTrailEntry(this, offset, validTuples(offset))
    store.trail(trailEntry)
  }

  /**
   * Restore validTuples[offset] to an old value.
   * @param offset the index of the Long to restore.
   * @param value the value to restore.
   */
  @inline final def restore(offset: Int, value: Long): Unit = validTuples(offset) = value







  /* ----- Functions used during the setup of the constraint ----- */

  /* Bits operations */
  @inline private def oneBitLong(pos: Int): Long = 1L << pos
  @inline private def bitLength(size: Int): Int = (size + 63) >>> 6 // = pos / 64 + 1
  @inline private def bitOffset(pos: Int): Int = pos >>> 6 // = pos / 64
  @inline private def bitPos(pos: Int): Int = pos & 63 // = pos % 63
  @inline private def setBit(bitset: Array[Long], pos: Int): Unit = bitset(bitOffset(pos)) |= oneBitLong(bitPos(pos))

  /**
   * Check if a tuple is valid.
   * @param tupleIndex the index of the tuple in the table.
   * @return true if the tuple is valid, false otherwise.
   */
  @inline private def isTupleValid(tupleIndex: Int): Boolean = {
    var varIndex = 0
    while (varIndex < arity) {
      if (!X(varIndex).hasValue(table(tupleIndex)(varIndex))) {
        return false
      }
      varIndex += 1
    }
    true
  }

  /**
   * Retrieve the valid tuples from the table and store their index in validTuplesBuffer.
   * @return Failure if there is no valid tuples, Suspend otherwise.
   */
  @inline private def fillValidTuples(): Unit = {
    validTuplesBuffer.clear()
    var tupleIndex = 0
    while (tupleIndex < nbTuples) {
      if (isTupleValid(tupleIndex)) {
        validTuplesBuffer += tupleIndex
      }
      tupleIndex += 1
    }

    if (validTuplesBuffer.isEmpty) {
      throw Inconsistency
    }

    /* Compute number of Long in a bitset */
    nbLongs = bitLength(validTuplesBuffer.length)
  }

  /**
   * Compute the mask for each variable value pair (x,a).
   */
  @inline private def fillMasksAndComputeSupports(): Unit = {


    inverseLastSupports = new Array[NodeSupport](nbLongs)

    initValidLongsToCheckSparseSet(nbLongs)


    tempMask = Array.fill(nbLongs)(0L)
    validTuples = Array.fill(nbLongs)(0L)
    lastMagics = Array.fill(nbLongs)(-1L)
    validLongs = Array.tabulate(nbLongs)(i => i)
    nValidLongsRev.value = nbLongs

    var validIndex = 0
    while (validIndex < validTuplesBuffer.length) {
      setBit(validTuples, validIndex)

      val tupleIndex = validTuplesBuffer(validIndex)
      var varIndex = 0
      while (varIndex < arity) {
        val value = table(tupleIndex)(varIndex)
        val valueIndex = value - originalMins(varIndex)
        var mask = masks(varIndex)(valueIndex)

        if (mask == null) {
          mask = Array.fill(nbLongs)(0L)
          masks(varIndex)(valueIndex) = mask
        }
        setBit(mask, validIndex)

        updateWordSupport(varIndex,valueIndex,validIndex >> 6)

        varIndex += 1
      }

      validIndex += 1
    }

    validTuplesBuffer.clear()
  }

  /**
   * Remove values not supported by any tuple.
   * @return the outcome i.e. Failure or Suspend.
   */
  @inline private def removeUnsupportedValues(): Unit = {
    var varIndex = 0
    while (varIndex < arity) {
      val intVar = X(varIndex)
      domainArraySize = intVar.fillArray(domainArray)
      var i = 0
      while (i < domainArraySize) {
        val value = domainArray(i)
        val valueIndex = value - originalMins(varIndex)
        if (masks(varIndex)(valueIndex) == null) {
          intVar.removeValue(value)
        }
        i += 1
      }
      varIndex += 1
    }
  }

}