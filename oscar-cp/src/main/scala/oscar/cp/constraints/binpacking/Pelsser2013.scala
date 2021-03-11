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

package oscar.cp.constraints.binpacking

import oscar.algo.Inconsistency
import oscar.algo.reversible.{ReversibleInt, ReversibleSparseSet}
import oscar.cp.core.delta.DeltaIntVar
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.{CPPropagStrength, Constraint}

/**
  * A faster implementation of the Bin-Packing with Cardinality constraint presented in
  *
  * Pelsser F., Schaus P., Régin JC. (2013) Revisiting the Cardinality Reasoning for BinPacking Constraint. In: Schulte C. (eds) Principles and Practice of Constraint Programming. CP 2013. Lecture Notes in Computer Science, vol 8124. Springer, Berlin, Heidelberg
  *
  * @param tmpX tmpX(j) is the bin assigned to item j
  * @param tmpSizes tmpSizes(j) is the weight of item j
  * @param l l(i) is the load of bin i
  * @param c c(i) is the cardinality of bin i
  */
class Pelsser2013(tmpX: Array[CPIntVar], tmpSizes: Array[Int], val l: Array[CPIntVar], val c: Array[CPIntVar]) extends Constraint(tmpX(0).store) {
  var sizeOrder: Array[Int] = tmpSizes.indices.sortBy(i => -tmpSizes(i)).toArray
  val x: Array[CPIntVar] = sizeOrder.map(tmpX(_))
  val sizes: Array[Int] = sizeOrder.map(tmpSizes(_))

  val nObj: Int = x.length
  val nBin: Int = l.length

  // Filled-in in setup
  var deltaX: Array[DeltaIntVar] = _

  // Candidates: unassigned items available for each bin
  val candidates: Array[ReversibleSparseSet] = Array.fill(nBin)(new ReversibleSparseSet(s, 0, nObj-1))

  // Current size
  val packedWeight: Array[ReversibleInt] = Array.fill(nBin)(new ReversibleInt(s, 0))
  val packed: Array[ReversibleInt] = Array.fill(nBin)(new ReversibleInt(s, 0))

  //priorityL2 = CPStore.MaxPriorityL2 - 4
  idempotent = false

  override def associatedVars(): Iterable[CPVar] = x ++ l ++ c

  override def setup(ps: CPPropagStrength): Unit = {
    deltaX = x.map(_.callPropagateOnChangesWithDelta(this))
    l.foreach(_.callPropagateWhenDomainChanges(this))
    c.foreach(_.callPropagateWhenDomainChanges(this))

    //Update candidates and info about packed items
    var i = 0
    while(i != x.length) {
      var j = 0
      while(j != candidates.length) {
        if(!x(i).hasValue(j))
          candidates(j).removeValue(i)
        if(x(i).isBoundTo(j)) {
          candidates(j).removeValue(i)
          packedWeight(j) += sizes(i)
          packed(j) += 1
        }
        j += 1
      }
      i += 1
    }

    propagate()
  }

  override def propagate(): Unit = {
    updateFromDelta()
    var bin = 0
    while(bin != nBin) {
      val (minCard, maxLoad) = genMinCardMaxLoad(bin)
      val (minLoad, maxCard) = genMinLoadMaxCard(bin)
      c(bin).updateMin(minCard)
      c(bin).updateMax(maxCard)
      l(bin).updateMin(minLoad)
      l(bin).updateMax(maxLoad)
      bin += 1
    }
  }

  /**
    * Update candidates/packedWeight/packed from deltaX
    */
  val deltaArray: Array[Int] = Array.ofDim(nBin)
  def updateFromDelta(): Unit = {
    var objIdx = 0
    while(objIdx != x.length) {
      if(deltaX(objIdx) != null && deltaX(objIdx).changed) {
        val length = deltaX(objIdx).fillArray(deltaArray)
        var deltaIdx = 0
        while(deltaIdx != length) {
          candidates(deltaArray(deltaIdx)).removeValue(objIdx)
          deltaIdx += 1
        }
        if(x(objIdx).isBound) {
          candidates(x(objIdx).min).removeValue(objIdx)
          packedWeight(x(objIdx).min) += sizes(objIdx)
          packed(x(objIdx).min) += 1
        }
      }
      objIdx += 1
    }
  }

  /**
    * Check first that the item obk can be given by all the bins that may need it to bin `bin`
    */
  @inline def availableForBin(obj: Int, bin: Int, nAvailableInBin: Array[Int]): Boolean = {
    var available = candidates(bin).hasValue(obj)
    var curBin = 0
    while(available && curBin < nBin) {
      if(curBin != bin && candidates(curBin).hasValue(obj))
        available = nAvailableInBin(curBin) != 0
      curBin += 1
    }
    available
  }

  /**
    * Given that item `obj` is being given to bin `bin`, update nAvailableInBin accordingly
    */
  @inline def updateAvailableForBin(obj: Int, bin: Int, nAvailableInBin: Array[Int]): Unit = {
    var curBin = 0
    while(curBin < nBin) {
      if(curBin != bin && candidates(curBin).hasValue(obj))
        nAvailableInBin(curBin) -= 1
      curBin += 1
    }
  }

  private val objDecreasingOrder: Array[Int] = x.indices.toArray
  private val objIncreasingOrder: Array[Int] = objDecreasingOrder.reverse

  val availableInBin = Array.ofDim[Int](nBin)
  val cardAndLoad = Array.ofDim[Int](2)
  def genMinLoadMaxCard(bin: Int): (Int, Int) = {
    var curBin = 0
    while (curBin != nBin) {
      availableInBin(curBin) = candidates(curBin).size - (c(curBin).min - packed(curBin).value)
      curBin += 1
    }
    cardAndLoad(0) = packed(bin).value
    cardAndLoad(1) = packedWeight(bin).value

    val stopAtLoad = l(bin).max
    val stopAtCard = c(bin).min

    var minLoad: Int = -1

    var curSeqPos = 0
    val objList = objIncreasingOrder
    while(curSeqPos < objList.length && cardAndLoad(1) < stopAtLoad) {
      val curObj = objList(curSeqPos)

      if(minLoad == -1 && cardAndLoad(0) >= stopAtCard) {
        minLoad = cardAndLoad(1)
      }

      //If the object is available...
      if(availableForBin(curObj, bin, availableInBin)) {
        //Take it for us!
        cardAndLoad(0) += 1
        cardAndLoad(1) += sizes(curObj)

        //Do not forget to update availableInBin
        updateAvailableForBin(curObj, bin, availableInBin)
      }
      curSeqPos += 1
    }

    if(minLoad == -1 && cardAndLoad(0) >= stopAtCard) {
      minLoad = cardAndLoad(1)
    }

    if(minLoad == -1)
      throw Inconsistency

    if(cardAndLoad(1) > stopAtLoad)
      (minLoad, cardAndLoad(0)-1) //since the cardAndLoad(0) less weighted items > stopAtLoad,
                                  //they are max cardAndLoad(0)-1 items to stay <= stopAtLoad
    else
      (minLoad, cardAndLoad(0))
  }

  def genMinCardMaxLoad(bin: Int): (Int, Int) = {
    var curBin = 0
    while (curBin != nBin) {
      availableInBin(curBin) = candidates(curBin).size - (c(curBin).min - packed(curBin).value)
      curBin += 1
    }
    cardAndLoad(0) = packed(bin).value
    cardAndLoad(1) = packedWeight(bin).value

    val stopAtLoad = l(bin).min
    val stopAtCard = c(bin).max

    var minCard: Int = -1

    var curSeqPos = 0
    val objList = objDecreasingOrder
    while(curSeqPos < objList.length && cardAndLoad(0) < stopAtCard) {
      val curObj = objList(curSeqPos)

      if(minCard == -1 && cardAndLoad(1) >= stopAtLoad) {
        minCard = cardAndLoad(0)
      }

      //If the object is available...
      if(availableForBin(curObj, bin, availableInBin)) {
        //Take it for us!
        cardAndLoad(0) += 1
        cardAndLoad(1) += sizes(curObj)

        //Do not forget to update availableInBin
        updateAvailableForBin(curObj, bin, availableInBin)
      }
      curSeqPos += 1
    }

    if(minCard == -1 && cardAndLoad(1) >= stopAtLoad) {
      minCard = cardAndLoad(0)
    }

    if(minCard == -1) {
      throw Inconsistency
    }
    (minCard, cardAndLoad(1))
  }
}
