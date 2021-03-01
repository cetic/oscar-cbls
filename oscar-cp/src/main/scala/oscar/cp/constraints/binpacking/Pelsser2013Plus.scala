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
  * An implementation of
  *
  * Pelsser F., Schaus P., Régin JC. (2013) Revisiting the Cardinality Reasoning for BinPacking Constraint. In: Schulte C. (eds) Principles and Practice of Constraint Programming. CP 2013. Lecture Notes in Computer Science, vol 8124. Springer, Berlin, Heidelberg
  *
  * that additionnaly adds the cardinality bound from IsolatedBinWithCardinality
  *
  * @param tmpX tmpX(j) is the bin assigned to item j
  * @param tmpSizes tmpSizes(j) is the weight of item j
  * @param l l(i) is the load of bin i
  * @param c c(i) is the cardinality of bin i
  */
class Pelsser2013Plus(tmpX: Array[CPIntVar], tmpSizes: Array[Int], val l: Array[CPIntVar], val c: Array[CPIntVar]) extends Constraint(tmpX(0).store) {
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
      updateMinCardMaxLoad(bin)
      updateMinLoadMaxCard(bin)
      bin += 1
    }
  }

  /**
    * Remove the possibility that an object can be assigned to the given bin.
    *
    * Ensures all structures are updated. Use is mandatory to make DeltaIntVars work!
    */
  def removeObjFromBin(obj: Int, bin: Int) = {
    x(obj).removeValue(bin)
    candidates(bin).removeValue(obj)
    if(x(obj).isBound) {
      candidates(x(obj).min).removeValue(obj)
      packedWeight(x(obj).min) += sizes(obj)
      packed(x(obj).min) += 1
    }
  }

  /**
    * Update candidates/packedWeight/packed from deltaX
    */
  val deltaArray: Array[Int] = Array.ofDim(nBin+1)
  def updateFromDelta(): Unit = {
    var objIdx = 0
    while(objIdx != x.length) {
      if(deltaX(objIdx) != null && deltaX(objIdx).changed) {
        val length = deltaX(objIdx).fillArray(deltaArray)
        var deltaIdx = 0
        var removedSmgt = false
        while(deltaIdx != length) {
          removedSmgt |= candidates(deltaArray(deltaIdx)).removeValue(objIdx)
          deltaIdx += 1
        }
        if(removedSmgt && x(objIdx).isBound) {
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

  val availableInBin = Array.ofDim[Int](nBin)
  def updateMinLoadMaxCard(bin: Int): Unit = {
    var curBin = 0
    while (curBin != nBin) {
      availableInBin(curBin) = candidates(curBin).size - (c(curBin).min - packed(curBin).value)
      curBin += 1
    }
    var currentCard = packed(bin).value
    var currentLoad = packedWeight(bin).value

    val stopAtLoad = l(bin).max
    val stopAtCard = c(bin).min

    var minLoad: Int = -1

    var loadAtMinCardMinus1 = currentLoad

    var curObj = nObj-1
    while(curObj >= 0 && currentLoad <= stopAtLoad) {
      if(minLoad == -1 && currentCard >= stopAtCard) {
        minLoad = currentLoad
      }
      if(currentCard <= stopAtCard - 1)
        loadAtMinCardMinus1 = currentLoad

      //If the object is available...
      if(availableForBin(curObj, bin, availableInBin)) {
        //Take it for us!
        currentCard += 1
        currentLoad += sizes(curObj)

        //Do not forget to update availableInBin
        updateAvailableForBin(curObj, bin, availableInBin)
      }

      curObj -= 1
    }

    if(minLoad == -1 && currentCard >= stopAtCard) {
      minLoad = currentLoad
    }

    if(minLoad == -1)
      throw Inconsistency

    l(bin).updateMin(minLoad)
    if(currentLoad > stopAtLoad)
      c(bin).updateMax(currentCard-1)//since the cardAndLoad(0) less weighted items > stopAtLoad,
                                     //they are max cardAndLoad(0)-1 items to stay <= stopAtLoad
    else
      c(bin).updateMax(currentCard)

    //loadAtMinCardMinus1 contains the load of the c(bin).min-1 less weighted items possible to fit according to our
    //(optimistic) heuristic. Let's remove all the items that can not fit in our higher load bound
    val maxWeight = l(bin).max-loadAtMinCardMinus1
    curObj = 0
    while (curObj < nObj) {
      if(candidates(bin).hasValue(curObj) && sizes(curObj) > maxWeight) {
        removeObjFromBin(curObj, bin)
      }
      else if(sizes(curObj) <= maxWeight)
        curObj = nObj
      curObj += 1
    }
  }

  def updateMinCardMaxLoad(bin: Int): Unit = {
    var curBin = 0
    while (curBin != nBin) {
      availableInBin(curBin) = candidates(curBin).size - (c(curBin).min - packed(curBin).value)
      curBin += 1
    }
    var currentCard = packed(bin).value
    var currentLoad = packedWeight(bin).value

    val stopAtLoad = l(bin).min
    val stopAtCard = c(bin).max

    var minCard: Int = -1

    var curObj = 0
    var loadBeforeLastObj = currentLoad
    while(curObj < nObj && currentCard < stopAtCard) {
      if(minCard == -1 && currentLoad >= stopAtLoad) {
        minCard = currentCard
      }

      //If the object is available...
      if(availableForBin(curObj, bin, availableInBin)) {
        //Take it for us!
        currentCard += 1
        loadBeforeLastObj = currentLoad
        currentLoad += sizes(curObj)

        //Do not forget to update availableInBin
        updateAvailableForBin(curObj, bin, availableInBin)
      }
      curObj += 1
    }

    if(minCard == -1 && currentLoad >= stopAtLoad) {
      minCard = currentCard
    }

    if(minCard == -1)
      throw Inconsistency

    c(bin).updateMin(minCard)
    l(bin).updateMax(currentLoad)
    c(bin).updateMax(currentCard)

    //loadBeforeLastObj contains the load of the c(bin).max-1 highest weighted items possible to fit according to our
    //(optimistic) heuristic. Let's remove all the items that can not fit in our lower load bound
    if(l(bin).min >= loadBeforeLastObj) {
      val minWeight = l(bin).min-loadBeforeLastObj
      curObj = nObj-1
      while (curObj >= 0) {
        if(candidates(bin).hasValue(curObj) && sizes(curObj) < minWeight) {
          removeObjFromBin(curObj, bin)
        }
        else if(sizes(curObj) >= minWeight)
          curObj = -1
        curObj -= 1
      }
    }
  }
}
