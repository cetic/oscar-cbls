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

import oscar.cp.constraints.GCCVarAC
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.CPIntVar

/**
  * @param items items(j) is the bin assigned to item j
  * @param sizes sizes(j) is the weight of item j. Must be ordered in descending order
  * @param l l(i) is the load of bin i
  * @param c c(i) is the cardinality of bin i
  */
class BPCFlow(items: Array[CPIntVar], sizes: Array[Int], val l: Array[CPIntVar], val c: Array[CPIntVar]) extends GCCVarAC(items, 0, c) {
  if(!sizes.sliding(2).forall(a => a(0) >= a(1)))
    throw new Exception("x/sizes must be ordered in decreasing order of size")

  idempotent = false

  private[this] val objDomain = Array.ofDim[Int](items.length, l.length)

  override def setup(st: CPPropagStrength): Unit = {
    l.foreach(_.callPropagateWhenBoundsChange(this))

    //deltaItems = items.map(_.delta(this))

    //Some safety checks before giving x to GCCVarAC
    items.foreach(_.updateMin(0))
    items.foreach(_.updateMax(l.length-1))

    super.setup(st)
  }

  override def propagate(): Unit = {
    // call GCCVarAc propagate first. It will take care of updating everything
    super.propagate()

    // Update the bounds low/up on the cardinalities to reflect the real ones.
    // They may have been changed by the function pruneBounds in GCCVarAC
    super.updateBounds()

    // Some safety checks...
    assert(minVal == 0) //minVal should never move here, as all values are constrained and the first bin is 0
    assert(maxVal == l.length-1) //same

    // Now we can work safely
    var bin = 0
    while(bin != l.length) {
      checkBinLoad(bin)
      bin += 1
    }
  }

  def findCycleForBin(bin: Int, finalObj: Int, minSize: Int, maxSize: Int): Boolean = {
    magic += 1
    valSeen(bin) = magic

    //Try first to increase the flow
    while(flow(bin) < up(bin)) {
      var nextBin = 0
      while(nextBin != l.length) {
        if(flow(nextBin) > low(nextBin) && nextBin != bin && findPathFromBinToBin(nextBin, varMatch(finalObj))) {
          assign(finalObj, bin)
          return true
        }
        nextBin += 1
      }
    }

    var obj = valMatch(bin)
    while(obj != NONE) {
      if(varSeen(obj) != magic && sizes(obj) >= minSize && sizes(obj) <= maxSize) {

        val length = items(obj).fillArray(objDomain(obj))
        var i = 0
        while(i != length) {
          val nextBin = objDomain(obj)(i)
          if(nextBin != bin && findPathFromBinToBin(nextBin, varMatch(finalObj))) {
            //assign the first object...
            assign(obj, nextBin)
            //and the last one.
            assign(finalObj, bin)
            return true
          }
          i += 1
        }
      }
      obj = next(obj)
    }

    false
  }

  var debug = false
  override def assign(k: Int, v: Int): Unit = {
    if(debug) {
      println("Assign "+k+" to "+v)
    }
    super.assign(k, v)
  }

  def findPathFromBinToBin(curBin: Int, toBin: Int): Boolean = {
    if(valSeen(curBin) == magic)
      return false
    valSeen(curBin) = magic

    if(curBin == toBin)
      return true

    // If we are here, we (virtually) have been assigned a new item
    // Let's try first to find an item that we can give to someone else
    var obj = valMatch(curBin)
    while(obj != NONE) {
      if(varSeen(obj) != magic) { //if item yet unvisited
        varSeen(obj) = magic

        val length = items(obj).fillArray(objDomain(obj))
        var i = 0
        while(i != length) {
          val nextBin = objDomain(obj)(i)
          if(nextBin != curBin && findPathFromBinToBin(nextBin, toBin)) { //if we can find a path...
            assign(obj, nextBin) //we win!
            return true
          }
          i += 1
        }

      }
      obj = next(obj)
    }

    //If we can accommodate an additional item, then we can simply continue to search in a bin that can lose an item
    if(flow(curBin) < up(curBin)) {
      var nextBin = 0
      while(nextBin != l.length) {
        if(flow(nextBin) > low(nextBin) && nextBin != curBin && findPathFromBinToBin(nextBin, toBin))
          return true
        nextBin += 1
      }
    }

    false
  }


  def getMinLoadForBin(bin: Int): Int = {
    var costCurrent = 0
    var costNAssign = 0
    var costNextObj = items.length-1

    while(costNAssign != low(bin)) {
      // If the next item is already assigned, simply update the info about the problem
      // Else, if this item can be assigned to bin, also update the infos
      if(varMatch(costNextObj) == bin ||
        (items(costNextObj).hasValue(bin) && findCycleForBin(bin, costNextObj, sizes(costNextObj)+1, Int.MaxValue))) {
        costNAssign += 1
        costCurrent += sizes(costNextObj)
      }
      costNextObj -= 1
    }
    costCurrent
  }

  def getMaxLoadForBin(bin: Int): Int = {
    var costCurrent = 0
    var costNAssign = 0
    var costNextObj = 0

    while(costNAssign != low(bin)) {
      // If the next item is already assigned, simply update the info about the problem
      // Else, if this item can be assigned to bin, also update the infos
      if(varMatch(costNextObj) == bin ||
        (items(costNextObj).hasValue(bin) && findCycleForBin(bin, costNextObj, 0, sizes(costNextObj)-1))) {
        costNAssign += 1
        costCurrent += sizes(costNextObj)
      }
      costNextObj += 1
    }
    costCurrent
  }

  private val reachableFrom = items.map(_ => NONE)
  private final val itemsDecrSize = items.indices
  private final val itemsIncrSize = items.indices.reverse
  def fillReachableFrom(bin: Int, shouldMin: Boolean): Unit = {
    var i = 0
    while(i != reachableFrom.length) {
      reachableFrom(i) = NONE
      i += 1
    }

    magic += 1
    valSeen(bin) = magic

    if(shouldMin) {
      var obj = items.length - 1
      while(obj >= 0) {
        if(varMatch(obj) == bin) {
          reachableFrom(obj) = sizes(obj) //force it as we cannot change it
          if(varSeen(obj) != magic)
            fillReachableFromRecurObj(obj, sizes(obj))
        }
        obj -= 1
      }
    }
    else {
      var obj = 0
      while(obj != items.length) {
        if(varMatch(obj) == bin) {
          reachableFrom(obj) = sizes(obj) //force it as we cannot change it
          if(varSeen(obj) != magic)
            fillReachableFromRecurObj(obj, sizes(obj))
        }
        obj += 1
      }
    }

    if(up(bin) > flow(bin)) {
      var otherBin = 0
      while(otherBin != l.length) {
        if(flow(otherBin) > low(otherBin) && valSeen(otherBin) != magic)
          fillReachableFromRecurBin(otherBin, 0)
        otherBin += 1
      }
    }
  }

  @inline def fillReachableFromRecurObj(obj: Int, toSet: Int): Unit = {
    if(varSeen(obj) == magic)
      return
    varSeen(obj) = magic
    reachableFrom(obj) = toSet

    val length = items(obj).fillArray(objDomain(obj))
    var i = 0
    while(i != length) {
      val bin = objDomain(obj)(i)
      if(varMatch(obj) != bin && valSeen(bin) != magic) {
        fillReachableFromRecurBin(bin, toSet)
      }
      i += 1
    }
  }

  def fillReachableFromRecurBin(bin: Int, toSet: Int): Unit = {
    if(valSeen(bin) == magic)
      return
    valSeen(bin) = magic

    // If we can add an item
    if(flow(bin) < up(bin)) {
      var otherBin = 0
      while(otherBin != l.length) {
        if(flow(otherBin) > low(otherBin) && valSeen(otherBin) != magic)
          fillReachableFromRecurBin(otherBin, toSet)
        otherBin += 1
      }
    }

    var curItem = valMatch(bin)
    while(curItem != NONE) {
      if(varSeen(curItem) != magic)
        fillReachableFromRecurObj(curItem, toSet)
      curItem = next(curItem)
    }
  }

  def correctFlow(bin: Int): Boolean = decreaseMax(bin) && increaseMin(bin)

  def checkBinLoad(bin: Int): Unit = {
    //First, let us generate a valid flow with min cardinality for this bin
    up(bin) = c(bin).min
    low(bin) = c(bin).min
    while(!correctFlow(bin)) {
      //even if we called GCCVarAC before, other checkBinLoad calls may have made this infeasible
      up(bin) += 1
      low(bin) += 1
      c(bin).updateMin(up(bin))
    }

    //Now, compute the minimum possible weight for this cardinality
    //l.updateMin(min weight for min card)
    var load = getMinLoadForBin(bin)
    l(bin).updateMin(load)

    //Increase nb of items for bin until we reach max load
    //c.updateMax(last card  that has min load < l.max)
    var dicoMin = up(bin)
    var dicoMinLoad = load
    var dicoMax = c(bin).max+1
    while(dicoMin != dicoMax-1) {
      val oldB = up(bin)
      up(bin) = (dicoMin + dicoMax)/2
      low(bin) = (dicoMin + dicoMax)/2

      val flowOk = correctFlow(bin)

      val loadOk = if(flowOk) {
        load = getMinLoadForBin(bin)
        load <= l(bin).max
      }
      else
        false

      if(loadOk) {
        dicoMin = up(bin)
        dicoMinLoad = load
      }
      else {
        dicoMax = up(bin)
      }
    }
    c(bin).updateMax(dicoMin)

    //Let's compute a valid flow with max cardinality for this bin
    up(bin) = c(bin).max
    low(bin) = c(bin).max
    while(!correctFlow(bin)) {
      //even if we called GCCVarAC before, other checkBinLoad calls may have made this infeasible
      up(bin) -= 1
      low(bin) -= 1
      c(bin).updateMax(low(bin))
    }

    //Compute maximum load with the maximum number of items
    //l.updateMax(max weight for max card)
    load = getMaxLoadForBin(bin)
    l(bin).updateMax(load)

    //Decrease max nb of items for bin until we reach min load
    //c.updateMin(first card that has max load > l.min)
    dicoMin = c(bin).min-1
    dicoMax = up(bin)
    var dicoMaxLoad = load
    while(dicoMin+1 != dicoMax) {
      val oldB = up(bin)
      up(bin) = (dicoMin + dicoMax)/2
      low(bin) = (dicoMin + dicoMax)/2

      val flowOk = correctFlow(bin)

      val loadOk = if(flowOk) {
        load = getMaxLoadForBin(bin)
        load >= l(bin).min
      }
      else
        false

      if(loadOk) {
        dicoMax = up(bin)
        dicoMaxLoad = load
      }
      else {
        dicoMin = up(bin)
      }
    }
    c(bin).updateMin(dicoMax)

    //Restore flow
    up(bin) = c(bin).max
    low(bin) = c(bin).min
    assert(correctFlow(bin))
  }
}
