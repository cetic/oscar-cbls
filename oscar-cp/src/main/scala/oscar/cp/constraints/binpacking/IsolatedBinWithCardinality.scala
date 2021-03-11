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
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.{CPStore, Constraint}

class IsolatedBinWithCardinality(tmpX: Array[CPIntVar], tmpSizes: Array[Int], val l: Array[CPIntVar], val c: Array[CPIntVar]) extends Constraint(tmpX(0).store) {
  var sizeOrder: Array[Int] = tmpSizes.indices.sortBy(i => -tmpSizes(i)).toArray
  val x: Array[CPIntVar] = sizeOrder.map(tmpX(_))
  val sizes: Array[Int] = sizeOrder.map(tmpSizes(_))

  val nObj = x.length
  val nBin = l.length

  priorityL2 = CPStore.MaxPriorityL2 - 4
  idempotent = false

  override def associatedVars(): Iterable[CPVar] = x ++ l ++ c

  override def setup(ps: CPPropagStrength): Unit = {
    //Let's call them after the calls to the other constraints
    x.foreach(_.callValRemoveWhenValueIsRemoved(this))
    x.foreach(_.callValBindWhenBind(this))

    l.zipWithIndex.foreach({ case (vari, bin) => vari.filterWhenBoundsChange(false, CPStore.MaxPriorityL2 - 3) {
      propagateForBin(bin)
      false
    }
    })

    c.zipWithIndex.foreach({ case (vari, bin) => vari.filterWhenBoundsChange(false, CPStore.MaxPriorityL2 - 3) {
      propagateForBin(bin)
      false
    }
    })

    //Propagate for everyone once
    (0 until nBin).foreach(propagateForBin)
  }

  override def valRemove(x: CPIntVar, value: Int): Unit = {
    propagateForBin(value)
  }

  override def valBind(x: CPIntVar): Unit = {
    propagateForBin(x.min)
  }

  val membersX = Array.ofDim[CPIntVar](x.length)
  val membersSize = Array.ofDim[Int](x.length)
  var membersLength = 0

  def propagateForBin(bin: Int): Unit = {
    membersLength = 0
    var curPos = 0
    while(curPos != x.length) {
      if(!x(curPos).isBound && x(curPos).hasValue(bin)) {
        membersX(membersLength) = x(curPos)
        membersSize(membersLength) = sizes(curPos)
        membersLength += 1
      }
      curPos += 1
    }

    if (membersLength == 0)
      return

    var boundWeight = 0
    var boundCard = 0
    curPos = 0
    while(curPos != nObj) {
      if(x(curPos).isBoundTo(bin)) {
        boundWeight += sizes(curPos)
        boundCard += 1
      }
      curPos += 1
    }

    var minWeight = l(bin).min - boundWeight
    var maxWeight = l(bin).max - boundWeight
    var minCard = c(bin).min - boundCard
    var maxCard = c(bin).max - boundCard

    if (membersLength < minCard)
      throw Inconsistency

    // BEGIN ADD PROPAGATE
    // Verify min/max card
    if (0 > minCard) {
      minCard = 0
      c(bin).updateMin(boundCard)
    }
    if (membersLength < maxCard) {
      maxCard = membersLength
      c(bin).updateMax(maxCard + boundCard)
    }
    if (minCard > maxCard)
      throw Inconsistency
    if (maxCard == 0) {
      curPos = 0
      while (curPos != membersLength) {
        membersX(curPos).removeValue(bin)
        curPos += 1
      }
      l(bin).assign(boundWeight)
      return
    }
    // END ADD PROPAGATE

    // What is the minimum weight that we can generate with minCard(bin)-1 objects?
    var minWeightM1 = 0
    var i = 0
    while (i < minCard - 1) {
      minWeightM1 += membersSize(membersLength - 1 - i)
      i += 1
    }

    // What is the maximum weight that we can generate with maxCard(bin)-1 objects?
    var maxWeightM1 = 0
    i = 0
    while (i < maxCard - 1) {
      maxWeightM1 += membersSize(i)
      i += 1
    }

    // Verify min/max weight

    // Min weight cannot be less than the minCard items that have the lesser weights
    if (minCard != 0) {
      val minWBound = minWeightM1 + membersSize(membersLength - 1 - (minCard - 1))
      if (minWeight < minWBound) {
        minWeight = minWBound
        l(bin).updateMin(minWeight + boundWeight)
      }
    }

    // Same for the max weight, with maxCard and higher weights :-)
    val maxWBound = maxWeightM1 + membersSize(maxCard - 1)
    if (maxWeight > maxWBound) {
      maxWeight = maxWBound
      l(bin).updateMax(maxWeight + boundWeight)
    }


    curPos = 0
    while (curPos != membersLength) {
      // It is impossible to take an object if its weight + the weight of the (minCard-1) objects that
      // have the lesser weights is above the maxWeight
      if(membersSize(curPos) + minWeightM1 > maxWeight)
        membersX(curPos).removeValue(bin)

      // It is impossible to take an object if its weight + the weight of the (maxCard-1) objects that
      // have the higher weights is below minWeight
      if(membersSize(curPos) + maxWeightM1 < minWeight)
        membersX(curPos).removeValue(bin)
      curPos += 1
    }
  }
}
