/*
 * *****************************************************************************
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
 * ****************************************************************************
 */

package oscar.flatzinc.cbls.support

import oscar.cbls.core.computation.{CBLSIntVar, IntValue}

/**
  * @author Gustav Bjordal
  */
abstract class NeighbourhoodAction {
  def computeAssignment(): Unit

  // Should be called after computeAssignment
  def isValid(): Boolean = true

  def isValid(acceptVar:CBLSIntVar=>Boolean): Boolean // = isValid()

  // Should be called after computeAssignment
  def modifies(): Boolean

  def performAssignment(): Unit

  def undo(): Unit

  def saveBest(): Unit

  def getCurrentMove(obj: Int): Move

  def getSavedMove(obj: Int): Move

  def onlyModifies(allowedVars: CBLSIntVar => Boolean):Boolean
}

case class AssignAction(target: CBLSIntVar, value: IntValue) extends NeighbourhoodAction {
  var computedValue: Int = 0
  var oldValue: Int = 0
  var savedValue: Int = 0

  override def computeAssignment(): Unit = {
    computedValue = value.value
    oldValue = target.value
  }

  override def modifies(): Boolean = {
    computedValue != oldValue
  }

  override def isValid(): Boolean = {
    target.domain.contains(computedValue)
  }

  override def isValid(acceptVar: (CBLSIntVar) => Boolean): Boolean =  {
    target.domain.contains(computedValue) &&
    acceptVar(target)
  }

  override def saveBest(): Unit = {
    savedValue = computedValue
  }

  override def performAssignment(): Unit = {
    target := computedValue
  }

  override def undo(): Unit = {
    target := oldValue
  }

  override def getCurrentMove(obj: Int): Move = {
    AssignMove(target, computedValue, obj)
  }

  override def getSavedMove(obj: Int): Move = {
    AssignMove(target, savedValue, obj)
  }

  override def onlyModifies(allowedVars: (CBLSIntVar) => Boolean): Boolean = {
    allowedVars(target)
  }
}

case class AssignArrayAction(targets: Array[CBLSIntVar], index: IntValue,
                             value: IntValue) extends NeighbourhoodAction {
  var computedValue: Int = 0
  var computedIndex: Int = 0
  var oldValue: Int = 0

  var savedValue: Int = 0
  var savedIndex: Int = 0

  override def computeAssignment(): Unit = {
    computedValue = value.value
    computedIndex = index.value-1
    oldValue = targets(computedIndex).value
  }

  override def modifies(): Boolean = {
    oldValue != computedValue
  }

  override def isValid(): Boolean = {
    computedIndex >= 0 &&
      computedIndex < targets.length &&
      targets(computedIndex).domain.contains(computedValue)
  }

  override def isValid(acceptVar: (CBLSIntVar) => Boolean): Boolean = {
      computedIndex >= 0 &&
      computedIndex < targets.length &&
      targets(computedIndex).domain.contains(computedValue) &&
      acceptVar(targets(computedIndex))
  }

  override def saveBest(): Unit = {
    savedValue = computedValue
    savedIndex = computedIndex
  }

  override def performAssignment(): Unit = {
    targets(computedIndex) := computedValue
  }

  override def undo(): Unit = {
    targets(computedIndex) := oldValue
  }

  override def getCurrentMove(obj: Int): Move = {
    AssignMove(targets(computedIndex), computedValue, obj)
  }

  override def getSavedMove(obj: Int): Move = {
    AssignMove(targets(savedIndex), savedValue, obj)
  }

  override def onlyModifies(allowedVars: (CBLSIntVar) => Boolean): Boolean = {
    allowedVars(targets(computedIndex))
  }
}

case class SwapAction(target1: CBLSIntVar, target2: CBLSIntVar) extends NeighbourhoodAction {

  var computedValue1: Int = 0
  var oldValue1: Int = 0

  var computedValue2: Int = 0
  var oldValue2: Int = 0

  var savedValue1: Int = 0
  var savedValue2: Int = 0

  override def computeAssignment(): Unit = {
    oldValue1 = target1.value
    oldValue2 = target2.value
    computedValue1 = oldValue2
    computedValue2 = oldValue1
  }


  override def saveBest(): Unit = {
    savedValue1 = computedValue1
    savedValue2 = computedValue2
  }

  override def performAssignment(): Unit = {
    target1 := computedValue1
    target2 := computedValue2
  }

  override def modifies(): Boolean = {
    computedValue1 != computedValue2
  }

  override def isValid(): Boolean = {
    target1.domain.contains(computedValue1) &&
    target2.domain.contains(computedValue2)
  }

  override def isValid(acceptVar: (CBLSIntVar) => Boolean): Boolean = {
    target1.domain.contains(computedValue1) &&
      target2.domain.contains(computedValue2) &&
      (acceptVar(target1) || acceptVar(target2))
  }

  override def undo(): Unit = {
    target1 := oldValue1
    target2 := oldValue2
  }

  override def getCurrentMove(obj: Int): Move = {
    SwapMove(target1, target2, obj)
  }

  override def getSavedMove(obj: Int): Move = {
    SwapMove(target1, target2, obj)
  }

  override def onlyModifies(allowedVars: (CBLSIntVar) => Boolean): Boolean = {
    allowedVars(target1) && allowedVars(target2)
  }
}

case class SwapArrayAction(targets1: Array[CBLSIntVar],
                           index1: IntValue,
                           targets2: Array[CBLSIntVar],
                           index2: IntValue) extends NeighbourhoodAction {
  var oldValue1: Int = 0
  var oldValue2: Int = 0
  var computedValue1: Int = 0
  var computedValue2: Int = 0
  var computedIndex1: Int = 0
  var computedIndex2: Int = 0
  var savedIndex1: Int = 0
  var savedIndex2: Int = 0
  var savedValue1: Int = 0
  var savedValue2: Int = 0

  override def computeAssignment(): Unit = {
    computedIndex1 = index1.value-1
    computedIndex2 = index2.value-1

    oldValue1 = targets1(computedIndex1).value
    oldValue2 = targets2(computedIndex2).value

    //Swapped index and targets
    computedValue1 = targets2(computedIndex2).value
    computedValue2 = targets1(computedIndex1).value
  }


  override def modifies(): Boolean = {
    computedValue1 != computedValue2
  }

  override def isValid(): Boolean = {
    computedIndex1 >= 0 &&
    computedIndex1 < targets1.length &&
    computedIndex2 >= 0 &&
    computedIndex2 < targets2.length &&
    targets1(computedIndex1).domain.contains(computedValue1) &&
    targets2(computedIndex2).domain.contains(computedValue2)
  }

  override def isValid(acceptVar: (CBLSIntVar) => Boolean): Boolean = {
    computedIndex1 >= 0 &&
      computedIndex1 < targets1.length &&
      computedIndex2 >= 0 &&
      computedIndex2 < targets2.length &&
      targets1(computedIndex1).domain.contains(computedValue1) &&
      targets2(computedIndex2).domain.contains(computedValue2) &&
      (acceptVar(targets1(computedIndex1)) || acceptVar(targets2(computedIndex2)) )
  }

  override def saveBest(): Unit = {
    savedIndex1 = computedIndex1
    savedIndex2 = computedIndex2
    savedValue1 = computedValue1
    savedValue2 = computedValue2
  }

  override def performAssignment(): Unit = {
    // targets1(computedIndex1) :=: targets2(computedIndex2)
    targets1(computedIndex1) := computedValue1
    targets2(computedIndex2) := computedValue2
  }

  override def undo(): Unit = {
    targets1(computedIndex1) := oldValue1
    targets2(computedIndex2) := oldValue2
    //targets1(computedIndex1) :=: targets2(computedIndex2)
  }

  override def getCurrentMove(obj: Int): Move = {
    SwapMove(targets1(computedIndex1), targets2(computedIndex2), obj)
  }

  override def getSavedMove(obj: Int): Move = {
    SwapMove(targets1(savedIndex1), targets2(savedIndex2), obj)
  }

  override def onlyModifies(allowedVars: (CBLSIntVar) => Boolean): Boolean = {
    allowedVars(targets1(computedIndex1)) && allowedVars(targets2(computedIndex2))
  }
}
