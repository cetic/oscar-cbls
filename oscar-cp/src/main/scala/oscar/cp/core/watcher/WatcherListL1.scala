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


package oscar.cp.core.watcher

import oscar.algo.reversible.TrailEntry
import oscar.cp.core.CPStore
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar

class WatcherListL1(store: CPStore) {

  private[this] val RESERVED_INDEX = Int.MinValue
  
  private[this] var lastMagic = -1L
  private[this] var constraintStack: Array[Constraint] = new Array[Constraint](4)
  private[this] var variablesStack: Array[CPIntVar] = new Array[CPIntVar](4)
  private[this] var indexStack: Array[Int] = new Array[Int](4)
  private[this] var index: Int = 0

  @inline final def length: Int = index

  @inline final def isEmpty = index == 0

  @inline final def register(constraint: Constraint, variable: CPIntVar, id: Int): Unit = {
    assert(id != RESERVED_INDEX, s"$RESERVED_INDEX is a reserved value that cannot be used as an id.")
    if (index == constraintStack.length) growStacks()
    constraintStack(index) = constraint
    variablesStack(index) = variable
    indexStack(index) = id
    trail()
    index += 1
  }

  @inline final def register(constraint: Constraint, variable: CPIntVar): Unit = {
    if (index == constraintStack.length) growStacks()
    constraintStack(index) = constraint
    variablesStack(index) = variable
    indexStack(index) = RESERVED_INDEX
    trail()
    index += 1
  }

  @inline final def clear(): Unit = {
    trail()
    index = 0
  }

  @inline final def enqueueBind(): Unit = {
    var i = index
    while (i > 0) {
      i -= 1
      val constraint = constraintStack(i)
      if (constraint.isActive) {
        val variable = variablesStack(i)
        val id = indexStack(i)
        if (id == RESERVED_INDEX) store.enqueueL1(constraint, constraint.priorityBindL1, constraint.valBind(variable))
        else store.enqueueL1(constraint, constraint.priorityBindL1, constraint.valBindIdx(variable, id))
      }
    }
  }
  
  @inline final def enqueueBounds(): Unit = {
    var i = index
    while (i > 0) {
      i -= 1
      val constraint = constraintStack(i)
      if (constraint.isActive) {
        val variable = variablesStack(i)
        val id = indexStack(i)
        if (id == RESERVED_INDEX) store.enqueueL1(constraint, constraint.priorityBoundsL1, constraint.updateBounds(variable))
        else store.enqueueL1(constraint, constraint.priorityBoundsL1, constraint.updateBoundsIdx(variable, id))
      }
    }
  }
    
  @inline final def enqueueRemove(value: Int): Unit = {
    var i = index
    while (i > 0) {
      i -= 1
      val constraint = constraintStack(i)
      if (constraint.isActive) {
        val variable = variablesStack(i)
        val id = indexStack(i)
        val transformed = variable.transform(value)
        if (id == RESERVED_INDEX) store.enqueueL1(constraint, constraint.priorityRemoveL1, constraint.valRemove(variable, transformed))
        else store.enqueueL1(constraint, constraint.priorityRemoveL1, constraint.valRemoveIdx(variable, id, transformed))
      }
    }
  }

  @inline private def trail(): Unit = {
    val contextMagic = store.magic
    if (lastMagic != contextMagic) {
      lastMagic = contextMagic
      val newIndex = index
      store.trail(new TrailEntry { final override def restore(): Unit = index = newIndex })
    }
  }

  // Double the size of the stack
  @inline private def growStacks(): Unit = {
    val newSize = index * 2
    val newConstraintStack = new Array[Constraint](newSize)
    val newVariablesStack = new Array[CPIntVar](newSize)
    val newIndexStack = new Array[Int](newSize)
    System.arraycopy(constraintStack, 0, newConstraintStack, 0, index)
    System.arraycopy(variablesStack, 0, newVariablesStack, 0, index)
    System.arraycopy(indexStack, 0, newIndexStack, 0, index)
    constraintStack = newConstraintStack
    variablesStack = newVariablesStack
    indexStack = newIndexStack
  }
}