/* ******************************************************************************
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

package oscar.cbls.algo.conflict

import scala.annotation.tailrec

/** A class that proposes several conflict search algorithms. Given a set of items I, and a
  * condition over subsets of I that is monotonic (forall X, Y subset of I, conflict(X) =>
  * conflict(X U Y) a minimal conflict C is a subset of I such that conflict(C) and if any part of C
  * is removed, C is not in conflict anymore.
  * @author
  *   renaud.delandtsheer@cetic.be
  */
object ConflictSearch {

  /** Computes a minimal conflict over a list of things. Implements the famous quickXplain algorithm
    * in a generic way [Ulrich Junker and F Valbonne, QuickXPlain: Conflict Detection for Arbitrary
    * Constraint Propagation Algorithms, 2001] and proposes a faster implementation in case that
    * additional operations can be performed on the state
    *
    * If no conflict is detected, the algorithms throws an exception
    *
    * @param init
    *   the initial S, typically empty
    * @param toInject
    *   a list of C among which the minimal conflict must be searched
    * @param inject
    *   the procedure to inject a C into the S, going towards conflict
    * @param isConflict
    *   the procedure to check whether or not the S is in conflict
    * @return
    *   a minimal subset of toInject such that, when injected into init, they cause a conflict
    */
  def quickXPlain[S, C](
    init: S,
    toInject: List[C],
    inject: (S, C) => S,
    isConflict: S => Boolean
  ): List[C] = {
    search[S, C](init, toInject, inject, isConflict)
  }

  /** Implements the QuickXPlain Algorithm. The algorithm is described in Figure 3 of the Paper. The
    * lines that are refered two are the line of the algorithm discribes in the Figure 3 of the
    * paper.
    *
    * @param init
    *   the initial constraint system (C in the parameters of the algorithm)
    * @param items
    *   the list of constraints to add to the system (U in the parameters of the algorithm)
    * @param inject
    *   the function that inject one constraint in the constraint system (a bit like \Pi in the
    *   algorithm)
    * @param isConflict
    *   a function that checks if the constraint system is consistent
    * @return
    *   the minimal list of constraints that need to be added to get an inconsistent system
    */
  private def search[S, C](
    init: S,
    items: List[C],
    inject: (S, C) => S,
    isConflict: S => Boolean
  ): List[C] = {

    if (isConflict(init)) return Nil                      // Line 1
    if (items.isEmpty) throw new Exception("no conflict") // Line 2

    // Implements lines 5 to 9
    @tailrec
    def getFstConflictingItemAndId(
      constraintSet: S,
      itemList: List[C],
      currentId: Int = 0
    ): (C, Int) = {
      itemList match {
        case Nil => throw new Exception("no conflict")
        case h :: t =>
          val nextConstraintSet = inject(constraintSet, h)
          if (isConflict(nextConstraintSet))
            (h, currentId)
          else
            getFstConflictingItemAndId(nextConstraintSet, t, currentId + 1)
      }
    }

    // Calls the function above
    val (conflictingItem, id) = getFstConflictingItemAndId(init, items)

    var x = List(conflictingItem) // Line 10

    // implements line 11 to 13
    @tailrec
    def splitItemList(
      id: Int,
      itemList: List[C] = items,
      currentId: Int = 0,
      u1: List[C] = Nil,
      u2: List[C] = Nil
    ): (List[C], List[C]) = {
      if (currentId == id) {
        (u1, u2)
      } else {
        if (currentId <= id / 2)
          splitItemList(id, itemList.tail, currentId + 1, itemList.head :: u1, u2)
        else
          splitItemList(id, itemList.tail, currentId + 1, u1, itemList.head :: u2)
      }
    }
    val (u1, u2) = splitItemList(id) // Calls the function above

    if (u2.nonEmpty) { // Line 14 to 18
      val ciAndConflictItem = u1.foldLeft(inject(init, conflictingItem))(inject)
      val delta2            = search(ciAndConflictItem, u2, inject, isConflict)
      x = x ::: delta2
    }
    if (u1.nonEmpty) { // Line 19 to 23
      val initAndX = x.foldLeft(init)(inject)
      val delta1   = search(initAndX, u1, inject, isConflict)
      x = x ::: delta1
    }
    x // Line 24
  }

  /** Computes a minimal conflict over a list of things. This version is faster : O(n) because it
    * benefits from a remove operation
    *
    * If no conflict is detected, the algorithms throws an exception
    *
    * @param init
    *   the initial S, typically empty
    * @param toInject
    *   a list of C among which the minimal conflict must be searched
    * @param inject
    *   the procedure to inject a C into the S, going towards conflict
    * @param remove
    *   the procedure to remove a C from the S, possibly removing the conflict
    * @param isConflict
    *   the procedure to check whether or not the S is in conflict
    * @return
    *   a minimal subset of toInject such that, when injected into init, they cause a conflict
    */
  def xPlainWithRemove[S, C](
    init: S,
    toInject: List[C],
    inject: (S, C) => S,
    remove: (S, C) => S,
    isConflict: S => Boolean
  ): List[C] = {
    if (isConflict(init)) return List.empty

    var accumulatorList: List[C] = List.empty
    var accumulatorState         = init
    var remaining                = toInject
    while (!isConflict(accumulatorState)) {
      if (remaining.isEmpty) throw new Exception("No conflict")
      val item = remaining.head
      accumulatorState = inject(accumulatorState, item)
      remaining = remaining.tail
      accumulatorList = item :: accumulatorList
    }

    var toreturn: List[C] = List.empty

    for (item <- accumulatorList) {
      val testState = remove(accumulatorState, item)
      if (!isConflict(testState)) {
        toreturn = item :: toreturn
      } else {
        accumulatorState = testState
      }
    }
    toreturn
  }
}
