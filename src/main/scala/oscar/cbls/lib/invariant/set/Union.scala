// OscaR is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 2.1 of the License, or
// (at your option) any later version.
//
// OscaR is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License  for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with OscaR.
// If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html

package oscar.cbls.lib.invariant.set

import oscar.cbls.core.computation.{Invariant, Store}
import oscar.cbls.core.computation.set.{SetNotificationTarget, SetVariable}

/** Companion object of the [[Union]] class. */
object Union {

  /** Creates an Union invariant, which maintains `A union B`.
    *
    * @param model
    *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
    * @param A
    *   The first set of the union.
    * @param B
    *   The second set of the union.
    * @param output
    *   The SetVariable evaluating to `A union B`.
    * @param name
    *   The (optional) name of the Invariant.
    */
  def apply(
    model: Store,
    A: SetVariable,
    B: SetVariable,
    output: SetVariable,
    name: Option[String] = None
  ): Union = new Union(model, A, B, output, name)

}

/** Invariant which maintains A union B. Update is in O(1).
  *
  * @param model
  *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
  * @param A
  *   The first set of the union.
  * @param B
  *   The second set of the union.
  * @param output
  *   The SetVariable evaluating to `A union B`.
  * @param name
  *   The (optional) name of the Invariant.
  */
class Union(model: Store, A: SetVariable, B: SetVariable, output: SetVariable, name: Option[String])
    extends Invariant(model, name)
    with SetNotificationTarget {

  require(A != B, "A and B cannot be the same instance for an union.")

  A.registerStaticallyAndDynamicallyListeningElement(this)
  B.registerStaticallyAndDynamicallyListeningElement(this)

  output.setDefiningInvariant(this)
  output := A.value().union(B.value())

  override def notifySetChanges(
    setVariable: SetVariable,
    index: Int,
    addedElems: Iterable[Int],
    removedElems: Iterable[Int],
    oldValue: Set[Int],
    newValue: Set[Int]
  ): Unit = {
    for (added   <- addedElems) notifyInsertOn(setVariable, added)
    for (removed <- removedElems) notifyDeleteOn(setVariable, removed)
  }

  override def checkInternals(): Unit = {
    val expectedSet: Set[Int] = A.value().union(B.value())

    require(
      output.pendingValue == expectedSet,
      s"checkInternal fails in invariant ${name()}. " +
        s"output != A union B. " +
        s"output: ${output.pendingValue} - expected set: $expectedSet " +
        s"- A: $A - B: $B"
    )
  }

  // Updates the output when a new value is inserted.
  private[this] def notifyInsertOn(set: SetVariable, value: Int): Unit = {
    assert(set == A || set == B, "The modified variable is neither A nor B")
    output :+= value
  }

  // Updates the output when a value must be removed.
  private[this] def notifyDeleteOn(set: SetVariable, value: Int): Unit = {
    assert(set == A || set == B, "The modified variable is neither A nor B")

    if (set == A) {
      if (!B.value().contains(value)) output :-= value
    } else if (set == B) {
      if (!A.value().contains(value)) output :-= value
    } else {
      require(
        requirement = false,
        "Tries to remove a value from a SetVariable which is neither A nor B."
      )
    }
  }
}
