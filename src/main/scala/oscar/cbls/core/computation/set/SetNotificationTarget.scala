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

package oscar.cbls.core.computation.set

trait SetNotificationTarget {

  /** Notifies the listening [[oscar.cbls.core.propagation.PropagationElement]] that the listened
    * [[SetVariable]] has changed. This method has to be implemented by the listening element, and
    * will be called by the listened variable.
    *
    * This method uses parameters providing incremental information (`addedElems` and
    * `removedElems`), as well as information on the entire value (`oldValue` and `newValue`), since
    * different propagation elements may make use of either or both kinds.
    *
    * The variable invoking this method makes sure that the set of added values and removed values
    * are mutually exclusive, and that `newValue = oldValue + addedElems - removedElems`
    *
    * @param setVariable
    *   The listened SetVariable
    * @param index
    *   The optional index of the SetVariable in the context of the listening Invariant. Default
    *   index is -1
    * @param addedElems
    *   The elements added to the SetVariable
    * @param removedElems
    *   The elements removed from the SetVariable
    * @param oldValue
    *   The previous value of the SetVariable
    * @param newValue
    *   The new value of the SetVariable
    */
  def notifySetChanges(
    setVariable: SetVariable,
    index: Int,
    addedElems: Iterable[Int],
    removedElems: Iterable[Int],
    oldValue: Set[Int],
    newValue: Set[Int]
  ): Unit
}
