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

package oscar.cbls.lib.invariant.logic

import oscar.cbls.core.computation.set.{SetNotificationTarget, SetVariable}
import oscar.cbls.core.computation.{IncredibleBulk, Invariant, Store}


/** [[Invariant]] such that output(i) = {j | i in input(j)}.
 *
 * @param model
 *   The [[oscar.cbls.core.propagation.PropagationStructure]] to which this invariant is linked.
 * @param input
 *   An [[Array]] of [[SetVariable]]
 * @param output
 *   An [[Array]] of [[SetVariable]] such that  output(i) = {j | i in input(j)}.
 * @param bulkIdentifier
 *   A [[IncredibleBulk]] is used when several [[Invariant]] listen to vars. Warning:
 *   [[IncredibleBulk]] are distinguished only by their identifier. Be sure to use the same one if
 *   you're referencing the same variables.
 * @param name
 *   The name (optional) of your Invariant
 */
class DenseRef(
  model: Store,
  input: Array[SetVariable],
  output: Array[SetVariable],
  bulkIdentifier: Option[String] = None,
  name: Option[String] = None
) extends Invariant(model, name)
    with SetNotificationTarget {

  override def notifySetChanges(
    setVariable: SetVariable,
    index: Int,
    addedElems: Iterable[Int],
    removedElems: Iterable[Int],
    oldValue: Set[Int],
    newValue: Set[Int]
  ): Unit = ???

  override def checkInternals(): Unit = ???
}
