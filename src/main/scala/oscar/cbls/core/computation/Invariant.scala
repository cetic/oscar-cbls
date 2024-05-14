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

package oscar.cbls.core.computation

import oscar.cbls.core.propagation._

/** Abstract structure for Invariant definition.
  *
  * An invariant is a propagation element that maintains the value of output variable given
  * modification of the input variable(s). For instance, a identity invariant that take a
  * IntVariable as input and output will ensure that the output variable is equal to the input
  * variable.
  *
  * The [[oscar.cbls.core.propagation.PropagationElement]] abstract class provides an "empty"
  * implementation of the method performPropagation. There is two ways of propagating invariant
  * result to its output variable. When you receive a notification of modification from the input
  * variable you ...
  *   - ... directly set the new value of the output variable. In that case you do not need to
  *     override performPropagation()
  *   - ... do some computation and SCHEDULE the invariant FOR PROPAGATION. In that case you need to
  *     override performPropagation()
  *
  * @param propagationStructure
  *   The propagation structure to which the element is attached
  * @param name
  *   The name (optional) of your Invariant
  */
abstract class Invariant(propagationStructure: PropagationStructure, name: Option[String] = None)
    extends PropagationElement(propagationStructure) {

  /** Registers an IncredibleBulk (a bulk/set of [[Variable]]) as listened by this Invariant
    *
    * @param incredibleBulk
    *   The bulk of [[Variable]]
    */
  def addIncredibleBulk(incredibleBulk: IncredibleBulk): Unit = {
    this.registerStaticallyListenedElement(incredibleBulk)
  }

  def name(): String = this.name.getOrElse(s"Invariant_$id")

  override def toString: String = this.name()
}
