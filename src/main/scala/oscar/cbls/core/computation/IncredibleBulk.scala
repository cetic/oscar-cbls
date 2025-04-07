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

import oscar.cbls.core.propagation.{PropagationElement, PropagationStructure}

import scala.collection.immutable.HashMap

/** Used to statically bulk-register a dependency to an Iterable of [[Variable]].
  */
object IncredibleBulk {

  // The map of recorded IncredibleBulk
  private var incredibleBulks: HashMap[Int, IncredibleBulk] = HashMap.empty

  /** Registers a new IncredibleBulk if it wasn't registered before.
    *
    * @note
    *   IncredibleBulk are distinguished only by their identifier. The identifier is the hashcode of
    *   the input iterable. The hashcode depends on the `n` items contained in the iterable. So, the
    *   bulk registering is in `O(n)`.
    *
    * @param bulkVariables
    *   The bulk registered variables
    * @param propagationStructure
    *   The propagation structure to which the element is attached
    * @return
    *   The IncredibleBulk listening to the variables.
    */
  def bulkRegistering(
    bulkVariables: Iterable[Variable],
    propagationStructure: PropagationStructure
  ): IncredibleBulk = {
    val hash: Int = bulkVariables.hashCode()
    if (!incredibleBulks.contains(hash)) {
      incredibleBulks =
        incredibleBulks + (hash -> new IncredibleBulk(bulkVariables, propagationStructure))
    }
    incredibleBulks(hash)
  }

}

/** Statically register an Iterable of Variable.
  *
  * This class is used when several [[Invariant]] must listen to several [[Variable]]. Instead of
  * having i*v connexion we use a IncredibleBulk. Only the IncredibleBulk will statically listen to
  * the Variables. Each Invariant, then, will listen to the IncredibleBulk.
  *
  * @param bulkVariables
  *   The iterable of Variable the IncredibleBulk will listen to.
  * @param propagationStructure
  *   The propagation structure to which the element is attached
  */
class IncredibleBulk(bulkVariables: Iterable[Variable], propagationStructure: PropagationStructure)
    extends PropagationElement(propagationStructure) {

  // Register (once) each variable to build the static graph.
  bulkVariables.foreach(v => registerStaticallyListenedElement(v))

  /** Allows to check and debug propagation elements.
    *
    * This method can be called after the propagation according to the debug level of the
    * propagation structure (see [[oscar.cbls.core.propagation.PropagationStructure]]). It can be
    * used to check if the invariant worked properly by, for example, recomputing the value from
    * scratch.
    */
  override def checkInternals(): Unit = {}
}
