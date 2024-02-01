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

package oscar.cbls.core.propagation

/** Manages propagation among propagation elements.
  *
  * A propagation structure handles the propagation among a set of propagation elements. The main
  * goal of the propagation is to ensure that when a value changes in the propagation graph, the
  * propagation elements are updated through a unique wave that reaches all the variables at most
  * once.
  *
  * To achive this goal, the propagation works as follows:
  *
  *   - When all the elements are registered, they are ordered according to the distance from the
  *     input (the propagation elements that depend on nothing). This ordering is achieve in the
  *     [[setupPropagationStructure]] method
  *   - When the propagation is triggered, the elements are updated following the order computed
  *     previously
  *
  * OscaR.cbls supports partial propagation. When a propagation element is registered for partial
  * propagation, the propagation structure will compute the elements on which this elements depends.
  * When a propagation is triggered, only this elements will be updated.
  *
  * @param debugLevel
  *   the level of debug
  */
class PropagationStructure(debugLevel: Int) {

  /** Prepares the propagation structure for the use of propagation.
    *
    *   - Compute the layer of each propagation element to compute the order of element update
    *   - Compute the tracks for the partial propagation
    */
  protected def setupPropagationStructure(): Unit = ???

  /** Register a propagation element for partial propagation
    *
    * When an element is scheduled for partial propagation, the propagation structure computes the
    * other elements on which this elements depends. Later, when a propapation up to this element is
    * required, only the elements on which this element depends will be propagated
    *
    * @param p
    *   The element that is registered for partial propagation
    */
  protected def registerForPartialPropagation(p: PropagationElement): Unit = ???

  /** Triggers the propagation in the graph.
    *
    * The propagation has a target and stops when the target of the propagation has bee reached
    *
    * @param upTo
    *   The target element of the propagation
    */
  protected final def propagate(upTo: PropagationElement): Unit = ???

  /** Schedules a propagation elements for the propagation
    *
    * @param p
    *   the element to schedule
    */
  private[propagation] def scheduleForPropagation(p: PropagationElement): Unit = ???

}
