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

/** This class manages propagation among propagation elements.
  *
  * This class is intended to be extended, and the overriding class must implement the method
  * getPropagationElements that returns the propagation elements to be considered Each propagation
  * element has a UniqueID. Those should be assigned continuously starting from 0.
  *
  * It is to be used as follows: once the set of propagation elements is stabilized, one must call
  * setupPropagationStructure, which will built the necessary internal data structure propagation
  * are triggered by calling the propagate method. additionally, before calling
  * setupPropagationStructure, the method registerForPartialPropagation can be called to specify
  * propagation elements that might require lazy propagation.
  *
  * Two debug mechanisms are provided: trace printing and debug mode.
  *
  * A trace printing is provided; the propagation structure prints a trace of what it is
  * propagating. This is activated by the Verbose parameter. All prints are preceded by
  * ''PropagationStruture'' This an be useful when checking the behavior of partial propagation.
  *
  * A self-check method is called by the propagation structure after propagation is performed. This
  * is activated by the Checker parameter. You should ensure that Asteroid is compiled with assert
  * activated if you are using the debug mode. It will considerably slow down Asteroid, as other
  * checks are implemented in the base modules.
  *
  * Also, although this propagation structure is intended to support acyclic graph for the static
  * dependency graph, you can deactivate the associated mechanism by setting the IsAcyclic graph to
  * true. If unsure, set to false (or do not set; it is false by default), the engine will discover
  * it by itself. See also method isAcyclic to query a propagation structure.
  *
  * @param verbose
  *   requires that the propagation structure prints a trace of what it is doing.
  * @param checker
  *   set a Some[Checker] top check all internal properties of invariants after propagation, set to
  *   None for regular execution
  * @param noCycle
  *   is to be set to true only if the static dependency graph is acyclic.
  * @param topologicalSort
  *   if true, use topological sort, false, use distance to input, and associated faster heap data
  *   structure
  * @param sortScc
  *   true if SCC should be sorted, false otherwise. Set to true, unless you know what your are
  *   doing. Setting to false might provide a speedup, but propagation will not be single pass on
  *   SCC anymore
  * @author
  *   renaud.delandtsheer@cetic.be
  */
class PropagationStructure {

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
    * @param upTo
    *   The target element of the propagation
    */
  protected final def propagate(upTo: PropagationElement): Unit = ???

  /** this method is used by propagationComponents to schedule themselves for propagation. */
  private[propagation] def scheduleForPropagation(p: PropagationElement): Unit = ???

}
