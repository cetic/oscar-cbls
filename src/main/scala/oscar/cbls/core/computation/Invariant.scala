package oscar.cbls.core.computation

import oscar.cbls.core.propagation._

class Invariant(uniqueId: Int, propagationStructure: PropagationStructure)
    extends PropagationElement(propagationStructure) {

  /**Registers the [[Variable]] in parameter as a dependency of this Invariant.
   * Static dependency is only used to define the propagation graph.
   *
   * @param variable The variable to add as dependence
   */
  def registerStaticDependency(variable:Variable): Unit ={
    registerStaticallyListenedElement(variable)
  }

  /**Registers the collection of [[Variable]] in parameter as dependencies of this Invariant.
   * Static dependencies are only used to define the propagation graph.
   *
   * @param variables The variables to add as dependence
   */
  def registerStaticDependencies(variables:Variable*): Unit ={
    for (variable <- variables)registerStaticallyListenedElement(variable)
  }

  /** this is the propagation method that should be overridden by propagation elements. notice that
   * it is only called in a propagation wave if: 1L: it has been registered for propagation since
   * the last time it was propagated 2L: it is included in the propagation wave: partial
   * propagation wave do not propagate all propagation elements; it only propagates the ones that
   * come in the predecessors of the targeted propagation element overriding this method is
   * optional, so an empty body is provided by default
   */
  override def performPropagation(): Unit = ???

  /** This is the debug procedure through which propagation element can redundantly check that the
   * incremental computation they perform through the performPropagation method is correct
   * overriding this method is optional, so an empty body is provided by default
   */
  override def checkInternals(): Unit = ???
}
