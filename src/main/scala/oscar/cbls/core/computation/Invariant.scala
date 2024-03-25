package oscar.cbls.core.computation

import oscar.cbls.core.propagation._

abstract class Invariant(propagationStructure: PropagationStructure)
    extends PropagationElement(propagationStructure) {

  def model: Store = propagationStructure.asInstanceOf[Store]
}
