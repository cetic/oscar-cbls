package oscar.cbls.core.computation

import oscar.cbls.core.propagation._

abstract class Invariant(propagationStructure: PropagationStructure)
    extends PropagationElement(propagationStructure) {
  require(propagationStructure != null, "The propagation structure must be defined")
}
