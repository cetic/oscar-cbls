package oscar.cbls.core.computation

import oscar.cbls.core.propagation._

abstract class Invariant(propagationStructure: PropagationStructure)
    extends PropagationElement(propagationStructure) {
  require(propagationStructure != null, "The propagation structure must be defined")

  override def performPropagation(): Unit = {
    require(
      false,
      "If you receive this error, it means that your Invariant was scheduled for propagation but does not have a proper implementation of performPropagation().\n" +
        "Either you directly update the output variable upon receiving notification of your input variable.\n" +
        "Or you schedule your invariant for propagation and override this method."
    )
  }
}
