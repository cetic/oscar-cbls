package oscar.cbls.core.computation

import oscar.cbls.core.propagation._

/** Abstract structure for Invariant definition.
  *
  * An invariant is a propagation element that maintains the value of output variable given
  * modification of the input variable(s). For instance, a identity invariant that take a
  * IntVariable as input and output will ensure that the output variable is equal to the input
  * variable.
  *
  * This abstract class provides an "empty" implementation of the method performPropagation. There
  * is two way of propagating invariant result to it's output variable. When you receive a
  * notification of modification from the input variable you ...
  *   - ... directly set the new value of the output variable. In that case you do not need to
  *     override performPropagation()
  *   - ... do some computation and SCHEDULE the invariant FOR PROPAGATION. In that case you need to
  *     override performPropagation()
  *
  * @param propagationStructure
  *   The propagation structure to which the element is attached
  */
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
