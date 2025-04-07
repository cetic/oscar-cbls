package oscar.cbls.lib.invariant.routing.abstractGenericConstraint.transferFunction

/** The UnidirectionalTransferFunction is a part of the [[TransferFunction]]. It holds the
  * TransferFunction value for one direction. Either backward or forward.
  *
  * See the [[TransferFunction]] description for more information.
  */
abstract class UnidirectionalTransferFunction {

  /** Composes this transfer function with another into a bigger one.
    *
    * @param otherTF
    *   The other transfer function.
    * @return
    *   The composed transfer function.
    */
  def compose(otherTF: UnidirectionalTransferFunction): UnidirectionalTransferFunction

  /** Throws a compose error stating that the two UnidirectionalTransferFunction are not compatible.
    *
    * @param tf1
    *   The first UnidirectionalTransferFunction.
    * @param tf2
    *   The second UnidirectionalTransferFunction.
    */
  protected def throwComposeError(
    tf1: UnidirectionalTransferFunction,
    tf2: UnidirectionalTransferFunction
  ): Unit = {
    require(requirement = false, s"Trying to compose a ${tf1.getClass} with a ${tf2.getClass}")
  }
}
