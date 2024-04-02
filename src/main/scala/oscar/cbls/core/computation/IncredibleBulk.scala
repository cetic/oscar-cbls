package oscar.cbls.core.computation

import oscar.cbls.core.propagation.{PropagationElement, PropagationStructure}

import scala.collection.immutable.HashMap

/** Used to statically bulk-register a dependency to an Iterable of [[Variable]].
  */
object IncredibleBulk {

  // The map of recorded IncredibleBulk
  private var incredibleBulks: HashMap[String, IncredibleBulk] = HashMap.empty

  /** Registers a new IncredibleBulk if it wasn't registered before.
    *
    * NOTE : IncredibleBulk are distinguished only by their identifier. Be sure to use the same one
    * if you're referencing the same Variables.
    *
    * @param bulkVariables
    *   The bulk registered variables
    * @param incredibleBulkIdentifier
    *   The incredible bulk identifier
    * @param propagationStructure
    *   The propagation structure to which the element is attached
    * @return
    *   The IncredibleBulk listening to the variables.
    */
  def bulkRegistering(
    bulkVariables: Iterable[Variable],
    incredibleBulkIdentifier: String,
    propagationStructure: PropagationStructure
  ): IncredibleBulk = {
    if (!incredibleBulks.contains(incredibleBulkIdentifier)) {
      incredibleBulks += (incredibleBulkIdentifier, IncredibleBulk(
        bulkVariables,
        propagationStructure
      ))
    }
    incredibleBulks(incredibleBulkIdentifier)
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
case class IncredibleBulk(
  bulkVariables: Iterable[Variable],
  propagationStructure: PropagationStructure
) extends PropagationElement(propagationStructure) {

  // Register (once) each variable to build the static graph.
  bulkVariables.foreach(v => registerStaticallyListenedElement(v))

  /** Allows to check and debug propagation elements.
    *
    * This method can be called after the propagation according to the debug level of the
    * propagation structure (see [[PropagationStructure]]). It can be used to check if the invariant
    * worked properly by, for example, recomputing the value from scratch.
    */
  override def checkInternals(): Unit = {}
}
