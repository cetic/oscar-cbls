package oscar.cbls.core.computation.set

/*
 * @author renaud.delandtsheer@cetic.be
 */
abstract class SetInvariant(
  initialValue: SortedSet[Int] = SortedSet.empty,
  initialDomain: Domain = DomainRange(0, Int.MaxValue)
) extends ChangingSetValue(initialValue, initialDomain)
    with Invariant {

  override def definingInvariant: Invariant = this

  override def isControlledVariable: Boolean = true

  override def isDecisionVariable: Boolean = false

  override def model: Store = propagationStructure.asInstanceOf[Store]

  override def hasModel: Boolean = schedulingHandler != null

  private var customName: String = null

  /** use this if you want to give a particular name to this concept, to be used in toString */
  def setName(n: String): SetInvariant = {
    customName = n
    this
  }

  override final def name: String =
    if (customName == null) this.getClass.getSimpleName else customName

  override final def performPropagation(): Unit = {
    performInvariantPropagation()
    performSetPropagation()
  }
}
