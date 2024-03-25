package oscar.cbls.core.computation.set

case class IndependentSerializableChangingSetValueSnapShot(
  uniqueId: Int,
  savedValue: SortedSet[Int]
) extends IndependentSerializableAbstractVariableSnapshot {
  override def makeLocal: AbstractVariableSnapShot =
    new ChangingSetValueSnapShot(uniqueId, savedValue)
}
