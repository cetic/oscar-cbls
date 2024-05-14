//package oscar.cbls.core.computation.set
//
//class ChangingSetValueSnapShot(val uniqueId: Int, val savedValue: SortedSet[Int])
//    extends AbstractVariableSnapShot(uniqueId) {
//  override protected def doRestore(m: Store): Unit = {
//    m.getSetVar(uniqueId) := savedValue
//  }
//
//  override def valueString(): String = "{" + savedValue.mkString(",") + "}"
//
//  override protected def doRestoreWithoutCheckpoints(m: Store): Unit = {
//    m.getSetVar(uniqueId) := savedValue
//  }
//
//  override def makeIndependentSerializable: IndependentSerializableAbstractVariableSnapshot =
//    IndependentSerializableChangingSetValueSnapShot(uniqueId, savedValue)
//}
