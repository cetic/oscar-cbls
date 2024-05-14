//package oscar.cbls.core.computation.set
//
//case class CBLSetVarCheckpoint(variable: CBLSSetVar, savedValue: SortedSet[Int])
//    extends VariableCheckpoint {
//  override def restoreAndReleaseCheckpoint(): Unit = {
//    variable := savedValue
//  }
//}
