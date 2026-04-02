package oscar.cbls.core.distributed.computation

/** A solution that is not bound to a store. It is generated from a
  * [[oscar.cbls.core.computation.Solution]] and can be turned back into a
  * [[oscar.cbls.core.computation.Solution]]. It should be serializable.
  */
case class StoreIndependentSolution(
  savedValues: List[StoreIndependentSavedValue],
  checkSumOnStoreStructure: Long
)
