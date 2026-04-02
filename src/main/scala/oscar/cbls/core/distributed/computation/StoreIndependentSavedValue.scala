package oscar.cbls.core.distributed.computation

/** A saved value that holds no reference to a [[oscar.cbls.core.computation.Store]] or to a
  * [[oscar.cbls.core.computation.Variable]] so that it can be serialized and transferred from one
  * store to another one
  */
sealed abstract class StoreIndependentSavedValue

/** A saved value of an [[oscar.cbls.IntVariable]] that holds no reference to a
  * [[oscar.cbls.core.computation.Store]] or to a [[oscar.cbls.core.computation.Variable]] so that
  * it can be serialized and transferred from one store to another one
  */
case class StoreIndependentIntSavedValue(variableID: Int, savedValue: Long)
    extends StoreIndependentSavedValue

/** A saved value of an [[oscar.cbls.SetVariable]] that holds no reference to a
  * [[oscar.cbls.core.computation.Store]] or to a [[oscar.cbls.core.computation.Variable]] so that
  * it can be serialized and transferred from one store to another one
  */
case class StoreIndependentSetSavedValue(variableID: Int, savedValue: Array[Int])
    extends StoreIndependentSavedValue

/** A saved value of an [[oscar.cbls.SeqVariable]] that holds no reference to a
  * [[oscar.cbls.core.computation.Store]] or to a [[oscar.cbls.core.computation.Variable]] so that
  * it can be serialized and transferred from one store to another one
  */
case class StoreIndependentSeqSavedValue(variableID: Int, savedValue: Array[Int])
    extends StoreIndependentSavedValue
