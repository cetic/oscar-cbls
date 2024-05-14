//package oscar.cbls.core.computation.set
//
//class ValueWiseKey(
//  originalKey: KeyForElementRemoval,
//  setValue: ChangingSetValue,
//  var target: SetNotificationTarget
//) {
//
//  val sizeOfSet: Int = setValue.max - setValue.min + 1
//  val offset: Int    = setValue.min
//  var currentValueWisePropagationWaveIdentifier: ValueWisePropagationWaveIdentifier = null
//
//  def performRemove(): Unit = {
//    // remove all values in the focus of this key
//    for (i <- valueToKey.indices) {
//      if (valueToKey(i) != null) {
//        valueToKey(i).delete()
//        valueToKey(i) = null
//      }
//    }
//    originalKey.performRemove()
//  }
//
//  val minValue: Int = setValue.min
//  val maxValue: Int = setValue.max
//
//  var valueToKey: Array[DLLStorageElement[ValueWiseKey]] = Array.fill(maxValue - minValue + 1)(null)
//  // val valueToKeyArray = Array.fill[DLLStorageElement[ValueWiseKey]](sizeOfSet)(null)
//
//  def addToKey(value: Int): Unit = {
//    if (!(minValue <= value && value <= maxValue)) return
//    // TODO: this is O(log n) and should be improved to O(1)!!!
//    valueToKey(value) = setValue.addToValueWiseKeys(this, value)
//  }
//
//  def removeFromKey(value: Int): Unit = {
//    if (!(minValue <= value && value <= maxValue)) return
//    val k = valueToKey(value)
//    k.delete()
//    valueToKey(value) = null
//  }
//}
