package oscar.cbls.core.computation

import oscar.cbls.algo.dll.DoublyLinkedList
import oscar.cbls.core.propagation.PropagationElement

/** A small wrapper class to contextualize the DoublyLinkedList
  *
  * When an [[Invariant]] registers a [[Variable]] as listened (dynamically), the Variable add the
  * listening element (here the Invariant) in it's DLL and returns a KeyForRemoval. This class wraps
  * the DLL element so that the Invariant can remove it when he's done listening to that Variable.
  *
  * @param listeningElement
  *   Reference of the listening element and it's in its DLL
  */
case class KeyForRemoval(
  listeningElement: DoublyLinkedList[(PropagationElement, Int)]#DLLStorageElement
) {

  /** Removes the listening element from the listening elements DLL */
  def stopListeningToThisElement(): Unit = listeningElement.delete()
}
