package oscar.cbls.core.computation

import oscar.cbls.algo.dll.DoublyLinkedList
import oscar.cbls.core.propagation.PropagationElement

/** A small class that simplifies the removal of a dynamically listened element.
  *
  * If you want to stop sending notification to an element, just use the performRemove method of
  * KeyForRemoval of the listening element.
  *
  * @param listeningElement
  */
case class KeyForRemoval(listeningElement: DoublyLinkedList[PropagationElement]#DLLStorageElement) {
  def performRemove: Unit = {
    listeningElement.delete()
  }
}
