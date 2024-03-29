package oscar.cbls.core.computation

import oscar.cbls.algo.dll.DoublyLinkedList
import oscar.cbls.core.propagation.PropagationElement

/** A small class that simplifies the removal of a dynamically listened element.
  *
  * If you want to stop sending notification to an element, just use the performRemove method of
  * KeyForRemoval of the listening element.
  *
  * @param listeningElement Reference of the listening element in its DLL
  * @param listenedElement Reference of the listened element in its DLL
  */
case class KeyForRemoval(listeningElement: DoublyLinkedList[PropagationElement]#DLLStorageElement,
                         listenedElement: DoublyLinkedList[(PropagationElement,Int)]#DLLStorageElement) {
  def performRemove(): Unit = {
    listeningElement.delete()
    listenedElement.delete()
  }
}
