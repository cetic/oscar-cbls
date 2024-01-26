package oscar.cbls.core.propagation

import oscar.cbls.algo.dll.DoublyLinkedList

class KeyForRemoval(
  listeningElement: DoublyLinkedList[PropagationElement]#DLLStorageElement,
  listenedElement: DoublyLinkedList[PropagationElement]#DLLStorageElement
) {

  def performRemove: Unit = {
    listenedElement.delete()
    listenedElement.delete()
  }

}
