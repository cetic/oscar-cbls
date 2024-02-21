package oscar.cbls.core.computation

import oscar.cbls.algo.dll.DoublyLinkedList
import oscar.cbls.core.propagation.PropagationElement

case class KeyForRemoval(
  listeningElement: DoublyLinkedList[PropagationElement]#DLLStorageElement,
) {

  def performRemove: Unit = {
    listeningElement.delete()
  }

}
