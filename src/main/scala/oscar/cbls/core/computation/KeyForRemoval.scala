// OscaR is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 2.1 of the License, or
// (at your option) any later version.
//
// OscaR is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License  for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with OscaR.
// If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html

package oscar.cbls.core.computation

import oscar.cbls.algo.dll.DoublyLinkedList
import oscar.cbls.core.propagation.PropagationElement

/** A small wrapper class to contextualize the DoublyLinkedList
  *
  * When an [[Variable]] registers dynamically a new listening [[Invariant]], the Variable add the
  * listening element (here the Invariant) in it's DLL and returns a KeyForRemoval. The Invariant
  * must extends the proper NotificationTarget trait (one trait for each Variable) to receive
  * notification from the Variable. This class wraps the DLL element so that the Invariant can
  * remove it when he's done listening to that Variable.
  *
  * The type T may vary depending on the [[Variable]] using this class. But it should have the
  * following structure : (NotificationTargetType, Int)
  *
  * @param listeningElement
  *   Reference of the listening element and it's in its DLL
  */
case class KeyForRemoval[T](listeningElement: DoublyLinkedList[T]#DLLStorageElement) {

  /** Removes the listening element from the listening elements DLL */
  def delete(): Unit = listeningElement.delete()
}
