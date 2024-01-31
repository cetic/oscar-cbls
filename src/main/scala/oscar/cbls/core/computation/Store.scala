package oscar.cbls.core.computation

import oscar.cbls.core.propagation._

import scala.collection.mutable

class Store(idToVariable: mutable.HashMap[Int,Variable]) extends PropagationStructure {

  def performPropagation(target: PropagationElement): Unit = ???

  override def registerForPartialPropagation(propagationElement: PropagationElement): Unit = ???

  def newUniqueId(): Int = ???

  override def scheduleForPropagation(propagationElement: PropagationElement): Unit = ???

  def save: Solution = ???
}
