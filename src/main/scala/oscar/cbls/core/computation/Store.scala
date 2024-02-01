package oscar.cbls.core.computation

import oscar.cbls.core.propagation._

import scala.collection.mutable

class Store(idToVariable: mutable.HashMap[Int,Variable], debugLevel: Int = 0) extends PropagationStructure(debugLevel) {

  def performPropagation(target: PropagationElement): Unit = ???

  override def registerForPartialPropagation(propagationElement: PropagationElement): Unit = ???

  def newUniqueId(): Int = ???

  override def scheduleForPropagation(propagationElement: PropagationElement): Unit = ???

  /** Saves the current value of the input [[Variable]] registered in the Store.
   *
   * NOTE : The input variables are not defined by any [[Invariant]]
   *
   * @return The Solution representing this Store
   */
  def save: Solution = ???
}
