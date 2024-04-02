package oscar.cbls.core.computation

import oscar.cbls.core.propagation._

import scala.collection.immutable.HashMap

/** Concrete implementation of a PropagationStructure
  *
  * It allows to create/restore Solution based on decision variables.
  *
  * @param debugLevel
  *   the level of debug
  */
class Store(debugLevel: Int = 0) extends PropagationStructure(debugLevel) {

  private var idToVariable: HashMap[Int, Variable]         = HashMap.empty
  private var idToDecisionVariable: HashMap[Int, Variable] = HashMap.empty
  private var lastSolutionNb: Int = -1

  private def nextSolutionNb: Int = {
    lastSolutionNb += 1
    lastSolutionNb
  }

  /** Saves the current value of the input [[Variable]] registered in the Store.
    *
    * NOTE : The input variables are not defined by any [[Invariant]]
    *
    * @return
    *   The Solution representing this Store
    */
  def save: Solution = {
    require(closed, "Model must be closed before saving a new solution")
    Solution(idToDecisionVariable.map(id2Decision => id2Decision._2.save()), this, nextSolutionNb)
  }

  /** Triggers the propagation on all the model. */
  def performTotalPropagation(): Unit = {
    super.propagate()
  }

  /** Trigger the propagation up to the targeted element.
    *
    * Only the needed propagation elements will be updated.
    *
    * @param target
    *   The variable whose new value is needed
    */
  def performPartialPropagation(target: Variable): Unit = {
    super.propagate(Some(target))
  }

  override def setupPropagationStructure(): Unit = {
    super.setupPropagationStructure()
    getPropagationElements.foreach {
      case v: Variable =>
        idToVariable += (v.id -> v)
        if (v.isADecisionVariable)
          idToDecisionVariable += (v.id -> v)
      case _ =>
    }
  }

  override def toString: String = "Store(vars:{" + idToVariable.values.mkString(";") + "})"

}
