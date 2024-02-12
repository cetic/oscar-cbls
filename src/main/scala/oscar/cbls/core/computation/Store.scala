package oscar.cbls.core.computation

import oscar.cbls.core.propagation._

import scala.collection.immutable.HashMap
import scala.collection.mutable

class Store(debugLevel: Int = 0) extends PropagationStructure(debugLevel) {

  private var idToVariable: HashMap[Int, Variable]       = HashMap.empty
  private var idToDecisionVariable: HashMap[Int, Variable] = HashMap.empty

  private var storeIsClosed: Boolean = false
  private var lastSolutionNb: Int = -1

  def registerVariable(variable: Variable): Unit =
    idToVariable += variable.id -> variable

  def decisionVariable(decisionVariableId: Int): Variable =
    idToDecisionVariable(decisionVariableId)

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
    require(storeIsClosed, "Model must be closed before saving a new solution")
    Solution(idToDecisionVariable.values.map(_.save()),this, lastSolutionNb)
  }

  def close(): Unit = {
    for(idAndVariable <- idToVariable){
      if(idAndVariable._2.isADecisionVariable)
        idToDecisionVariable += idAndVariable
    }
    storeIsClosed = true
  }

  override def toString:String = "Store(vars:{" + idToVariable.values.mkString(";") + "})"

}
