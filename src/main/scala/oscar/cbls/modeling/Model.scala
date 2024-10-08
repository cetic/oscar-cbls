package oscar.cbls.modeling

import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.objective.{Minimize, Objective}
import oscar.cbls.modeling.invariant.{Logic, MinMax, Numeric}

import scala.collection.mutable

/** Companion object of the [[Model]] class. */
object Model {

  /** Declares a [[Model]] with the given name and debug level. */
  def apply(name: String = "<unnamed model>", debugLevel: Int = 0) = new Model(name, debugLevel)
}

/** This class provides the main functionality necessary to express an optimization model: it allows
  * the instantiation of variables, constraints, and objective function. Furthermore, it allows
  * expressing derived quantities (such as the sum over an array of variables) through objects that
  * collect the associated methods, with each object representing a category.
  *
  * '''WARNING:''' Must be declared as an `implicit` value in order to enable variable conversions and to let
  * expressions work properly.
  *
  * @param name
  *   the name of the model
  * @param debugLevel
  *   the debug level of the associated [[oscar.cbls.core.computation.Store]]
  */
class Model(val name: String, debugLevel: Int = 0) {

  protected[cbls] val store = new Store(debugLevel)

  object logic extends Logic

  object minMax extends MinMax

  object numeric extends Numeric

  private var isOpen = true

  private val hardConstraints = mutable.HashSet.empty[IntVariable]

  def addHardConstraint(value: IntVariable): Unit = {
    require(this.isOpen, "Cannot add constraints once model is closed")
    hardConstraints.add(value)
  }

  /** Closes the model, to allow the full instantiation of the internal propagation structure and
    * the definition of local search procedures that use the model. Once invoked, is not possible to
    * add further variables, constraints, or objective.
    */
  def close(): Unit = {
    isOpen = false
  }

  /** Add an integer variable to the model.
    *
    * @param min
    *   minimum value that the variable can assume
    * @param max
    *   maximum value that the variable can assume
    * @param name
    *   optional name of the variable
    */
  def intVar(min: Long, max: Long, name: String = ""): IntVariable = {
    require(this.isOpen, "Cannot add variables once model is closed")
    val intVar = IntVariable(store, 0, isConstant = false, if (name == "") None else Some(name))
    intVar.setDomain(min, max)
    intVar
  }

  /** Add boolean (0-1) variable to the model.
    *
    * @param name
    *   optional name of the variable
    */
  def booleanVar(name: String = ""): IntVariable = intVar(0, 1, name)

  /** Defines the objective to be the minimization of the given variable.
    *
    * @param obj
    *   variable expressing the objective function to minimize
    * @param underApproximatedObjValue
    *   An optional approximated IntVariable whose value is supposedly lesser or equal to the
    *   objValue. Used when the computation of the objValue is really expensive
    */
  def minimize(
    obj: IntVariable,
    underApproximatedObjValue: Option[IntVariable] = None
  ): Objective = {
    require(this.isOpen, "Cannot set an objective once model is closed")
    Minimize(obj, hardConstraints.toList, underApproximatedObjValue)
  }
}
