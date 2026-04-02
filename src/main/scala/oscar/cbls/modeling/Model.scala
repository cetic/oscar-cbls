package oscar.cbls.modeling

import org.apache.pekko.actor.typed.{ActorRef, ActorSystem, SupervisorStrategy}
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.slf4j.{Logger, LoggerFactory}
import oscar.cbls.Neighborhood
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.{IntConstant, IntVariable}
import oscar.cbls.core.computation.objective._
import oscar.cbls.core.computation.seq.SeqVariable
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.core.distributed.protocol.MessageToSupervisor
import oscar.cbls.core.distributed.actors.Supervisor
import oscar.cbls.core.distributed.computation.SearchConnector
import oscar.cbls.modeling.routing.VRS

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
  * @note
  *   This class is designed to be used as an
  *   [[https://docs.scala-lang.org/scala3/book/ca-context-parameters.html implicit value]]. In our
  *   context, an implicit value is similar to a default value. This makes the code less verbose and
  *   allows us to use
  *   [[https://docs.scala-lang.org/scala3/book/ca-implicit-conversions.html implicit conversions]]
  *   (see [[oscar.cbls.examples.WLPAdvancedModelingExample]]). <br>
  *
  * '''WARNING:''' Don't use implicit values if you are working with multiple models. This can lead
  * to errors. Use a syntax similar to [[oscar.cbls.examples.WLPBeginnerModelingExample]].
  *
  * @param name
  *   the name of the model
  * @param debugLevel
  *   the debug level of the associated [[oscar.cbls.core.computation.Store]]
  */
class Model(val name: String, debugLevel: Int = 0) {

  private val _store = new Store(debugLevel)

  /** Returns a reference to the propagation structure associated to this model. */
  def store: Store = _store

  def isClosed: Boolean = store.isClosed

  private val hardConstraints = mutable.HashSet.empty[IntVariable]

  /** Add a constraint to the model, expressed as an integer variable that must be 0 in order for
    * the constraint to be satisfied.
    *
    * @param value
    *   the variable associated to the constraint
    */
  def addConstraint(value: IntVariable): Unit = {
    require(!this.isClosed, "Cannot add constraints once model is closed")
    hardConstraints.add(value)
  }

  /** Closes the model, to allow the full instantiation of the internal propagation structure and
    * the definition of local search procedures that use the model. Once invoked, is not possible to
    * add further variables, constraints, or objective.
    */
  def close(): Unit = {
    _store.close()
  }

  /** Add a [[oscar.cbls.core.computation.integer.IntVariable]] to the model.
    *
    * @param initialValue
    *   initial value of the variable
    * @param min
    *   minimum value that the variable can assume
    * @param max
    *   maximum value that the variable can assume
    * @param name
    *   optional name of the variable
    * @return
    *   a reference to the variable
    */
  def intVar(initialValue: Long, min: Long, max: Long, name: String = ""): IntVariable = {
    require(!this.isClosed, "Cannot add variables once model is closed")
    require(min <= max, s"Invalid domain: $min > $max")
    require(
      initialValue <= max && initialValue >= min,
      s"Initial value $initialValue non consistent with given domain [$min, $max]"
    )
    val intVar =
      IntVariable(_store, initialValue, isConstant = false, if (name == "") None else Some(name))
    intVar.setDomain(min, max)
    intVar
  }

  /** Add and return a [[oscar.cbls.core.computation.integer.IntConstant]] to the model.
    *
    * @param value
    *   the value of the constant
    * @return
    *   a reference to the constant
    */
  def intConst(value: Long): IntConstant = IntConstant(_store, value)

  /** Add a [[oscar.cbls.core.computation.set.SetVariable]] to the model.
    *
    * @param initialValue
    *   initial value of the variable
    * @param min
    *   minimum value that an integer element of the set can have
    * @param max
    *   maximum value that an integer element of the set can have
    * @param name
    *   optional name of the variable
    * @return
    *   a reference to the variable
    */
  def setVar(initialValue: Set[Int], min: Int, max: Int, name: String = ""): SetVariable = {
    commonRequires(initialValue, min, max)
    val setVar = SetVariable(_store, initialValue, name = if (name == "") None else Some(name))
    setVar.setDomain(min, max)
    setVar
  }

  /** Add a [[oscar.cbls.core.computation.seq.SeqVariable]] to the model.
    *
    * @param initialValue
    *   initial value of the variable
    * @param min
    *   minimum value that an integer element of the sequence can have
    * @param max
    *   maximum value that an integer element of the sequence can have
    * @param name
    *   optional name of the variable
    * @return
    *   a reference to the variable
    */
  def seqVar(initialValue: List[Int], min: Int, max: Int, name: String = null): SeqVariable = {
    commonRequires(initialValue, min, max)
    val seqVar = SeqVariable(_store, initialValue, name)
    seqVar.setDomain(min, max)
    seqVar
  }

  // requirements used for both seqVar and setVar
  private def commonRequires(initialValue: Iterable[Int], min: Int, max: Int): Unit = {
    require(!this.isClosed, "Cannot add variables once model is closed")
    require(min <= max, s"Invalid domain: $min > $max")
    require(
      initialValue.isEmpty || (initialValue.min >= min && initialValue.max <= max),
      "Initial value non consistent with given bounds"
    )
  }

  /** Add a binary (0-1) variable to the model.
    *
    * @param initialValue
    *   the initial value of the variable
    * @param name
    *   optional name of the variable
    * @return
    *   a reference to the variable
    */
  def binaryVar(initialValue: Long, name: String = ""): IntVariable =
    intVar(initialValue, 0, 1, name)

  /** Adds a vehicle routing structure [[VRS]] with `n` nodes and `v` vehicles for this model.
    *
    * @param n
    *   number of nodes in the VRS
    * @param v
    *   number of vehicles in the VRS
    * @param maxPivotPerValuePercent
    *   maximum number of pivots in the sequence variable underlying the VRS, affecting
    *   regularization when checkpoints are defined
    * @param debug
    *   whether debug mode is activated or not
    * @return
    *   a reference to the [[VRS]] just defined
    */
  def vrs(n: Int, v: Int, maxPivotPerValuePercent: Int = 4, debug: Boolean = false): VRS = {
    VRS(_store, n, v, maxPivotPerValuePercent, debug)
  }

  /** Defines the objective to be the minimization of the given variable.
    *
    * @param obj
    *   variable expressing the objective function to minimize
    * @param underApproximatedObjValue
    *   An optional approximated IntVariable whose value is supposedly lesser or equal to the
    *   objValue. Used when the computation of the objValue is really expensive
    * @return
    *   a reference to the objective
    */
  def minimize(
    obj: IntVariable,
    underApproximatedObjValue: Option[IntVariable] = None
  ): Objective = {
    require(!this.isClosed, "Cannot set an objective once model is closed")
    Minimize(obj, hardConstraints.toList, underApproximatedObjValue)
  }

  /** Defines the objective to be the maximization of the given variable.
    *
    * @param obj
    *   variable expressing the objective function to minimize
    * @param underApproximatedObjValue
    *   An optional approximated IntVariable whose value is supposedly lesser or equal to the
    *   objValue. Used when the computation of the objValue is really expensive
    * @return
    *   a reference to the objective
    */
  def maximize(
    obj: IntVariable,
    underApproximatedObjValue: Option[IntVariable] = None
  ): Objective = {
    require(!this.isClosed, "Cannot set an objective once model is closed")
    Maximize(obj, hardConstraints.toList, underApproximatedObjValue)
  }

  /** Defines the objective to be an acceptAll objective
    *
    * @param obj
    *   The objective with wich this accept all is constructed
    * @param allowsConstraintViolation
    *   Should the objective accept solutions that violate some strong constraints. <br>
    *   '''WARNING:''' if set to `true`, it can lead to chaotic behaviour of the search. For
    *   example, in a pick-up and delivery problem the search can be stuck in solution where pick-up
    *   point are positioned after the delivery points.
    */
  def acceptAll(obj: Objective, allowsConstraintViolation: Boolean = false): Objective = {
    AcceptAll(obj.objValue, obj.mustBeZero, allowsConstraintViolation)
  }

}
