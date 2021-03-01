package oscar.algebra

import scala.collection.mutable

/**
 * Represents an objective for a mathematical model.
 *
 * @tparam O the degree of the objective
 * @tparam V the type of values contained by the variables in the objective
 */
abstract class Objective[O <: ExpressionDegree, V: Numeric] {
  /**
   * Returns the expression to be either minimized or maximized
   */
  def expression: NormalizedExpression[O, V]
}

object Maximize{
  def apply[O <: ExpressionDegree, V: Numeric](expression: NormalizedExpression[O, V]): Maximize[O,V] = new Maximize[O,V](expression)
}

object Minimize{
  def apply[O <: ExpressionDegree, V: Numeric](expression: NormalizedExpression[O, V]): Minimize[O,V] = new Minimize[O,V](expression)
}

/**
 * Describes a maximization objective.
 *
 * @param expression expression to be maximized
 * @tparam O the degree of the objective
 * @tparam V the type of values contained by the variables in the objective
 */
class Maximize[O <: ExpressionDegree, V: Numeric](val expression: NormalizedExpression[O, V]) extends Objective[O, V]

/**
 * Describes a minimization objective.
 *
 * @param expression expression to be minimized
 * @tparam O the degree of the objective
 * @tparam V the type of values contained by the variables in the objective
 */
class Minimize[O <: ExpressionDegree, V: Numeric](val expression: NormalizedExpression[O, V]) extends Objective[O, V]

/**
 * A [[Model]] represents a mathematical model (X,C,O) where X is a set of numerical variables, C a set of constraints
 * and O a set of objectives to be optimized.
 *
 * Please note that such [[Model]] is mutable and there is no check to ensure that it is not modified after a solver
 * has begun solving it.
 *
 * @tparam O the maximum degree of the objectives to be added to this model
 * @tparam C the maximum degree of the constraints to be added to this model
 * @tparam V the type value stored in the variables (Double, Int, ...). For now, mainly Double is used here.
 */
class Model[O >: Constant <: ExpressionDegree , C <: ExpressionDegree, V: Numeric] {

  private val _variables: scala.collection.mutable.ArrayBuffer[Var[V]] = new scala.collection.mutable.ArrayBuffer[Var[V]]()
  /**
   * Returns the list of [[Var]] of the [[Model]]
   */
  def variables: scala.collection.IndexedSeq[Var[V]] = _variables

  /**
   * Returns the maximum index of all the variables
   */
  def maxIndex: Int = _variables.size

  private var _objective: Objective[O, V] = new Minimize[O,V](Const(implicitly[Numeric[V]].zero).normalized[V])
  /**
   * Returns the [[Objective]] to be optimized
   */
  def objective: Objective[O, V] = _objective


  private val _constraints = scala.collection.mutable.ListBuffer[EquationSystem[C, V]]()
  /**
   * Returns the list of constraints (as [[EquationSystem]]) for this [[Model]]
   */
  def constraints: mutable.Seq[EquationSystem[C,V]] = _constraints

  /**
   * Adds a variable in this model
   *
   * @param v the variable to be added
   * @return the index of the added variable in this model
   */
  def addVariable(v: Var[V]): Int = {
    _variables += v
    maxIndex - 1
  }

  /**
   * Adds a constraint to the model
   *
   * @param eq the equation to be added
   */
  def subjectTo(eq: Equation[C, V]*): Unit = {
    _constraints += new EquationSystem[C, V](eq.toIterable)
  }

  /**
   * Adds a set of constraint to the model
   *
   * @param eqs the set of constraints to be added
   */
  def subjectTo(eqs: EquationSystem[C, V]): Unit = {
    _constraints += eqs
  }

  /**
   * Adds an objective
   */
  def withObjective(obj: Objective[O, V]): Unit = {
    _objective = obj
  }

  override def toString: String = _constraints.mkString("\n")
}

