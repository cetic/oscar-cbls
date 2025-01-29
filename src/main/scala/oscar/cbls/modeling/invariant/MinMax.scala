package oscar.cbls.modeling.invariant

import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.set.{SetConstant, SetVariable}
import oscar.cbls.lib.invariant.minmax._
import oscar.cbls.modeling.Model

trait MinMax {

  /** Returns a variable representing the minimum value between the two input values.
    *
    * @param a
    *   The first value on which compute the minimum.
    * @param b
    *   The second value on which compute the minimum.
    */
  def min(a: IntVariable, b: IntVariable)(implicit m: Model): IntVariable = {
    val output = IntVariable(m.store, 0)
    Min2(m.store, a, b, output)
    output
  }

  /** Returns a variable representing the maximum value between the two input values.
    *
    * @param a
    *   The first value on which compute the maximum.
    * @param b
    *   The second value on which compute the maximum.
    */
  def max(a: IntVariable, b: IntVariable)(implicit m: Model): IntVariable = {
    val output = IntVariable(m.store, 0)
    Max2(m.store, a, b, output)
    output
  }

  /** Returns a variable representing the minimum of all the input values.
    *
    * @param input
    *   Array of variables on which to compute the minimum.
    * @param default
    *   The default value of the minimum. By default, it is set to `Int.MaxValue`.
    * @param bulkIdentifier
    *   A [[oscar.cbls.core.computation.IncredibleBulk]] is used when several Invariant listen to
    *   vars. Warning: [[oscar.cbls.core.computation.IncredibleBulk]] are distinguished only by
    *   their identifier. Be sure to use the same one if you're referencing the same variables.
    * @param name
    *   Optional name of the resulting variable.
    */
  def min(
    input: Array[IntVariable],
    default: Long = Int.MaxValue,
    bulkIdentifier: String = "",
    name: String = ""
  )(implicit m: Model): IntVariable = {
    val allIndices = SetConstant(m.store, input.indices.toSet)
    partialMin(input, allIndices, default, bulkIdentifier, name)(m)
  }

  /** Returns a variable representing the maximum of all the input values.
    *
    * @param input
    *   Array of variables on which to compute the maximum.
    * @param default
    *   The default value of the maximum. By default, it is set to `Int.MinValue`.
    * @param bulkIdentifier
    *   A [[oscar.cbls.core.computation.IncredibleBulk]] is used when several Invariant listen to
    *   vars. Warning: [[oscar.cbls.core.computation.IncredibleBulk]] are distinguished only by
    *   their identifier. Be sure to use the same one if you're referencing the same variables.
    * @param name
    *   Optional name of the resulting variable.
    */
  def max(
    input: Array[IntVariable],
    default: Long = Int.MinValue,
    bulkIdentifier: String = "",
    name: String = ""
  )(implicit m: Model): IntVariable = {
    val allIndices = SetConstant(m.store, input.indices.toSet)
    partialMax(input, allIndices, default, bulkIdentifier, name)(m)
  }

  /** Returns a variable representing the minimum of the input values such that their indices belong
    * to a given set, i.e., `Min(input(i) | i in indices)` where `input` is an array of integer
    * variables.
    *
    * @param input
    *   Array of variables on which to compute the minimum.
    * @param indices
    *   Variable maintaining the indices of the input array to take into account to calculate the
    *   minimum.
    * @param default
    *   The default value of the minimum. By default, it is set to `Int.MaxValue`.
    * @param bulkIdentifier
    *   A [[oscar.cbls.core.computation.IncredibleBulk]] is used when several Invariant listen to
    *   vars. Warning: [[oscar.cbls.core.computation.IncredibleBulk]] are distinguished only by
    *   their identifier. Be sure to use the same one if you're referencing the same variables.
    * @param name
    *   Optional name of the resulting variable.
    */
  def partialMin(
    input: Array[IntVariable],
    indices: SetVariable,
    default: Long = Int.MaxValue,
    bulkIdentifier: String = "",
    name: String = ""
  )(implicit m: Model): IntVariable = {
    require(
      indices.value().isEmpty || (indices.value().min >= 0 && indices.value().max < input.length),
      "Elements of the indices SetVariable are out of bounds for the input array"
    )

    val optBulk = bulkIdentifier match {
      case "" => None
      case x  => Some(x)
    }
    val optName = name match {
      case "" => None
      case x  => Some(x)
    }

    val output = IntVariable(m.store, 0)

    Min(m.store, input, indices, output, default, optBulk, optName)
    output
  }

  /** Returns a variable representing the maximum of the input values such that their indices belong
    * to a given set, i.e., `Max(input(i) | i in indices)` where `input` is an array of integer
    * variables.
    *
    * @param input
    *   Array of variables on which to compute the maximum.
    * @param indices
    *   Variable maintaining the indices of the input array to take into account to calculate the
    *   maximum.
    * @param default
    *   The default value of the maximum. By default, it is set to `Int.MinValue`.
    * @param bulkIdentifier
    *   A [[oscar.cbls.core.computation.IncredibleBulk]] is used when several Invariant listen to
    *   vars. Warning: [[oscar.cbls.core.computation.IncredibleBulk]] are distinguished only by
    *   their identifier. Be sure to use the same one if you're referencing the same variables.
    * @param name
    *   Optional name of the resulting variable.
    */
  def partialMax(
    input: Array[IntVariable],
    indices: SetVariable,
    default: Long = Int.MinValue,
    bulkIdentifier: String = "",
    name: String = ""
  )(implicit m: Model): IntVariable = {
    require(
      indices.value().isEmpty || (indices.value().min >= 0 && indices.value().max < input.length),
      "Elements of the indices SetVariable are out of bounds for the input array"
    )

    val optBulk = bulkIdentifier match {
      case "" => None
      case x  => Some(x)
    }
    val optName = name match {
      case "" => None
      case x  => Some(x)
    }

    val output = IntVariable(m.store, 0)

    Max(m.store, input, indices, output, default, optBulk, optName)
    output
  }

  /** Returns a variable representing the minimum value in a subset of a given array of constant
    * integers, i.e., `Min{input(i) | i in indices}`.
    *
    * @param input
    *   Array of constants on which to compute the minimum.
    * @param indices
    *   Variable maintaining the indices of the input array to take into account to calculate the
    *   minimum.
    * @param default
    *   The default value of the minimum. By default, it is set to `Int.MaxValue`.
    * @param name
    *   Optional name of the resulting variable.
    */
  def minOfConstants(
    input: Array[Long],
    indices: SetVariable,
    default: Long = Int.MaxValue,
    name: String = ""
  )(implicit m: Model): IntVariable = {
    require(
      indices.value().isEmpty || (indices.value().min >= 0 && indices.value().max < input.length),
      "Elements of the indices SetVariable are out of bounds for the input array"
    )
    val store = m.store
    val out   = IntVariable(store, 0)
    val optName = name match {
      case "" => None
      case x  => Some(x)
    }
    MinConst(store, input, indices, out, default, name = optName)
    out
  }

  /** Returns a variable representing the maximum value in a subset of a given array of constant
    * integers, i.e., `Max{input(i) | i in indices}`.
    *
    * @param input
    *   Array of constants on which to compute the maximum.
    * @param indices
    *   Variable maintaining the indices of the input array to take into account to calculate the
    *   maximum.
    * @param default
    *   The default value of the maximum. By default, it is set to `Int.MinValue`.
    * @param name
    *   Optional name of the resulting variable.
    */
  def maxOfConstants(
    input: Array[Long],
    indices: SetVariable,
    default: Long = Int.MinValue,
    name: String = ""
  )(implicit m: Model): IntVariable = {
    require(
      indices.value().isEmpty || (indices.value().min >= 0 && indices.value().max < input.length),
      "Elements of the indices SetVariable are out of bounds for the input array"
    )
    val store = m.store
    val out   = IntVariable(store, 0)
    val optName = name match {
      case "" => None
      case x  => Some(x)
    }
    MaxConst(store, input, indices, out, default, name = optName)
    out
  }

  /** Returns a variable representing the minimum of the input set.
    *
    * @param input
    *   Set variable on which to compute the minimum.
    * @param default
    *   The default value of the minimum. By default, it is set to `Int.MaxValue`.
    * @param name
    *   Optional name of the resulting variable.
    */
  def minSet(input: SetVariable, default: Long = Int.MaxValue, name: String = "")(implicit
    m: Model
  ): IntVariable = {
    val store = m.store
    val out   = IntVariable(store, 0)
    val optName = name match {
      case "" => None
      case x  => Some(x)
    }
    MinSet(store, input, out, default, optName)
    out
  }

  /** Returns a variable that maintain the maximum of the input set.
    *
    * @param input
    *   Set variable on which to compute the maximum.
    * @param default
    *   The default value of the maximum. By default, it is set to `Int.MinValue`.
    * @param name
    *   Optional name of the resulting variable.
    */
  def maxSet(input: SetVariable, default: Long = Int.MinValue, name: String = "")(implicit
    m: Model
  ): IntVariable = {
    val store = m.store
    val out   = IntVariable(store, 0)
    val optName = name match {
      case "" => None
      case x  => Some(x)
    }
    MaxSet(store, input, out, default, optName)
    out
  }
}
