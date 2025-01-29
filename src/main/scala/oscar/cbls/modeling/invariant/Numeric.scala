package oscar.cbls.modeling.invariant

import oscar.cbls.core.computation.integer.{IntConstant, IntVariable}
import oscar.cbls.core.computation.set.{SetConstant, SetVariable}
import oscar.cbls.lib.invariant.numeric._
import oscar.cbls.modeling.Model

trait Numeric {

  /** Returns a variable that maintains the sum of all variables in the input array.
    *
    * Uses the [[oscar.cbls.lib.invariant.numeric.Sum]] invariant.
    *
    * @param input
    *   Array of variables to sum.
    * @param bulkIdentifier
    *   An [[oscar.cbls.core.computation.IncredibleBulk]] is used when several Invariant listen to
    *   vars. Warning: [[oscar.cbls.core.computation.IncredibleBulk]] are distinguished only by
    *   their identifier. Be sure to use the same one if you're referencing the same variables.
    * @param name
    *   optional name of the resulting variable.
    */
  def sum(input: Array[IntVariable], bulkIdentifier: String = "", name: String = "")(implicit
    m: Model
  ): IntVariable = {
    val store = m.store
    if (input.length == 0) IntConstant(m.store, 0)
    else {
      val out        = IntVariable(store, 0)
      val allIndices = SetConstant(store, input.indices.toSet)
      val optBulk = bulkIdentifier match {
        case "" => None
        case x  => Some(x)
      }
      val optName = name match {
        case "" => None
        case x  => Some(x)
      }
      Sum(store, input, allIndices, out, optBulk, optName)
      out
    }
  }

  /** Returns a variable maintaining the sum over the elements of an array such that their indices
    * belong to a given set, i.e., `Sum(input(i) | i in indices)`, where `input` is an array of
    * [[oscar.cbls.core.computation.integer.IntVariable]].
    *
    * Uses the [[oscar.cbls.lib.invariant.numeric.Sum]] invariant.
    *
    * @param input
    *   Array variables to sum.
    * @param indices
    *   Variable maintaining the set of indices of the integers to sum.
    * @param bulkIdentifier
    *   An [[oscar.cbls.core.computation.IncredibleBulk]] is used when several Invariant listen to
    *   vars. Warning: [[oscar.cbls.core.computation.IncredibleBulk]] are distinguished only by
    *   their identifier. Be sure to use the same one if you're referencing the same variables.
    * @param name
    *   Optional name of the resulting variable.
    */
  def partialSum(
    input: Array[IntVariable],
    indices: SetVariable,
    bulkIdentifier: String = "",
    name: String = ""
  )(implicit m: Model): IntVariable = {
    val store = m.store
    if (input.length == 0) IntConstant(m.store, 0)
    else {
      val output = IntVariable(store, 0)
      val optBulk = bulkIdentifier match {
        case "" => None
        case x  => Some(x)
      }
      val optName = name match {
        case "" => None
        case x  => Some(x)
      }
      Sum(store, input, indices, output, optBulk, optName)
      output
    }
  }

  /** Returns a variable maintaining the sum over the elements of an array such that their indices
    * belong to a given set, i.e., `Sum(input(i) | i in indices)`, where `input` is an array of
    * constant integers.
    *
    * Uses the [[oscar.cbls.lib.invariant.numeric.SumConst]] invariant.
    *
    * @param input
    *   Array of constant integers to sum.
    * @param indices
    *   Variable maintaining the set of indices of the integers to sum.
    * @param name
    *   Optional name of the resulting variable.
    */
  def partialSumOfConstants(input: Array[Long], indices: SetVariable, name: String = "")(implicit
    m: Model
  ): IntVariable = {
    val store = m.store
    if (input.length == 0) {
      new IntConstant(store, 0)
    } else {
      val out = IntVariable(store, 0)
      SumConst(
        store,
        input,
        indices,
        out,
        name = name match {
          case "" => None
          case x  => Some(x)
        }
      )
      out
    }
  }

  /** Returns a variable that maintains the product of all variables in the input array.
    *
    * Uses the [[oscar.cbls.lib.invariant.numeric.Prod]] invariant.
    *
    * @param input
    *   Array of variables to multiply.
    * @param bulkIdentifier
    *   An [[oscar.cbls.core.computation.IncredibleBulk]] is used when several Invariant listen to
    *   vars. Warning: [[oscar.cbls.core.computation.IncredibleBulk]] are distinguished only by
    *   their identifier. Be sure to use the same one if you're referencing the same variables.
    * @param name
    *   optional name of the resulting variable.
    */
  def prod(input: Array[IntVariable], bulkIdentifier: String = "", name: String = "")(implicit
    m: Model
  ): IntVariable = {
    val store = m.store
    if (input.length == 0) IntConstant(m.store, 0)
    else {
      val out        = IntVariable(store, 0)
      val allIndices = SetConstant(store, input.indices.toSet)
      val optBulk = bulkIdentifier match {
        case "" => None
        case x  => Some(x)
      }
      val optName = name match {
        case "" => None
        case x  => Some(x)
      }
      Prod(store, input, allIndices, out, optBulk, optName)
      out
    }
  }

  /** Returns a variable maintaining the product over the elements of an array such that their
    * indices belong to a given set, i.e., `Prod(input(i) | i in indices)`, where `input` is an
    * array of [[oscar.cbls.core.computation.integer.IntVariable]].
    *
    * Uses the [[oscar.cbls.lib.invariant.numeric.Prod]] invariant.
    *
    * @param input
    *   Array variables to sum.
    * @param indices
    *   Variable maintaining the set of indices of the integers to sum.
    * @param bulkIdentifier
    *   An [[oscar.cbls.core.computation.IncredibleBulk]] is used when several Invariant listen to
    *   vars. Warning: [[oscar.cbls.core.computation.IncredibleBulk]] are distinguished only by
    *   their identifier. Be sure to use the same one if you're referencing the same variables.
    * @param name
    *   Optional name of the resulting variable.
    */
  def partialProd(
    input: Array[IntVariable],
    indices: SetVariable,
    bulkIdentifier: String = "",
    name: String = ""
  )(implicit m: Model): IntVariable = {
    val store = m.store
    if (input.length == 0) IntConstant(m.store, 0)
    else {
      val output = IntVariable(store, 0)
      val optBulk = bulkIdentifier match {
        case "" => None
        case x  => Some(x)
      }
      val optName = name match {
        case "" => None
        case x  => Some(x)
      }
      Prod(store, input, indices, output, optBulk, optName)
      output
    }
  }

  /** Returns a variable maintaining the product over the elements of an array such that their
    * indices belong to a given set, i.e., `Sum(input(i) | i in indices)`, where `input` is an array
    * of constant integers.
    *
    * Uses the [[oscar.cbls.lib.invariant.numeric.ProdConst]] invariant.
    *
    * @param input
    *   Array of constant integers to sum.
    * @param indices
    *   Variable maintaining the set of indices of the integers to sum.
    * @param name
    *   Optional name of the resulting variable.
    */
  def partialProdOfConstants(input: Array[Long], indices: SetVariable, name: String = "")(implicit
    m: Model
  ): IntVariable = {
    val store = m.store
    if (input.length == 0) {
      new IntConstant(store, 0)
    } else {
      val out = IntVariable(store, 0)
      ProdConst(
        store,
        input,
        indices,
        out,
        name = name match {
          case "" => None
          case x  => Some(x)
        }
      )
      out
    }
  }

  /** Returns the absolute value of the input variable. */
  def abs(a: IntVariable, name: String = ""): IntVariable = {
    Abs.result(
      a,
      name match {
        case "" => None
        case x  => Some(x)
      }
    )
  }

  /** Returns the square of this variable. */
  def square(a: IntVariable, name: String = ""): IntVariable = {
    Square.result(
      a,
      name match {
        case "" => None
        case x  => Some(x)
      }
    )
  }

  /** Returns an IntVariable that is the square root of the input variable. */
  def sqrt(a: IntVariable, name: String = ""): IntVariable = {
    Sqrt.result(
      a,
      name match {
        case "" => None
        case x  => Some(x)
      }
    )
  }

  /** Returns a variable maintaining `fun(a, b)`. The used invariant is not incremental. So this
    * method should be used with very simple function.
    *
    * @param a
    *   The first parameter of the function.
    * @param b
    *   The second parameter of the function.
    * @param fun
    *   The function to maintain. It is supposed not to listen to any variable of the model.
    * @param name
    *   Optional name of the resulting variable.
    */
  def intInt2Int(a: IntVariable, b: IntVariable, fun: (Long, Long) => Long, name: String = "")(
    implicit m: Model
  ): IntVariable = {
    val store  = m.store
    val output = IntVariable(store, 0)
    new IntInt2Int(
      store,
      a,
      b,
      output,
      fun,
      name match {
        case "" => None
        case x  => Some(x)
      }
    )
    output
  }

  /** Returns a variable maintaining `fun(input)`. The used invariant is not incremental. So this
    * method should be used with very simple function.
    *
    * @param input
    *   The listened IntVariable.
    * @param fun
    *   The function to maintain. It is supposed not to listen to any variable in the model.
    * @param cached
    *   Set to true to have a cache of size 1. Set to false to have no cache. A cache can provide
    *   speedup if fun is time-consuming.
    * @param name
    *   Optional name of the resulting variable.
    */
  def int2Int(input: IntVariable, fun: Long => Long, cached: Boolean = false, name: String = "")(
    implicit m: Model
  ): IntVariable = {
    val store  = m.store
    val output = IntVariable(store, 0)
    new Int2Int(
      store,
      input,
      output,
      fun,
      cached,
      name match {
        case "" => None
        case x  => Some(x)
      }
    )
    output
  }

}
