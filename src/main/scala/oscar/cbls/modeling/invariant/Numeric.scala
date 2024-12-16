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
    *   array of variables to sum
    * @param name
    *   optional name of the resulting variable
    */
  def sum(input: Array[IntVariable], name: String = "")(implicit m: Model): IntVariable = {
    val store = m.store
    if (input.length == 0) new IntConstant(m.store, 0)
    else {
      val out        = IntVariable(store, 0)
      val allIndices = SetConstant(store, input.indices.toSet)
      Sum(
        store,
        input,
        allIndices,
        out,
        name = name match {
          case "" => None
          case x  => Some(x)
        }
      )
      out
    }
  }

  /** Returns a variable maintaining the sum over the elements of an array such that their indices
    * belong to a given set, i.e., `Sum(input(i) | i in indices)`, where `input` is an array of
    * constant integers.
    *
    * Uses the [[oscar.cbls.lib.invariant.numeric.SumConst]] invariant.
    *
    * @param input
    *   array of constant integers to sum
    * @param indices
    *   variable maintaining the set of indices of the integers to sum
    * @param name
    *   optional name of the resulting variable
    */
  def partialSum(input: Array[Long], indices: SetVariable, name: String = "")(implicit
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

}
