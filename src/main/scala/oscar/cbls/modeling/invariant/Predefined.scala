package oscar.cbls.modeling.invariant

import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.modeling.Model

/** This trait collects basic invariants to allow invocation of simple expressions without using
  * method-style calls.
  */
trait Predefined {

  private object n extends Numeric

  /** Returns a variable that maintains the sum of all variables in the input array.
    *
    * Uses the [[oscar.cbls.lib.invariant.numeric.Sum]] invariant.
    *
    * @param input
    *   array of variables to sum
    * @param name
    *   optional name of the resulting variable
    */
  def sum(input: Array[IntVariable], name: String = "")(implicit m: Model): IntVariable =
    n.sum(input, name)(m)

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
  ): IntVariable =
    n.partialSum(input, indices, name)

}
