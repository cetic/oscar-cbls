package oscar.cbls.modeling.invariant

import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.lib.invariant.minmax._
import oscar.cbls.modeling.Model

trait MinMax {

  /** Returns a variable representing the minimum value in a subset of a given array of constant
    * integers, i.e., `Min{input(i) | i in indices}`.
    *
    * @param input
    *   array of constants on which to compute the minimum
    * @param indices
    *   variable maintaining the indices of the input array to take into account to calculate the
    *   minimum
    * @param name
    *   optional name of the resulting variable
    */
  def min(input: Array[Long], indices: SetVariable, name: String = "")(implicit
    m: Model
  ): IntVariable = {
    require(
      indices.value().isEmpty || (indices.value().min >= 0 && indices.value().max < input.length),
      "Elements of the indices SetVariable are out of bounds for the input array"
    )
    val store = m.store
    val out   = IntVariable(model = store, initialValue = 0)
    MinConst(
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
