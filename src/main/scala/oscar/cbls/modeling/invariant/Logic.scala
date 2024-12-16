package oscar.cbls.modeling.invariant

import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.core.computation.set.{SetConstant, SetVariable}
import oscar.cbls.lib.invariant.logic._
import oscar.cbls.modeling.Model

trait Logic {

  /** Returns a variable representing the subset of the indices of the input variable array that
    * satisfy the given predicate, i.e., `{i in input.indices | predicate(input(i))}`. The default
    * predicate is whether the given variable is strictly larger than zero.
    *
    * Uses the [[oscar.cbls.lib.invariant.logic.Filter]] invariant.
    *
    * @param input
    *   an array of integer variables
    * @param predicate
    *   the predicate that elements in the output variables must satisfy
    * @param name
    *   optional name of the returned variable
    */
  def filter(input: Array[IntVariable], predicate: Long => Boolean = _ > 0, name: String = "")(
    implicit m: Model
  ): SetVariable = {
    val store = m.store
    if (input.length == 0) {
      new SetConstant(store, Set.empty)
    } else {
      val out = SetVariable(store, Set.empty)
      Filter(
        store,
        input,
        out,
        predicate,
        name = name match {
          case "" => None
          case x  => Some(x)
        }
      )
      out
    }
  }

}
