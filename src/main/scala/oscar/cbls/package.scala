package oscar

import oscar.cbls.core.computation.integer.IntConstant
import oscar.cbls.modeling.Model
import oscar.cbls.modeling.invariant.Predefined

import scala.language.implicitConversions

package object cbls extends Predefined {

  type Model        = oscar.cbls.modeling.Model
  type VRS          = oscar.cbls.modeling.routing.VRS
  type IntVariable  = oscar.cbls.core.computation.integer.IntVariable
  type SetVariable  = oscar.cbls.core.computation.set.SetVariable
  type SeqVariable  = oscar.cbls.core.computation.seq.SeqVariable
  type Objective    = oscar.cbls.core.computation.objective.Objective
  type Neighborhood = oscar.cbls.core.search.Neighborhood

  /** Implicit conversion of a constant integer value to an associated integer variable in a
    * [[oscar.cbls.modeling.Model]].
    *
    * @param x
    *   the constant integer
    * @param model
    *   the model to which the constant is added
    */
  implicit def int2IntConst(x: Int)(implicit model: Model): IntVariable =
    new IntConstant(model.store, value = x)

  /** Implicit conversion of a constant Long value to an associated integer variable in a
    * [[oscar.cbls.modeling.Model]].
    *
    * @param x
    *   The constant integer.
    * @param model
    *   The model to which the constant is added.
    */
  implicit def long2IntConst(x: Long)(implicit model: Model): IntVariable =
    new IntConstant(model.store, value = x)

  /** Shortcut for adding an integer variable to the implicitly defined [[Model]].
    *
    * @param initialValue
    *   initial value of the variable
    * @param min
    *   minimum value that the variable can assume
    * @param max
    *   maximum value that the variable can assume
    * @param name
    *   optional name of the variable
    * @param model
    *   the implicitly defined model
    */
  implicit def intVar(initialValue: Long, min: Long, max: Long, name: String = "")(implicit
    model: Model
  ): IntVariable = {
    model.intVar(initialValue: Long, min, max, name)
  }

  /** Creates an instance of a [[oscar.cbls.modeling.Model]].
    *
    * @param name
    *   the name of the model
    * @param debugLevel
    *   debug level of associated [[oscar.cbls.core.computation.Store]]
    */
  def model(name: String, debugLevel: Int = 0): Model = Model(name, debugLevel)

}
