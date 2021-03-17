/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/

package oscar.modeling.models

import oscar.modeling.algebra.Expression
import oscar.modeling.constraints.Constraint
import oscar.modeling.models.operators.ModelOperator

/**
  * Created by dervalguillaume on 17/11/16.
  */
trait ModelDeclarationInterface {

  /**
    * Get the current model
    */
  def getCurrentModel: Model

  /**
   * Set the current model
   */
  def setCurrentModel(model: Model): Unit

  /**
    * Apply the function func, which uses Var declared in this ModelDeclaration,
    * on the model (inheriting for this object too), temporarily changing the current model.
    *
    * @param model : model on which to apply the function
    * @param func  : function to apply
    */
  def apply[RetVal](model: Model)(func: => RetVal): RetVal

  /**
    * Post a new constraint
    *
    * @param constraint the constraint to post
    */
  def post(constraint: Constraint): Unit
  def post(constraints: Seq[Constraint]): Unit = constraints.foreach(post)
  /**
    * Add a new constraint to the model
    *
    * @param constraint the constraint to add
    */
  def add(constraint: Constraint): Unit
  def add(constraints: Seq[Constraint]): Unit = constraints.foreach(add)

  /**
    * Minimize on variable v
    *
    * @param v variable to minimize
    */
  def minimize(v: Expression): Unit

  /**
    * Maximize on variable v
    *
    * @param v variable to maximize
    */
  def maximize(v: Expression): Unit

  /**
    * Remove the optimisation method
    */
  def removeOptimization(): Unit

  /**
    * Apply a model operator
    *
    * @param operator operator to apply
    */
  def apply[OutputType <: Model](operator: ModelOperator[OutputType]): Unit

  /**
   * Fork (in the model tree). All the actions made on the model in a fork{} call will be reverted after the call.
   */
  def fork[T](func: => T): T
}
