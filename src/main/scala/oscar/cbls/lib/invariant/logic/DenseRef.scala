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
/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/
package oscar.cbls.lib.invariant.logic

import oscar.cbls.core.computation.{CBLSSetVar, ChangingSetValue, Invariant, InvariantHelper, SetNotificationTarget, SetValue, Store}
import oscar.cbls.core.propagation.Checker

import scala.collection.immutable.SortedSet

/**maintains the reverse references. Referencing(i) = {j | Reference(j) includes i}
  * @author renaud.delandtsheer@cetic.be
  * */
case class DenseRef(references:Array[SetValue], referencing:Array[CBLSSetVar])
  extends Invariant
  with SetNotificationTarget{

  for (v <- references.indices) registerStaticAndDynamicDependency(references(v),v)

  finishInitialization()

  for(c <- referencing){c.setDefiningInvariant(this); c.setValue(SortedSet.empty)}

  for(v <- references.indices){
    for (r <- references(v).value){
      referencing(r).insertValue(v)
    }
  }

  override def notifySetChanges(v: ChangingSetValue, id: Int, addedValues: Iterable[Int], removedValues: Iterable[Int], oldValue: SortedSet[Int], newValue: SortedSet[Int]): Unit = {
    for (added <- addedValues) referencing(added).insertValue(id)
    for (deleted <- removedValues)  referencing(deleted).deleteValue(id)
  }

  /** To override whenever possible to spot errors in invariants.
    * this will be called for each invariant after propagation is performed.
    * It requires that the Model is instantiated with the variable debug set to true.
    */
  override def checkInternals(c: Checker): Unit = {
  //Referencing(i) = {j | Reference(j) includes i}
    for (referencesId <- references.indices){
      for (referencingId <- referencing.indices){
        if (references(referencesId).value.contains(referencingId))
          c.check(referencing(referencingId).value.contains(referencesId))
        else c.check(!referencing(referencingId).value.contains(referencesId))
      }
    }
  }
}

object DenseRef{
  def makeDenseRef(references:Array[SetValue]): DenseRef = {
    val (minMin,maxMax) = InvariantHelper.getMinMaxBoundsSet(references)
    val m:Store = InvariantHelper.findModel(references)
    assert(minMin == 0L)
    val referencing = Array.tabulate(maxMax + 1)(i => new CBLSSetVar(m,SortedSet.empty, references.indices, "referencing_" + i))
    new DenseRef(references,referencing)
  }
}
