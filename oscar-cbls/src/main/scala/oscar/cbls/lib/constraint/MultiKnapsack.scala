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
/******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/
package oscar.cbls.lib.constraint

import oscar.cbls.core.computation.{IntValue, SetValue, Value}
import oscar.cbls.core.constraint.Constraint
import oscar.cbls.core.propagation.Checker
import oscar.cbls.lib.invariant.logic.{Cluster, IntElement}
import oscar.cbls.lib.invariant.numeric.Sum

import scala.collection.immutable.SortedMap
;

/**This is the standard bin packing constraint
 *
 * @param items the items, designing the bins they are placed into
 * @param itemsizes the size of the items
 * @param binsizes the max size of the available bins
  * @author renaud.delandtsheer@cetic.be
 */
case class MultiKnapsack(items: Array[IntValue], itemsizes: Array[IntValue], binsizes:Array[IntValue])
  extends Constraint {

  assert(items.map(_.min).min == 0L, "bin 0L must be included in possible bins of items")
  assert(items.map(_.min).max <= binsizes.length-1L, "the range of item bins should be not bigger than the available bins")
  assert(items.length == itemsizes.length)

  registerConstrainedVariables(items)
  registerConstrainedVariables(itemsizes)
  registerConstrainedVariables(binsizes)

  private val bincontents:Array[SetValue] = Cluster.makeDense(items).clusters
  private val binfilling:Array[IntValue] = bincontents.map(bincontent => Sum(itemsizes,bincontent))

  private val binviolations:Array[IntValue] = (
    for (binid <- binsizes.indices)
    yield (binfilling(binid) le binsizes(binid)).violation).toArray

  private val itemviolations = items.map(itemval => binviolations.element(itemval))

  /**The violation of the constraint is the sum on all excess in all bins.
    */
  override val violation = Sum(binviolations)

  val Violations:SortedMap[IntValue,IntValue] = {
    var acc = SortedMap.empty[IntValue,IntValue]
    for(itemid <- items.indices){
      acc += ((items(itemid),itemviolations(itemid)))
      acc += ((itemsizes(itemid),itemviolations(itemid)))
    }
    for(binid <- binsizes.indices){
      acc += ((binsizes(binid),binviolations(binid)))
    }
    acc
  }

  /**The violation of an item is the excess of the bin it is located into,
   * The violation of a bin is the excess in the bin
   */
  override def violation(v: Value): IntValue = {
    val tmp = Violations.getOrElse(v.asInstanceOf[IntValue],null)
    assert(tmp != null)
    tmp
  }

  def violationOfBin(binNumber:Int) = binviolations(binNumber)
  def itemsInBin(binNumber:Int) = bincontents(binNumber)
  def fillingOfBin(binNumber:Int) = binfilling(binNumber)

  /** To override whenever possible to spot errors in invariants.
    * this will be called for each invariant after propagation is performed.
    * It requires that the Model is instantiated with the variable debug set to true.
    */
  override def checkInternals(c: Checker): Unit = {
    c.check(true,Some("nothing to check, invariant is discharged"))
  }
}

/**This is the bin_packing_load constraint where the bin load
  * is equal to the size of the items placed in the bin,
  * as opposed to bin size being greater than or equal.
  *
  * @param items the items, designing the bins they are placed into
  * @param itemsizes the size of the items
  * @param binload the total size of items placed in a bin
  * @author renaud.delandtsheer@cetic.be
  * @author gustav.bjordal@it.uu.se
  */
case class MultiKnapsackLoad(items: Array[IntValue], itemsizes: Array[IntValue], binload:Array[IntValue])
  extends Constraint {

  assert(items.map(_.min).min == 0L, "bin 0L must be included in possible bins of items")
  assert(items.map(_.min).max <= binload.length-1L, "the range of item bins should be not bigger than the available bins")
  assert(items.length == itemsizes.length)

  registerConstrainedVariables(items)
  registerConstrainedVariables(itemsizes)
  registerConstrainedVariables(binload)

  private val bincontents = Cluster.makeDense(items).clusters
  private val binfilling = bincontents.map(bincontent => Sum(itemsizes,bincontent))

  private val binviolations:Array[IntValue] = (
    for (binid <- binload.indices)
      yield (binfilling(binid) === binload(binid)).violation).toArray

  private val itemviolations = items.map(itemval =>  IntElement(itemval,binviolations))

  /**The violation of the constraint is the sum on all excess or shortage in all bins.
    */
  override val violation = Sum(binviolations)

  val Violations:SortedMap[IntValue,IntValue] = {
    var acc = SortedMap.empty[IntValue,IntValue]
    for(itemid <- items.indices){
      acc += ((items(itemid),itemviolations(itemid)))
      acc += ((itemsizes(itemid),itemviolations(itemid)))
    }
    for(binid <- binload.indices){
      acc += ((binload(binid),binviolations(binid)))
    }
    acc
  }

  /**The violation of an item is the excess or shortage of the bin it is located into,
    * The violation of a bin is the excess or shortage in the bin
    */
  override def violation(v: Value): IntValue = {
    val tmp = Violations.getOrElse(v.asInstanceOf[IntValue],null)
    assert(tmp != null)
    tmp
  }

  def violationOfBin(binNumber:Int) = binviolations(binNumber)
  def itemsInBin(binNumber:Int) = bincontents(binNumber)
  def fillingOfBin(binNumber:Int) = binfilling(binNumber)

  /** To override whenever possible to spot errors in invariants.
    * this will be called for each invariant after propagation is performed.
    * It requires that the Model is instantiated with the variable debug set to true.
    */
  override def checkInternals(c: Checker): Unit = {
    c.check(true,Some("nothing to check, invariant is discharged"))
  }
}
