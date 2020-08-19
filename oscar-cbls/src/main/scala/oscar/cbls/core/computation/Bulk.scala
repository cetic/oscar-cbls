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
  *            Yoann Guyot
  ******************************************************************************/
package oscar.cbls.core.computation

import oscar.cbls.core.propagation.{BulkPropagator, Checker}

import scala.collection.immutable.SortedMap

//TODO: we cannot have constants in bulked arrays, actually.
/**Invariants over arrays can implement this trait to make it possible to bulk load their dependencies
  *
  * @author renaud.delandtsheer@cetic.be
  * */
trait Bulked[VarType <: Value, BulkedComputationResult] extends Invariant {

  /**
   * registers a static dependency to all variables mentioned in the bulkedVars.
   * @param bulkedVars: an iterable of variables to bulk together
   * @param id a reference name to identify to which bulk in the invariant this belongs to. Several bulks can be done with a single invariants
   * @return the result of performBulkComputation(bulkedVars),  possibly computed by co-bulking invariants
   */
  final def bulkRegister(bulkedVars: Array[VarType], id: Long = 0L): BulkedComputationResult = {

    val m = this.preFinishInitialization()
    if (m == null) {
      //no bulking possible
      this.registerStaticDependencies(bulkedVars:_*)
      performBulkComputationID(bulkedVars, id)
    } else {
      //check for existing bulk
      val identifyingString = s"${this.getClass.getName}/$id"

      val incredibleBulk = m.getBulk(identifyingString, bulkedVars.asInstanceOf[Array[Value]])

      if (incredibleBulk == null) {
        //create a new bulk
        val bcr = performBulkComputation(bulkedVars)
        val newBulk = new Bulk(m, bulkedVars.asInstanceOf[Array[Value]], bcr)
        this.registerStaticallyListenedElement(newBulk)
        m.registerBulk(identifyingString, newBulk)
        bcr
      } else {
        //we got it
        this.registerStaticallyListenedElement(incredibleBulk)

        incredibleBulk.bulkedComputationResult.asInstanceOf[BulkedComputationResult]
      }
    }
  }

  def performBulkComputationID(vars: Array[VarType], id: Long): BulkedComputationResult = performBulkComputation(vars)
  def performBulkComputation(vars: Array[VarType]): BulkedComputationResult = null.asInstanceOf[BulkedComputationResult]
}

/**
 * This is the node that is put in the propagation graph
 * used by BulkLoad only
 * @author renaud.delandtsheer@cetic.be
 */
class Bulk(m: Store, val bulkedVars: Array[Value], val bulkedComputationResult: Any)
  extends Invariant with BulkPropagator {

  for (dd <- bulkedVars) registerStaticallyListenedElement(dd)
  finishInitialization(m)

  override def checkInternals(c: Checker): Unit = c.check(true)
}

/**This is the dictionaries where bulks are stored, and can be searched for
  * @author renaud.delandtsheer@cetic.be
  */
trait Bulker {

  private var Bulked: SortedMap[String, List[Bulk]] = SortedMap.empty

  def getBulk(identifyingName: String, bulkedVars: Array[Value]): Bulk = {
    val bulks = Bulked.getOrElse(identifyingName, null)

    if (bulks == null) return null

    for (b <- bulks) {
      if (bulkedVars == b.bulkedVars) {
        return b
      }
    }
    null
  }

  def registerBulk(identifyingName: String, bulk: Bulk): Unit ={
    val knownbulk = Bulked.getOrElse(identifyingName, List.empty)
    Bulked += ((identifyingName, bulk :: knownbulk))
  }

  protected def killBulker(): Unit ={
    Bulked = null
  }
}
