///** ***************************************************************************** OscaR is free
//  * software: you can redistribute it and/or modify it under the terms of the GNU Lesser General
//  * Public License as published by the Free Software Foundation, either version 2.1 of the License,
//  * or (at your option) any later version.
//  *
//  * OscaR is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
//  * the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
//  * General Public License for more details.
//  *
//  * You should have received a copy of the GNU Lesser General Public License along with OscaR. If
//  * not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
//  */
///** ***************************************************************************** Contributors: This
//  * code has been initially developed by CETIC www.cetic.be by Renaud De Landtsheer
//  */
//package oscar.cbls.core.computation.set
//
//import oscar.cbls.core.computation.Invariant
//
//import scala.collection.immutable.SortedSet
//
//object IdentitySet {
//  def apply(toValue: CBLSSetVar, fromValue: SetValue): Unit = {
//    fromValue match {
//      case c: CBLSSetConst     => toValue := c.value
//      case c: ChangingSetValue => new IdentitySet(toValue, c)
//    }
//  }
//}
//
///** an invariant that is the identity function
//  * @author
//  *   renaud.delandtsheer@cetic.be
//  */
//class IdentitySet(toValue: CBLSSetVar, fromValue: ChangingSetValue)
//    extends Invariant
//    with SetNotificationTarget {
//
//  registerStaticAndDynamicDependency(fromValue)
//  toValue.setDefiningInvariant(this)
//  finishInitialization()
//
//  toValue := fromValue.value
//
//  override def notifySetChanges(
//    v: ChangingSetValue,
//    id: Int,
//    addedValues: Iterable[Int],
//    removedValues: Iterable[Int],
//    oldValue: SortedSet[Int],
//    newValue: SortedSet[Int]
//  ): Unit = {
//    assert(v == this.fromValue)
//    for (added   <- addedValues) toValue.insertValueNotPreviouslyIn(added)
//    for (deleted <- removedValues) toValue.deleteValuePreviouslyIn(deleted)
//  }
//
//  override def checkInternals(c: Checker): Unit = {
//    c.check(toValue.value equals fromValue.value)
//  }
//}
