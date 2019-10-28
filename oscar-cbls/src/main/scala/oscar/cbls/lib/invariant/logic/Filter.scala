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

import oscar.cbls._
import oscar.cbls.core._
import oscar.cbls.core.computation.IntValue

import scala.collection.immutable.SortedSet

/** { i in index(values) | cond(values[i] }
 * @param values is an array of IntVar
 * @param cond is a function that selects values to be includes in the output set.
  *             This ''cond'' function cannot depend on any IntVar,
  *             as updates to these IntVars will not trigger propagation of this invariant.
  *             By default, cond is "_ > 0L"
  * @author renaud.delandtsheer@cetic.be
  * */
case class Filter(values:Array[IntValue], cond:(Long=>Boolean)=_>0L)
  extends SetInvariant(values.indices.foldLeft(SortedSet.empty[Long])((acc:SortedSet[Long],indice:Int) => if(cond(values(indice).value)){acc+indice}else acc),
    0 until values.length)
  with IntNotificationTarget{

  for (v <- values.indices) registerStaticAndDynamicDependency(values(v),v)
  finishInitialization()

  @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, OldVal: Long, NewVal: Long) {
    val OldCond = cond(OldVal)
    val NewCond = cond(NewVal)
    if(OldCond  && !NewCond) this.deleteValue(index)
    else if(NewCond && !OldCond) this.insertValue(index)
  }

  override def checkInternals(c:Checker){
    for(i <- values.indices){
      c.check(!cond(values(i).value) || this.value.contains(i),
          Some("!cond(values(i).value) || this.value.contains(i)"))
      c.check(cond(values(i).value) || !this.value.contains(i),
          Some("cond(values(i).value) || !this.value.contains(i)"))
    }
  }
}
