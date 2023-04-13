package oscar.cbls.test.search

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

import oscar.cbls.algo.search.InstrumentedRange

import scala.collection.immutable.SortedSet
import scala.collection.immutable.NumericRange

/**
 * Created by rdl on 16/07/2014.
 */
object HotRestart extends App{

  println(new InstrumentedRange(NumericRange.inclusive[Int](0,9,1)) startBy 5)

  val s:SortedSet[Int] = SortedSet(1, 2, 3, 4, 7, 8, 9)
  val it = s.iteratorFrom(7)
  while(it.hasNext) println("next:" + it.next())


  println(oscar.cbls.algo.search.HotRestart(s,0))

}
