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
package oscar.algo.reversible.test


import org.scalatest.FunSuite
import oscar.algo.search._
import oscar.algo.reversible._
import scala.jdk.CollectionConverters._

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class ReversibleSetTest extends FunSuite {

  test("test reversible set 1") {

    val rc = new ReversibleContextImpl()

    val s = new ReversibleSet(rc)
    s.add(5)
    s.add(6)
    s.add(7)
    // {5,6,7}
    assert(s.toSet == Set(5, 6, 7))
    rc.pushState()
    s.add(8)
    s.remove(5)
    s.remove(5)
    s.add(8)
    s.add(9)
    s.add(10)
    s.remove(9)
    s.remove(9)
    // {6,7,8,10}
    assert(s.toSet == Set(6, 7, 8, 10))
    rc.pop()
    // {5,6,7}
    assert(s.toSet == Set(5, 6, 7))

  }
  

}

