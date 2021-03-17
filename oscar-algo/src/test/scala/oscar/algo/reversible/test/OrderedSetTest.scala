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


import org.scalatest.funsuite.AnyFunSuite
import oscar.algo.reversible._
import scala.jdk.CollectionConverters._

import org.scalatest.matchers.should.Matchers


class OrderedSetTest extends AnyFunSuite with Matchers {
    test("test 1") {

    	val myset = new OrderedSet(3,10)
    	myset.asScala.toSet should equal((3 to 10).toSet)
    	
    	myset.removeValue(3)
    	myset.asScala.toSet should equal((4 to 10).toSet)
    	myset.hasValue(3) should equal(false)
    	
    	myset.removeValue(10)
    	myset.asScala.toSet should equal((4 to 9).toSet)
    	myset.hasValue(10) should equal(false)
    	myset.hasValue(9) should equal(true)
    	
    	myset.removeValue(5)
    	myset.asScala.toSet should equal((4 to 9).toSet - 5)
    	myset.hasValue(5) should equal(false)
    	
    	myset.removeValue(6)
    	myset.removeValue(7)
    	myset.removeValue(8)
    	myset.removeValue(9)
    	
    	myset.asScala.toSet should equal(Set(4))
    	myset.getSize should equal(1)
    	

    	myset.hasValue(4) should equal(true)
    	myset.removeValue(5)
    	myset.removeValue(4)
    	
    	myset.asScala.toSet should equal(Set())
    	myset.getSize should equal(0)
    	myset.hasValue(4) should equal(false)


    }

}

