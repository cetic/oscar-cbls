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
import org.scalatest.Matchers
import oscar.algo.search._
import oscar.algo.reversible._
import scala.jdk.CollectionConverters._

class ReversibleIntTest extends FunSuite {

  test("test reversibility") {
    val r = new ReversibleContextImpl()
    val a = new ReversibleInt(r, 0)
    val b = new ReversibleInt(r, 0)

    assert(a.getValue() == 0)
    assert(a.getValue() == 0);
    assert(b.getValue() == 0);

    //a = null, b = null
    r.pushState();

    a.setValue(1);
    a.setValue(2);
    b.setValue(3);
    b.setValue(2);

    //a = 2, b = 2
    r.pushState();

    a.setValue(4);
    b.setValue(6);
    a.setValue(1);
    b.setValue(1);

    //a = 1, b = 1
    r.pushState();

    a.setValue(9);
    b.setValue(8);
    a.setValue(2);
    b.setValue(6);

    r.pop();
    assert(a.getValue() == 1);
    assert(b.getValue() == 1);

    r.pop();
    assert(a.getValue() == 2);
    assert(b.getValue() == 2);

    r.pop();
    assert(a.getValue() == 0);
    assert(b.getValue() == 0);

  }
  
  test("test setter/getter and assignment operator") {
    val r = new ReversibleContextImpl()
    val a = new ReversibleInt(r, 0)
    val b = new ReversibleInt(r, 0)

    assert(a.value == 0);
    assert(b.value == 0);

    //a = null, b = null
    r.pushState();

    a.value = 1
    a.value = 2
    b := 3
    b := 2

    //a = 2, b = 2
    r.pushState();

    a := 4
    b := 6
    a := 1
    b := 1

    //a = 1, b = 1
    r.pushState();

    a := 9
    b :=8
    a := 2
    b := 6

    r.pop();
    assert(a.getValue() == 1);
    assert(b.getValue() == 1);

    r.pop();
    assert(a.getValue() == 2);
    assert(b.getValue() == 2);

    r.pop();
    assert(a.getValue() == 0);
    assert(b.getValue() == 0);

  }  
  
  test("test implicit") {
    val r = new ReversibleContextImpl()
    val a = new ReversibleInt(r, 0)
    val b = new ReversibleInt(r, 0)

    a += 1
    b += 2
    assert(a.value == 1)
    assert(b.value == 2)
    assert((1+b) == 3)
    assert((1+b) == 3)
    assert((b-1) == 1)
  }
}

