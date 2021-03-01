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
package oscar.cp.test

import oscar.cp._
import oscar.cp.constraints.{AtLeastNValueAC, AtMostNValue}
import oscar.cp.testUtils._

class TestAtLeastNValue extends TestSuite {
  
  val rand = new scala.util.Random(0)

  // test with minVal != 0
  def randomDom: Set[Int] = Array.fill(10)(3+rand.nextInt(8)).toSet
  
  def atMostNValueDecomp(cp: CPSolver, X: Array[CPIntVar], N: CPIntVar) = {
    val minVal = X.map(_.min).min
    val maxVal = X.map(_.max).max

    val present: Array[CPBoolVar] = (minVal to maxVal).toArray.map(v => isOr(X.map(_ ?=== v)))

    cp.add(sum(present) <= N)
  }



  def nbSol(domx: Array[Set[Int]],Nmin: Int, Nmax: Int, decomp: Boolean = false): Int = {
	  implicit val cp = CPSolver()
	  val N = CPIntVar(Nmin to Nmax)
	  val X = Array.tabulate(domx.size)(i => CPIntVar(domx(i)))
	  if (decomp) atMostNValueDecomp(cp,X,N)
	  else cp.add(new AtMostNValue(X,N))
	  search(binaryStatic(X :+ N))
	  val stat = start()
    stat.nSols
  }


  test("atAtLeast1") {
    val n = 4
    implicit val cp = CPSolver()
    val Xs = Array.fill(n)(CPIntVar.sparse(0, 3))
    val y = CPIntVar.sparse(2,2)

    add(atLeastNValue(Xs,y),Strong)

    search(binaryFirstFail(Xs))
    onSolution {
      val values = Array.fill(5)(0)
      Xs.foreach(x => values(x.min) = 1)
      assert(values.sum >= y.min)
    }
    val stats = start()
    assert(stats.nSols == 84)
  }

  test("atAtLeast2") {
    val n = 4
    implicit val cp = CPSolver()
    val Xs = Array.fill(n)(CPIntVar.sparse(0, 3))
    val y = CPIntVar.sparse(2,5)

    add(Xs(0) === 0)
    add(Xs(1) === 0)
    add(Xs(2) === 0)
    add(atLeastNValue(Xs,y),Strong)

    assert(!Xs(3).hasValue(0))
    assert(Xs(3).size == 3)
    assert(y.max == 2)

  }

  test("atAtLeast3") {
    val n = 4
    implicit val cp = CPSolver()
    val Xs = Array(CPIntVar(Set(-5,5)),CPIntVar(Set(-5,5)),CPIntVar(Set(-5,5)),CPIntVar(Set(-9,-4,-5,0,4,6,10)))
    val y = CPIntVar.sparse(3,5)


    add(atLeastNValue(Xs,y),Strong)



    assert(!Xs(3).hasValue(-5))
    assert(Xs(3).size == 6)
    assert(y.max == 3)

  }

  test("atAtLeast4") {
    val n = 4
    implicit val cp = CPSolver()

    val Xs = Array.fill(n)(CPIntVar.sparse(1, 4))
    val y = CPIntVar.sparse(1,2) // Expanded domain here

    add(Xs(0) === 1)
    add(Xs(1) === 1)
    add(Xs(2) === 2)
    add(new AtLeastNValueAC(Xs,y),Weak)
    println("y:"+y)
    search(binaryFirstFail(Array(y) ++ Xs))
    onSolution{
      println("Xs: " + Xs.map(_.value).mkString("[",", ","]"))
      println("y: " + y.min + ".." + y.max)
      println("———")
    }
    val stats = start()
    println(stats)

  }



}
