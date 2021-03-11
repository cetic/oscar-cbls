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
package oscar.dfo.singleobjective.test

import org.scalatest.FunSuite
import org.scalatest.Matchers
import oscar.dfo.singleobjective.algos._
import oscar.util.Interval

class SingleObjectiveDFO extends FunSuite with Matchers {
  
  //Get a String representation of an array of double
  def pointToStr(point: Array[Double]) : String = {
    var res = "("
    for(i <- 0 to point.length - 2)
      res += s"${point(i)}, "
    res += s"${point(point.length - 1)})"
    res
  }
  
  val domain2D = Array.tabulate(2)(i => Interval(-100.0, 100.0))
  val domain10D = Array.tabulate(10)(i => Interval(-100.0, 100.0))
  val bases2D = Array.tabulate(2)(i => Array.tabulate(2)(j => if(i==j) 1.0 else 0.0))
  val bases10D = Array.tabulate(10)(i => Array.tabulate(10)(j => if(i==j) 1.0 else 0.0))
  
  val startPoint2D = Array.fill(2){42.0}
  val startPointRand2D = Array.fill(2){scala.util.Random.nextDouble}
  val startPoint10D = Array.fill(10){42.0}
  val startPointRand10D = Array.fill(10){scala.util.Random.nextDouble}
  
  def singleObjCompare(a1: Array[Double], a2:Array[Double]): Boolean = {
    a1(0) < a2(0)
  }
  
  //f(X) = Sum(1, N) x(i)^2
  def f1(x: Array[Double]): Array[Double] = {
    var eval = 0.0
    x.foreach(element=> eval += element*element)
    Array(eval)
  }
  
  //f(X) = Sum(1, N-1)(1-x_(i))^2 + 100(x_(i+1) - x_(i)^2)^2
  def f2(x: Array[Double]): Array[Double] = {
    var eval = 0.0
    for (i <- 0 to x.length - 2)
      eval += (1 - x(i)) * (1 - x(i)) + 100 * (x(i+1) - x(i) * x(i)) * (x(i+1) - x(i) * x(i))
    Array(eval)
  }
  
  //f(x,y) = (x^2 + y - 11)^2 + (x + y^2 -7)^2
  def f3(x: Array[Double]): Array[Double] = {
    Array((x(0)*x(0) + x(1) - 11)*(x(0)*x(0) + x(1) - 11) + (x(0) + x(1)*x(1) -7)*(x(0) + x(1)*x(1) -7))
  }
  
  //potential energy of a two-link truss function
  def f4(x: Array[Double]): Array[Double] = {
    val F1 = 0.0
    val F2 = -3.0/10.0
    val a = scala.math.sqrt(x(0) * x(0) + x(1)* x(1)) / scala.math.sqrt(2)
    val wa = scala.math.pow(a, 4.0) / 24.0 + 1.0 / (12.0 * a * a) - 1.0 / 8.0
    val b = scala.math.sqrt((x(0) - 1.0)*(x(0) - 1.0) + x(1) * x(1))
    val wb = scala.math.pow(b, 4.0) / 24.0 + 1.0 / (12.0 * b * b) - 1.0 / 8.0
    Array(scala.math.sqrt(2) * wa + wb - F1*(x(0) - 1.0) - F2 * (x(1) - 1.0))
  }
  
  test("optimize => Dummy 2D function") {
    println("=" * 80)
    println("=" * 24 + "  optimize => Dummy 2D function  " + "=" * 23)
    println("=" * 80)
    val dds = DDS(f1, 1, singleObjCompare, startPoint2D, domain2D, bases2D)
    val ddsAns = dds.optimize(math.pow(10,-2), 10000, 100)
    println("DDS: optimize(Dummy 2D): " + ddsAns._2(0) + " from point " + pointToStr(ddsAns._1) + "\nNumber of evaluations: " + dds.evalCount / 2)
    println("-" * 80)
    val mds = MDS(f1, 1, singleObjCompare, startPoint2D, domain2D)
    val mdsAns = mds.optimize(math.pow(10,-2), 10000, 100)
    println("MDS: optimize(Dummy 2D): " + mdsAns._2(0) + " from point " + pointToStr(mdsAns._1) + "\nNumber of evaluations: " + mds.evalCount / 2)
    println("-" * 80)
    val nm = NelderMead(f1, 1, singleObjCompare, startPoint2D, domain2D)
    val nmAns = nm.optimize(math.pow(10,-2), 10000, 100)
    println("Nelder-Mead: optimize(Dummy 2D): " + nmAns._2(0) + " from point " + pointToStr(nmAns._1) + "\nNumber of evaluations: " + nm.evalCount / 2)
  }
  
  test("optimize => Rosenbrock 2D function") {
    println("=" * 80)
    println("=" * 21 + "  optimize => Rosenbrock 2D function  " + "=" * 21)
    println("=" * 80)
    val dds = DDS(f2, 1, singleObjCompare, startPoint2D, domain2D, bases2D)
    val ddsAns = dds.optimize(math.pow(10,-2), 10000, 100)
    println("DDS: optimize(Rosenbrock 2D): " + ddsAns._2(0) + " from point " + pointToStr(ddsAns._1) + "\nNumber of evaluations: " + dds.evalCount / 2)
    println("-" * 80)
    val mds = MDS(f2, 1, singleObjCompare, startPoint2D, domain2D)
    val mdsAns = mds.optimize(math.pow(10,-2), 10000, 100)
    println("MDS: optimize(Rosenbrock 2D): " + mdsAns._2(0) + " from point " + pointToStr(mdsAns._1) + "\nNumber of evaluations: " + mds.evalCount / 2)
    println("-" * 80)
    val nm = NelderMead(f2, 1, singleObjCompare, startPoint2D, domain2D)
    val nmAns = nm.optimize(math.pow(10,-2), 10000, 100)
    println("Nelder-Mead: optimize(Rosenbrock 2D): " + nmAns._2(0) + " from point " + pointToStr(nmAns._1) + "\nNumber of evaluations: " + nm.evalCount / 2)
  }
  
  test("optimize => Rosenbrock 10D function") {
    println("=" * 80)
    println("=" * 21 + "  optimize => Rosenbrock 10D function  " + "=" * 20)
    println("=" * 80)
    val dds = DDS(f2, 1, singleObjCompare, startPoint10D, domain10D, bases10D)
    val ddsAns = dds.optimize(math.pow(10,-2), 10000, 100)
    println("DDS: optimize(Rosenbrock 10D): " + ddsAns._2(0) + " from point " + pointToStr(ddsAns._1) + "\nNumber of evaluations: " + dds.evalCount / 2)
    println("-" * 80)
    val mds = MDS(f2, 1, singleObjCompare, startPoint10D, domain10D)
    val mdsAns = mds.optimize(math.pow(10,-2), 10000, 100)
    println("MDS: optimize(Rosenbrock 10D): " + mdsAns._2(0) + " from point " + pointToStr(mdsAns._1) + "\nNumber of evaluations: " + mds.evalCount / 2)
    println("-" * 80)
    val nm = NelderMead(f2, 1, singleObjCompare, startPoint10D, domain10D)
    val nmAns = nm.optimize(math.pow(10,-2), 10000, 100)
    println("Nelder-Mead: optimize(Rosenbrock 10D): " + nmAns._2(0) + " from point " + pointToStr(nmAns._1) + "\nNumber of evaluations: " + nm.evalCount / 2)
  }
  
  test("optimize => Himmelbau 2D function") {
    println("=" * 80)
    println("=" * 22 + "  optimize => Himmelbau 2D function  " + "=" * 21)
    println("=" * 80)
    val dds = DDS(f3, 1, singleObjCompare, startPoint2D, domain2D, bases2D)
    val ddsAns = dds.optimize(math.pow(10,-2), 10000, 100)
    println("DDS: optimize(Himmelbau 2D): " + ddsAns._2(0) + " from point " + pointToStr(ddsAns._1) + "\nNumber of evaluations: " + dds.evalCount / 2)
    println("-" * 80)
    val mds = MDS(f3, 1, singleObjCompare, startPoint2D, domain2D)
    val mdsAns = mds.optimize(math.pow(10,-2), 10000, 100)
    println("MDS: optimize(Himmelbau 2D): " + mdsAns._2(0) + " from point " + pointToStr(mdsAns._1) + "\nNumber of evaluations: " + mds.evalCount / 2)
    println("-" * 80)
    val nm = NelderMead(f3, 1, singleObjCompare, startPoint2D, domain2D)
    val nmAns = nm.optimize(math.pow(10,-2), 10000, 100)
    println("Nelder-Mead: optimize(Himmelbau 2D): " + nmAns._2(0) + " from point " + pointToStr(nmAns._1) + "\nNumber of evaluations: " + nm.evalCount / 2)
  }
  
  test("optimize => Two-link truss 2D function") {
    println("=" * 80)
    println("=" * 19 + "  optimize => Two-link truss 2D function  " + "=" * 19)
    println("=" * 80)
    val dds = DDS(f4, 1, singleObjCompare, startPoint2D, domain2D, bases2D)
    val ddsAns = dds.optimize(math.pow(10,-2), 10000, 100)
    println("DDS: optimize(Two-link truss 2D): " + ddsAns._2(0) + " from point " + pointToStr(ddsAns._1) + "\nNumber of evaluations: " + dds.evalCount / 2)
    println("-" * 80)
    val mds = MDS(f4, 1, singleObjCompare, startPoint2D, domain2D)
    val mdsAns = mds.optimize(math.pow(10,-2), 10000, 100)
    println("MDS: optimize(Two-link truss 2D): " + mdsAns._2(0) + " from point " + pointToStr(mdsAns._1) + "\nNumber of evaluations: " + mds.evalCount / 2)
    println("-" * 80)
    val nm = NelderMead(f4, 1, singleObjCompare, startPoint2D, domain2D)
    val nmAns = nm.optimize(math.pow(10,-2), 10000, 100)
    println("Nelder-Mead: optimize(Two-link truss 2D): " + nmAns._2(0) + " from point " + pointToStr(nmAns._1) + "\nNumber of evaluations: " + nm.evalCount / 2)
  }
  
  if(false){
  test("sampledOptimize => Dummy 2D function") {
    println("=" * 80)
    println("=" * 20 + "  sampledOptimize => Dummy 2D function  " + "=" * 20)
    println("=" * 80)
    val dds = DDS(f1, 1, singleObjCompare, startPoint2D, domain2D, bases2D)
    val ddsAns = dds.sampledOptimize(100, math.pow(10, -2))
    println("DDS: optimize(Dummy 2D): " + ddsAns._2(0) + " from point " + pointToStr(ddsAns._1) + "\nAverage number of evaluations: " + dds.evalCount / 200.0)
    println("-" * 80)
    val mds = MDS(f1, 1, singleObjCompare, startPoint2D, domain2D)
    val mdsAns = mds.sampledOptimize(100, math.pow(10, -2))
    println("MDS: optimize(Dummy 2D): " + mdsAns._2(0) + " from point " + pointToStr(mdsAns._1) + "\nAverage number of evaluations: " + mds.evalCount / 200.0)
    println("-" * 80)
    val nm = NelderMead(f1, 1, singleObjCompare, startPoint2D, domain2D)
    val nmAns = nm.sampledOptimize(100, math.pow(10, -2))
    println("Nelder-Mead: optimize(Dummy 2D): " + nmAns._2(0) + " from point " + pointToStr(nmAns._1) + "\nAverage number of evaluations: " + nm.evalCount / 200.0)
  }
  
  test("sampledOptimize => Rosenbrock 2D function") {
    println("=" * 80)
    println("=" * 18 + "  sampledOptimize => Rosenbrock 2D function  " + "=" * 17)
    println("=" * 80)
    val dds = DDS(f2, 1, singleObjCompare, startPoint2D, domain2D, bases2D)
    val ddsAns = dds.sampledOptimize(100, math.pow(10, -2))
    println("DDS: optimize(Rosenbrock 2D): " + ddsAns._2(0) + " from point " + pointToStr(ddsAns._1) + "\nAverage number of evaluations: " + dds.evalCount / 200.0)
    println("-" * 80)
    val mds = MDS(f2, 1, singleObjCompare, startPoint2D, domain2D)
    val mdsAns = mds.sampledOptimize(100, math.pow(10, -2))
    println("MDS: optimize(Rosenbrock 2D): " + mdsAns._2(0) + " from point " + pointToStr(mdsAns._1) + "\nAverage number of evaluations: " + mds.evalCount / 200.0)
    println("-" * 80)
    val nm = NelderMead(f2, 1, singleObjCompare, startPoint2D, domain2D)
    val nmAns = nm.sampledOptimize(100, math.pow(10, -2))
    println("Nelder-Mead: optimize(Rosenbrock 2D): " + nmAns._2(0) + " from point " + pointToStr(nmAns._1) + "\nAverage number of evaluations: " + nm.evalCount / 200.0)
  }
  
  test("sampledOptimize => Rosenbrock 10D function") {
    println("=" * 80)
    println("=" * 17 + "  sampledOptimize => Rosenbrock 10D function  " + "=" * 17)
    println("=" * 80)
    val dds = DDS(f2, 1, singleObjCompare, startPoint10D, domain10D, bases10D)
    val ddsAns = dds.sampledOptimize(100, math.pow(10, -2))
    println("DDS: optimize(Rosenbrock 10D): " + ddsAns._2(0) + " from point " + pointToStr(ddsAns._1) + "\nAverage number of evaluations: " + dds.evalCount / 200.0)
    println("-" * 80)
    val mds = MDS(f2, 1, singleObjCompare, startPoint10D, domain10D)
    val mdsAns = mds.sampledOptimize(100, math.pow(10, -2))
    println("MDS: optimize(Rosenbrock 10D): " + mdsAns._2(0) + " from point " + pointToStr(mdsAns._1) + "\nAverage number of evaluations: " + mds.evalCount / 200.0)
    println("-" * 80)
    val nm = NelderMead(f2, 1, singleObjCompare, startPoint10D, domain10D)
    val nmAns = nm.sampledOptimize(100, math.pow(10, -2))
    println("Nelder-Mead: optimize(Rosenbrock 10D): " + nmAns._2(0) + " from point " + pointToStr(nmAns._1) + "\nAverage number of evaluations: " + nm.evalCount / 200.0)
  }
  
  test("sampledOptimize => Himmelbau 2D function") {
    println("=" * 80)
    println("=" * 18 + "  sampledOptimize => Himmelbau 2D function  " + "=" * 18)
    println("=" * 80)
    val dds = DDS(f3, 1, singleObjCompare, startPoint2D, domain2D, bases2D)
    val ddsAns = dds.sampledOptimize(100, math.pow(10, -2))
    println("DDS: optimize(Himmelbau 2D): " + ddsAns._2(0) + " from point " + pointToStr(ddsAns._1) + "\nAverage number of evaluations: " + dds.evalCount / 200.0)
    println("-" * 80)
    val mds = MDS(f3, 1, singleObjCompare, startPoint2D, domain2D)
    val mdsAns = mds.sampledOptimize(100, math.pow(10, -2))
    println("MDS: optimize(Himmelbau 2D): " + mdsAns._2(0) + " from point " + pointToStr(mdsAns._1) + "\nAverage number of evaluations: " + mds.evalCount / 200.0)
    println("-" * 80)
    val nm = NelderMead(f3, 1, singleObjCompare, startPoint2D, domain2D)
    val nmAns = nm.sampledOptimize(100, math.pow(10, -2))
    println("Nelder-Mead: optimize(Himmelbau 2D): " + nmAns._2(0) + " from point " + pointToStr(nmAns._1) + "\nAverage number of evaluations: " + nm.evalCount / 200.0)
  }
  
  test("sampledOptimize => Two-link truss 2D function") {
    println("=" * 80)
    println("=" * 16 + "  sampledOptimize => Two-link truss 2D function  " + "=" * 15)
    println("=" * 80)
    val dds = DDS(f4, 1, singleObjCompare, startPoint2D, domain2D, bases2D)
    val ddsAns = dds.sampledOptimize(100, math.pow(10, -2))
    println("DDS: optimize(Two-link truss 2D): " + ddsAns._2(0) + " from point " + pointToStr(ddsAns._1) + "\nAverage number of evaluations: " + dds.evalCount / 200.0)
    println("-" * 80)
    val mds = MDS(f4, 1, singleObjCompare, startPoint2D, domain2D)
    val mdsAns = mds.sampledOptimize(100, math.pow(10, -2))
    println("MDS: optimize(Two-link truss 2D): " + mdsAns._2(0) + " from point " + pointToStr(mdsAns._1) + "\nAverage number of evaluations: " + mds.evalCount / 200.0)
    println("-" * 80)
    val nm = NelderMead(f4, 1, singleObjCompare, startPoint2D, domain2D)
    val nmAns = nm.sampledOptimize(100, math.pow(10, -2))
    println("Nelder-Mead: optimize(Two-link truss 2D): " + nmAns._2(0) + " from point " + pointToStr(nmAns._1) + "\nAverage number of evaluations: " + nm.evalCount / 200.0)
  }
  }
}

/**
 * Unit test on DDS (Direct Directional Search)
 */
class DDSTest extends FunSuite with Matchers {
  def f1(x: Array[Double]): Array[Double] = {
    var eval = 0.0;
    x.foreach(element=> eval += element*element)
    Array(eval)
  }
  
  def singleObjCompare(a1: Array[Double], a2:Array[Double]): Boolean = {
    a1(0) < a2(0)
  }
  
  
  def resetDDS() = {
    val testDDS = DDS(f1, 1, singleObjCompare, Array(0.5, 0.5), Array.tabulate(2)(i => Interval(-100.0, 100.0)), Array.tabulate(2)(i => Array.tabulate(2)(j => if(i==j) 1.0 else 0.0)))
    testDDS
  }  
  
  test("negateBase(Array[Double]) function") {
	val testDDS = resetDDS()
    testDDS.negateBasis(Array(1.0 ,0.0)) should equal (Array(-1.0, 0.0))
  }
  
  test("normalizeBasis(Array[Double])") {
    val testDDS = resetDDS()
    val testBasis = Array.fill(2){1.0}
    testDDS.normalizeBasis(testBasis)
    testBasis(0) should equal (1.0/ math.sqrt(2.0))
    testBasis(1) should equal (1.0/ math.sqrt(2.0))
  }
  
  test("newMeshPoint(Array[Double], Double) function") {
    val testDDS = resetDDS()
    val basis = Array(1.0, 0.0)
    val tmp = testDDS.newMeshPoint(basis) // go in direction (1,0) from starting point (0.5,0.5) at distance 2
    tmp._1 should equal (Array(0.5 + basis(0) * testDDS.alpha(0), 0.5 + basis(1) * testDDS.alpha(1)))
  }
  
  test("promoteBasis(Int) function") {
  	val testDDS = resetDDS()
    testDDS.promoteBasis(0)
    
  	testDDS.bases(0) should equal (Array(1,0))
  	testDDS.bases(1) should equal (Array(0,1))
  	
  	testDDS.promoteBasis(1)
  	
  	testDDS.bases(0) should equal (Array(0,1))
  	testDDS.bases(1) should equal (Array(1,0))
  }
}

/**
 * Unit test on MDS (Multi Directional Search)
 */
class MDSTest extends FunSuite with Matchers {
  def f1(x: Array[Double]): Array[Double] = {
    var eval = 0.0
    x.foreach(element=> eval += element*element)
    Array(eval)
  }
  
  def singleObjCompare(a1: Array[Double], a2:Array[Double]): Boolean = {
    a1(0) < a2(0)
  }

  
  def resetMDS() = {
    val testMDS = MDS(f1, 1, singleObjCompare, Array.tabulate(2)(i => 42.0), Array.fill(2){Interval(-100.0, 100.0)})
	testMDS.simplex(0) = (Array(0.0, 0.0),Array(0.0)) // best point of the simplex
    testMDS.simplex(1) = (Array(0.0, 1.0),Array(1.0))
    testMDS.simplex(2) = (Array(1.0, 0.0),Array(1.0))
    testMDS
  }
  
  test("pointDistance(Array[Double], Array[Double]) function") {
    val mds = resetMDS()
    val p1 = Array.tabulate(2)(i => 1.0 * (i + 1))
    val p2 = Array.tabulate(2)(i => 2.0 * (i + 1))
    mds.pointDistance(p1, p2) should equal (math.sqrt(5.0))
  }
  
  test("rotateSimplex(Double) function") {
    val mds = resetMDS()
    
    val simp = mds.rotateSimplex()
    
    simp(0)._1 should equal (Array(0.0, 0.0)) 

  }
  
  test("expandSimplex(Double) function") {
    val mds = resetMDS()
    val simp = mds.expandSimplex()
    simp(1)._1 should equal (Array(0.0, -mds.gammaE))
    simp(2)._1 should equal (Array(-mds.gammaE, 0.0)) // we use an expension coefficient = gammaE
    
  }
  
  test("shrinkSimplex(Double) function") {
    val mds = resetMDS()
    val simp = mds.shrinkSimplex()
    simp(1)._1 should equal (Array(0.0, mds.gammaS))
    simp(2)._1 should equal (Array(mds.gammaS, 0.0)) // we use an shrink coefficient = gammaS
  }
}

/**
 * Unit test on Nelder Mead
 */
class NelderMeadTest extends FunSuite with Matchers {
  def f1(x: Array[Double]): Array[Double] = {
    var eval = 0.0;
    x.foreach(element=> eval += element*element)
    Array(eval)
  }
  
  def singleObjCompare(a1: Array[Double], a2:Array[Double]): Boolean = {
    a1(0) < a2(0)
  }
  

  
  def resetSimplex(): NelderMead = {
	val testNMS = NelderMead(f1, 1, singleObjCompare, Array.tabulate(2)(i => 42.0), Array.tabulate(2)(i => Interval(-100.0, 100.0)))
	testNMS.simplex(0) = (Array(0.0, 0.0),Array(0.0)) // best point of the simplex
    testNMS.simplex(1) = (Array(0.0, 1.0),Array(1.0))
    testNMS.simplex(2) = (Array(1.0, 0.0),Array(1.0))
    testNMS.centroid = testNMS.findCentroid()
    testNMS
  }
  
  test("putInDomain(Array[Double]) function") {
    val testNMS = resetSimplex()
    testNMS.putInDomain(Array.tabulate(2)(i => 200.0)) should equal (Array.tabulate(2)(i => 100.0))
    testNMS.putInDomain(Array.tabulate(2)(i => -200.0)) should equal (Array.tabulate(2)(i => -100.0))
    testNMS.putInDomain(Array.tabulate(2)(i => 50.0)) should equal (Array.tabulate(2)(i => 50.0))
  }
  
  test("findCentroid function") {
    val testNMS = resetSimplex()
    testNMS.centroid = testNMS.findCentroid()
    testNMS.centroid should equal (Array(0, 0.5))
  }
  
  test("reflectPoint(Double, Double) function") {
    val testNMS = resetSimplex()
    val tmp = testNMS.reflectPoint() // return (reflected point, eval)
    tmp._1  should equal (Array(-1, 1)) // point test
    tmp._2 should equal (Array(2.0)) // objective test
  }
  
  test("contractPoint(Boolean, Double, Double) function") {
  	val testNMS = resetSimplex()
    val ins = testNMS.contractPoint(true) // true = inside contraction
    ins._1  should equal (Array(0.5, 0.25)) // test the contracted point
  	
    val out = testNMS.contractPoint(false) // false = outside contraction  
  	out._1  should equal ( Array(-0.5, 0.75)) // test the contracted point
  }
  
  test("shrink(Double, Double) function") {
    val testNMS = resetSimplex()
    
    testNMS.shrink() // it will modify directly the simplex
    
    testNMS.simplex(0)._1 should equal (Array(0,0)) 
  	testNMS.simplex(1)._1 should equal (Array(0, 0.5))
    testNMS.simplex(2)._1 should equal (Array(0.5, 0))  
  }  
  
  test("insertInSimplex((Array[Double], Double)) function") {
    val testNMS = resetSimplex()
    val x = Array(0,0.5)
    val fx = f1(x)
    testNMS.insertInSimplex((x, fx))
    
  	testNMS.simplex(1)._1 should equal (x)
  	testNMS.simplex(1)._2 should equal (fx)
  }
  
  test("iterNM() function") {
    val testNMS = resetSimplex()
    val formerSimp = testNMS.formerSimplex
    val simp = testNMS.simplex
    testNMS.iterNM()
  }
}
