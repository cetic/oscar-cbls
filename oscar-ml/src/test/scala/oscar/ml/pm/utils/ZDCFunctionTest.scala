package oscar.ml.pm.utils

import oscar.cp.testUtils.TestSuite

class ZDCFunctionTest extends TestSuite {

  def chi2(a:Int, b:Int, A:Int, B:Int): Double ={

    def p(X:Int, Y:Int, x:Int, y:Int): Double =
      (x+y)*X*1.0/(X+Y)

    def q(X:Int, Y:Int, x:Int, y:Int) : Double =
      if (p(X, Y, x, y) != 0)
        Math.pow(x - p(X, Y, x, y), 2)/p(X, Y, x, y)
      else
        0.0

    def Q(X:Int, Y:Int, x:Int, y:Int) : Double =
      q(X, Y, x, y) + q(Y, X, y, x)

    Q(A, B, a, b) + Q(A, B, A - a, B - b)
  }

  def gain(a:Int, b:Int, A:Int, B:Int): Double ={
    def nlogn(x: Double): Double =
      if (x > 0.0) -Math.log(x) * x else 0.0

    def H(p: Double): Double =
      nlogn(p) + nlogn(1-p)

    def f(x: Int, y: Int) : Double =
      if ( (x+y) != 0 ) x*1.0/(x+y) else 0.0

    H(f(A,B)) - ((a+b)*1.0/(A+B))*H(f(a,b)) - ((A+B-a-b)*1.0/(A+B))*H(f(A-a, B-b))
  }


  def gini(a:Int, b:Int, A:Int, B:Int): Double ={
    def G(x: Double): Double =
      1 - x*x - (1-x)*(1-x)

    def f(x: Int, y: Int) : Double =
      if ( (x+y) != 0 ) x*1.0/(x+y) else 0.0

    G(f(A,B)) - ((a+b)*1.0/(A+B))*G(f(a,b)) - ((A+B-a-b)*1.0/(A+B))*G(f(A-a, B-b))
  }

  def checkIt(x:Int, y:Int, X:Int, Y:Int, gainVal:Int, chi2Val:Int, giniVal:Int): Unit ={
    assert(new ZDCScaled(InfGain, 100).eval(x, y, X, Y).toInt == (gain(x, y, X, Y)*100).toInt)
    assert(new ZDCScaled(Chi2, 100).eval(x, y, X, Y).toInt == (chi2(x, y, X, Y)*100).toInt)
    assert(new ZDCScaled(Gini, 100).eval(x, y, X, Y).toInt == (gini(x, y, X, Y)*100).toInt)

    (new ZDCScaled(InfGain, 100).eval(x, y, X, Y).toInt) should be(gainVal)
    (new ZDCScaled(Chi2, 100).eval(x, y, X, Y).toInt) should be(chi2Val)
    (new ZDCScaled(Gini, 100).eval(x, y, X, Y).toInt) should be(giniVal)

    assert(Chi2.eval(0, Y, X, Y) == (X+Y))
    assert(Chi2.eval(X, 0, X, Y) == (X+Y))

    val fun:Array[ZDCFunction] = Array(InfGain, Chi2, Gini)

    var i = 0
    while (i < fun.length) {
      assert(fun(i).eval(0, 0, X, Y)== 0.0)
      assert(fun(i).eval(X, Y, X, Y) == 0.0)

      assert(fun(i).eval(0, Y, X, Y) > fun(i).eval(0, y, X, Y) ) // y < Y
      assert(fun(i).eval(X, 0, X, Y) > fun(i).eval(x, 0, X, Y) ) // x < X

      i+= 1
    }
  }

  test("test Measures") {
    checkIt(1, 3, 6, 4, 17, 340, 16)
    checkIt(618, 123, 625, 187, 10, 19768, 8)
  }

}
