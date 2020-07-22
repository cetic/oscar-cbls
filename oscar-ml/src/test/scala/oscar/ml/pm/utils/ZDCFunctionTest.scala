package oscar.ml.pm.utils

import oscar.cp.testUtils.TestSuite

class ZDCFunctionTest extends TestSuite {

  def checkIt(x:Int, y:Int, X:Int, Y:Int, gainVal:Int, chi2Val:Int, giniVal:Int): Unit ={
    assert(new ZDCScaled(InfGain, 100).eval(x, y, X, Y).toInt == (TestHelpers.gain(x, y, X, Y)*100).toInt)
    assert(new ZDCScaled(Chi2, 100).eval(x, y, X, Y).toInt == (TestHelpers.chi2(x, y, X, Y)*100).toInt)
    assert(new ZDCScaled(Gini, 100).eval(x, y, X, Y).toInt == (TestHelpers.gini(x, y, X, Y)*100).toInt)

    (new ZDCScaled(InfGain, 100).eval(x, y, X, Y).toInt) should be(gainVal)
    (new ZDCScaled(Chi2, 100).eval(x, y, X, Y).toInt) should be(chi2Val)
    (new ZDCScaled(Gini, 100).eval(x, y, X, Y).toInt) should be(giniVal)

    assert(Chi2.eval(0, Y, X, Y) == (X+Y))
    assert(Chi2.eval(X, 0, X, Y) == (X+Y))
    Chi2.eval(X, 0, X, Y) should be(X+Y)

    Array(1, 3, 5).zipWithIndex.filter(_._1 >= 3)

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
