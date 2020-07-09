package oscar.ml.pm.utils

object TestHelpers {
  def printMat(mat: Array[Array[Int]]): Unit =
    println(mat.map(_.mkString(", ")).mkString("\n"))

  def checkArray(input: Array[Array[Int]], output: Array[Array[Int]]): Boolean = {
    if (input.length != output.length) return false

    var i = 0
    while (i < input.length) {
      if (!(input(i) sameElements output(i)))
        return false
      i += 1
    }

    true
  }

  def chi2(a: Int, b: Int, A: Int, B: Int): Double = {

    def p(X: Int, Y: Int, x: Int, y: Int): Double =
      (x + y) * X * 1.0 / (X + Y)

    def q(X: Int, Y: Int, x: Int, y: Int): Double =
      if (p(X, Y, x, y) != 0)
        Math.pow(x - p(X, Y, x, y), 2) / p(X, Y, x, y)
      else
        0.0

    def Q(X: Int, Y: Int, x: Int, y: Int): Double =
      q(X, Y, x, y) + q(Y, X, y, x)

    Q(A, B, a, b) + Q(A, B, A - a, B - b)
  }

  def gain(a: Int, b: Int, A: Int, B: Int): Double = {
    def nlogn(x: Double): Double =
      if (x > 0.0) -Math.log(x) * x else 0.0

    def H(p: Double): Double =
      nlogn(p) + nlogn(1 - p)

    def f(x: Int, y: Int): Double =
      if ((x + y) != 0) x * 1.0 / (x + y) else 0.0

    H(f(A, B)) - ((a + b) * 1.0 / (A + B)) * H(f(a, b)) - ((A + B - a - b) * 1.0 / (A + B)) * H(f(A - a, B - b))
  }


  def gini(a: Int, b: Int, A: Int, B: Int): Double = {
    def G(x: Double): Double =
      1 - x * x - (1 - x) * (1 - x)

    def f(x: Int, y: Int): Double =
      if ((x + y) != 0) x * 1.0 / (x + y) else 0.0

    G(f(A, B)) - ((a + b) * 1.0 / (A + B)) * G(f(a, b)) - ((A + B - a - b) * 1.0 / (A + B)) * G(f(A - a, B - b))
  }
}
