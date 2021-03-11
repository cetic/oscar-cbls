package oscar.ml.pm.utils

/**
 * ZDC functions
 *      Find the ZDC functions formula in my thesis
 *      http://hdl.handle.net/2078.1/218062 pages 48, 49,
 *      formula 3.(16,17,18)
 * @author John Aoga johnaoga@gmail.com
 * @author Pierre Schaus pschaus@gmail.com
 *
 */
abstract class ZDCFunction {
  def eval(p: Int, n: Int, nTransP: Int, nTransN: Int): Double
}

class ZDCScaled(f: ZDCFunction, val scale: Double = 1.0) extends ZDCFunction {
  def eval(p: Int, n: Int, nTransP: Int, nTransN: Int): Double = {
    f.eval(p, n, nTransP, nTransN) * scale
  }
}

object Fisher extends ZDCFunction {
  def eval(p: Int, n: Int, nTransP: Int, nTransN: Int): Double = {

    val nTrans: Double = nTransP + nTransN
    val precision = 1.0

    val covered: Double = n + p

    val mu = covered / nTrans
    val muP = p * 1.0 / nTransP
    val muN = n * 1.0 / nTransN

    val numer: Double = p * (muP - mu) * (muP - mu) + n * (muN - mu) * (muN - mu)
    val denom: Double = (p - (nTransP * muP * muP)) + (n - (nTransN * muN * muN))

    if (denom == 0) if (numer == 0) 0 else scala.Double.PositiveInfinity
    else precision * numer / denom

  }
}

object InfGain extends ZDCFunction {
  def eval(p: Int, n: Int, nTransP: Int, nTransN: Int): Double = {

    val nTrans: Double = nTransP + nTransN
    val precision = 1.0

    def nlogn(x: Double): Double = {
      if (x > 0.0) {
        // In Siegfried implem, the natural logarithm is used
        // They use log2 in this paper https://dl.acm.org/doi/10.1145/1557019.1557092
        return -Math.log(x) * x
      }
      0.0
    }

    val covered: Double = n + p
    val notCovered: Double = nTrans - covered

    var before: Double = 0.0
    if (nTrans != 0) before = nlogn(nTransP / nTrans) + nlogn(nTransN / nTrans)

    var after: Double = 0.0

    if (covered != 0) {
      after += covered * (nlogn(p / covered) + nlogn(n / covered))
    }

    if (notCovered != 0) {
      after += notCovered * (nlogn((nTransP - p) / notCovered) + nlogn((nTransN - n) / notCovered))
    }

   precision * (before - after / nTrans)
  }
}

object Gini extends ZDCFunction {
  def eval(p: Int, n: Int, nTransP: Int, nTransN: Int): Double = {

    val nTrans: Double = nTransP + nTransN
    val precision = 1.0

    val covered: Double = n + p
    val notCovered: Double = nTrans - covered

    var before: Double = 0.0
    if (nTrans != 0) before = 1.0 -
      ((nTransP / nTrans) * (nTransP / nTrans)) -
      ((nTransN / nTrans) * (nTransN / nTrans))

    var after: Double = 0.0

    if (covered != 0) {
      after = covered * (1.0 -
        ((p / covered) * (p / covered)) -
        ((n / covered) * (n / covered)))
    }
    if (notCovered != 0) {
      after += notCovered * (1.0 -
        (((nTransP - p) / notCovered) * ((nTransP - p) / notCovered)) -
        (((nTransN - n) / notCovered) * ((nTransN - n) / notCovered)))
    }

    precision * (before - after / nTrans)
  }
}

object Chi2 extends ZDCFunction {
  def eval(p: Int, n: Int, nTransP: Int, nTransN: Int): Double = {

    val nTrans = nTransP + nTransN
    val precision = 1.0

    val cover = (p + n) * 1.0

    val pp = nTransP - p
    val nn = nTransN - n

    val a = cover / nTrans
    val ap = a * nTransP
    val an = a * nTransN
    val b = (nTrans - cover) / nTrans
    val bp = b * nTransP
    val bn = b * nTransN

    var x1, x2, x3, x4: Double = 0

    if (a != 0) {
      x1 = (p - ap) * (p - ap) / ap
      x2 = (n - an) * (n - an) / an
    }

    if (b != 0) {
      x3 = (pp - bp) * (pp - bp) / bp
      x4 = (nn - bn) * (nn - bn) / bn
    }
    val res: Double = x1 + x2 + x3 + x4

   res * precision
  }
}


