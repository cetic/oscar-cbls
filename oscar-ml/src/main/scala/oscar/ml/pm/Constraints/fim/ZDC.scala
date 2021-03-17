/** *****************************************************************************
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
 * *****************************************************************************/

package oscar.ml.pm.Constraints.fim

import oscar.algo.Inconsistency
import oscar.cp._
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.CPVar
import oscar.ml.pm.utils.ZDCFunction

/**
 *
 * ZDC = Zero Diagonal Convex : Eval of discriminant patterns
 *
 * @author John Aoga johnaoga@gmail.com
 * @author Pierre Schaus pschaus@gmail.com
 *
 *         Relevant paper: Compact table and http://becool.info.ucl.ac.be/biblio/coversize-global-constraint-frequency-based-itemset-mining
 *
 *         PART OF SOLVER OSCAR (https://bitbucket.org/oscarlib/oscar/wiki/Home)
 */
class ZDC(val P: CPIntVar, val N: CPIntVar, val nTransP: Int, val nTransN: Int, val f: ZDCFunction, fScore: CPIntVar) extends Constraint(P.store, "ZDC") {
  override def associatedVars(): Iterable[CPVar] = Array(P, N, fScore)

  private[this] val nTrans = nTransP + nTransN


  def f(p: Int, n: Int): Double = f.eval(p, n, nTransP, nTransN)


  override def setup(l: CPPropagStrength): Unit = {

    if (!P.isBound) P.callPropagateWhenBoundsChange(this)
    if (!N.isBound) N.callPropagateWhenBoundsChange(this)
    if (!fScore.isBound) fScore.callPropagateWhenBoundsChange(this)

    propagate()

  }

  def ceil(d: Double): Int = Math.ceil(d).toInt

  def floor(d: Double): Int = Math.floor(d).toInt


  override def propagate(): Unit = {

    //println(s"propagate ZDC: $P, $N $fScore")

    //get current P,N bounds
    val pmax = P.max
    val pmin = P.min
    val nmax = N.max
    val nmin = N.min

    //get f(p,n) bounds
    val fPminNmax = f(pmin, nmax)
    val fPmaxNmax = f(pmax, nmax)
    val fPminNmin = f(pmin, nmin)
    val fPmaxNmin = f(pmax, nmin)

    //our frequency
    val fScoreMin = fScore.min

    //when out of both opposite corner => not correct f
    if (fPminNmax < fScoreMin && fPmaxNmin < fScoreMin) throw Inconsistency

    //UBchi2 =the max between top-left (ftl) and  bottom-right (fbr) values of f

    //println("update max:"+(Math.max(fPmaxNmin, fPminNmax)).toInt)
    fScore.updateMax((Math.max(fPmaxNmin, fPminNmax)).toInt)

    //LBchi2 = min(ftl, fbr) if both ftl and fbr are over/under diagonal and valid
    val nMinCoef = nmin * 1.0 / nTransN
    val nMaxCoef = nmax * 1.0 / nTransN
    val pMinCoef = pmin * 1.0 / nTransP
    val pMaxCoef = pmax * 1.0 / nTransP

    if (((((nMaxCoef < pMinCoef) && (nMinCoef < pMaxCoef)) ||
      ((nMaxCoef > pMinCoef) && (nMinCoef > pMaxCoef))) &&
      fPmaxNmin >= fScoreMin && fPminNmax >= fScoreMin) || (P.isBound && N.isBound)) {

      fScore.updateMin((Math.min(fPmaxNmin, fPminNmax)).toInt)

    }

    ///Pruning of P and N
    // both bottom-(left,right) f points are out of corner, GROW nMin
    if (fPminNmin < fScoreMin && fPmaxNmin < fScoreMin) {

      var a = nmin
      var b = nmax
      while (b - a > 1) {
        val mid = a + (b - a) / 2
        if (f(pmin, mid) < fScoreMin) {
          a = mid
        } else {
          b = mid
        }
      }
      N.updateMin(b)

    }

    // both top-(left,right) f points are out of corner, DECREASE nMax
    if (fPminNmax < fScoreMin && fPmaxNmax < fScoreMin) {

      var a = nmax
      var b = nmin
      while (a - b > 1) {
        val mid = a - (a - b) / 2
        if (f(pmax, mid) < fScoreMin) {
          a = mid
        } else {
          b = mid
        }
      }
      N.updateMax(b)


    }

    // both left-(bottom,top) f points are out of corner, GROW pMin
    if (fPminNmin < fScoreMin && fPminNmax < fScoreMin) {

      var a = pmin
      var b = pmax
      while (b - a > 1) {
        val mid = a + (b - a) / 2
        if (f(mid, nmin) < fScoreMin) {
          a = mid
        } else {
          b = mid
        }
      }
      P.updateMin(b)


    }

    // both right-(bottom,top) f points are out of corner, DECREASE pMax
    if (fPmaxNmax < fScoreMin && fPmaxNmin < fScoreMin) {

      var a = pmax
      var b = pmin
      while (a - b > 1) {
        val mid = a - (a - b) / 2
        if (f(mid, nmax) < fScoreMin) {
          a = mid
        } else {
          b = mid
        }
      }
      P.updateMax(b)

    }
  }

}

