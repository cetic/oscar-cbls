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
package oscar.dfo.multiobjective

import oscar.util.Interval
import oscar.util.QuasiRandomSequence
import oscar.dfo.utils.DeterministicEvaluation

/** A Derivative Free MultiObjective Optimization (MODFO) Solver.
  *
  * The classes using this trait should implement the following function:
  *   findPareto: Performs the MODFO algorithm and returns an approximation of the Pareto set
  *
  * The classes using the trait are MOBinaryMSOPS, FormerMODMS, MODMS, MOSPEA2,
  *                                 MOVEGA, MOCOMBI, MOSAC, MOGEN 
  * @author Cyrille Dejemeppe 
  */
trait MODFOptimizer {  
  /** The random number generator */
  val rand = MODFOptimizer.randGen
  
  /** The number of dimensions of the search space */
  val dim: Int
  
  /** The number of objective functions to minimize */
  val nbObjectives: Int
  
  /** The quasi-random sequence generator */
  val sampler = MODFOptimizer.samplerGen

  /** The number of evaluations performed by the algorithm */
  var evalCount = 0
  
  /** Set to true when at least one element in the archive respects the objective bounds */
  var boundsOK = false
  
  /** The evaluator used to evaluate the stochastic functions */
  val evaluator = new DeterministicEvaluation()
  
  /** The number of iterations performed */
  var nbIter = 0
  
  /** Returns an approximation of the Pareto set.
    * 
    * @param nbPoints The number of points one want to obtain as approximation of the Pareto set
    * @param tol The tolerance which defines the desired level of precision
    * @return A list of pairs (point, evaluations) being a set of points that don't dominate
    *         each other and that are an approximation of the Pareto set of the objective functions */
  def findPareto(nbPoints: Int, tol: Double): List[(Array[Double], Array[Double])]
  
  /** Returns a string representation of an array of Double.
    * 
    * @param point An array of Double
    * @return A String that is the representation of the array of Double specified
    *         as parameter under the syntax (e1, e2, ..., en) */
  def pointToStr(point: Array[Double]) : String = {
    var res = "("
    for(i <- 0 to point.length - 2)
      res += s"${point(i)}, "
    res += s"${point(point.length - 1)})"
    res
  }
  
  /** Returns a triple representing respectively the number of equal values in sol1 and sol2,
    * the number of lower values in sol1 than in sol2 and the number of lower values in sol2 than in sol1.
    * 
    * This function is used to determine if a solution is equal to the other, if one solution dominates the other
    * one or if no solution dominates the other one.
    * 
    * @param sol1 An array of Double to be compared with sol2 (in general the evaluations of objective functions)
    * @param sol2 An array of Double to be compared with sol1 (in general the evaluations of objective functions)
    * @param alpha A Double representing the advancement of the algorithm (used to impose sufficient decrease)
    * @return A triple (e1, e2, e3) where
    *            - e1 is the number of equal values in sol1 and sol2 (with forcing function to impose sufficient
    *              decrease)
    *            - e2 is the number of lower values in sol1 than in sol2
    *            - e3 is the number of lower values in sol2 than in sol1
    * @example compareSolution(Array(0, 1), Array(1, 2)) will return (0, 2, 0)
    *          compareSolution(Array(0, 1), Array(0, 2)) will return (1, 1, 0)
    *          compareSolution(Array(0, 1), Array(0, 1)) will return (2, 0, 0)
    *          compareSolution(Array(0, 1), Array(1, 0)) will return (0, 1, 1) */
  def compareSolution(sol1: Array[Double], sol2: Array[Double], alpha: Double): (Int, Int, Int) = {
    var eq = 0
    var lower1 = 0
    var lower2 = 0
    for (i <- 0 until sol1.length) {
      if (sol1(i) <= sol2(i) + rho(alpha) && sol1(i) > sol2(i) - rho(alpha))
        eq += 1
      else if(sol1(i) < sol2(i))
        lower1 += 1
      else
        lower2 += 1
    }
    (eq, lower1, lower2)
  }
  
  /** Return a boolean indicating whether a point is in an archive.
    * 
    * @param p1 The evaluation of the objective functions to check if it
    *        does not already exist in the specified archive
    * @param ar A list of pairs representing an archive (current approcximation
    *        of the Pareto set)
    * @return A Boolean that is true if the evaluations p1 are already in the
    *         archive ar and False otherwise */
  def inArchive(p1: Array[Double], ar: List[(Array[Double], Array[Double])]): Boolean = {
    for (j <- 0 until ar.length) {
      var equalities = 0
      for (i <- 0 until p1.length) {
        if (p1(i) == ar(j)._2(i))
          equalities += 1
      }
      if (equalities == p1.length)
        return true
    }
    return false
  }
  
  /** Returns the euclidian distance between p1 and p2.
    * 
    * @param p1 Array of double representing point coordinates or objective evaluations
    * @param p2 Array of double representing point coordinates or objective evaluations
    * @return A Double being the euclidian distance between p1 and p2
    */
  def computeDistance(p1: Array[Double], p2: Array[Double]): Double = {
    var sumSquare = 0.0
    for (i <- 0 until p1.length)
      sumSquare += math.pow((p2(i) - p1(i)), 2)
    math.sqrt(sumSquare)
  }
  
  /** Returns true if the two points are equal, false otherwise.
    *
    * @param p1 An array of Double that is to be compared with p2
    * @param p2 An array of Double that is to be compared with p1
    * @return A Boolean that is true if p1 and p2 contains the same
    *         elements at the same indexes */
  def areEqual(p1: Array[Double], p2: Array[Double]): Boolean = {
    for (i <- 0 until p1.length) {
      if (p1(i) != p2(i))
        return false
    }
    true
  }
  
  /** Returns a Boolean indicating whether the evaluations specified as parameters are
    * lower than the objective bounds.
    * 
    * @param evals An array of Double representing objective function evaluations
    * @param inBoubds A Boolean that is true if the current archive is contains points
    *        that are already within bounds
    * @param objBounds The maximal value for the objective evaluations
    * @return A Boolean that is true if every element in evals is lower than the
    *         objective bounds */
  def withinBounds(evals: Array[Double], inBounds: Boolean, objBounds: Array[Double]): Boolean = {
    if (!inBounds)
      return true
    for (i <- 0 until evals.length) {
      if (evals(i) > objBounds(i))
        return false
    }
    true
  }
  
  /** Returns an array that is the input point put in the domain if it wasn't.
    * 
    * If the input already lies within the specified domain, it is simply returned.
    * Otherwise, for every coordinate that doesn't lie within the interval of its
    * dimension, we put it to the closest bound of the interval.
    * 
    * @param x An array of Double that is the point to put in the specified domain
    * @param domain An array of interval within which we must ensure the point lies
    * @return An array of Double being the input point lying within the specified domain */
  def putInDomain(x: Array[Double], domain: Array[Interval]): Array[Double] = {
    Array.tabulate(x.length)(i => if (domain(i).isInInterval(x(i))) x(i) else domain(i).getClosestBound(x(i)))
  }
  
  /** Returns true if x is in the specified domain, false otherwise.
    *  
    * @param x An array of Double that is the point to put in the specified domain
    * @param domain An array of interval within we want to know whether x is
    * @return An Boolean that is true if x is in the domain, false otherwise */
  def inDomain(x: Array[Double], domain: Array[Interval]): Boolean = {
    for (i <- 0 until x.length) {
      if (!domain(i).isInInterval(x(i)))
        return false
    }
    true
  }
  
  /** Returns the average of an array of Double.
    * 
    * @param ar The array of Double from which we compute the average
    * @return A Double that is the average of the array of Double specified as argument */
  def average(ar: Array[Double]): Double = {
    val a = ar.sum
    a / (ar.length).toDouble
  }
  
  /** The forcing function used for sufficient decrease.
    * 
    * @param t A Double representing the advancement of the algorithm
    * @return A Double that is the value of the forcing function of the specified input */
  def rho(t: Double): Double = {
    val a = if (t > 1.0) -11.0 else 9.0
    math.pow(t, (1.0 + a))
  }
  
  /** Returns an array of weights to scale the objective functions
    * 
    * @param archive A list of pairs (point, evaluations) from which we want
    *        the obtain the objective weights
    * @return An array of Double that is weights of the objective functions*/
  def scaleObjectives(archive: List[(Array[Double], Array[Double])]): Array[Double] = {
    val min = Array.fill(nbObjectives){Double.MaxValue}
    val max = Array.fill(nbObjectives){Double.MinValue}
    for (i <- 0 until archive.length) {
      for (j <- 0 until nbObjectives) {
        if(archive(i)._2(j) < min(j))
          min(j) = archive(i)._2(j)
        if(archive(i)._2(j) > max(j))
          max(j) = archive(i)._2(j)
      }
    }
    Array.tabulate(nbObjectives)(i => 1.0 / max(i))
  }
  
  /** Returns the maximal scaled difference between two evaluations.
    * 
    * @param p1 An array of Double representing evaluations for a point
    * @param p2 An array of Double representing evaluations for a point
    * @param scaling An array of Double that is the scaling of objective function evaluations
    * @return A Double being the maximal scaled difference between two evaluations specified
    *         as arguments */
  def computeScaledDistance(p1: Array[Double], p2: Array[Double], scaling: Array[Double]): Double = {
    var maxDist = Double.MinValue
    for (i <- 0 until p1.length) {
      if ((math.abs(p2(i) - p1(i)) * scaling(i)) > maxDist)
        maxDist = (math.abs(p2(i) - p1(i)) * scaling(i))
    }
    maxDist
  }
  
  /** Returns the current archive.
    * 
    * @return A list of pairs (point, evaluations) being the current archive
    *         (i.e. the current approximation of the Pareto set) of the algorithm */
  def getArchive: List[(Array[Double], Array[Double])]
}

/** Used to have a single random number generator and a single quasi-random
  * sequence generator for all the DFO algorithms. */ 
object MODFOptimizer{
  
  /** A random number generator */
  val randGen = new scala.util.Random(scala.util.Random.nextInt)
  
  /** A quasi-random sequence generator */
  val samplerGen = new QuasiRandomSequence(randGen)
}
