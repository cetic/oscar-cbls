package oscar.util.instanceGenerators.utils

import scala.util.Random

/**
 * Created by saschavancauwelaert on 18/09/15.
 */
object Utils {

  def getFamilies(transitionTimes: Array[Array[Int]]): (Array[Int], Array[Array[Int]]) = {
    val nActivities = transitionTimes.length
    // Assigning a family to each activity
    val families = Array.fill(nActivities)(-1)
    var curFamily = 0
    for (i <- 0 until nActivities) {
      if (families(i) < 0) {
        families(i) = curFamily
        for (j <- i + 1 until nActivities) {
          if (transitionTimes(i)(j) == 0 && (0 until nActivities).forall(k => transitionTimes(i)(k) == transitionTimes(j)(k) && transitionTimes(k)(i) == transitionTimes(k)(j))) {
            families(j) = families(i)
          }
        }
        curFamily += 1
      }
    }
    val nFamilies = curFamily
    // Building the family matrix and ensuring all families are correct
    val familyMatrix = Array.fill[Int](nFamilies, nFamilies)(Int.MaxValue)
    for (f1 <- 0 until nFamilies) {
      val indicesOnF1 = (0 until nActivities).filter(i => families(i) == f1)
      for (f2 <- 0 until nFamilies) {
        val indicesOnF2 = (0 until nActivities).filter(i => families(i) == f2)
        for (i <- indicesOnF1; j <- indicesOnF2) {
          familyMatrix(f1)(f2) = math.min(familyMatrix(f1)(f2), transitionTimes(indicesOnF1(0))(indicesOnF2(0)))
        }
      }
    }
    (families, familyMatrix)
  }

  def getTriangularTTMatrix(nState: Int, minTransition: Int, maxTransition: Int, randGen: Random): Array[Array[Int]] = {
    val matrix = Array.tabulate(nState, nState)( (i,j) => if(i != j) randGen.nextInt(maxTransition - minTransition + 1) + minTransition else 0)
    val goodMatrix = MatrixUtils.allPairShortestPath(matrix)
    goodMatrix
  }



}
