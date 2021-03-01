package oscar.cp.constraints.nooverlap.util

import TSPUtils._

/**
  * Created by saschavancauwelaert on 18/02/16.
  */
object DynamicProgrammingTSP {

  val UNDEF = -1

  def transitionFamilyFirstOrLast(familyTransitions : Array[Array[Int]], familyIsFirst : Boolean) : Array[Array[Int]] = {

    val nFamilies = familyTransitions.length
    val nSets = nFamilySet(nFamilies)

    val resultMatrix = Array.fill[Int](nFamilies, nSets + 1)(UNDEF)

    // init empty set and singleton sets (rule (16) in artigues paper)
    for(f <- 0 until nFamilies) {
      resultMatrix(f)(1 << f) = 0
    }

    for(setSize <- 1 until nFamilies) {
      val allSets = allBitSetsOfSize(setSize, nFamilies)
      for (f <- 0 until nFamilies) {
        for (setWithoutF <- allSets if !isFamilyInSet(f, setWithoutF)) {
          val setWithF = setWithoutF | (1 << f)
          for (g <- setIDToFamilies(setWithoutF)) {
            val transitionFG = if(familyIsFirst) familyTransitions(f)(g) else familyTransitions(g)(f)
            if (resultMatrix(g)(setWithoutF) != UNDEF) {
              if (resultMatrix(f)(setWithF) != UNDEF) {
                resultMatrix(f)(setWithF) = math.min(resultMatrix(f)(setWithF), transitionFG + resultMatrix(g)(setWithoutF))
              }
              else {
                resultMatrix(f)(setWithF) = transitionFG + resultMatrix(g)(setWithoutF)
              }
            }
          }
        }
      }
    }
    resultMatrix
  }

  def transitionFamilyFirst(familyTransitions : Array[Array[Int]]) : Array[Array[Int]] = {
    transitionFamilyFirstOrLast(familyTransitions, true)
  }

  def transitionFamilyLast(familyTransitions : Array[Array[Int]]) : Array[Array[Int]] = {
    transitionFamilyFirstOrLast(familyTransitions, false)
  }

  def transitionFamilyWholeSet(familyTransitions : Array[Array[Int]]) : Array[Int] = {
    val withFirst = transitionFamilyFirst(familyTransitions)
    Array.tabulate(withFirst(0).length){ bitset =>
      if(bitset != 0) {
        var minTransitionForSet = Int.MaxValue
        for(f <- familyTransitions.indices) {
          if(withFirst(f)(bitset) != -1) {
            minTransitionForSet = math.min(withFirst(f)(bitset), minTransitionForSet)
          }
        }
        minTransitionForSet
      }
      else {
        -1
      }
    }
  }

  def transitionFamilyPerBitSet(familyTransitions : Array[Array[Int]]) : Array[Int] = {
    val res = transitionFamilyWholeSet(familyTransitions)
    res(0) = 0
    res
  }

  def minTransitionTimesPerCardinality(familyTransitions : Array[Array[Int]]): Array[Int] = {
    val transitionWholeSets = transitionFamilyWholeSet(familyTransitions)

    val nFamilies = familyTransitions.length

    (1 to nFamilies).map { card =>
        val sets =  allBitSetsOfSize(card, nFamilies)
        var minTrans = Int.MaxValue
        for (set <- sets) {
          minTrans = math.min(transitionWholeSets(set), minTrans)
        }
        minTrans
      }.toArray
  }



}
