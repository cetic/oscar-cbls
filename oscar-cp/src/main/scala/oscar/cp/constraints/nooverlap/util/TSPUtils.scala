package oscar.cp.constraints.nooverlap.util

/**
  * Created by saschavancauwelaert on 18/02/16.
  */
object TSPUtils {

  def nFamilySet(nFamilies : Int) : Int = math.pow(2, nFamilies).toInt - 1

  def familiesToSetID(families: Array[Int]): Int = {
    var id = 0
    for (f <- families) {
      id += math.pow(2, f).toInt
    }
    id
  }

  def setIDToFamilies(originalId: Int): Array[Int] = {
    var id = originalId
    var families = List[Int]()
    var pow2 = 30
    while (id > 0) {
      val pow2Result = 1 << pow2
      if (id >= pow2Result) {
        families ::= pow2
        id -= pow2Result
      }
      pow2 -= 1
    }
    families.toArray
  }

  def allBitSetsOfSize(k : Int, nFamilies : Int): Array[Int] = {

    //Gosper's hack
    // find next k-combination
    def nextCombination(x : Int) : Int = { // assume x has form x'01^a10^b in binary
    val u : Int = x & -x // extract rightmost bit 1; u =  0'00^a10^b
    val v = u + x // set last non-trailing bit 0, and clear to the right; v=x'10^a00^b
      if (v<=0) { // then overflow in v, or x==0
        throw new Exception("the bit set cannot be a negative number")
      }
      v +(((v^x)/u)>>2); // v^x = 0'11^a10^b, (v^x)/u = 0'0^b1^{a+2}, and x ← x'100^b1^a
    }

    def factorial(k :Int): Long = {
      var acc = 1L
      var tmp : Long = k
      while(tmp > 1L) {
        acc *= tmp
        tmp -= 1L
      }
      acc
    }

    def nCombinationsWithoutReplacement(n: Int, k : Int) = factorial(n)/(factorial(k) * factorial (n - k))

    val nSetsOfSizeK = nCombinationsWithoutReplacement(nFamilies, k).toInt

    val sets = Array.ofDim[Int](nSetsOfSizeK)

    // first set is the smallest with k bits set to 1
    sets(0) = math.pow(2, k).toInt - 1

    var s = 1
    while(s < nSetsOfSizeK) {
      sets(s) = nextCombination(sets(s - 1))
      s += 1
    }

    sets
  }

  def isFamilyInSet(f : Int, set : Int) = ((1 << f) & set) != 0

  def minimumFamilyTransition(familyMatrix : Array[Array[Int]]) : Int = {

    var minTrans = Int.MaxValue
    for(i <- familyMatrix.indices ; j <- familyMatrix.indices ; if i != j) {
      minTrans = math.min(minTrans, familyMatrix(i)(j))
    }

    minTrans
  }

}
