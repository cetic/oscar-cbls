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
package oscar.util

/** A quasi-random sequence generator.
  *
  * @constructor Create a new quasi-random sequence genrator using random numbers generated
  *              with the random number generator passed as argument
  * @param rand The random number generator */
class QuasiRandomSequence(rand: scala.util.Random) {
  /** Array containing the 500 first prime numbers (needed for the Halton sequence) */
  val primeNumbers = Array(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71,
      179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281,
      283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409,
      419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541,
      547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659,
      661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809,
      811, 821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941,
      947, 953, 967, 971, 977, 983, 991, 997, 1009, 1013, 1019, 1021, 1031, 1033, 1039, 1049, 1051, 1061, 1063, 1069,
      1087, 1091, 1093, 1097, 1103, 1109, 1117, 1123, 1129, 1151, 1153, 1163, 1171, 1181, 1187, 1193, 1201, 1213, 1217, 1223,
      1229, 1231, 1237, 1249, 1259, 1277, 1279, 1283, 1289, 1291, 1297, 1301, 1303, 1307, 1319, 1321, 1327, 1361, 1367, 1373,
      1381, 1399, 1409, 1423, 1427, 1429, 1433, 1439, 1447, 1451, 1453, 1459, 1471, 1481, 1483, 1487, 1489, 1493, 1499, 1511,
      1523, 1531, 1543, 1549, 1553, 1559, 1567, 1571, 1579, 1583, 1597, 1601, 1607, 1609, 1613, 1619, 1621, 1627, 1637, 1657,
      1663, 1667, 1669, 1693, 1697, 1699, 1709, 1721, 1723, 1733, 1741, 1747, 1753, 1759, 1777, 1783, 1787, 1789, 1801, 1811,
      1823, 1831, 1847, 1861, 1867, 1871, 1873, 1877, 1879, 1889, 1901, 1907, 1913, 1931, 1933, 1949, 1951, 1973, 1979, 1987,
      1993, 1997, 1999, 2003, 2011, 2017, 2027, 2029, 2039, 2053, 2063, 2069, 2081, 2083, 2087, 2089, 2099, 2111, 2113, 2129,
      2131, 2137, 2141, 2143, 2153, 2161, 2179, 2203, 2207, 2213, 2221, 2237, 2239, 2243, 2251, 2267, 2269, 2273, 2281, 2287,
      2293, 2297, 2309, 2311, 2333, 2339, 2341, 2347, 2351, 2357, 2371, 2377, 2381, 2383, 2389, 2393, 2399, 2411, 2417, 2423,
      2437, 2441, 2447, 2459, 2467, 2473, 2477, 2503, 2521, 2531, 2539, 2543, 2549, 2551, 2557, 2579, 2591, 2593, 2609, 2617,
      2621, 2633, 2647, 2657, 2659, 2663, 2671, 2677, 2683, 2687, 2689, 2693, 2699, 2707, 2711, 2713, 2719, 2729, 2731, 2741,
      2749, 2753, 2767, 2777, 2789, 2791, 2797, 2801, 2803, 2819, 2833, 2837, 2843, 2851, 2857, 2861, 2879, 2887, 2897, 2903,
      2909, 2917, 2927, 2939, 2953, 2957, 2963, 2969, 2971, 2999, 3001, 3011, 3019, 3023, 3037, 3041, 3049, 3061, 3067, 3079,
      3083, 3089, 3109, 3119, 3121, 3137, 3163, 3167, 3169, 3181, 3187, 3191, 3203, 3209, 3217, 3221, 3229, 3251, 3253, 3257,
      3259, 3271, 3299, 3301, 3307, 3313, 3319, 3323, 3329, 3331, 3343, 3347, 3359, 3361, 3371, 3373, 3389, 3391, 3407, 3413,
      3433, 3449, 3457, 3461, 3463, 3467, 3469, 3491, 3499, 3511, 3517, 3527, 3529, 3533, 3539, 3541, 3547, 3557, 3559, 3571)
      
  /** Recenters the Halton number found in [0, 1] in the specified interval.
    * 
    * @param halton The Halton number found in the [0, 1] interval
    * @param interval The interval in which we would like to fit the Halton number
    * @return A Double that is the recentered Halton number
    * @example The instruction convertToInterval(0.5, Interval(-20.0, 40.0))
    *          will return 10.0 */
  def convertToInterval(halton: Double, interval: Interval): Double = {
    interval.min + halton * interval.size
  }
  
  /** Returns the random permutation without replacement of the elements in an array
    * 
    * @param arr The array to be permuted
    * @return An array whose elements are permutations without replacements of the
    *         elements of the array specified as parameter */
  def randomPermutation(arr: Array[Double]): Array[Double] = {
    var l = List[Double]()
    for (i <- 0 until arr.length)
      l = l ::: List(arr(i))
    val newArr = Array.fill(arr.length){Double.MaxValue}
    for (i <- 0 until newArr.length) {
      val randIndex = rand.nextInt(l.length)
      newArr(i) = l(randIndex)
      l = l.take(randIndex) ::: l.drop(randIndex + 1)
    }
    newArr
  }
  
  /** Computes the element at the specified index of the Halton sequence of the specified basis
    * 
    * @param index The index of the Halton number to be computed in the quasi-random sequence
    * @param basis The basis in which the quasi-random sequence is computed
    * @return A Double taht is the element at the specified index of the Halton sequence of the
    *         specified basis */
  def halton(index: Int, basis: Int): Double = {
    var result = 0.0
    var factor = 1.0 / basis
    var i = index + 1
    while (i > 0) {
      result = result + factor * (i % basis)
      i /= basis
      factor /= basis
    }
    result
  }
  
  /** Returns the Halton 1D sequence of the specified basis with specified size.
    * 
    * @param basis The basis in which the quasi-random sequence is computed
    * @param interval The interval within which the Halton sequence is computed
    * @param size The size of the Halton sequence
    * @return An array that is the 1D Halton sequence of the specified size and basis
    * @example halton1D(2, Interval(0.0, 1.0), 5) will generate the following array:
    *          Array(0.5, 0.25, 0.75, 0.125, 0.625) */
  def halton1D(basis: Int, interval: Interval, size: Int): Array[Double] = {
     Array.tabulate(size)(i => convertToInterval(halton(i, basis), interval))
  }
  
  /** Returns the Halton sequence of the specified size in the space of dimension specified by the domain.
    * 
    * @param size The size of the Halton sequence
    * @param dom The domain of the space in which the Halton sequence is computed
    * @return An array of arrays of double representing the Halton sequence of the dimension specified by
    *         the parameters. Every element of the returned array is an array representing a 1D
    *         Halton sequence with bases different from each other
    * @example haltonSequence(5, Array(Interval(0.0, 1.0), Interval(0.0, 1.0))) will generate the following array:
    *          Array(
    *            Array(0.5, 0.25, 0.75, 0.125, 0.625),
    *            Array(0.333, 0.666, 0.111, 0.444, 0.777)) */
  def haltonSequence(size: Int, dom: Array[Interval]): Array[Array[Double]] = {
    val bases = Array.tabulate(dom.length)(i => primeNumbers(i))
    val halt1D = Array.tabulate(dom.length)(i => halton1D(bases(i), dom(i), size))
    Array.tabulate(size)(i => Array.tabulate(dom.length)(j => halt1D(j)(i)))
  }
  
  /** Returns the scrambled Halton sequence of the specified size in the space of dimension specified by the domain.
    * 
    * @param size The size of the Halton sequence
    * @param dom The domain of the space in which the Halton sequence is computed
    * @return An array of arrays of double representing the scrambled Halton sequence of the
    *         dimension specified by the parameters. Every element of the returned array is an array representing a 1D
    *         Halton sequence with bases different from each other */
  def scrambledHaltonSequence(size: Int, dom: Array[Interval]): Array[Array[Double]] = {
    val bases = Array.tabulate(dom.length)(i => primeNumbers(i))
    var halt1D = Array.tabulate(dom.length)(i => halton1D(bases(i), dom(i), size))
    for(i <- 0 until halt1D.length)
      halt1D(i) = randomPermutation(halt1D(i))
    Array.tabulate(size)(i => Array.tabulate(dom.length)(j => halt1D(j)(i)))
  }
}

/** Factory for the QuasiRandomSequence instances. */
object QuasiRandomSequence {
  /** Create a QuasiRandomSequence with the specified random number generator
    *
    * @param rand The random number generator used to scramble the Halton sequences
    * @return a QuasiRandomSequence with the spacified random number generator */
  def apply(rand: scala.util.Random) = new QuasiRandomSequence(rand)
}
