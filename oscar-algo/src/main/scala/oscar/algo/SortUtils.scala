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
package oscar.algo

import scala.reflect.ClassTag
import scala.math.Ordering
import scala.math.{min, max}

object SortUtils {
  
	def stableSort[K: ClassTag](a: Array[K], l : Int, r : Int, f: (K, K) => Boolean) = {

		stableSort1(a, l, r-1, new Array[K](a.length), f)
	}

	private def stableSort1[K : ClassTag](a: Array[K], lo: Int, hi: Int, scratch: Array[K], f: (K,K) => Boolean): Unit = {
	    if (lo < hi) {
			val mid = (lo+hi) / 2
			stableSort1(a, lo, mid, scratch, f)
			stableSort1(a, mid+1, hi, scratch, f)
			var k, t_lo = lo
			var t_hi = mid + 1
			while (k <= hi) {
			    if ((t_lo <= mid) && ((t_hi > hi) || (!f(a(t_hi), a(t_lo))))) {
			    	scratch(k) = a(t_lo)
			    	t_lo += 1
			    } else {
			    	scratch(k) = a(t_hi)
			    	t_hi += 1
			    }
			    k += 1
			}
			k = lo
			while (k <= hi) {
			    a(k) = scratch(k)
			    k += 1
			}
	    }
	}
	
	/* Sorts the elements e in nondecreasing order according to their keys[e].
	 * This is a stable sort.
	 * @param elements have to be in [0, keys.length)
	 */
	final def mergeSort(elements: Array[Int], keys: Array[Int]): Unit = {
	  mergeSort(elements, keys, 0, elements.length)
	}
	
	
	/* Sorts the elements e in nondecreasing order according to their keys[e],
	 * but only those in [base, topExcluded) 
	 * This is a stable sort.
	 * @param elements whose index are in [base, topExcluded) have to be in [0, keys.length)
	 */
	final def mergeSort(elements: Array[Int], keys: Array[Int], base: Int, topExcluded: Int): Unit = {
	  val n = elements.length
	  val runs = new Array[Int](n+1)
    val aux = new Array[Int](n)
    
	  mergeSort(elements, keys, base, topExcluded, runs, aux)
	}

	  
	final def mergeSort(elements: Array[Int], keys: Array[Int], base: Int, topExcluded: Int, runs: Array[Int], aux: Array[Int]): Unit = {
	  val n = elements.length
	  assert(base >= 0)
	  assert(topExcluded <= n)
	  assert(elements slice(base, topExcluded) forall { e => e >= 0 && e < keys.length },
	         "mergeSort input error: elements whose index are in [base, topExcluded) have to be in [0, keys.length)")
	  assert(runs.length >= n + 1)
	  assert(aux.length >= n)
	         
	  if (topExcluded - base > 1) {
  	  // runs holds the size of successive increasing runs, initialize it
	    var el = base
	    var rSize = 1
	    var rP = 0
	    
	    do {
	      // invariant: there must be a nonempty increasing run: find its size and stack it in runs
	      el += 1
	      while(el < topExcluded && keys(elements(el-1)) <= keys(elements(el))) {
	        rSize += 1
	        el += 1
	      }
	      runs(rP) = rSize
	      rSize = 1
	      rP += 1
	    } while(el < topExcluded)
	    runs(rP) = 0
	      
      if (rP > 1) {  // array is not sorted
        var finalBase = base
        var finalTop = topExcluded
        
        // if there are little runs, surely we can find elements that do not need to be sorted
        // typically useful when rP == 2, which is the case targeted by this sorting procedure
        if (rP <= 4) {  
          // maybe 
          var minLocation = base + runs(0)
          var minUnsorted = keys(elements(minLocation))
          
          var runP = 1
          while (runP < rP - 1) {
            minLocation += runs(runP)
            minUnsorted = min(minUnsorted, keys(elements(minLocation)))
            runP += 1
          }
        
          var maxLocation = minLocation - 1
          var maxUnsorted = keys(elements(maxLocation))          
          
          runP -= 1  // runP = rP - 2
          
          while (runP > 0) {
            maxLocation -= runs(runP)
            maxUnsorted = max(maxUnsorted, keys(elements(maxLocation)))
            runP -= 1  // we don't want 0!
          }

          while(keys(elements(finalBase)) < minUnsorted) finalBase += 1
          while(keys(elements(finalTop - 1)) > maxUnsorted) finalTop -= 1
          runs(0) -= finalBase - base
          runs(rP - 1) -= topExcluded - finalTop
        } 
        
	      val whichArray = mergeSort1(elements, aux, keys, runs, rP + 1, finalBase, 0)
	      if (whichArray == 1) System.arraycopy(aux, finalBase, elements, finalBase, finalTop - finalBase)
	    }
	  }
	}
	
	@annotation.tailrec
	@inline
	private final def mergeSort1(tab1: Array[Int], tab2: Array[Int], keys: Array[Int], runs: Array[Int], runsSize: Int, base: Int, which: Int): Int = {
    var runP = 0
    var baseP = base

    // take next two runs, merge them into tab2, repeat
    while (runP + 1 < runsSize) {
      var list1P = baseP
      val limit1 = baseP + runs(runP)
      var list2P = limit1
      val limit2 = limit1 + runs(runP + 1)
      var tab2P = baseP
      
      // merge the two lists until a list is empty
      while (list1P < limit1 && list2P < limit2) {
        val e1 = tab1(list1P)
        val e2 = tab1(list2P)
        if (keys(e1) <= keys(e2)) {
          tab2(tab2P) = e1
          list1P += 1
        }
        else {
          tab2(tab2P) = e2
          list2P += 1
        }
        tab2P += 1
      }
      
      // copy the remainder of the list
      if (list1P < limit1) System.arraycopy(tab1, list1P, tab2, tab2P, limit1 - list1P)
      else                 System.arraycopy(tab1, list2P, tab2, tab2P, limit2 - list2P)
      
      baseP = limit2
      runs(runP >> 1) = runs(runP) + runs(runP + 1)
      runP += 2
    }
    
    // if runsSize was odd, just stick last element at the end
    if (runP < runsSize) {
      runs(runP >> 1) = runs(runP)
      System.arraycopy(tab1, baseP, tab2, baseP, runs(runP))
    }
  
    val newRunsSize = (runsSize + 1) >> 1
	  if (newRunsSize == 1)  1 - which
	  else                   mergeSort1(tab2, tab1, keys, runs, newRunsSize, base, 1 - which)
	}
	
	
	
	def main(args: Array[String]): Unit = {
		
		val a = Array(5,6,2,9,6,8,3)
		stableSort(a, 0, 4, (a : Int,b:Int) => a < b)
		println(a.mkString(","))
		
		val b = Array(5,6,2,9,6,8,3,1,5,4,9,7,9,21,2,56,9,4,8,2,8,4,9,5,4,8,4,1,89,1,47,9,2,4,8,5,1,8)
		val bi = b.indices.toArray
		mergeSort(bi, b)
		println(bi.map(b).mkString(","))
		// println(bi.mkString(","))
		
	  val c = Array(5,6,2,9,6,8,3,1,5,4,9,7,9,21,2,56,9,4,8,2,8,4,9,5,4,8,4,1,89,1,47,9,2,4,8,5,1,8)
		val ci = b.indices.toArray
		mergeSort(ci, c, 4, 10)
		println(ci.map(c).mkString(","))
		// println(ci.mkString(","))

	}
}