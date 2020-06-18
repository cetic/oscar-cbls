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
/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Renaud De Landtsheer
  ******************************************************************************/

package oscar.cbls.lib.search

import scala.util.Random

/**Provides a set of selectors to ease the development of search engines
  * @param isRandomized can be set to false if one wants a reproductible behavior of the search engine
  * @author renaud.delandtsheer@cetic.be
  * */
class LinearSelectorClass(isRandomized: Boolean = true) extends LinearSelectors{
  setRandomized(isRandomized)
}

trait LinearSelectors{
  var RandomGenerator: Random = null

  var Randomized:Boolean = true
  setRandomized(true)
  def setRandomized(Randomized:Boolean): Unit ={
    this.Randomized = Randomized

    RandomGenerator = if (Randomized) { new Random() } else { new Random(0L) }
  }

  @inline
  private def flattenTwoIterables[R,S](r: Iterable[R],
                                       s: Iterable[S]):Iterable[(R,S)]={
    val flattened = for (rr <- r.iterator; ss <- s.iterator) yield (rr,ss)
    // Scala 2.12
    flattened.toIterable
    // Scala 2.13
    //flattened.iterator.to(Iterable)
  }

  /**return a couple (r,s) that is allowed: st(r,s) is true, and maximizing f(r,s) among the allowed couples
    * this selector is not randomized; in case of tie breaks the first one is returned
    * @param st is optional and set to true if not specified
    */
  def selectMaxNR2[R,S](r: Iterable[R],
                        s: Iterable[S],
                        f: (R,S) => Long,
                        st: (R,S) => Boolean = (_:R, _:S) => true): (R,S) = {
    selectMaxNR[(R,S)](flattenTwoIterables(r,s), (rands:(R,S)) => f(rands._1,rands._2), (rands:(R,S)) => st(rands._1,rands._2))
  }

  /**return an element r that is allowed: st(r) is true, and maximizing f(r) among the allowed couples
    * this selector is not randomized; in case of tie breaks the first one is returned
    * @param st is optional and set to true if not specified
    */
  def selectMaxNR[R](r: Iterable[R],
                     f: R => Long,
                     st: R => Boolean = (_:R) => true): R = {
    var GotOne: Boolean = false
    var MaxSoFar = 0L
    var Val:R = null.asInstanceOf[R]
    for (i <- r) {
      if (st(i)) {
        val v = f(i)
        if (v > MaxSoFar || !GotOne) {
          GotOne = true
          Val = i
          MaxSoFar = v
        }
      }
    }
    if(GotOne)Val else null.asInstanceOf[R]
  }

  /**return a couple (r,s) that is allowed: st(r,s) is true, and minimizing f(r,s) among the allowed couples
    * this selector is not randomized; in case of tie breaks the first one is returned
    * @param st is optional and set to true if not specified
    */
  def selectMinNR2[R,S](r: Iterable[R],
                        s: Iterable[S],
                        f: (R,S) => Long,
                        st: (R,S) => Boolean = (_:R, _:S) => true): (R,S) = {
    selectMaxNR2[R,S](r , s, -f(_,_), st)
  }

  /**return an element r that is allowed: st(r) is true, and minimizing f(r) among the allowed couples
    * this selector is not randomized; in case of tie breaks the first one is returned
    * @param st is optional and set to true if not specified
    */
  def selectMinNR[R](r: Iterable[R],
                     f: R => Long,
                     st: R => Boolean = (_:R) => true): R = {
    selectMaxNR[R](r , -f(_), st)
  }

  /**return a couple (r,s) that is allowed: st(r,s) is true, and minimizing f(r,s) among the allowed couples
    * this selector is randomized; in case of tie breaks the returned one is chosen randomly
    * @param st is optional and set to true if not specified
    * @param stop earlier stop if a couple (and output value is found)
    */
  def selectMin2[R,S](r: Iterable[R],
                      s: Iterable[S],
                      f: (R,S) => Long,
                      st: (R,S) => Boolean = (_:R, _:S) => true,
                      stop: (R,S,Long) => Boolean = (_:R,_:S,_:Long) => false): (R,S) = {
    selectMax2[R,S](r , s, -f(_,_), st, stop)
  }

  /**return a couple (r,s) that is allowed: st(r,s) is true, and maximizing f(r,s) among the allowed couples
    * this selector is randomized; in case of tie breaks the returned one is chosen randomly
    * @param st is optional and set to true if not specified
    */
  def selectMax2[R,S](r: Iterable[R],
                      s: Iterable[S],
                      f: (R,S) => Long,
                      st: (R,S) => Boolean = (_:R, _:S) => true,
                      stop: (R,S,Long) => Boolean = (_:R,_:S,_:Long) => false): (R,S) = {
    selectMax[(R,S)](flattenTwoIterables(r,s), (rands:(R,S)) => f(rands._1,rands._2), (rands:(R,S)) => st(rands._1,rands._2))
  }

  /**return an element r that is allowed: st(r) is true, and maximizing f(r) among the allowed couples
    * this selector is randomized; in case of tie breaks the returned one is chosen randomly
    * @param st is optional and set to true if not specified
    */
  def selectMax[R](r: Iterable[R],
                   f: R => Long,
                   st: R => Boolean = (_:R) => true,
                   stop: (R,Long) => Boolean = (_:R,_:Long) => false): R = {
    var MaxSoFar = Long.MinValue
    var Val:List[R] = List.empty
    for (i <- r) {
      if (st(i)) {
        val v:Long = f(i)
        if(stop(i,v)){
          return i
        }
        if (v > MaxSoFar || Val.isEmpty) {
          Val = List(i)
          MaxSoFar = v
        } else if (Val.nonEmpty && v == MaxSoFar) {
          Val = i :: Val
        }
      }
    }
    if(Val.nonEmpty){
      Val.apply(RandomGenerator.nextInt(Val.length))
    } else null.asInstanceOf[R]
  }

  /**return a couple (r,s) that is allowed: filter(r,s) is true, and minimizing f(r,s) among the allowed couples
    * this selector is randomized; in case of tie breaks the returned one is chosen randomly
    */
  def selectMin[R,S](r: Iterable[R],
                     s: Iterable[S])
                    (f: (R,S) => Long,
                     filter: (R,S) => Boolean): (R,S) = {
    selectMin[(R,S)](flattenTwoIterables(r,s))((rands:(R,S)) => f(rands._1,rands._2), (rands:(R,S)) => filter(rands._1,rands._2))
  }

  /**return an element r that is allowed: st(r) is true, and minimizing f(r) among the allowed couples
    * this selector is randomized; in case of tie breaks the returned one is chosen randomly
    * @param st is optional and set to true if not specified
    */
  def selectMin[R](r: Iterable[R])
                  (f: R => Long,
                   st: R => Boolean = (_:R) => true): R = {
    var MinSoFar = Long.MaxValue
    var Val:List[R] = List.empty
    for (i <- r) {
      if (st(i)) {
        val v = f(i)
        if (v < MinSoFar || Val.isEmpty) {
          Val = List(i)
          MinSoFar = v
        } else if (Val.nonEmpty && v == MinSoFar) {
          Val = i :: Val
        }
      }
    }

    if(Val.nonEmpty){
      Val.apply(RandomGenerator.nextInt(Val.length))
    } else null.asInstanceOf[R]
  }

  /**return a couple (r,s) of elements r that is allowed: st(r,s) is true
    * this selector is randomized; in case of tie breaks the returned one is chosen randomly
    * @param st is optional and set to true if not specified
    */
  def selectFrom2[R,S](r: Iterable[R],
                       s: Iterable[S],
                       st: (R,S) => Boolean = (_:R, _:S) => true): (R,S) = {
    selectFrom[(R,S)](flattenTwoIterables(r,s), (rands:(R,S)) => st(rands._1,rands._2))
  }

  /**return an element r that is allowed: st(r) is true
    * this selector is randomized; in case of tie breaks the returned one is chosen randomly
    * @param st is optional and set to true if not specified
    */
  def selectFrom[R](r: Iterable[R],
                    st: R => Boolean = null): R = {
    if (st == null){
      var i = RandomGenerator.nextInt(r.size)
      val it = r.iterator
      while(i > 0L){it.next(); i-=1}
      it.next()
    }else{
      val emptyRlist:List[R]=List.empty
      val filteredList:List[R] = r.foldLeft(emptyRlist)((acc,rr) => if (st(rr)) rr :: acc else acc)
      if (filteredList.isEmpty) return null.asInstanceOf[R]
      val i = RandomGenerator.nextInt(filteredList.size)
      filteredList(i)
    }
  }

  /**return an element of the range. IT is selected randomly with a uniform distribution
    * this is performed in O(1)
    */
  def selectFromRange(r: Range): Int = {
    val i = RandomGenerator.nextInt(r.size)
    r.apply(i)
  }

  /**return the first couple of elements (r,s) that is allowed: st(r) is true
    *the order is lexicographic
    * @param st is optional and set to true if not specified
    */
  def selectFirst2[R,S](r: Iterable[R],
                        s: Iterable[S],
                        st: (R,S) => Boolean = (_:R, _:S) => true): (R,S) = {
    selectFirst[(R,S)](flattenTwoIterables(r,s), (rands:(R,S)) => st(rands._1,rands._2))
  }

  /**return the first element r that is allowed: st(r) is true
    * @param st is optional and set to true if not specified
    */
  def selectFirst[R](r: Iterable[R],
                     st: R => Boolean = (_:R) => true): R = {
    for(rr <- r) if(st(rr)) return rr
    null.asInstanceOf[R]
  }

  /**return the first element r that is allowed: st(r) is true
    * @param st is optional and set to true if not specified
    */
  def selectFirstDo[R](r: Iterable[R],
                       st: R => Boolean = (_:R) => true)
                      (doIt: R => Unit,
                       ifNone: ()=> Unit = ()=>{println("no suitable item found")}): Unit = {
    val rit = r.iterator
    while(rit.hasNext) {
      val rr = rit.next()
      if (st(rr)) {
        doIt(rr)
        return
      }
    }
    ifNone()
  }

  /**returns a randomly chosen boolean (50L%-50L%)*/
  def flip(PercentTrue:Int = 50):Boolean = RandomGenerator.nextInt(100) < PercentTrue

  /**returns a random permutation of the integers in [0; N]*/
  def getRandomPermutation(N:Int):Iterator[Int] = {
    val intarray:Array[Int] = new Array(N)
    for(i <- 0 until N) intarray(i)=i
    for(i <- 0 until N){
      val other = selectFromRange(0 until N)
      val old = intarray(i)
      intarray(i) = intarray(other)
      intarray(other) = old
    }
    intarray.iterator
  }
}
