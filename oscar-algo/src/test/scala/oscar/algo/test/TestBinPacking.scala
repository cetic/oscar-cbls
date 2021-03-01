package oscar.algo.test

import org.scalatest.{FunSuite, Matchers}
import oscar.algo.BinPacking

import scala.collection.mutable.ArrayBuffer

class TestBinPacking extends FunSuite with Matchers {
  
  
  def generateInstance1(c: Int, n: Int) = {
     // we fill n bin with capa c completely (with random items)
     val c = 10
     val n = 100
     
     val rand = new scala.util.Random(0)
     
     val bins = for (i <- 0 until n) yield {
       var l = 0
       var items = new ArrayBuffer[Int]()
       while (l < c) {
         val item = (3+rand.nextInt(c)).min(c-l)
         l += item
         items += item
       }
       items.toArray
     }
     val items = bins.flatMap(l => l).sortBy(i => -i)
     items 
  }
  
  
  test("test first-fit") {
	  for (i <- 0 until 400) {
	    val items = generateInstance1(10,100).toArray
	    val res = BinPacking.firstFitDecreasing(items, 10)
	    val ub = res.max+1
	    ub should be(100)
	  }
  }  
 
  
  test("test lower bound") {
    
	  for (i <- 0 until 400) {
	    val items = generateInstance1(10,100).toArray
	    val lb = BinPacking.labbeLB(items, 10)
	    lb should be(100)
	  }
  }
  
  
  test("test lower bound compared to first fit") {
      val rand = new scala.util.Random()
      
      def test(items: Array[Int]): Unit = {
	    val lb = BinPacking.labbeLB(items, 10)
	    val ub = BinPacking.firstFitDecreasing(items, 10).max + 1
	    lb should be <=(ub)
	    lb should be >=(ub*3/4)        
      }
	  for (i <- 0 until 1000) {
	    test(Array.fill(500)(rand.nextInt(7)+3).sortBy(-_))
	    test(Array.fill(500)(rand.nextInt(3)+1).sortBy(-_))
	    test(Array.fill(500)(rand.nextInt(4)+5).sortBy(-_))
	    test(Array.fill(10)(rand.nextInt(10)+1).sortBy(-_))
	  }
	  
  }  

}
