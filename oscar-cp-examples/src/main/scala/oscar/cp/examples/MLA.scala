package oscar.cp.examples

import oscar.cp.{CPIntVar, CPSolver, Strong, add, allDifferent, binaryFirstFail, minimize, onSolution, search, start, startSubjectTo, sum}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
object MLA extends App {

  var lines = Source.fromFile("data/mla/gd95c.rmf").getLines.toArray

  val vv = new ArrayBuffer[Int]()
  val ev = new ArrayBuffer[(Int,Int)]()

  val l = lines.head
  lines = lines.drop(1)

  val Array(n,ne) = l.split("[ ]").filterNot(_ == "").drop(2).map(_.toInt)



  for (l <- lines) {
    val values = l.drop(1).split("[ ]").filterNot(_ == "")
    val edge = (values(0).toInt,values(1).toInt)
    println(edge)
    ev.append(edge)

  }


	val adjacency = Array.fill(n,n)(false)
  for ((i,j) <- ev) {
    adjacency(i-1)(j-1) = true
    adjacency(j-1)(i-1) = true
  }


	implicit val cp = CPSolver()
	val x = Array.tabulate(n)(i => CPIntVar(0 until n))

	add(allDifferent(x),Strong)
	val distance = for (i <- 0 until n; j <- i+1 until n; if (adjacency(i)(j))) yield {
	  (x(i)-x(j)).abs
	}


  val obj = sum(distance)



	minimize(obj)


	val bestSolution = Array.fill(n)(0)
	val t0 = System.currentTimeMillis()
  onSolution {
    println("newsol time:"+(System.currentTimeMillis()-t0)/1000+" s objective:"+obj)
	  //println(Array.tabulate(n)(i => (i+1,x(i).value+1)).mkString(","))
	  for (i <- 0 until n) {
	    bestSolution(i) = x(i).value
	  }

    //for (i <- 0 until n) {
      //println(i+1+"->"+(x(i).value+1))
    //}
	}


	search {
	  binaryFirstFail(x,_.randomValue)
	}


	val stat = start(nSols=1)



  val rand = new scala.util.Random(0)

	for (iter <- 0 until 10000000) {
	  val stat = startSubjectTo(failureLimit = 100) {
      add((for (i <- 0 until n; if (rand.nextInt(100) > 10)) yield x(i) === bestSolution(i)))

	  }
    //print(if (stat.completed) "!" else "R")

	}


}
