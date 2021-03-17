package oscar.cp.examples.userguide

import oscar.cp._

object QuadraticAssignment extends CPModel with App {

  val n = 9

  var w: Array[Array[Int]] = Array.ofDim(n,n)  //weight matrix
  var d: Array[Array[Int]] = Array.ofDim(n,n) //distance matrix

  // fill distance and weight matrix randomly
  val random = solver.getRandom
  for (i <- 0 until n; j <- i+1 until n) {
    w(i)(j) = random.nextInt(100)
    w(j)(i) = w(i)(j)
    d(i)(j) = random.nextInt(100)
    d(j)(i) = d(i)(j)
  }

  val x = Array.fill(n)(CPIntVar(0 until n))
  val D = Array.tabulate(n, n)((i, j) => d(x(i))(x(j))) //matrix of variables   representing the distances

  add(allDifferent(x), Strong)
  minimize(sum(0 until n,0 until n)((i, j) => D(i)(j) * w(i)(j)))


  search {
    binaryFirstFail(x)
  }
  println(start())

}

