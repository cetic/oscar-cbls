package oscar.cp.examples.userguide

import oscar.cp._

object QuadraticAssignmentLNS extends CPModel with App {

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
  val bestSol = Array.fill(n)(0)
  val D = Array.tabulate(n, n)((i, j) => d(x(i))(x(j))) //matrix of variables   representing the distances

  add(allDifferent(x), Strong)
  minimize(sum(0 until n,0 until n)((i, j) => D(i)(j) * w(i)(j)))



  onSolution {
    (0 until n).foreach(i => bestSol(i) = x(i).value)
  }
  search {
    binaryFirstFail(x,_.randomValue)
  } start(nSols = 1)

  for (r <- 1 to 100) {
    // do a new run, not limiting number of solution and with 60 backtrack limit
    startSubjectTo(failureLimit = 1000) {
      // relax randomly 50% of the variables
      add((0 until n).filter(i => random.nextInt(100) < 50).map(i => x(i) === bestSol(i)))
    }
  }

}

