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

package oscar.cbls.benchmarks

import oscar.cbls._
import oscar.cbls.core.constraint.ConstraintSystem
import oscar.cbls.lib.constraint.AllDiff
import oscar.cbls.lib.invariant.logic.SelectLESetQueue
import oscar.cbls.lib.invariant.minmax._
import oscar.cbls.lib.search.LinearSelectorClass
import oscar.cbls.util.StopWatch

import scala.util.Random

//Beware: this requires a lot of memory, so I use to put this in the command line.
//-Xms1000M -Xmx1000M

/**
 * NQueen for larger problems : 
 * - queens are always on different rows 
 * - neightboorhood is queen swap
 * - first queeen is among most violated ones; maintained through invariant
 * - second is first one leading to a decrease in violation 
 * when swap is performed tabu on moved queens.
 * - jump away through random swap if plateau
 *
 * The program accepts an argument which is the problem size
 * Otherwise it performs a benchmarking over a range of sizes (this takes time)
 */
object NQueensBench extends LinearSelectorClass(true) with StopWatch{

  def nStrings(N: Long, C: String): String = if (N <= 0) "" else s"$C${nStrings(N - 1, C)}"
  def padToLength(s: String, l: Int) = (s + nStrings(l, " ")).substring(0, l)

  val help = "Benchmarking NQueen \n" +
    "advise: specify -Xms1000M -Xmx1000M (eg. 2000M if going for 50k Queens)\n" +
    "takes four parameters or zero\n"+
    "if zero parameters, solves 1k, 2k ... 10k queens, with an extra dry run of 1k queen at the beginning\n"+
    "if four parameters: nQueen nRun random dryRun (they must all be present)\n"+
    "example: 50000 10 0 0\n"+
    "nQueen is the number of queens to test\n"+
    "nRun is the number of time this should be tested\n"+
    "dryRun: 0 for no dry run, 1 for a dry run\n"+
    "random 1 for a real random, 0 for a pseudo-random. pseudo-random it will exhibit the same trajectory every time you call the bench\n"

  def main(args: Array[String]): Unit ={

    println(help)
    if (args.length<1) {
      println("Benchmarking NQueen - this takes time")
      println(padToLength("N", 15) + padToLength("tClose[ms]", 15) + padToLength("tTotal[ms]", 15) + "it")

      // first run could have some overhead so ignoring it
      SolveNQueen(1000,Random)

      // multiple runs
      for (n <- 1000 to 10000 by 1000){
        SolveNQueen(n,Random)
        System.gc()
      }

    } else {
      val N:Int=args(0).toInt
      val nRun = args(1).toInt
      val pseudoRandom = args(2).toInt == 0
      val r:Random = if(args(2).toInt == 0) new Random(0) else new Random(System.currentTimeMillis())
      val dryRun = args(3).toInt != 0
      println("nQueen:" + N + " nRun:" + nRun + (if (dryRun) " withDryRun" else " noDryRun") + (if(pseudoRandom) " deterministicRandom" else " realRandom"))
      println(padToLength("N", 15) + padToLength("tClose[ms]", 15) + padToLength("tTotal[ms]", 15) + "it")
      if(dryRun) SolveNQueen(1000,r)
      for(i <- 1 to nRun)
        SolveNQueen(N,r)
    }
  }

  def SolveNQueen(N:Int, r:Random): Unit ={
    print(padToLength("" + N, 15))

    startWatch()
    val queensRange:Range = Range(0,N)
    val tabulength = 10

    val m = Store()
    val init = Random.shuffle(queensRange.toList).iterator
    val queens:Array[CBLSIntVar] = queensRange.map(q => CBLSIntVar(m, init.next(), 0 until N,"queen" + q)).toArray

    val c = ConstraintSystem(m)

    // c.post(AllDiff(Queens))
    // enforced because we swap queens and they are always alldiff
    c.post(AllDiff(queensRange.map(q => queens(q) + q)))
    c.post(AllDiff(queensRange.map(q => q - queens(q))))

    val tabu = queensRange.map(q => CBLSIntVar(m, 0, 0 to Int.MaxValue, "Tabu_queen" + q)).toArray
    val it = CBLSIntVar(m,1, 0 to Int.MaxValue,"it")
    val nonTabuQueens = SelectLESetQueue(tabu, it).setName("non tabu queens")
    val nonTabuMaxViolQueens = ArgMax(c.violations(queens), nonTabuQueens)

    m.close()
    print(padToLength("" + getWatch, 15))

    while(c.violation.value > 0){
      require(it.value < N, "NQueens seems to diverge: " + it + " N "+ N)
      val oldviolation:Long = c.violation.value

      selectFirstDo(nonTabuMaxViolQueens.value)((q1:Int) => {
        selectFirstDo(nonTabuQueens.value, (q2:Int) => {
          q2!=q1 && c.swapVal(queens(q1),queens(q2)) < oldviolation
        })((q2:Int) => {
          //println("" + it.value + " swapping " + q1 +"(tabu: " + tabu(q1) + ") and " + q2 +"(tabu: " + tabu(q2) + ")")
          queens(q1) :=: queens(q2)
          tabu(q1) := it.value + tabulength
          tabu(q2) := it.value + tabulength

        },()=>println("Warning: Tabu it too big compared to queens count"))},
        ()=>println("Warning: Tabu it too big compared to queens count"))

      it ++

    }

    println(padToLength("" + getWatch, 15) + it.value)
    //println(m.stats)
  }
}
