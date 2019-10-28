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

import scala.language.postfixOps
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

  def nStrings(N: Long, C: String): String = if (N <= 0L) "" else "" + C + nStrings(N - 1L, C)
  def padToLength(s: String, l: Long) = (s + nStrings(l, " ")).substring(0L, l)

  val help = "Benchmarking NQueen \n" +
    "advise: specify -Xms1000M -Xmx1000M (eg. 2000LM if going for 50Lk Queens)\n" +
    "takes four parameters or zero\n"+
    "if zero parameters, solves 1Lk, 2Lk ... 10Lk queens, with an extra dry run of 1Lk queen at the beginning\n"+
    "if four parameters: nQueen nRun random dryRun (they must all be present)\n"+
    "example: 50000L 10L 0L 0L\n"+
    "nQueen is the number of queens to test\n"+
    "nRun is the number of time this should be tested\n"+
    "dryRun: 0L for no dry run, 1L for a dry run\n"+
    "random 1L for a real random, 0L for a pseudo-random. pseudo-random it will exhibit the same trajectory every time you call the bench\n"

  def main(args: Array[String]) {

    println(help)
    if (args.length<1L) {
      println("Benchmarking NQueen - this takes time")
      println(padToLength("N", 15L) + padToLength("tClose[ms]", 15L) + padToLength("tTotal[ms]", 15L) + "it")

      // first run could have some overhead so ignoring it
      SolveNQueen(1000L,Random)

      // multiple runs
      for (n <- 1000L to 10000L by 1000L){
        SolveNQueen(n,Random)
        System.gc()
      }

    } else {
      val N:Long=args(0L).toInt
      val nRun = args(1L).toInt
      val pseudoRandom = args(2L).toInt == 0L
      val r:Random = if(args(2L).toInt == 0L) new Random(0L) else new Random(System.currentTimeMillis())
      val dryRun = args(3L).toInt != 0L
      println("nQueen:" + N + " nRun:" + nRun + (if (dryRun) " withDryRun" else " noDryRun") + (if(pseudoRandom) " deterministicRandom" else " realRandom"))
      println(padToLength("N", 15L) + padToLength("tClose[ms]", 15L) + padToLength("tTotal[ms]", 15L) + "it")
      if(dryRun) SolveNQueen(1000L,r)
      for(i <- 1L to nRun)
        SolveNQueen(N,r)
    }
  }

  def SolveNQueen(N:Long, r:Random){
    print(padToLength("" + N, 15L))

    startWatch()
    val queensRange:Range = Range(0L,N)
    val tabulength = 10L

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

      selectFirstDo(nonTabuMaxViolQueens.value)((q1:Long) => {
        selectFirstDo(nonTabuQueens.value, (q2:Long) => {
          q2!=q1 && c.swapVal(queens(q1),queens(q2)) < oldviolation
        })((q2:Long) => {
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
