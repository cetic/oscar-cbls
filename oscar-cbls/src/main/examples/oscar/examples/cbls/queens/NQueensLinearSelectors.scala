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

package oscar.examples.cbls.queens

import oscar.cbls._
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
object NQueensLinearSelectors extends LinearSelectorClass(true) with StopWatch{

  def nStrings(N: Int, C: String): String = if (N <= 0) "" else s"$C${nStrings(N - 1, C)}"
  def padToLength(s: String, l: Int): String = (s + nStrings(l, " ")).substring(0, l)

  def main(args: Array[String]) {

    if (args.length<1) {
      println("Benchmarking NQueens - this takes time")
      println("advise: put -Xms1000M -Xmx1000M")
      println(padToLength("N", 15) + padToLength("tClose[ms]", 15) + padToLength("tTotal[ms]", 15) + "it")

      // first run could have some overhead so ignoring it
      SolveNQueen(1000)

      // multiple runs
      for (n <- 1000 to 10000 by 1000){
        SolveNQueen(n)
        System.gc()
      }

    } else {
      val N:Int=args(0).toInt
      println("Running NQueens - this takes time depending on N")
      println(padToLength("N", 15) + padToLength("tClose[ms]", 15) + padToLength("tTotal[ms]", 15) + "it")
      SolveNQueen(1000)
      for(_ <- 1 to 5)
        SolveNQueen(N)
    }
  }

  def SolveNQueen(N:Int){
    print(padToLength(s"$N", 15))

    startWatch()
    val queensRange:Range = Range(0,N)
    val tabulength = 10

    val m = Store()
    val init = Random.shuffle(queensRange.toList).iterator
    val queens:Array[CBLSIntVar] = queensRange.map(q => CBLSIntVar(m, init.next(), 0 until N, s"queen$q")).toArray

    val c = ConstraintSystem(m)

    //c.post(AllDiff(Queens)) //enforced because we swap queens and they are always alldiff
    c.post(AllDiff(queensRange.map(q => queens(q) + q)))
    c.post(AllDiff(queensRange.map(q => q - queens(q))))

    val tabu = queensRange.map(q => CBLSIntVar(m, 0, 0 to Int.MaxValue, s"Tabu_queen$q")).toArray
    val it = CBLSIntVar(m,1, 0 to Int.MaxValue,"it")
    val nonTabuQueens = SelectLESetQueue(tabu, it).setName("non tabu queens")
    val nonTabuMaxViolQueens = ArgMax(c.violations(queens), nonTabuQueens)

    m.close()
    print(padToLength(s"$getWatch", 15))

    while(c.violation.value > 0){
      require(it.value < N, s"NQueens seems to diverge: $it N $N")
      val oldviolation:Int = c.violation.valueInt

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

      it ++ ()

    }

    println(padToLength(s"$getWatch", 15) + it.value)
    //println(m.stats)
  }
}
