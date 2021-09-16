package oscar.examples.cbls.userguide

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

import oscar.cbls._

import scala.util.Random

object NQueensBasic extends CBLSModel with App{

  val nQueens = 100
  println(s"NQueensBasic($nQueens)")

  val queensRange:Range = Range(0,nQueens)
  val init = Random.shuffle(queensRange.toList).iterator

  //declaring the variables of the problem, that is an array of CBLSIntVar
  val queens = queensRange.map(q => CBLSIntVar(init.next(), 0 until nQueens, s"queen$q")).toArray

  //posting the constraints of the problem.
  //They are added to the default constraint system, which is used as a weak constraint (as a lagrangian relaxation, so to say)
  c.post(allDiff(queensRange.map(q => queens(q) + q)))
  c.post(allDiff(queensRange.map(q => q - queens(q))))

  //the model is complete, close it
  s.close()
  println("model closed, start search " + getWatchString)

  //search procedure: swaps the queens so as to decrease the violation degree as much as possible
  var it = 0
  while(c.violation.value > 0 && it < nQueens){

    val (q1,q2) = selectMin(queensRange,queensRange)(
      (p,q) => c.swapVal(queens(p),queens(q)),
      (p,q) => p < q)

    //this is the swap operator between CBLSIntVar
    queens(q1) :=: queens(q2)
  }

  //Some println at the end
  println("finished: " + getWatchString)
  if(c.violation.value == 0) {
    println("solved")
    println("solution:" + queens.mkString(","))
  } else println("not solved")
}
