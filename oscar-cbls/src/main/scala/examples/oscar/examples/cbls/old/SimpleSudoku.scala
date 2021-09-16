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
 *         by Christophe Ponsard
 ******************************************************************************/


package oscar.examples.cbls.old
/*
import oscar.cbls.core.computation._
import oscar.cbls.core.constraint.ConstraintSystem
import oscar.cbls.lib.constraint.AllDiff
import oscar.cbls.lib.search.LinearSelectorClass
import oscar.cbls.util.StopWatch

/**
 * Simple Sudoku (for 9x9 grid) 
 * This is a 9 coloring problem but a specific heuristic is used here
 * - solution proceeds by working on the internal square
 * - AllDiff on squares are kept invariant (initialisation + swap strategy)
 * - best delta is used and switch cells are added to tabu
 * - could be generalised
 * @author christophe.ponsard@cetic.be
 */
object SimpleSudoku extends LinearSelectorClass with StopWatch {
  
  def main(args: Array[String]) {
 
    val C:Int=3
    val N:Int=C*C
    val M:Int=N*N
    //val SubIndexes:Range=Range(0,C)
    val Indexes:Range=Range(0,N)
    val LinearIndexes:Range=Range(0,M)
    val Values:Range=Range(1,N+1)

    // TODO add more sample grids here
    val problem:Array[Int]= 
    			  Array(5,0,0,0,0,8,0,7,0,
    					0,0,3,4,7,6,1,5,0,
        				1,0,0,0,0,0,3,0,8,
        				0,6,5,0,4,2,0,0,1,
        				0,8,0,5,6,1,0,4,0,
        				4,0,0,8,3,0,5,9,0,
        				6,0,7,0,0,0,0,0,5,
        				0,5,1,6,8,4,7,0,0,
        				0,4,0,7,0,0,0,0,9)    

    // Search control
    val MAX_IT = 1000
    val TABU_LENGTH = 5

    println("Sudoku(9x9)")

    startWatch()
        
    // TODO implement some consistency checks !
    
    // open cells
    val openCells=(for(i <- LinearIndexes if problem(i)==0) yield i).toArray // keep track of non fixed cells
    for(i <- openCells.indices) print(openCells(i)+" ")
    println()

    // internal squares
    val square=Array.ofDim[Int](N,N) // index are (square nr, position in square) - return position in grid
    val squareOf=Array.ofDim[Int](M) // keep track of the square of a given index
    for(v <- LinearIndexes) {
      val ni=v/N
      val nj=v%N
      val ns=(ni/C)*C+(nj/C)
      val ps=(ni%C)*C+nj%C
//      println(v+" "+ni+" "+nj+" "+ns+" "+ps)
      square(ns)(ps)=v
      squareOf(v)=ns
    }
                
    // model
    val m: Store = Store(false,None,true)
        
    // grid definition and initialisation
    val grid = Array.ofDim[CBLSIntVar](M)
        
    for(ns <- Indexes) {
      var squareSet:Set[Int]=Set() ++ (for(ps <- Indexes if problem(square(ns)(ps))!=0) yield problem(square(ns)(ps))).toArray
      for(ps <- Indexes) {
        val i=square(ns)(ps)
        var vinit=problem(i)
        for (k <- Values if vinit==0 && !squareSet.contains(k)) {
        	vinit=k
    	    squareSet += k
        }
        grid(i)=CBLSIntVar(m, vinit, 1 to N, "v_"+i)
      }
    }
    showGrid(grid,N)
            
    // constraint system
    val c = ConstraintSystem(m)
    
    for(i <- Indexes) c.post(AllDiff(for(j <- Indexes) yield grid(i*N+j))) // lines
    for(j <- Indexes) c.post(AllDiff(for(i <- Indexes) yield grid(i*N+j))) // columns
    // note: square constraints will be enforced (initially true and maintained by swap strategy)
    
    // register for violation
    for (i <- LinearIndexes) { c.violation(grid(i)) }

    // working variables
    val Tabu:Array[CBLSIntVar] = (for (i <- LinearIndexes) yield CBLSIntVar(m, 0, 0 to Int.MaxValue, "Tabu_"+i)).toArray
    var it:Int=1
    
    // closing model
    m.close()
      
    // search
    while((c.violation.value > 0) && (it < MAX_IT)){
      val allowed = openCells.filter(v => Tabu(v).value < it)
      val (v1,v2)= selectMin(allowed, allowed) ((v1,v2) => c.swapVal(grid(v1),grid(v2)), (v1,v2) => (v1 < v2) && (squareOf(v1)==squareOf(v2))) // swap on the same line

      grid(v1) :=: grid(v2)
      Tabu(v1) := it + TABU_LENGTH
      Tabu(v2) := it + TABU_LENGTH

      it=it+1   
      println("it: "+ it + " " + c.violation + " (swapped "+ grid(v1) + " and " + grid(v2) + ") in square "+squareOf(v1))
    }

    if (c.violation.value==0) {
      println("problem solved")
      showGrid(grid,N)    
    } else {
      println("Not found after "+MAX_IT+" iterations")
      showGrid(grid,N)
    }
    
    println("run time: "+ getWatch)
  }

  def showGrid(tab:Array[CBLSIntVar],N:Int) {
    for (i <- tab.indices) {
      if ((i%N)==0) println()
      print(tab(i).value+" ")
    }
    println()
  }
  
}
*/