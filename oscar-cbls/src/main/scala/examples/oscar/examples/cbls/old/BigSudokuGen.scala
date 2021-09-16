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
/******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Christophe Ponsard and Renaud De Landtsheer
 ******************************************************************************/


package oscar.examples.cbls.old
/*
import java.awt.Color

import javax.swing.border.LineBorder
import oscar.cbls.core.computation._
import oscar.cbls.core.constraint.ConstraintSystem
import oscar.cbls.lib.constraint.AllDiff
import oscar.cbls.lib.search.LinearSelectors
import oscar.cbls.util.StopWatch

import scala.swing.{GridPanel, Label, MainFrame, SimpleSwingApplication}

/**
 * Sudoku generator on NxN grids where N is a Square
 * This is a N coloring Problem but a specific heuristic is used here
 * - this is a generator assuming an empty grid
 * - solution proceeds by working on the internal Square
 * - AllDiff on Squares are kept invariant (initialisation + swap strategy)
 * - best delta is used and switch cells are added to tabu
 * @author christophe.ponsard@cetic.be
 * */
object BigSudokuGen extends SimpleSwingApplication with LinearSelectors with StopWatch {

  val C:Int=4
  val N:Int=C*C
  val M:Int=N*N
  val SubIndexes:Range=Range(0,C)
  val Indexes:Range=Range(0,N)
  val LinearIndexes:Range=Range(0,M)
  val Values:Range=Range(1,N+1)
  val Square=Array.ofDim[Int](N,N) // index are (Square nr, position in Square) - return position in grid
  val SquareOf=Array.ofDim[Int](M) // keep track of the Square of a given index
  initSquares()
  
  // Search control
  val MAX_IT = 2000
  val TABU_LENGTH=2 // TODO not sure tabu is effective here
  val PAUSE=0
 
  // UI stuff
  val tab = Array.ofDim[Label](N, N)
    
  def top = new MainFrame {
    title = "Big Sudoku Generator ("+N+")"
     
    val BorderInt=new LineBorder(Color.GRAY,1)
    val BorderExt=new LineBorder(Color.BLACK,1)
    
    contents = new GridPanel(C, C) {
      for(ns <- Indexes) {
    	contents +=new GridPanel(C,C) {
          for (ps <- Indexes) {
            val v = Square(ns)(ps)
            val lab = new Label("XX")
            lab.border=BorderInt
            tab(v/N)(v%N) = lab 
            contents += lab
          }
          border=BorderExt
    	}
      }
    }
    
    new Thread {
      override def run() {
        solve()
      }
    }.start()
  }
  
  /**
   * Initialisazing function to track Squares
   */
  def initSquares() {
    // definition of Squares
    for(v <- LinearIndexes) {
      val ni=v/N
      val nj=v%N
      val ns=(ni/C)*C+(nj/C)
      val ps=(ni%C)*C+nj%C
//      println(v+" "+ni+" "+nj+" "+ns+" "+ps)
      Square(ns)(ps)=v
      SquareOf(v)=ns
    }    
  }
  
  def solve() {
    
    startWatch()
        
    // model
    val m: Store = Store(false,None,true)
        
    // grid definition and initialisation
    val grid=Array.ofDim[CBLSIntVar](M)
    for(ns <- Indexes) {
      val perm:Iterator[Int]=getRandomPermutation(N)
      for(ps <- Indexes) {
        val v=Square(ns)(ps)
        val vinit=perm.next()+1
        grid(v)= CBLSIntVar(m, vinit, 1 to N, "v_"+v)
        tab(v/N)(v%N).text=vinit+""
      }
    }
    showGrid(grid,N)

    // constraint system
    val c = ConstraintSystem(m)
    
    for(i <- Indexes) c.post(AllDiff(for(j <- Indexes) yield grid(i*N+j))) // lines
    for(j <- Indexes) c.post(AllDiff(for(i <- Indexes) yield grid(i*N+j))) // columns    
    // note: Square constraints will be enforced (initially true and maintained by swap strategy)
    
    // register for violation
    for (i <- LinearIndexes) { c.violation(grid(i)) }

    // working variables
    val Tabu:Array[CBLSIntVar] = (for (i <- LinearIndexes) yield CBLSIntVar(m, 0, 0 to Int.MaxValue, "Tabu_"+i)).toArray
    
    // closing model
    m.close()
    
    // search
    var it:Int=1
    while((c.violation.value > 0) && (it < MAX_IT)){
      val allowed = LinearIndexes.filter(v => Tabu(v).value < it)
      val (v1,v2) = selectMin(allowed, allowed)((v1,v2) => c.swapVal(grid(v1),grid(v2)),
                                                (v1,v2) => (v1 < v2) && (SquareOf(v1) == SquareOf(v2))) // swap on the same line
      
//      require(SquareOf(v1)==SquareOf(v2))
           
      grid(v1) :=: grid(v2)
      
      // UI update
      tab(v1/N)(v1%N).text=grid(v1).value+""
      tab(v2/N)(v2%N).text=grid(v2).value+""
      for(v <- LinearIndexes.indices) {
        if (c.violation(grid(v)).value>0)
          tab(v/N)(v%N).foreground=Color.RED
        else
          tab(v/N)(v%N).foreground=Color.BLACK
      }

      Tabu(v1) := it + TABU_LENGTH
      Tabu(v2) := it + TABU_LENGTH

      it=it+1   
      println("it: "+ it + " " + c.violation + " (swapped "+ grid(v1) + " and " + grid(v2) + ") in Square "+SquareOf(v1))
      
      Thread.sleep(PAUSE)
    }

    println("Solution: "+m.solution(true))

    if (c.violation.value==0) {
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
  
//=====================================================================
// predefined grid at the end for readability (so using lazy keyword)
  
// easy
lazy val Problem9_1:Array[Int]= Array(
5,0,0,0,0,8,0,7,0,
0,0,3,4,7,6,1,5,0,
1,0,0,0,0,0,3,0,8,
0,6,5,0,4,2,0,0,1,
0,8,0,5,6,1,0,4,0,
4,0,0,8,3,0,5,9,0,
6,0,7,0,0,0,0,0,5,
0,5,1,6,8,4,7,0,0,
0,4,0,7,0,0,0,0,9)    

// difficult
lazy val Problem9:Array[Int]=Array( 
0,0,0,2,0,0,0,0,0,
0,6,2,0,0,0,5,0,0,
0,0,0,1,0,0,8,3,0,
0,0,0,0,7,0,0,0,5,
6,0,0,3,0,0,7,0,0,
2,3,0,6,0,5,0,8,1,
0,8,0,0,0,1,0,0,0,
7,5,0,0,0,0,0,0,0,
1,0,3,0,0,0,0,4,0)

// easy
lazy val Problem9_3:Array[Int]=Array(
0,0,0,2,0,9,5,0,3,
4,5,0,0,0,0,0,0,2,
0,0,0,0,7,0,0,0,0,
0,1,7,0,4,0,0,0,0,
0,2,0,0,0,0,0,0,0,
0,0,4,0,0,1,0,0,0,
0,3,9,0,8,0,0,1,0,
0,0,0,0,1,7,0,2,0,
0,0,8,0,0,0,9,0,5)

lazy val Problem9_4:Array[Int]=Array(
0,0,0,0,0,0,2,0,0,
0,5,8,0,0,6,0,0,0,
0,0,0,3,0,0,0,8,5,
0,1,0,4,7,0,6,0,0,
9,0,6,0,0,0,5,0,7,
0,0,7,0,3,9,0,4,0,
7,6,0,0,0,8,0,0,0,
0,0,0,9,0,0,8,1,0,
0,0,9,0,0,0,0,0,0)

lazy val Problem16:Array[Int]=Array(        				
0,0,16,0,0,15,0,1,0,0,0,11,0,13,5,6,
2,15,0,0,7,0,0,14,0,0,0,8,0,0,0,0,
0,0,0,0,0,2,12,0,1,0,0,0,0,0,3,10,
0,0,0,0,0,0,8,0,5,0,0,7,0,16,1,0,
0,0,0,0,0,1,6,0,4,11,3,0,10,0,16,0,
13,0,4,0,0,0,5,0,0,0,0,0,0,0,0,2,
9,0,0,1,0,3,10,0,6,0,15,0,0,7,14,12,
0,0,5,0,0,12,11,15,0,16,0,14,0,0,0,4,
0,0,8,3,0,0,2,0,11,4,0,1,6,15,0,0,
0,0,0,0,0,14,0,0,0,0,0,0,12,0,0,7,
1,0,13,4,0,7,15,3,8,12,0,0,0,9,0,0,
0,0,10,0,0,0,0,6,0,0,0,0,0,2,11,0,
10,1,9,0,0,0,0,0,0,7,0,6,15,0,12,0,
0,0,0,13,0,0,0,10,15,0,5,0,14,0,0,0,
5,14,0,0,0,0,13,0,0,9,12,16,0,0,0,0,
0,0,6,11,0,0,0,0,0,2,0,13,0,3,9,16)        				
        				
lazy val Problem36:Array[Int]=Array(
0,0,11,0,24,25,0,0,34,0,16,0,12,0,35,0,31,0,0,20,29,17,3,7,0,0,9,0,0,22,0,14,0,0,0,0,
0,0,3,0,0,7,0,27,0,0,13,1,0,0,32,0,25,0,0,23,15,0,28,0,0,20,8,31,17,0,6,22,24,0,29,0,
0,0,0,0,28,0,29,0,4,15,19,0,21,36,0,0,8,0,33,0,0,0,0,31,7,23,16,0,0,0,9,0,32,0,35,0,
5,17,0,0,0,6,3,2,0,0,14,0,23,0,0,0,0,0,13,0,0,0,0,0,30,0,0,0,0,12,16,0,0,0,26,27,
23,21,8,30,14,22,0,6,9,24,0,35,0,19,1,0,0,16,0,32,0,0,25,2,0,36,27,0,26,15,0,3,34,0,0,0,
0,0,15,0,35,0,0,0,31,22,0,11,10,0,5,9,4,20,8,0,0,0,36,14,3,0,28,1,0,34,0,0,0,18,0,0,
0,0,22,0,2,33,12,14,23,35,9,29,0,0,15,1,0,7,31,0,17,0,0,0,28,18,11,4,0,0,3,20,0,0,34,19,
28,0,0,3,25,34,36,0,21,0,0,0,0,2,4,23,0,0,19,16,0,11,0,1,20,0,0,0,0,0,26,5,0,0,0,0,
0,15,30,0,0,19,0,4,0,17,0,20,0,14,25,0,0,31,23,13,35,3,32,0,0,0,34,10,0,0,21,0,0,0,0,28,
36,35,32,0,12,9,18,0,8,10,0,3,0,0,0,0,26,0,15,21,0,0,22,0,0,0,31,5,0,0,11,2,0,13,0,0,
20,27,0,24,17,13,15,16,22,0,0,34,0,0,0,0,21,0,0,0,30,9,10,8,36,0,26,0,0,0,0,0,0,0,23,31,
0,0,0,0,26,0,0,0,2,31,0,0,0,0,9,3,0,8,7,0,34,29,20,0,0,0,0,25,24,13,12,32,0,0,27,36,
15,6,0,12,20,0,0,23,26,0,0,0,0,33,30,0,0,0,0,10,3,14,4,5,0,0,0,0,0,0,27,0,0,0,0,16,
0,0,0,0,0,27,19,12,6,13,4,31,29,0,0,5,0,0,22,9,0,15,1,16,0,0,18,32,30,0,0,25,2,11,0,0,
9,4,0,0,0,10,0,20,0,0,0,5,32,0,14,15,12,0,0,0,0,0,0,24,0,0,2,13,0,0,0,0,29,0,0,0,
0,7,18,0,29,0,10,0,0,0,0,0,0,0,22,0,6,0,0,0,0,0,0,32,0,0,0,23,0,0,0,0,0,0,0,0,
3,14,0,25,0,0,17,34,24,28,0,0,1,0,0,36,0,19,0,0,21,7,0,13,0,0,0,0,33,0,0,23,12,8,0,0,
32,5,0,0,0,0,30,0,0,29,0,9,0,0,0,17,3,0,0,25,36,2,0,0,0,0,0,0,31,24,0,0,0,0,0,18,
13,0,29,4,27,23,11,0,20,0,1,0,0,0,6,0,34,0,0,0,9,0,14,33,17,0,3,0,5,19,7,0,0,0,21,0,
0,8,0,0,11,0,0,0,0,0,0,0,0,1,16,0,2,0,26,0,0,28,6,22,0,15,0,29,18,0,32,34,0,0,13,33,
0,0,10,0,0,0,0,30,0,32,0,26,17,3,0,0,0,25,4,7,24,31,21,0,14,35,0,36,20,0,0,0,0,0,16,2,
30,0,0,0,0,16,0,0,0,0,12,0,0,0,26,0,19,9,0,5,23,10,0,0,6,33,0,0,2,0,8,18,0,31,28,0,
0,0,6,26,0,0,0,18,28,0,10,8,20,0,0,12,0,0,35,0,0,34,0,0,32,0,0,7,0,30,1,11,14,23,25,29,
0,0,0,0,33,32,34,13,0,0,23,15,0,0,10,0,22,36,0,1,25,0,0,17,0,0,0,0,4,11,19,12,0,35,0,30,
2,16,0,0,31,0,0,0,0,0,34,0,0,0,0,0,0,1,0,0,0,20,0,0,0,0,14,0,0,0,28,0,30,24,0,25,
1,22,0,6,0,12,13,0,19,0,0,7,3,0,0,31,0,0,0,0,8,0,29,25,0,17,0,0,0,0,34,0,0,0,0,0,
0,0,0,13,0,4,0,0,0,16,0,36,0,21,0,18,23,6,9,14,33,32,0,0,0,0,0,0,29,1,0,0,8,2,10,0,
0,24,28,36,7,0,0,32,0,23,31,0,9,0,0,0,20,33,16,35,11,0,0,26,0,4,5,0,21,2,29,6,1,0,22,0,
33,0,0,0,0,0,0,9,11,25,0,0,14,0,0,0,0,34,0,15,10,0,0,0,0,0,32,0,8,0,0,0,5,26,0,13,
21,25,0,20,19,17,14,35,5,0,28,0,0,0,8,0,24,0,0,30,13,22,31,6,0,0,0,27,0,0,0,0,9,32,0,0,
0,29,0,9,0,1,22,0,27,36,0,18,0,28,0,6,7,32,0,0,0,0,16,0,8,26,20,0,23,0,0,0,0,0,30,10,
10,30,16,0,32,0,28,31,7,0,29,0,5,26,0,19,0,22,0,8,6,0,0,18,0,13,24,0,14,0,33,0,0,36,9,0,
0,31,24,28,6,0,0,0,32,0,0,4,16,0,0,0,14,11,36,34,2,13,33,12,0,0,17,35,0,18,0,0,0,3,0,20,
0,0,26,23,0,8,0,0,0,5,11,33,25,0,0,24,18,3,28,0,0,4,0,9,27,0,0,0,32,0,0,7,15,17,0,12,
25,0,21,0,15,0,0,0,0,30,3,2,31,0,23,0,0,27,20,0,0,0,0,0,0,0,0,0,0,0,0,19,16,0,8,11,
0,0,7,18,34,36,0,19,0,9,17,24,8,0,13,0,15,0,14,0,0,26,0,0,4,10,22,11,16,0,0,21,0,27,0,35)

}
*/