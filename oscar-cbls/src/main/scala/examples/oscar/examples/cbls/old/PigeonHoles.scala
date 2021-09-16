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
import oscar.cbls._
import oscar.cbls.lib.search.LinearSelectorClass
import oscar.cbls.util.StopWatch

/**
 * Very simple example showing how to use Asteroid on the basic pigeon hole problem
 * Using constraint system (better alternative: use ArgMax to keep track of violation)
 * @author christophe.ponsard@cetic.be
 * */
object PigeonHoles extends LinearSelectorClass with StopWatch {
  
  def main(args: Array[String]) {

    if (args.length<2) {
      println("argument: nr_holes nr_pigeons")
      System.exit(0)
    }
    
    startWatch()

    // nr of pigeons
    val N:Int=args(0).toInt

    // nr of holes
    val M:Int=args(1).toInt

    val range:Range = Range(0,M) // note Scala seems to consider 0..M-1 as range

    println("PigeonHoles(" + N + "," + M + ")")

    // model
    val m: Store = new Store(false,None,true)
       
    // holes
    val holes:Array[CBLSIntVar] = (for(i <- range) yield CBLSIntVar(m, 0, 0 to N, "h" + (i+1))).toArray
    
    // initially all pigeons are in the first hole...
    holes(0).setValue(N)

    // print initial state
    println(holes.toList)
    
    // constraint system (alternative: use ArgMax rather than Constraint System)
    val c = ConstraintSystem(m)
    
    // requiring all holes to have less that a pigeon
    //val ONE=new IntVar(m,0,N,1, "ONE")
    for (i <- range) {
      c.post(holes(i) le 1)
    }
    // enforcing sum (not required if transfer is used during search)

    m.close()

    println("run time after model close: "+ getWatchString)

    var it:Int =0
    val MaxIT = 2*N

    while((c.violation.value > 0) && (it < MaxIT)){
      val holeMax:(Int)=selectMax(range, (p:Int) => holes(p).value)
      val holeMin:(Int)=selectMin(range)(p => holes(p).value, (p:Int) => p != holeMax )

      holes(holeMax).setValue(holes(holeMax).value-1)
      holes(holeMin).setValue(holes(holeMin).value+1)
      
      it += 1
      println("it: " + it + " " + c.violation + " (moved from "+ holeMax + " to " + holeMin + ")")
    }

    println(c.violation)
    println(m.solution(true))
    println("run time: "+ getWatchString)
  }
}
*/