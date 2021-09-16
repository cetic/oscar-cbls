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
 *         by Renaud De Landtsheer and Christophe Ponsard
 ******************************************************************************/


package oscar.examples.cbls.old
/*
import oscar.cbls._
import oscar.cbls.lib.constraint.{EQ, NE}
import oscar.cbls.lib.invariant.logic._
import oscar.cbls.lib.invariant.minmax._
import oscar.cbls.lib.search.LinearSelectorClass
import oscar.cbls.util.StopWatch

/**
 * Very simple example showing how to use Asteroid on the basic SEND+MORE=MONEY
 * Using a generic constrained directed search
 * @author christophe.ponsard@cetic.be
 */
object SendMoreMoney extends LinearSelectorClass with StopWatch {
  
  def main(args: Array[String]) {
    
    startWatch()
    
    println("SEND+MORE=MONEY")

    // enum letters
    object Letter extends Enumeration {
      type Letter = Value
      val S,E,N,D,M,O,R,Y,X1,X2 = Value
      val list=List[Letter](S,E,N,D,M,O,R,Y,X1,X2)
    }    
    
    print("Letter: ")
    for (l <- Letter.list) { print( l +" ") }
    println()
        
    // 4 carries
    object Carry extends Enumeration {
      type Carry = Value
      val c1,c2,c3,c4 = Value
      val list=List[Carry](c1,c2,c3,c4)
    }    
    print("Carries: ")
    for (c <- Carry.list) { print( c + " ") }
    println()
    
    // Search control
    val MAX_IT = 10000
    val TABU_LENGTH = 4

    // model
    val m: Store = new Store(false,None,true)
        
    // letter and carriage values
    // d initialised with 0..10, r with 0
    val d:Array[CBLSIntVar]= (for(l <- Letter.list) yield CBLSIntVar(m, l.id, 0 to 9, l+"")).toArray
    val r:Array[CBLSIntVar]= (for(c <- Carry.values) yield CBLSIntVar(m, 0, 0 to 9, c+"")).toArray
          
    // constraint system
    val c = ConstraintSystem(m)
    
    // all digits should be different
    // c.post(AllDiff(d)) // will be enforced
    c.post(NE(d(Letter.S.id),0))
    c.post(NE(d(Letter.M.id),0))
    c.post(EQ(r(Carry.c4.id),d(Letter.M.id)))
    c.post((r(Carry.c3.id) + d(Letter.S.id) + d(Letter.M.id)) === (d(Letter.O.id) + (10 * d(Letter.M.id))))
    c.post((r(Carry.c2.id) + d(Letter.E.id) + d(Letter.O.id)) === (d(Letter.N.id) + (10 * r(Carry.c3.id))))
    c.post((r(Carry.c1.id) + d(Letter.N.id) + d(Letter.R.id)) === (d(Letter.E.id) + (10 * r(Carry.c2.id))))
    c.post(                 (d(Letter.D.id) + d(Letter.E.id)) === (d(Letter.Y.id) + (10 * r(Carry.c1.id))))

    r(Carry.c1.id) <== ((d(Letter.D.id) + d(Letter.E.id)) / 10)
    r(Carry.c2.id) <== ((d(Letter.N.id) + d(Letter.R.id) + r(Carry.c1.id)) / 10)
    r(Carry.c3.id) <== ((d(Letter.E.id) + d(Letter.O.id) + r(Carry.c2.id)) / 10)
    r(Carry.c4.id) <== ((d(Letter.S.id) + d(Letter.M.id) + r(Carry.c3.id)) / 10)

    // search variables
    val ViolationArray = (for(l <- Letter.list) yield c.violation(d(l.id))).toArray
    val Tabu = (for (i <- Letter.list) yield CBLSIntVar(m, 0, 0 to Int.MaxValue, "Tabu_" + i)).toArray
    val It = CBLSIntVar(m,0, 0 to Int.MaxValue,"it")
    val NonTabuLetter = SelectLESetQueue(Tabu, It)
    val NonTabuMaxViolLetter = ArgMax(ViolationArray, NonTabuLetter)
    
    // closing model
    m.close()

    // search
    while((c.violation.value > 0) && (It.value < MAX_IT)){
      val l1 = selectFrom(NonTabuMaxViolLetter.value)
      val l2 = selectMin(NonTabuLetter.value)(i => c.swapVal(d(l1),d(i)), (i:Int) => i!=l1)

      // swapping so this enforces all d are different
      d(l1) :=: d(l2)
      println(c.violation.toString +" "+c.violation.value+" "+c.swapVal(d(l1),d(l2)))
      // enforcing carries are matching constraints

      //r(Carry.c1.id).setValue((d(Letter.D.id).value+d(Letter.E.id).value) / 10)
      //r(Carry.c2.id).setValue((d(Letter.N.id).value+d(Letter.R.id).value+r(Carry.c1.id).value) / 10)
      //r(Carry.c3.id).setValue((d(Letter.E.id).value+d(Letter.O.id).value+r(Carry.c2.id).value) / 10)
      //r(Carry.c4.id).setValue((d(Letter.S.id).value+d(Letter.M.id).value+r(Carry.c3.id).value) / 10)

      Tabu(l1) := It.value + TABU_LENGTH
      // Tabu(l2) := It.value + TABU_LENGTH // not a good idea to tabu the second variable
      
      It.++ // try without the dot (strange line behavior)
      println(It.toString + " " + c.violation+" switching "+d(l1).name+" "+d(l2).name)
    }	

    println("Solution: "+m.solution(true))
    println("run time: "+ getWatchString)
  }

}
*/