/*
 * *****************************************************************************
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
 * ****************************************************************************
 */

package oscar.flatzinc.cp

import oscar.cp._

import oscar.flatzinc.model.Constraint

import scala.collection.mutable.{Map => MMap}
import oscar.flatzinc.model._


/**
  * @author Gustav Bjordal
  * @author Jean-Noel Monette
  */
class FZCPBasicModel(val pstrength: oscar.cp.core.CPPropagStrength = oscar.cp.Medium, val ignoreUnkownConstraints: Boolean = false) {

  def printVars() = {
    println(dictVars.mkString("\n"))
  }

  implicit val solver: CPSolver = CPSolver(pstrength)
  solver.silent = true
  val poster = new CPConstraintPoster(pstrength);
  // TODO: Is there a danger in mapping Variable to CPIntVar as oppose to String
  // What happens when something goes wrong?
  val dictVars = MMap.empty[Variable,CPIntVar]
  val solutionMap = MMap.empty[Variable,Int]

  def getIntVar(v:Variable):CPIntVar = {
    dictVars.get(v) match {
      case None if v.isBound =>
        val c = v match{
          case v:IntegerVariable => CPIntVar(v.value);
          case v:BooleanVariable => CPBoolVar(v.boolValue);
        }
        dictVars += v -> c;
        c
      case Some(c) => c;

    }
  }
  def getBoolVar(v:Variable):CPBoolVar = {
    dictVars.get(v) match {
      case None if v.isBound =>
        val c = v match{
          case v:BooleanVariable => CPBoolVar(v.boolValue);
        }
        dictVars += v -> c;
        c
      case Some(c) => c.asInstanceOf[CPBoolVar];
    }
  }
  def createVariables(variables: Iterable[Variable]): Unit ={
    for(v <- variables){
      dictVars(v) = v match{
        case bv:BooleanVariable => CPBoolVar()
        case iv:IntegerVariable => iv.domain match{
          case FzDomainRange(min, max) => CPIntVar(min, max)
          case FzDomainSet(v) => CPIntVar(v)
          case _ => throw new RuntimeException("unknown domain")
        }
      }
    }
  }
  def createConstraints(constraints:Iterable[Constraint]): Unit ={
    //TODO: Put all the added cstrs in a ArrayBuffer and then post them all at once.
    for(c <- constraints){
      //TODO: Take consistency annotation to post constraints.
      try{
        val cons = poster.getConstraint(c,getIntVar,getBoolVar)
        add(cons)
      }catch{
        case e: scala.MatchError if ignoreUnkownConstraints => Console.err.println("% ignoring in CP: "+c)
      }
    }

  }

  def add(c:Array[(oscar.cp.Constraint,oscar.cp.core.CPPropagStrength)]): Unit ={
    for(cs <- c){
      solver.add(cs._1,cs._2)
    }
  }

  def createObjective(obj:oscar.flatzinc.model.Objective.Value, objVar:Option[Variable] = None): Unit ={
    obj match{
      case Objective.SATISFY =>
      case Objective.MAXIMIZE => maximize(getIntVar(objVar.get))
      case Objective.MINIMIZE => minimize(getIntVar(objVar.get))
    }
  }

  def getMinFor(v:IntegerVariable): Int = {
    getIntVar(v).getMin
  }
  def getMaxFor(v:IntegerVariable): Int = {
    getIntVar(v).getMax
  }
  def getMinFor(v:BooleanVariable): Int = {
    getBoolVar(v).getMin
  }
  def getMaxFor(v:BooleanVariable): Int = {
    getBoolVar(v).getMax
  }

  def createDefaultSearch() = {
    //TODO: Take into account the search annotations
    solver.search(oscar.cp.binaryLastConflict(dictVars.values.toArray))
  }

  def createRandomSearch() = {
    //TODO: Take into account the search annotations
    solver.search(oscar.cp.binary(dictVars.values.toSeq, _.size, _.randomValue))
  }

  def startSearch():(Boolean,MMap[Variable,Int]) = {
    onSolution {
                 for((k,v) <- dictVars){
                   v match {
                     case b:CPBoolVar =>
                       solutionMap(k) = 1-b.min //0 is true and >0 is false in CBLS solver.
                     case i:CPIntVar =>
                       solutionMap(k) = i.min
                   }
                 }
               }

    val stats = solver.start(nSols = 1)

    if(stats.nSols == 0){
        println("% No solution found")
      return (false, MMap.empty)
    }
    return (true, solutionMap)
  }

  def push() = solver.pushState()
  def pop() = solver.pop()
}