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
/**
 * @author Jean-Noel Monette
 */
package oscar.flatzinc.cp



import oscar.algo.Inconsistency
import oscar.cp._

import oscar.flatzinc.Options
import oscar.flatzinc.parser.FZParser

import scala.collection.mutable.{Map => MMap}
import oscar.flatzinc.model._
import oscar.flatzinc.UnsatException
import oscar.flatzinc.transfo.FZModelTransfo


import scala.util.Random
class FZCPModel(val model:oscar.flatzinc.model.FZProblem, val pstrength: oscar.cp.core.CPPropagStrength = oscar.cp.Medium, val ignoreUnkownConstraints: Boolean = false) {






  def printVars() = {
    println(dictVars.mkString("\n"))
  }

  implicit val solver: CPSolver = CPSolver(pstrength)
  solver.silent = true
  val poster = new CPConstraintPoster(pstrength);
  val dictVars = MMap.empty[String,CPIntVar]
  def getIntVar(v:Variable):CPIntVar = {
    dictVars.get(v.id) match {
      case None if v.isBound =>
        val c = v match{
          case v:IntegerVariable => CPIntVar(v.value);
          case v:BooleanVariable => CPBoolVar(v.boolValue);
        }
        dictVars += v.id -> c;
        c
	  case Some(c) => c;

	}
  }
  def getBoolVar(v:Variable):CPBoolVar = {
	dictVars.get(v.id) match {
	  case None if v.isBound =>
	    val c = v match{
	      case v:BooleanVariable => CPBoolVar(v.boolValue);
	    }
	    dictVars += v.id -> c;
	    c
	  case Some(c) => c.asInstanceOf[CPBoolVar];
	}
  }
  def createVariables(): Unit ={
    for(v <- model.variables){
      dictVars(v.id) = v match{
        case bv:BooleanVariable => CPBoolVar()
        case iv:IntegerVariable => iv.domain match{
          case FzDomainRange(min, max) => CPIntVar(min, max)
          case FzDomainSet(v) => CPIntVar(v)
          case _ => throw new RuntimeException("unknown domain")
        }
      }
    }
  }
  def createConstraints(): Unit ={
    //TODO: Add a try catch for if the problem fails at the root.
    //TODO: Put all the added cstrs in a ArrayBuffer and then post them all at once.
    //try{
        for(c <- model.constraints){
          //TODO: Take consistency annotation to post constraints.
          try{
            val cons = poster.getConstraint(c,getIntVar,getBoolVar)
            add(cons)
          }catch{
            case e: scala.MatchError if ignoreUnkownConstraints => Console.err.println("% ignoring in CP: "+c)
            case foo: Throwable =>
              println(foo)
          }
        }
    //}catch{
    //  case e => //throw new UnsatException(e.getMessage());
    //}
  }
  //TODO: why do we need a separate method?
  def add(c:Array[(oscar.cp.Constraint,oscar.cp.core.CPPropagStrength)]): Unit ={
    for(cs <- c){
      solver.add(cs._1,cs._2)
    }
  }
  def createObjective(): Unit ={
    model.search.obj match{
     case Objective.SATISFY => 
     case Objective.MAXIMIZE => maximize(getIntVar(model.search.variable.get))
     case Objective.MINIMIZE => minimize(getIntVar(model.search.variable.get))
    }
  }

  def fix(vars: Iterable[(String,Int)]):(Boolean,List[String]) = {
    var fixedVariables = List.empty[String]
    solver.pushState()
    try {
      for ((name, v) <- Random.shuffle(vars)) {
        fixedVariables = name :: fixedVariables
        //println("Fixing " + name + " to " + v)
        solver.add(dictVars(name) === v)
        //dictVars(name).assign(v)
      }
    }catch{
      case e:RuntimeException =>
        //println(e)
        solver.pop()
        return (false, fixedVariables)
    }
    return (true,fixedVariables)
  }

  def updateBestObjectiveValue(value: Int): Boolean = {
    try{
      model.search.obj match{
       case Objective.SATISFY =>
       case Objective.MAXIMIZE => getIntVar(model.search.variable.get).updateMin(value+1)
       case Objective.MINIMIZE => getIntVar(model.search.variable.get).updateMax(value-1)
      }
        solver.propagate()
        true
    }catch{
      case Inconsistency => false
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
  def handleSolution() = {
     model.solution.handleSolution(
      (s: String) => dictVars.get(s) match {
        case Some(intVar) =>
          intVar.value.toString
        case r => if(s=="true" || s=="false") s 
        else try{
          s.toInt.toString
        }catch{
          case e: NumberFormatException => {
            throw new Exception(s"Unhappy: $r $s")
          }
        }
     });
   }
  def createSearch() = {
    //TODO: Take into account the search annotations
    solver.search(oscar.cp.binaryLastConflict((model.variables/*-model.search.variable.get*/).map(getIntVar).toArray[CPIntVar]))
  }

  //Not clear if there is any advantage to keep this over setIntermediate..
  def updateIntermediateModelDomains(): Boolean ={
    var tmpV:Variable = null;
    try{
      for(v <- model.variables){
        tmpV =v
        v match{
          case bv:BooleanVariable =>
            if(getMinFor(bv)>=1)bv.bind(true)
            if(getMaxFor(bv)<=0)bv.bind(false)
          case iv:IntegerVariable =>
            if(getIntVar(iv).isContinuous) {
              iv.geq(getMinFor(iv))
              iv.leq(getMaxFor(iv))
            }else{
              iv.intersect(FzDomainSet(getIntVar(iv).iterator.toSet))
            }
        }
      }
    }catch{
      case e:UnsatException =>
        println(e)
        println(getMinFor(tmpV.asInstanceOf[IntegerVariable]))
        println(getMaxFor(tmpV.asInstanceOf[IntegerVariable]))
        println("Failed to update intermediate model domains")
        false
    }
    true
  }

  def setIntermediateModelDomains(): Boolean ={
    var tmpV:Variable = null;
    try{
      for(v <- model.variables){
        tmpV =v
        v match{
          case bv:BooleanVariable =>
            bv.setDomain(getBoolVar(bv).iterator.toSet)
          case iv:IntegerVariable =>
            if(getIntVar(iv).isContinuous) {
              iv.setDomain(getMinFor(iv) to getMaxFor(iv))
            }else{
              iv.setDomain(getIntVar(iv).iterator.toSet)
            }
        }
      }
    }catch{
      case e:UnsatException =>
        println("Failed to set intermediate model domains")
        println(getMinFor(tmpV.asInstanceOf[BooleanVariable]))
        println(getMaxFor(tmpV.asInstanceOf[BooleanVariable]))
        println(getBoolVar(tmpV.asInstanceOf[BooleanVariable]).iterator.toSet)
        false
    }
    true
  }
}

class FZCPSolver {
  val pstrength = oscar.cp.Medium 
  
  def solve(opts: Options): Unit ={
    val log = opts.log();
    log("start")
    val model = FZParser.readFlatZincModelFromFile(opts.fileName,log, false).problem;
     
    
    log("Parsed.")
    FZModelTransfo.propagate(model)(log)
    log("initial variable reduction (to avoid overflows)")
    
    //TODO: Find binary constraints that can be used for views.
    val cpmodel = new FZCPModel(model)
    
    cpmodel.createVariables();
    log("created variables")
    
    cpmodel.createConstraints();
    log("constraints posted")
    
    cpmodel.createObjective();
    log("objective created")
    
    cpmodel.createSearch()
    log("search created")
    
    cpmodel.solver.onSolution{
      //println("found")
      cpmodel.handleSolution()
    }
    
    //TODO: search for more than one solution for optimisation
    //TODO: Remove the time spent in parsing and posting
    val stats = cpmodel.solver.start(nSols= Int.MaxValue,timeLimit=60*15)
    if(stats.completed) println("==========")
    log(stats.toString)

    

  }
}
