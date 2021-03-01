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
 * @author Leonard Debroux
 * @author Gustav Björdal
 * @author Jean-Noël Monette
 */
package oscar.flatzinc.model

import scala.collection.mutable.{Set => MSet}

class FZProblem {
  val variables: MSet[Variable] = MSet.empty[Variable]
  val constraints: MSet[Constraint] = MSet.empty[Constraint]
  val neighbourhoods: MSet[FZNeighbourhood] = MSet.empty[FZNeighbourhood]

  val solution:FZSolution = new FZSolution();
  val search = new Search();
  
  def addVariable(variable:Variable): Unit = {
    variables += variable
  }

  def addConstraint(c: Constraint): Unit = {
    constraints += c
  }

  def addNeighbourhood(n: FZNeighbourhood): Unit ={
    neighbourhoods += n
  }


  def addVariable(id: String, dom: FzDomain, bool: Boolean): Variable = {
    if(bool) addBooleanVariable(id,dom)
    else addIntegerVariable(id,dom)
  }
  def addIntegerVariable(id: String, dom: FzDomain): Variable = {
    val variable: Variable = new IntegerVariable(id, dom)
    variables += variable
    variable
  }
  def addBooleanVariable(id: String, dom: FzDomain): Variable = {
    val variable: Variable = new BooleanVariable(id, dom)
    variables += variable
    variable
  }

  

  
  def satisfy(anns:Iterable[Annotation]): Unit = {
    search.obj = Objective.SATISFY
    search.setHeuristic(anns)
  }
  def minimize(obj: IntegerVariable,anns:Iterable[Annotation]): Unit = {
    search.obj = Objective.MINIMIZE
    search.variable = Some(obj)
    search.setHeuristic(anns)
  }
  def maximize(obj: IntegerVariable,anns:Iterable[Annotation]): Unit = {
    search.obj = Objective.MAXIMIZE
    search.variable = Some(obj)
    search.setHeuristic(anns)
  }

  
//  def addSearch(s: Array[Variable],vrh: VariableHeuristic.Value,vh: ValueHeuristic.Value) {
//    //println("search "+vrh+" "+vh+ " variables:"+s.mkString(","))
//    search.heuristics =  search.heuristics :+ (s,vrh,vh)
//  }
//  
//  def nSols(n: Int) {
//    search.nSols = n
//  }
}

//TODO: should go to the CP specific part
object VariableHeuristic extends Enumeration {
  val FIRST_FAIL = Value("first_fail")
  val INPUT_ORDER = Value("input_order")
  val ANTI_FIRST_FAIL = Value("anti_first_fail")
  val SMALLEST = Value("smallest")
  val LARGEST = Value("largest")
  val OCCURENCE = Value("occurence")
  val MOST_CONSTRAINED = Value("most_constrained")
  val MAX_REGRET = Value("max_regret") 
}

object ValueHeuristic extends Enumeration {
  val INDOMAIN_MIN = Value("indomain_min")
  val INDOMAIN_MAX = Value("indomain_max")
  val INDOMAIN_MIDDLE = Value("indomain_middle")
  val INDOMAIN_MEDIAN = Value("indomain_median")
  val INDOMAIN = Value("indomain")
  val INDOMAIN_RANDOM = Value("indomain_random")
  val INDOMAIN_SPLIT = Value("indomain_split")
  val INDOMAIN_REVERSE_SPLIT = Value("indomain_reverse_split")
  val INDOMAIN_INTERVAL = Value("indomain_interval")  
}

object Objective extends Enumeration {
  val MINIMIZE = Value("minimize")
  val MAXIMIZE = Value("maximize")
  val SATISFY = Value("satisfy")
}



class Search() {
  //var nSols = 0
  var obj: Objective.Value = Objective.SATISFY
  var variable: Option[IntegerVariable] = None
  //var heuristics: Vector[(Array[Variable],VariableHeuristic.Value,ValueHeuristic.Value)] = Vector.empty 
  private[this] var anns: Iterable[Annotation] = List.empty[Annotation]
  def setHeuristic(anns: Iterable[Annotation]) = { 
    this.anns = anns
    //if(this.anns.size > 0) Console.err.println("% ignoring search annotations")
  }
  def getHeuristic(): Iterable[Annotation] = anns
}
