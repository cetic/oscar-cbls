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
 * @author Gustav Björdal
 * @author Jean-Noël Monette
 */
package oscar.flatzinc.model


//TODO: The annotation list is not used!

abstract class Constraint(val variables: Array[Variable],val annotations: List[Annotation]) {
  for(v <- variables){
    v.addConstraint(this);
  }
  //Can be posted as an invariant
  def canDefineVar = false
  //The variables appearing in the constraint
  def getVariables():Array[Variable] = variables
  
  //The variables of the constraint which it can functionally define 
  //TODO generalize to arrays of defined variables (e.g. in sort/bin-packing)
  var candidates:Array[Variable] = null
  def getCandidateDefVars(): Array[Variable] = {
    if(candidates == null)createCandidates()
    candidates
  }
  //we filter out the variables that appear more than once in the constraint.
  private def createCandidates()={
    candidates = getMaybeCandidateDefVars().filter(v => variables.count(vv => v==vv)==1)
  }
  def getMaybeCandidateDefVars():Array[Variable] = Array.empty[Variable]
  //def simplify(p: FZProblem){}
  //The variable the constraint functionally defines
  //TODO: generalize to arrays of defined variables (e.g. in sort/bin-packing)
  def setDefinedVar(v: Variable) = {
      definedVars match {
      case None => v.definingConstraint match {
        case None =>
          //Make sure that this can indeed be defined. It might be that some annotations from fzn are wrong. We simply ignore those.
          if(getCandidateDefVars().contains(v)){
            v.definingConstraint = Some(this);
            definedVars = Some(v);
          }
          /*else{
            Console.err.println(v+" can not be defined by "+this)
          }*/
        case Some(cc) =>
          //Console.err.println(v +" is already defined by "+cc+". But "+this+" wants to define it as well. The second one is ignored.");
      }
      case Some(vv) => throw new Exception("Not supported yet.")
    }
  }
  def unsetDefinedVar(v: Variable) = {
    definedVars = definedVars match {
      case Some(vv) if v==vv => {
        v.definingConstraint = None
        None  
      }
      case _ => throw new Exception(v+" was not defined by "+this)
    }
  }
  private var definedVars = Option.empty[Variable]
  def definedVar = definedVars
  
  //True if the constraints annotations says that it defines x
  //def definesVar(x: Variable):Boolean = {annotations.foldLeft(false)((acc,y)=> (y.name == "defines_var" && y.args(0).asInstanceOf[ConcreteVariable].id == x.id ) || acc)}
  
  //this must be called when a constraint is removed from the model to remove references from variables. 
  def retract(): Unit ={
    if(definedVar.isDefined)unsetDefinedVar(definedVar.get)
    for(v <- variables){
      v.removeConstraint(this)
    }
  }
  def insert(): Unit ={
    for(v <- variables){
       v.addConstraint(this);
    }
  }
}

abstract class SimpleDefiningConstraint(variables: Array[Variable], val maybeDefinedVar: Variable, ann:List[Annotation])
  extends Constraint(variables, ann){
  override def canDefineVar = true
  override def getMaybeCandidateDefVars() = Array(maybeDefinedVar)
}
abstract class ReifiedConstraint(variables: Array[Variable], r: BooleanVariable, ann:List[Annotation]) extends SimpleDefiningConstraint(variables++Array(r),r,ann) {

}

case class reif(val c: Constraint,r: BooleanVariable) extends ReifiedConstraint(c.variables,r,c.annotations){
  c.retract()
}

abstract class AllDefiningConstraint(variables: Array[Variable], ann:List[Annotation])
  extends Constraint(variables, ann){
  override def canDefineVar = true
  override def getMaybeCandidateDefVars() = variables
}

//TODO: Flatten the args into the array of variables
case class GeneratedConstraint(name:String, args:List[Any],signature:List[Pattern]) extends Constraint(GC.flatten(args), List.empty[Annotation])
case class GenericConstraint(name:String, args:List[Any], ann: List[Annotation]) extends Constraint(GC.flatten(args), ann)
object GC{
  def flatten(args: List[Any]): Array[Variable] = {
    args.flatMap(_ match{
      case v:Variable => List(v) 
      case vs:Array[Variable] => vs
      case d:FzDomain => List.empty[Variable]
    }).toArray
  }
}
// ----------------------------------

case class array_bool_and(as: Array[BooleanVariable], r: BooleanVariable, ann: List[Annotation] = List.empty[Annotation]) 
  extends SimpleDefiningConstraint(as++Array(r),r,ann);

//Here, the "as" are NOT Variables, but Constants!
case class array_bool_element(b: IntegerVariable, as: Array[BooleanVariable], c: BooleanVariable, ann: List[Annotation]) 
  extends SimpleDefiningConstraint(Array(b,c),c,ann);
  
case class array_bool_or(as: Array[BooleanVariable], r: BooleanVariable, ann: List[Annotation] = List.empty[Annotation]) 
  extends SimpleDefiningConstraint(as++Array(r),r,ann){
  //override def toString() ={"array_bool_or("+as.mkString("[", ",", "]")+","+r+","+ann+")"}
}
case class array_int_element(b: IntegerVariable, as: Array[IntegerVariable], c: IntegerVariable, ann: List[Annotation] = List.empty[Annotation]) 
  extends SimpleDefiningConstraint(Array(b,c),c,ann);

case class array_var_bool_element(b: IntegerVariable, as: Array[BooleanVariable], c: BooleanVariable, ann: List[Annotation] = List.empty[Annotation])
  extends SimpleDefiningConstraint(as++Array(b,c),c,ann);

case class array_var_int_element(b: IntegerVariable, as: Array[IntegerVariable], c: IntegerVariable, ann: List[Annotation] = List.empty[Annotation])
  extends SimpleDefiningConstraint(as++Array(b,c),c,ann){
  override def toString() ={"array_var_int_element("+b+","+as.mkString("[", ", ", "]")+","+c+","+ann+")"}
}

case class array_bool_xor(as: Array[BooleanVariable], ann: List[Annotation] = List.empty[Annotation]) 
  extends AllDefiningConstraint(as++Array.empty[Variable],ann)

case class bool2int(b: BooleanVariable, x: IntegerVariable, ann: List[Annotation] = List.empty[Annotation]) 
  extends AllDefiningConstraint(Array(b,x),ann)

case class bool_and(a: BooleanVariable, b: BooleanVariable, r: BooleanVariable, ann: List[Annotation] = List.empty[Annotation]) 
  extends SimpleDefiningConstraint(Array(a,b,r),r,ann);
  
case class bool_clause(a: Array[BooleanVariable], b: Array[BooleanVariable], ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(a++b,ann){
  override def toString() ={"bool_clause("+a.mkString("[", ", ", "]")+","+b.mkString("[", ", ", "]")+","+ann+")"}
}

case class bool_eq(x: BooleanVariable, y: BooleanVariable, ann: List[Annotation] = List.empty[Annotation]) 
  extends AllDefiningConstraint(Array(x,y),ann)

case class bool_le(a: BooleanVariable, b: BooleanVariable, ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(Array(a,b),ann)

case class bool_lin_eq(params:Array[IntegerVariable],vars:Array[BooleanVariable], sum:IntegerVariable, ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(vars++Array.empty[Variable],ann){
  override def canDefineVar = true
  override def getMaybeCandidateDefVars():Array[Variable] = Array(sum)
  override def toString() ={"bool_lin_eq("+params.mkString("[", ", ", "]")+","+vars.mkString("[", ", ", "]")+","+sum+","+ann+")"}
}

case class bool_lin_le(params:Array[IntegerVariable],vars:Array[BooleanVariable], sum:IntegerVariable, ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(vars++Array.empty[Variable],ann)

case class bool_lt(a: BooleanVariable, b: BooleanVariable, ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(Array(a,b),ann)

case class bool_not(a: BooleanVariable, b: BooleanVariable, ann: List[Annotation] = List.empty[Annotation])
  extends AllDefiningConstraint(Array(a,b),ann)

case class bool_or(a: BooleanVariable, b: BooleanVariable, r: BooleanVariable, ann: List[Annotation] = List.empty[Annotation]) 
  extends ReifiedConstraint(Array(a,b),r,ann)

case class bool_xor(a: BooleanVariable, b: BooleanVariable, r: BooleanVariable, ann: List[Annotation] = List.empty[Annotation]) 
  extends ReifiedConstraint(Array(a,b),r,ann)

case class int_abs(a: IntegerVariable, b: IntegerVariable, ann: List[Annotation] = List.empty[Annotation]) 
  extends SimpleDefiningConstraint(Array(a,b),b,ann)

case class int_div(a: IntegerVariable, b: IntegerVariable, c: IntegerVariable, ann: List[Annotation] = List.empty[Annotation]) 
  extends SimpleDefiningConstraint(Array(a,b,c),c,ann)
  
case class int_eq(x: IntegerVariable, y: IntegerVariable, ann: List[Annotation] = List.empty[Annotation]) 
  extends AllDefiningConstraint(Array(x,y),ann)

case class int_le(a: IntegerVariable, b: IntegerVariable, ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(Array(a,b),ann)

case class int_lin_eq(params:Array[IntegerVariable],vars:Array[IntegerVariable], sum:IntegerVariable, ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(vars++Array(sum),ann){
  override def canDefineVar = true
  override def getMaybeCandidateDefVars():Array[Variable]  = {
    return vars.zip(params).filter((t) => Math.abs(t._2.min) == 1 && t._1.domainSize >= 1).map(_._1)
  }
  override def toString() ={"int_lin_eq("+params.mkString("[", ", ", "]")+","+vars.mkString("[", ", ", "]")+","+sum+","+ann+")"}
}

case class int_lin_le(params:Array[IntegerVariable],vars:Array[IntegerVariable], sum:IntegerVariable, ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(vars++Array.empty[Variable],ann)

case class int_lin_ne(params:Array[IntegerVariable],vars:Array[IntegerVariable], sum:IntegerVariable, ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(vars++Array.empty[Variable],ann)

case class int_lt(a: IntegerVariable, b: IntegerVariable, ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(Array(a,b),ann)

case class int_min(a: IntegerVariable, b: IntegerVariable, c: IntegerVariable, ann: List[Annotation] = List.empty[Annotation]) 
  extends SimpleDefiningConstraint(Array(a,b,c),c,ann)

case class int_max(a: IntegerVariable, b: IntegerVariable, c: IntegerVariable, ann: List[Annotation] = List.empty[Annotation]) 
  extends SimpleDefiningConstraint(Array(a,b,c),c,ann)

case class int_mod(a: IntegerVariable, b: IntegerVariable, c: IntegerVariable, ann: List[Annotation] = List.empty[Annotation]) 
  extends SimpleDefiningConstraint(Array(a,b,c),c,ann)

case class int_ne(x: IntegerVariable, y: IntegerVariable, ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(Array(x,y),ann)

case class int_plus(x: IntegerVariable, y: IntegerVariable, z: IntegerVariable, ann: List[Annotation] = List.empty[Annotation]) 
  extends AllDefiningConstraint(Array(x,y,z),ann)

case class int_times(x: IntegerVariable, y: IntegerVariable, z: IntegerVariable, ann: List[Annotation] = List.empty[Annotation])
  extends SimpleDefiningConstraint(Array(x,y,z),z,ann)

case class int_pow(a: IntegerVariable, b: IntegerVariable, c: IntegerVariable, ann: List[Annotation] = List.empty[Annotation])
  extends SimpleDefiningConstraint(Array(a,b,c),c,ann)

case class set_in(x: IntegerVariable, s: FzDomain, ann: List[Annotation] = List.empty[Annotation])
  extends Constraint(Array(x),ann)


case class all_different_int(xs: Array[IntegerVariable], ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(xs++Array.empty[Variable],ann){
  override def toString() = {"all_different_int(" +xs.mkString("[",", ","]") + ") :: " + ann.mkString(" :: ")}
}


case class at_least_int(n:IntegerVariable,x:Array[IntegerVariable],v:IntegerVariable, ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(x++Array.empty[Variable],ann)
case class at_most_int(n:IntegerVariable,x:Array[IntegerVariable],v:IntegerVariable, ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(x++Array.empty[Variable],ann)
case class exactly_int(n:IntegerVariable,x:Array[IntegerVariable],v:IntegerVariable, ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(x++Array.empty[Variable],ann)
case class among(n:IntegerVariable, x:Array[IntegerVariable], v:FzDomain, ann: List[Annotation] = List.empty[Annotation])
  extends SimpleDefiningConstraint(x++Array(n),n,ann)
case class count_eq(xs:Array[IntegerVariable], y:IntegerVariable, cnt:IntegerVariable, ann: List[Annotation] = List.empty[Annotation])
  extends SimpleDefiningConstraint(xs++Array(y,cnt),cnt,ann)
//count == count_eq
case class count(xs:Array[IntegerVariable], y:IntegerVariable, cnt:IntegerVariable, ann: List[Annotation] = List.empty[Annotation])
  extends SimpleDefiningConstraint(xs++Array(y,cnt),cnt,ann)

case class circuit(x:Array[IntegerVariable], ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(x++Array.empty[Variable],ann)
case class subcircuit(x:Array[IntegerVariable], ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(x++Array.empty[Variable],ann)

case class bin_packing_capa(c:Array[IntegerVariable],bin:Array[IntegerVariable],w:Array[IntegerVariable], ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(bin++Array.empty[Variable],ann)
case class bin_packing(c:IntegerVariable,bin:Array[IntegerVariable],w:Array[IntegerVariable], ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(bin++Array.empty[Variable],ann)
//TODO: defines the loads
case class bin_packing_load(load:Array[IntegerVariable],bin:Array[IntegerVariable],w:Array[IntegerVariable], ann: List[Annotation] = List.empty[Annotation])
  extends Constraint(load++bin,ann)

case class table_int(xs:Array[IntegerVariable],ts:Array[IntegerVariable], ann: List[Annotation] = List.empty[Annotation])
  extends Constraint(xs++Array.empty[Variable],ann)

case class table_bool(xs:Array[BooleanVariable],ts:Array[BooleanVariable], ann: List[Annotation] = List.empty[Annotation])
  extends Constraint(xs++Array.empty[Variable],ann)

case class cumulative(s:Array[IntegerVariable], d:Array[IntegerVariable],r:Array[IntegerVariable],b:IntegerVariable,ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(s++d++r++Array(b),ann)

case class diffn(x:Array[IntegerVariable], y:Array[IntegerVariable],dx:Array[IntegerVariable],dy:Array[IntegerVariable],ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(x++y++dx++dy,ann)

//TODO: defines card
case class distribute(card:Array[IntegerVariable], value:Array[IntegerVariable],base:Array[IntegerVariable],ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(card++value++base,ann)
//TODO: defines the counts
case class global_cardinality_closed(x:Array[IntegerVariable], cover:Array[IntegerVariable],count:Array[IntegerVariable],ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(x++count,ann)
//TODO: defines the counts
case class global_cardinality(x:Array[IntegerVariable], cover:Array[IntegerVariable],count:Array[IntegerVariable],ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(x++count,ann)
case class global_cardinality_low_up(x:Array[IntegerVariable], cover:Array[IntegerVariable],lbound:Array[IntegerVariable],ubound:Array[IntegerVariable],ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(x++Array.empty[Variable],ann)
case class global_cardinality_low_up_closed(x:Array[IntegerVariable], cover:Array[IntegerVariable],lbound:Array[IntegerVariable],ubound:Array[IntegerVariable],ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(x++Array.empty[Variable],ann)

//TODO: defines f or invf
case class inverse(f:Array[IntegerVariable], invf:Array[IntegerVariable],ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(f++invf,ann)

case class maximum_int(m:IntegerVariable, x:Array[IntegerVariable],ann: List[Annotation] = List.empty[Annotation]) 
  extends SimpleDefiningConstraint(Array(m)++x,m,ann)
case class minimum_int(m:IntegerVariable, x:Array[IntegerVariable],ann: List[Annotation] = List.empty[Annotation]) 
  extends SimpleDefiningConstraint(Array(m)++x,m,ann)
case class member_int(x:Array[IntegerVariable],y:IntegerVariable, ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(Array(y)++x,ann)
case class nvalue_int(y:IntegerVariable, x:Array[IntegerVariable], ann: List[Annotation] = List.empty[Annotation])
  extends SimpleDefiningConstraint(Array(y)++x,y,ann)


case class sort(x:Array[IntegerVariable], y:Array[IntegerVariable],ann: List[Annotation] = List.empty[Annotation]) 
  extends Constraint(x++y,ann)
