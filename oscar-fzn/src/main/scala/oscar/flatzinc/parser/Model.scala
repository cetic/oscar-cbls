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
 * @author Jean-Noël Monette
 */
package oscar.flatzinc.parser

import oscar.flatzinc.model.FZProblem

import scala.collection.JavaConverters._
import scala.collection.mutable.{Map, Set => MSet}
import oscar.flatzinc.parser.intermediatemodel._
import oscar.flatzinc.model.Annotation

import scala.collection.JavaConverters._
import java.util.ArrayList

import oscar.flatzinc.model.FZProblem
import oscar.flatzinc.model._
import oscar.flatzinc.NoSuchConstraintException
import java.lang.reflect.Constructor

import oscar.flatzinc.ParsingException
import oscar.flatzinc.Log
import oscar.flatzinc.parser.intermediatemodel.ASTDecls.{ASTParamDecl, ASTVarDecl}
import oscar.flatzinc.parser.intermediatemodel.ASTLiterals._

import scala.collection.mutable.WrappedArray



//class VarRef(val v: Variable) extends Element()

class OldModel(val log: Log, val acceptAnyCstr: Boolean) {

  def handleModel(m:ASTModel) = {
    //Add params
    for(pd:ASTParamDecl <- m.getParamDecls.asScala){

    }

    //Add Vars
    for(vd:ASTVarDecl <- m.getVarDecls.asScala){

    }
  }

  def elementFromASTLit(l:ASTLit):Element = {
    l match {
      case array:ASTArray =>
        val a = new ArrayOfElement()
        a.typ = new Type(Type.NULL); a.typ.isArray = true;  a.typ.size = 0
        for(e <- array.getElems.asScala){
          val tmp:Element = elementFromASTLit(e)
          a.elements.add(tmp); a.typ.size +=1;if(tmp.typ.isVar){a.typ.isVar}
        }
        a.close(); a

    }
  }

  val problem: FZProblem = new FZProblem()
  val dict: Map[String,Element] = Map.empty[String,Element]
  val dictAnn: Map[String, Iterable[Annotation]] = Map.empty[String,Iterable[Annotation]] // Maps ids (vars and cons) to which annotations they have

  def addId(id: String, e: Element)={
    //Console.err.println("% added" + id)
    dict(id) = e
  }
  def hasId(id: String) = dict.contains(id)
  def findId(id: String): Element = {
    if(dict.contains(id))dict(id)
    else{//TODO: That should be an annotation, how to check this?
      val e = new Element();
      e.name = id;
      e.value = new Annotation(e.name);
      e.typ = new Type(Type.ANNOTATION)
      e
    }
  }

  def createDomain(e: FzDomain, t: Integer): FzDomain  = {
    if(e==null){
      if(t==Type.BOOL) new FzDomainRange(0, 1)
      else new FzDomainRange(Helper.FznMinInt, Helper.FznMaxInt)//TODO: This is dangerous!
    }else e;
  }
  def copy(d: FzDomain): FzDomain = {
    d match {
      case FzDomainSet(v) => new FzDomainSet(v)
      case FzDomainRange(mi, ma) => new FzDomainRange(mi, ma)
      case _ => null.asInstanceOf[FzDomain]
    }
  }

  def addNewVariable(t: Type, de: Element, name: String, anns0: java.util.List[Annotation])={
    if(!(t.typ == Type.INT||t.typ == Type.BOOL))
      throw new ParsingException("Only Supporting Int and Bool variables.");
    val anns = anns0.asScala;
    val d = createDomain(if(de!=null)de.value.asInstanceOf[FzDomain]else null, t.typ)
    if(t.isArray) {
        val a = new ArrayOfElement();
        //if(d!=null)a.domain = d;
        a.name = name;
        a.typ = t;
        //a.annotations = anns0;
        for(i <- 0 to t.size-1){
            val n = name+"["+(i+1)+"]"
            val v = problem.addVariable(n,copy(d),t.typ==Type.BOOL);
            val vr = new Element(v);
            a.elements.add(vr);
            vr.typ = new Type(t);
            vr.typ.isArray = false;
            vr.typ.size = 1;
            vr.name = n;
            //if(d!=null)vr.domain = d;
        }
        addId(name,a);
        handleVarAnnotations(name, a, anns)
      }else{
        val v = problem.addVariable(name,d,t.typ==Type.BOOL)
        val vr = new Element(v);
        //if(d!=null)vr.domain = d;
        vr.name = name;
        vr.typ = t;
        //vr.annotations = anns0;
        addId(name,vr);
        handleVarAnnotations(name, vr, anns)
      }
  }
  def addAliasVariable(t: Type, de: Element, name: String, e:Element, anns: java.util.List[Annotation])={
    //TODO: When can de be null?
    val d = if(de!=null)de.value.asInstanceOf[FzDomain]else null
    //if(!name.equals(e.name)) System.out.println("% Not the same name: "+e.name+" vs "+name);
    if(!t.equals(e.typ)){
      if(e.typ.typ==Type.NULL){
        e.typ.typ = t.typ;
      }else{
        if(!(t.isVar && !e.typ.isVar))
          log(1,"Not the same type: "+e.typ+" vs "+t);
      }
    }
    /*if(d!=null && !d.equals(e.domain)){
      //System.out.println("% Not the same domain: "+e.domain+" vs "+d);
      if(e.domain==null)e.domain = d
      else e.domain.intersect(d)
    }*/
    //if(!anns.equals(e.annotations)) System.out.println("% Not the same annotations: "+e.annotations+" vs "+anns);
    addId(name,e);
    handleVarAnnotations(name, e, anns.asScala)
  }

  def isIntroducedVar(id: String): Boolean = {
    dictAnn.contains(id) &&
    dictAnn(id).exists(_.name == "var_is_introduced");
  }
  def isDefinedVar(id: String): Boolean = {
    dictAnn(id).exists(_.name == "is_defined_var");
  }
  def isOutputVar(id: String): Boolean = {
    dictAnn(id).exists(_.name == "output_var")
  }
  def isOutputArray(id: String): Boolean = {
    dictAnn(id).exists(_.name == "output_array")
  }

  private def handleVarAnnotations(name: String, e: Element, anns: Iterable[oscar.flatzinc.model.Annotation]): Any = {
    dictAnn(name) = anns;
    if(e.typ.isArray){
      if (anns.exists((a: Annotation) => a.name == "output_array")) {
        val a = e.asInstanceOf[ArrayOfElement]
          if(e.typ.typ==Type.INT){
            problem.solution.addOutputArrayVarInt(name,
                                                  a.elements.asScala.toArray.map(e => e.value match{case vr: Variable => vr.id; case other => if(e.name==null)other.toString() else e.name}),
                                                  anns.find((p:Annotation) => p.name == "output_array").get.args(0).asInstanceOf[Array[FzDomain]].map(e=> e.asInstanceOf[FzDomainRange].toRange).toList)
          }
          if(e.typ.typ==Type.BOOL){
            problem.solution.addOutputArrayVarBool(name,a.elements.asScala.toArray.map(e => e.value match{case vr: Variable => vr.id; case other => if(e.name==null)other.toString() else e.name}),
                           anns.find((p:Annotation) => p.name == "output_array").get.args(0).asInstanceOf[Array[FzDomain]].map(e=> e.asInstanceOf[FzDomainRange].toRange).toList)
                           
          }
        }
    }else{
      if(anns.exists((a: Annotation) => a.name == "output_var")) {
        //println("000000 "+name)
        if(e.typ.typ==Type.INT) problem.solution.addOutputVarInt(name,e.name)
        if(e.typ.typ==Type.BOOL)problem.solution.addOutputVarBool(name,e.name)
      }
    }
  }
  
  
  def addAnnArg(a: Annotation, x: Any): Unit ={
	val newarg = x match{
      case e: Element =>    
        if(e.typ.typ==Type.INT)
          if(e.typ.isArray) getIntVarArray(e)
          else getIntVar(e) //TODO: differentiate par vs var
        else if(e.typ.typ==Type.BOOL)
          if(e.typ.isArray) getBoolVarArray(e)
          else getBoolVar(e) //TODO: differentiate par vs var
        else if(e.typ.typ==Type.SET) 
          if(e.typ.isArray) getIntSetArray(e)
          else getIntSet(e)
        else if (e.typ.typ ==Type.ANNOTATION)
          if(e.typ.isArray) e.asInstanceOf[ArrayOfElement].elements.asScala.toArray.map(v => v.value.asInstanceOf[Annotation])
          else e.value.asInstanceOf[Annotation]
        else throw new Exception("Case not handled: "+e)
    }
	if(newarg==null) println(x)
	//println(newarg)
    a.add(newarg)
  }
  
  def addConstraint(name: String, args: java.util.List[Element], anns: java.util.List[Annotation]) = {
    //ann_other is not used yet!
    val (ann_def,ann_other) = anns.asScala.toList.partition(a => a.name == "defines_var")
    val cstr = constructConstraint(name, args.asScala.toList, anns.asScala.toList)
    //Added the test because Mzn 2.0 adds some defined_var(12) with constants.
    ann_def.foreach(a => a.args(0) match{ case v:Variable => cstr.setDefinedVar(v); case _ => })
    problem.addConstraint(cstr)
  }
  
  def setSATObjective(anns: java.util.List[Annotation])= {
    problem.satisfy(anns.asScala)
  }
  def setMINObjective(e: Element, anns: java.util.List[Annotation])= {
    problem.minimize(getIntVar(e),anns.asScala)
  }
  def setMAXObjective(e: Element, anns: java.util.List[Annotation])= {
    problem.maximize(getIntVar(e),anns.asScala)
  }
  def getIntVar(e: Element): IntegerVariable = {
    e.value match{
      case v:IntegerVariable => v
      case i:Integer => new IntegerVariable(i.toString(),Int.unbox(i))
      case _ => throw new ParsingException("Expected a var int but got: "+e)
    }
  }
  def getBoolVar(e: Element): BooleanVariable = {
    e.value match{
      case v:BooleanVariable => v
      case b:java.lang.Boolean => new BooleanVariable(b.toString(),Some(Boolean.unbox(b)))
      case _ => throw new ParsingException("Expected a var bool but got: "+e)
    }
  }
  def getBoolVarArray(e: Element): Array[BooleanVariable] = { 
    //TODO: Do the same memoization as for Integer arrays.
    if(e.isInstanceOf[ArrayOfElement]){
      val a = e.asInstanceOf[ArrayOfElement]
      a.elements.asScala.toArray.map(v => getBoolVar(v))
    }else{
      throw new ParsingException("Expected a array of var bool but got: "+e)
    }
  }

  //TODO: Check if this actually reduces the memory footprint and does not increase parsing time too much...
  var knownarrays = Map.empty[WrappedArray[IntegerVariable],Array[IntegerVariable]]
  //TODO: This is actually late to do that memoization as elements might be repeated!
  def getIntVarArray(e: Element): Array[IntegerVariable] = { 
    if(e.isInstanceOf[ArrayOfElement]){
      val array = e.asInstanceOf[ArrayOfElement].elements.asScala.toArray.map(v => getIntVar(v))
      val wrap = genericWrapArray(array)
      if(knownarrays.contains(wrap)){
        //Console.err.println("% reuse "+knownarrays.size)
        knownarrays(wrap)
      }else{
        knownarrays(wrap) = array
        array
      }
    }else{
      throw new ParsingException("Expected a array of var int but got: "+e)
    }
  }
  
  def getIntSet(e: Element): FzDomain = {
    e.value.asInstanceOf[FzDomain]
  }
  def getIntSetArray(e: Element): Array[FzDomain] = {
    if(e.isInstanceOf[ArrayOfElement]){
      val a = e.asInstanceOf[ArrayOfElement]
      a.elements.asScala.toArray.map(v => getIntSet(v))
    }else{
      throw new ParsingException("Expected a array of sets but got: "+e)
    }
  }
  
  val dictCons: Map[String,(List[Element],List[Annotation])=>Constraint] = Map(
    "int_eq" -> ((varList,ann) => int_eq(getIntVar(varList(0)),getIntVar(varList(1)),ann)),
    "int_eq_reif" -> ((varList,ann) => reif(int_eq(getIntVar(varList(0)),getIntVar(varList(1)),ann),getBoolVar(varList(2)))),
    "int_ne" -> ((varList,ann) => int_ne(getIntVar(varList(0)),getIntVar(varList(1)),ann)),
    "int_ne_reif" -> ((varList,ann) => reif(int_ne(getIntVar(varList(0)),getIntVar(varList(1)),ann),getBoolVar(varList(2)))),
    "int_le" -> ((varList,ann) => int_le(getIntVar(varList(0)),getIntVar(varList(1)),ann)),
    "int_le_reif" -> ((varList,ann) => reif(int_le(getIntVar(varList(0)),getIntVar(varList(1)),ann),getBoolVar(varList(2)))),
    "int_lt" -> ((varList,ann) => int_lt(getIntVar(varList(0)),getIntVar(varList(1)),ann)),
    "int_lt_reif" -> ((varList,ann) => reif(int_lt(getIntVar(varList(0)),getIntVar(varList(1)),ann),getBoolVar(varList(2)))),
    "bool_eq" -> ((varList,ann) => bool_eq(getBoolVar(varList(0)),getBoolVar(varList(1)),ann) ),
    "bool_lt" -> ((varList,ann) => bool_lt(getBoolVar(varList(0)),getBoolVar(varList(1)),ann) ),
    "bool_le" -> ((varList,ann) => bool_le(getBoolVar(varList(0)),getBoolVar(varList(1)),ann) ),
    "bool_not" -> ((varList,ann) => bool_not(getBoolVar(varList(0)),getBoolVar(varList(1)),ann) ),
    "bool_xor" -> ((varList,ann) => bool_xor(getBoolVar(varList(0)),getBoolVar(varList(1)),getBoolVar(varList(2)),ann) ),
    "array_bool_or" -> ((varList,ann) => array_bool_or(getBoolVarArray(varList(0)),getBoolVar(varList(1)),ann)),
    "array_bool_xor" -> ((varList,ann) => array_bool_xor(getBoolVarArray(varList(0)),ann)),
    "array_bool_and" -> ((varList,ann) => array_bool_and(getBoolVarArray(varList(0)),getBoolVar(varList(1)),ann)),
    "bool_clause" -> ((varList,ann) => bool_clause(getBoolVarArray(varList(0)),getBoolVarArray(varList(1)),ann)),
    "bool2int" -> ((varList,ann) => bool2int(getBoolVar(varList(0)),getIntVar(varList(1)),ann)),
    "array_int_element" -> ((varList,ann) => array_int_element(getIntVar(varList(0)),getIntVarArray(varList(1)),getIntVar(varList(2)),ann)),
    "array_var_int_element" -> ((varList,ann) => array_var_int_element(getIntVar(varList(0)),getIntVarArray(varList(1)),getIntVar(varList(2)),ann)),
    "array_bool_element" -> ((varList,ann) => array_bool_element(getIntVar(varList(0)),getBoolVarArray(varList(1)),getBoolVar(varList(2)),ann)),
    "array_var_bool_element" -> ((varList,ann) => array_var_bool_element(getIntVar(varList(0)),getBoolVarArray(varList(1)),getBoolVar(varList(2)),ann)),
    "int_lin_eq" -> ((varList,ann) => int_lin_eq(getIntVarArray(varList(0)),getIntVarArray(varList(1)),getIntVar(varList(2)),ann)),
    "int_lin_le" -> ((varList,ann) => int_lin_le(getIntVarArray(varList(0)),getIntVarArray(varList(1)),getIntVar(varList(2)),ann)),
    "int_lin_ne" -> ((varList,ann) => int_lin_ne(getIntVarArray(varList(0)),getIntVarArray(varList(1)),getIntVar(varList(2)),ann)),
    "int_lin_eq_reif" -> ((varList,ann) => reif(int_lin_eq(getIntVarArray(varList(0)),getIntVarArray(varList(1)),getIntVar(varList(2)),ann),getBoolVar(varList(3)))),
    "int_lin_le_reif" -> ((varList,ann) => reif(int_lin_le(getIntVarArray(varList(0)),getIntVarArray(varList(1)),getIntVar(varList(2)),ann),getBoolVar(varList(3)))),
    "int_lin_ne_reif" -> ((varList,ann) => reif(int_lin_ne(getIntVarArray(varList(0)),getIntVarArray(varList(1)),getIntVar(varList(2)),ann),getBoolVar(varList(3)))),
    "count" -> ((varList,ann) => count(getIntVarArray(varList(0)),getIntVar(varList(1)),getIntVar(varList(2)),ann)),
    "exactly_int" -> ((varList,ann) => exactly_int(getIntVar(varList(0)),getIntVarArray(varList(1)),getIntVar(varList(2)),ann)),
    "at_least_int" -> ((varList,ann) => at_least_int(getIntVar(varList(0)),getIntVarArray(varList(1)),getIntVar(varList(2)),ann)),
    "at_most_int" -> ((varList,ann) => at_most_int(getIntVar(varList(0)),getIntVarArray(varList(1)),getIntVar(varList(2)),ann)),
    "count_eq" -> ((varList,ann) => count_eq(getIntVarArray(varList(0)),getIntVar(varList(1)),getIntVar(varList(2)),ann)),
    "int_max" -> ((varList,ann) => int_max(getIntVar(varList(0)),getIntVar(varList(1)),getIntVar(varList(2)),ann)),
    "int_min" -> ((varList,ann) => int_min(getIntVar(varList(0)),getIntVar(varList(1)),getIntVar(varList(2)),ann)),
    "int_times" -> ((varList,ann) => int_times(getIntVar(varList(0)),getIntVar(varList(1)),getIntVar(varList(2)),ann)),
    "int_plus" -> ((varList,ann) => int_plus(getIntVar(varList(0)),getIntVar(varList(1)),getIntVar(varList(2)),ann)),
    "int_div" -> ((varList,ann) => int_div(getIntVar(varList(0)),getIntVar(varList(1)),getIntVar(varList(2)),ann)),
    "int_mod" -> ((varList,ann) => int_mod(getIntVar(varList(0)),getIntVar(varList(1)),getIntVar(varList(2)),ann)),
    "int_abs" -> ((varList,ann) => int_abs(getIntVar(varList(0)),getIntVar(varList(1)),ann)),
    "all_different_int" -> ((varList,ann) => all_different_int(getIntVarArray(varList(0)),ann)),
    "set_in" -> ((varList,ann) => set_in(getIntVar(varList(0)),getIntSet(varList(1)),ann)),
    "member_int" -> ((varList,ann) => member_int(getIntVarArray(varList(0)),getIntVar(varList(1)),ann)),
    "maximum_int" -> ((varList,ann) => maximum_int(getIntVar(varList(0)),getIntVarArray(varList(1)),ann)),
    "minimum_int" -> ((varList,ann) => minimum_int(getIntVar(varList(0)),getIntVarArray(varList(1)),ann)),
    //TODO: If the name is not the same, it should not be same in the output either.
    "inverse_no_offset" -> ((varList,ann) => inverse(getIntVarArray(varList(0)),getIntVarArray(varList(1)),ann)),
    "subcircuit_no_offset" -> ((varList,ann) => subcircuit(getIntVarArray(varList(0)),ann)),
    "circuit_no_offset" -> ((varList,ann) => circuit(getIntVarArray(varList(0)),ann)),
    "global_cardinality" -> ((varList,ann) => global_cardinality(getIntVarArray(varList(0)),getIntVarArray(varList(1)),getIntVarArray(varList(2)),ann)),
    "global_cardinality_closed" -> ((varList,ann) => global_cardinality_closed(getIntVarArray(varList(0)),getIntVarArray(varList(1)),getIntVarArray(varList(2)),ann)),
    "global_cardinality_low_up" -> ((varList,ann) => global_cardinality_low_up(getIntVarArray(varList(0)),getIntVarArray(varList(1)),getIntVarArray(varList(2)),getIntVarArray(varList(3)),ann)),
    "global_cardinality_low_up_closed" -> ((varList,ann) => global_cardinality_low_up_closed(getIntVarArray(varList(0)),getIntVarArray(varList(1)),getIntVarArray(varList(2)),getIntVarArray(varList(3)),ann)),
    "cumulative" -> ((varList,ann) => cumulative(getIntVarArray(varList(0)),getIntVarArray(varList(1)),getIntVarArray(varList(2)),getIntVar(varList(3)),ann)),
    "nvalue_int" -> ((varList,ann) => nvalue_int(getIntVar(varList(0)),getIntVarArray(varList(1)),ann)),
    "bin_packing_load" -> ((varList,ann) => bin_packing_load(getIntVarArray(varList(0)),getIntVarArray(varList(1)),getIntVarArray(varList(2)),ann)),
    "table_int" -> ((varList,ann) => table_int(getIntVarArray(varList(0)),getIntVarArray(varList(1)),ann)),
    "table_bool" -> ((varList,ann) => table_bool(getBoolVarArray(varList(0)),getBoolVarArray(varList(1)),ann))
  )


  def constructConstraint(cstr: String, varList: List[Element], ann:List[Annotation]): Constraint = {
    //special case
//    if(cstr=="bool_eq_reif" && !varList(1).typ.isVar && !varList(1).value.asInstanceOf[Boolean]){
//      makeConstraint("bool_not",varList.head :: varList.tail.tail,ann)
//    }else
      if(cstr.endsWith("_reif")){
      reif(makeConstraint(cstr.substring(0,cstr.length-5),varList.dropRight(1),ann),getBoolVar(varList.last))
    }else{
      makeConstraint(cstr,varList,ann)
    }
  }


  def makeConstraint(c: String, args:List[Element], ann:List[Annotation]): Constraint = {
    dictCons.getOrElse(c, (varList: List[Element], ann:List[Annotation]) =>
      try{
        val cl = Class.forName("oscar.flatzinc.model."+c)
        Console.err.println("MISSING CONSTRAINT: "+c)
        makeConstraint(cl,args,ann)
      }catch{
        case e: ClassNotFoundException => 
          if(acceptAnyCstr)
            makeGenericConstraint(c,args,ann)
          else 
            throw new NoSuchConstraintException(c,"Intermediate Representation");
      }
    )(args,ann)
  
  }
  
  def makeGenericConstraint(c: String, args:List[Element], ann:List[Annotation]): Constraint = {
    val args2 = args.map((a) => 
      if(a.typ.typ==Type.INT)
        if(a.typ.isArray) getIntVarArray(a)
        else getIntVar(a) //TODO: differentiate par vs var
      else if(a.typ.typ==Type.BOOL)
        if(a.typ.isArray) getBoolVarArray(a)
        else getBoolVar(a) //TODO: differentiate par vs var
      else if(a.typ.typ==Type.SET) getIntSet(a)
      else throw new Exception("Case not handled: "+a))
    GenericConstraint(c,args2,ann)
  }
  
  
  //TODO: Might actually memoize the used constructors as most problems only involve a handful of constraint predicates.
  def makeConstraint[A]/* <: Constraint]*/(c:Class[A],args:List[Element], ann:List[Annotation]): Constraint = {
    val cc:Constructor[A] = c.getConstructors()(0).asInstanceOf[Constructor[A]];
    val p = cc.getParameterTypes();
    //println(p.mkString(","))
    val arg = new Array[Object](p.length)
    for(i <- 0 to p.length-2){
      arg(i) = if (p(i).equals(classOf[Array[IntegerVariable]])) getIntVarArray(args(i))
                else if (p(i).equals(classOf[IntegerVariable])) getIntVar(args(i))//TODO: differentiate par vs var
                else if (p(i).equals(classOf[Array[BooleanVariable]])) getBoolVarArray(args(i))
                else if (p(i).equals(classOf[BooleanVariable])) getBoolVar(args(i))//TODO: differentiate par vs var
                else if(p(i).equals(classOf[FzDomain])) getIntSet(args(i))
                else throw new Exception("Case not handled: "+p(i));
    }/**/
    arg(p.length-1) = ann;
    //println(arg.length)
    val x = arg;//new AsJava(arg)
    val h = new Help()
    //GenericConstraint(c.toString(),List.empty[Object],ann)
    h.buildConstraint(cc.asInstanceOf[Constructor[Constraint]],x/*.asJava*/)
    
    
    //GenericConstraint(c.toString(),arg.toList.take(p.length-1),ann)
    //cc.newInstance(x).asInstanceOf[Constraint]
    //.tupled(arg)
  }
  
  def createDomainSet(s:java.util.Set[Integer]): FzDomainSet = {
    new FzDomainSet(s.asScala.map(i=> Int.unbox(i)).toSet)
  }
}


  