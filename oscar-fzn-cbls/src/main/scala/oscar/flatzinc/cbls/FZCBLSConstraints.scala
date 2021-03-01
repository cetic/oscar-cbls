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
package oscar.flatzinc.cbls

import oscar.cbls.core.constraint.ConstraintSystem
import oscar.cbls.core.constraint.{Constraint => CBLSConstraint}
import oscar.cbls.core.computation.{Variable => CBLSVariable}
import oscar.cbls.lib.invariant.logic._
import oscar.cbls.lib.invariant.minmax._
import oscar.cbls.lib.invariant.numeric._
import oscar.cbls.lib.constraint._
import oscar.flatzinc.model._
import oscar.flatzinc.model.Variable
import oscar.flatzinc.model.Constraint

import scala.collection.mutable.{Map => MMap}
import scala.collection.immutable.SortedMap
import oscar.flatzinc.NoSuchConstraintException
import oscar.cbls.core.computation.CBLSIntConst
import oscar.cbls.core.computation.CBLSSetConst
import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.IntValue
import oscar.cbls.core.computation.CBLSIntVar
import oscar.cbls.lib.constraint._

import scala.collection.immutable.SortedSet
import oscar.flatzinc.cbls.support.Weight
import oscar.flatzinc.cbls.support.EnsureDomain
import oscar.cbls.lib.invariant.numeric.Step





class FZCBLSConstraintPoster(val c: ConstraintSystem, implicit val getCBLSVar: Variable => IntValue) {
  val m: Store = c.model   
  
  /*def get_count_eq(xs:Array[Variable], y: Variable, cnt:Variable, ann: List[Annotation])(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    //xs domain goes from i to j but cnts will be from 0 to i-j, so need to use the offset (built by DenseCount)
    val dc = DenseCount.makeDenseCount(xs.map(getIntValue(_)));
    val cnts = dc.counts
    EQ(cnt,IntElement(y,cnts,dc.offset))
  }*/
  
  
  def get_alldifferent(xs: Array[IntegerVariable], ann: List[Annotation]) = {
    AllDiff(xs.map(getCBLSVar(_)))
  }
  
  def get_cumulative(s: Array[IntegerVariable], d: Array[IntegerVariable],r: Array[IntegerVariable],b: IntegerVariable, ann: List[Annotation]) = {
    val disjunctive = r.forall(v => 2*v.min > b.max) 
    val fixedduration = d.forall(v => v.isBound)
    val unitduration = d.forall(v => v.isBound && v.value==1)

    val useNewCumulative = true

    if(disjunctive && unitduration){
      AllDiff(s.map(getCBLSVar(_)))
    }else if(disjunctive && fixedduration){
      Disjunctive(s.map(getCBLSVar(_)),d.map(getCBLSVar(_)))
    }else if(useNewCumulative){
      val start = s.foldLeft(Int.MaxValue)((acc,v) => if (v.min < acc) v.min else acc)
      val ns = new Array[IntValue](s.length)
      val horizon = s.foldLeft(Int.MinValue)((acc,v) => if (v.max > acc) v.max else acc)
      
      val maxprofile = r.foldLeft(0)((s,r) => s + r.max)
      if(start!=0){
        val offset = CBLSIntConst(-start)
        for(i <- 0 to s.length-1){
          ns(i) = Sum2(s(i),offset)
        }
      }else{
        for(i <- 0 to s.length-1){
          ns(i) = s(i)
        }
      }
      CumulativePrototype(ns,d.map(getCBLSVar(_)),r.map(getCBLSVar(_)),getCBLSVar(b));
    }else{
      val start = s.foldLeft(Int.MaxValue)((acc,v) => if (v.min < acc) v.min else acc)
      val ns = new Array[IntValue](s.length)
      val horizon = s.foldLeft(Int.MinValue)((acc,v) => if (v.max > acc) v.max else acc)
      val p = new Array[CBLSIntVar](horizon-start+1)

      val maxprofile = r.foldLeft(0)((s,r) => s + r.max)
      for(i <- 0 to horizon-start){
        p(i) = CBLSIntVar(m,0,0 to maxprofile,"Profile("+i+")")
      }
      if(start!=0){
        val offset = CBLSIntConst(-start)
        for(i <- 0 to s.length-1){
          ns(i) = Sum2(s(i),offset)
        }
      }else{
        for(i <- 0 to s.length-1){
          ns(i) = s(i)
        }
      }
      val cumul = CumulativeNoSet(ns,d.map(getCBLSVar(_)),r.map(getCBLSVar(_)),p);
      GE(b,MaxArray(p.asInstanceOf[Array[IntValue]]))//TODO: What we should actually do is to create the array in CumulativeNoSet
      //TODO: The following class may have some bugs
      //CumulativePrototype(ns,d.map(getIntValue(_)),r.map(getIntValue(_)),getIntValue(b));
    }
  }
  
  
  
  def get_array_bool_and_inv(as: Array[BooleanVariable], r: BooleanVariable, defId: String, ann: List[Annotation]) = {
    And(as.map(getCBLSVar(_)))
  }
  
  def get_bool_and_inv(a: BooleanVariable, b: BooleanVariable, ann: List[Annotation]) = {
    And(List(a,b))
  }
  def get_array_int_element_inv(b: IntegerVariable, as: Array[IntegerVariable], r: IntegerVariable, defId: String, ann: List[Annotation]) = {
   val idx = Sum2(b,-1)

    val k = Max2(0,Min2(idx,as.length-1))
    add_constraint(EQ(k,idx))

    if(as.forall(_.isBound)) IntElementNoVar(k, as.map(_.value))
    else IntElement(k, as.map(getCBLSVar(_)))

    //if(as.forall(_.isBound)) IntElementNoVar(Sum2(b,-1), as.map(_.value))
    //else IntElement(Sum2(b,-1), as.map(getIntValue(_)))
    //TODO: Integrate the offset in the invariant?
  }
  def get_array_bool_element_inv(b: IntegerVariable, as: Array[BooleanVariable], r: BooleanVariable, defId: String, ann: List[Annotation]) = {
    val idx = Sum2(b,-1)
    val k = Max2(0,Min2(idx,as.length-1))
    add_constraint(EQ(k,idx))

    if(as.forall(_.isBound)) IntElementNoVar(k, as.map(_.violValue))
    else IntElement(k, as.map(getCBLSVar(_)))

    //if(as.forall(_.isBound)) IntElementNoVar(Sum2(b,-1), as.map(_.intValue))
    //else IntElement(Sum2(b,-1), as.map(getIntValue(_)))
    //TODO: Integrate the offset in the invariant?
  }

  def get_array_bool_or(as: Array[BooleanVariable], r: BooleanVariable, ann: List[Annotation]) = {
    BoolEQ(Or(as.map(getCBLSVar(_))),getCBLSVar(r))
  }

  def get_array_bool_or_inv(as: Array[BooleanVariable], r: BooleanVariable, defId: String, ann: List[Annotation]) = {
    Or(as.map(getCBLSVar(_)))
  }

  def get_array_bool_xor(as: Array[BooleanVariable], ann: List[Annotation]) = {
    EQ(0,XOR(as.map(getCBLSVar(_)))) //EQ(Mod(Sum(as.map(getIntValue(_))), 2), 1)
  }

  def get_array_bool_xor_inv(as: Array[BooleanVariable], defId: String, ann: List[Annotation]) = {
    val index = as.indexWhere(p => p.id == defId);
    if(index < 0){
      throw new Exception(defId + " is defined by this constraint but is not one of the variables the constraint is applied on.")
    }
    val defVar = as(index);
    val vars2 = (as.take(index) ++ as.drop(index + 1)).map(getCBLSVar(_));
    Not(XOR(vars2))
  }

  def get_bool_clause(as: Array[BooleanVariable], bs: Array[BooleanVariable], ann: List[Annotation]) = {

    if(as.exists(_.isTrue) || bs.exists(_.isFalse)){
      System.err.println("% Redundant variable in bool_clause")
    }

    BoolLE(if(as.length == 1) as.head else Or(as.map(getCBLSVar(_))),
           if(bs.length == 1) bs.head else And(bs.map(getCBLSVar(_))))
  }

  def get_bool_not_inv(a: BooleanVariable, b: BooleanVariable, defId: String, ann: List[Annotation]) = {
    if (a.id == defId) {
      Not(b)
    } else {
      Not(a)
    }
  }

  def get_bool_or_inv(a: BooleanVariable, b: BooleanVariable, r: BooleanVariable, defId: String, ann: List[Annotation]) = {
    Or(Array(a,b))
  }

  
  def get_int_abs_inv(a: IntegerVariable, b: IntegerVariable, defId: String, ann: List[Annotation]) = {
    Abs(a)
  }

  
  def get_int_div_inv(a: IntegerVariable, b: IntegerVariable, c: IntegerVariable, defId: String, ann: List[Annotation]) = {
    //TODO: can this also define a and b? NO
    Div(a, b)
  }

  def get_bool_eq_inv(x: Variable, y: Variable, defId: String, ann: List[Annotation]) = {
    if (x.id == defId) {
      getCBLSVar(y)
    } else {
      getCBLSVar(x)
    }
  }

  def get_int_eq_inv(x: Variable, y: Variable, defId: String, ann: List[Annotation]) = {
    if (x.id == defId) {
      getCBLSVar(y)
    } else {
      getCBLSVar(x)
    }
  }

  def get_bool_2_int_inv(x: Variable, y: Variable, defId: String, ann: List[Annotation]) = {
    if (y.id == defId) {
      Bool2Int(x)
    } else {
      //Ensure domain before restricting it in case something has been posted on the old domain.
      EnsureDomain(getCBLSVar(y), FzDomainRange(0, 1), c)
      getCBLSVar(y).restrictDomain(0 to 1)
      Minus(1,getCBLSVar(y))
    }
  }


  def get_int_lin_eq(params: Array[IntegerVariable], vars: Array[IntegerVariable], sum: IntegerVariable, ann: List[Annotation]) = {
    EQ(new Linear(vars.map(getCBLSVar(_)),params.map(_.value)), sum)
  }
  def get_bool_lin_eq(params: Array[IntegerVariable], vars: Array[BooleanVariable], sum: IntegerVariable, ann: List[Annotation]) = {
    EQ(new Linear(vars.map(Bool2Int(_)),params.map(_.value)), sum)
  }
  //TODO: Why is params an array of _Variable_ and not _Parameters_?
  def get_int_lin_eq_inv(params: Array[IntegerVariable], vars: Array[IntegerVariable], sum: IntegerVariable, defId: String, ann: List[Annotation]) = {
    //Note that sum is always a constant
    val index = vars.indexWhere(p => p.id == defId);
    val defParam = params(index);
    val defVar = vars(index);
    val params2 = params.take(index) ++ params.drop(index + 1)
    val vars2 = vars.take(index) ++ vars.drop(index + 1)

    val linear = new Linear(vars2.map(getCBLSVar(_)),params2.map(_.value))
    if (defParam.value == 1) {
      Minus(sum, linear)
    } else if (defParam.value == -1) {
      Minus(linear, sum)
    } else {
      Console.err.println("% Defining var with a scalar that isn't +-1, this can cause serious problems")
      Div(Minus(sum, linear), defParam)
    }
  }
  def get_bool_lin_eq_inv(params: Array[IntegerVariable], vars: Array[BooleanVariable], sum: IntegerVariable, defId: String, ann: List[Annotation]) = {
    //Note that sum is NOT a constant!!!
    //We can only define sum!!
    new Linear(vars.map(Bool2Int(_)),params.map(_.value))
  }
  
  def get_int_lin_le(params: Array[IntegerVariable], vars: Array[IntegerVariable], sum: IntegerVariable, ann: List[Annotation]) = {
    //LE(new Sum(vars.zip(params).map{ case (v,p) => Prod2(getIntValue(v),p.value)}), sum)
    LE(new Linear(vars.map(getCBLSVar(_)),params.map(_.value)), sum)
  }
  def get_bool_lin_le(params: Array[IntegerVariable], vars: Array[BooleanVariable], sum: IntegerVariable, ann: List[Annotation]) = {
    //LE(new Sum(vars.zip(params).map{ case (v,p) => Prod2(getIntValue(v),p.value)}), sum)
    LE(new Linear(vars.map(Bool2Int(_)),params.map(_.value)), sum)
  }
  def get_int_lin_ne(params: Array[IntegerVariable], vars: Array[IntegerVariable], sum: IntegerVariable, ann: List[Annotation]) = {
    NE(new Linear(vars.map(getCBLSVar(_)),params.map(_.value)), sum)
  }

  def get_int_max(a: IntegerVariable, b: IntegerVariable, c: IntegerVariable, ann: List[Annotation]) = {
    EQ(Max2(a, b), c)
  }
  def get_int_max_inv(a: IntegerVariable, b: IntegerVariable, c: IntegerVariable, defId: String, ann: List[Annotation]) = {
    if (defId == c.id) {
      Max2(a, b)
    } else if (defId == a.id) { // This is superweird but it happened in a fzModel...
      Max2(c, b)
    } else {
      Max2(a, c)
    }
  }

  def get_int_min(a: IntegerVariable, b: IntegerVariable, c: IntegerVariable, ann: List[Annotation]) = {
    EQ(Min2(a, b), c)
  }
  def get_int_min_inv(a: IntegerVariable, b: IntegerVariable, c: IntegerVariable, defId: String, ann: List[Annotation]) = {
    if (defId == c.id) {
      Min2(a, b)
    } else if (defId == a.id) { // This is superweird but it happened in a fzModel...
      Min2(c, b)
    } else {
      Min2(a, c)
    }
  }

  def get_int_pow(a: IntegerVariable, b: IntegerVariable, c: IntegerVariable, ann: List[Annotation]) = {
    EQ(Pow(a, b), c)
  }
  def get_int_pow_inv(a: IntegerVariable, b: IntegerVariable, c: IntegerVariable, defId: String, ann: List[Annotation]) = {
    if (defId == c.id) {
      Pow(a, b)
    } else if (defId == a.id) { // This should never happen...
      Pow(c, b)
    } else {
      Pow(a, c)
    }
  }

  def get_int_mod_inv(a: IntegerVariable, b: IntegerVariable, c: IntegerVariable, defId: String, ann: List[Annotation]) = {
    Mod(a, b)
  }

  def get_int_ne(x: Variable, y: Variable, ann: List[Annotation]) = {
    NE(x, y)
  }

  def get_int_plus(x: IntegerVariable, y: IntegerVariable, z: IntegerVariable, ann: List[Annotation]) = {
    EQ(Sum2(x, y), z)
  }
  def get_int_plus_inv(x: IntegerVariable, y: IntegerVariable, z: IntegerVariable, defId: String, ann: List[Annotation]) = {
    if (x.id == defId) {
      Minus(z, y)
    } else if (y.id == defId) {
      Minus(z, x)
    } else {
      Sum2(x, y)
    }
  }

  def get_int_times_inv(x: IntegerVariable, y: IntegerVariable, z: IntegerVariable, defId: String, ann: List[Annotation]) = {
    //TODO: Can times define x and y? NO
    assert(defId.equals(z.id));
    Prod2(x, y)
  }
  
  def get_set_in(x: IntegerVariable, s: FzDomain, ann: List[Annotation]) = {
    BelongsToConst(x, s.toSortedSet)
  }
  
  def get_maximum_inv(x: Array[IntegerVariable], ann: List[Annotation]) = {
    MaxArray(x.map(getCBLSVar(_)))
  }
  def get_minimum_inv(x: Array[IntegerVariable], ann: List[Annotation]) = {
    MinArray(x.map(getCBLSVar(_)))
  }
  
  def get_inverse(xs: Array[IntegerVariable], ys:Array[IntegerVariable]) = {
    //TODO: Add alldiff as redundant constraint?
    //TODO: check the index_sets? Assumes it starts at 1
    xs.zipWithIndex.map{case (xi,i) => EQ(i,Sum2(IntElement(Sum2(xi,-1),ys.map(getCBLSVar(_))),-1))}.toList
  }
  
  def get_count_eq_inv(xs:Array[IntegerVariable], y: IntegerVariable, C:IntegerVariable, defined: String, ann: List[Annotation]) = {

    if(y.min == y.max){
      ConstCount(xs.map(getCBLSVar(_)),y.min)
    }else {
      //TODO: DenseCount might be quite expensive...
      //xs domain goes from i to j but cnts will be from 0 to i-j
      val dc = DenseCount.makeDenseCount(xs.map(getCBLSVar(_)));
      val cnts = dc.counts.map(_.asInstanceOf[IntValue])

      val newY = Sum2(y, dc.offset)

      val k = Max2(0, Min2(newY, cnts.length - 1))
      add_constraint(EQ(k, newY))

      IntElement(k, cnts)
      //IntElement(newY,cnts);
    }
  }
  
  def get_nvalue_inv(xs:Array[IntegerVariable], ann: List[Annotation]) = {
    val inv = new Nvalue(xs.map(getCBLSVar(_)))
    inv
  }
  
  
  def get_at_least_int(n:IntegerVariable,xs: Array[IntegerVariable], v:IntegerVariable, ann: List[Annotation]) = {
    val cnt = new CBLSIntVar(m,0,0 to xs.length,"Count("+v.value+")")
    val sc = SparseCount(xs.map(getCBLSVar(_)),Map((v.value,cnt)))
    LE(n.value,cnt)
    //AtLeast(xs.map(getIntValue(_)),SortedMap((v.min,n)));
  }
  def get_at_most_int(n:IntegerVariable,xs: Array[IntegerVariable], v:IntegerVariable, ann: List[Annotation]) = {
    val cnt = new CBLSIntVar(m,0,0 to xs.length,"Count("+v.value+")")
    val sc = SparseCount(xs.map(getCBLSVar(_)),Map((v.value,cnt)))
    GE(n.value,cnt)
    //AtMost(xs.map(getIntValue(_)),SortedMap((v.min,n.min)));
  }
  def get_exactly_int(n:IntegerVariable,xs: Array[IntegerVariable], v:IntegerVariable, ann: List[Annotation]) = {
    //TODO: Implement lightweight version of this and the two above ones.
    //List(AtMost(xs.map(getIntValue(_)),SortedMap((v.min,n.min))),AtLeast(xs.map(getIntValue(_)),SortedMap((v.min,n))));
    val cnt = new CBLSIntVar(m,0,0 to xs.length,"Count("+v.value+")")
    val sc = SparseCount(xs.map(getCBLSVar(_)),Map((v.value,cnt)))
    EQ(n.value,cnt)
  }
  /*def get_among_inv(n:Variable,xs: Array[Variable], v:Variable, ann: List[Annotation])(implicit c: ConstraintSystem, cblsIntMap: MMap[String, CBLSIntVarDom]) = {
    List(AtMost(xs.map(getIntValue(_)),SortedMap((v.min,n.min))),AtLeast(xs.map(getIntValue(_)),SortedMap((v.min,n))));
  }*/
  //constrains all variables in xs to take their value in dom
  def domains(xs: Array[IntegerVariable], dom: Array[Int]) = {
      //val setVar = new CBLSSetConst(dom.to[SortedSet])
    xs.toList.map(x => Weight(BelongsToConst(getCBLSVar(x),dom.toSet),100))//TODO: Why 100?
  }
  def get_global_cardinality_low_up(closed: Boolean, xs: Array[IntegerVariable],vs: Array[IntegerVariable],lows: Array[Int],ups:Array[Int]) = {
    //TODO: Use a lighter version!
    val atleast = AtLeast(xs.map(getCBLSVar(_)),SortedMap(vs.zip(lows).map(vl => (vl._1.min,CBLSIntConst(vl._2))): _*))
    val atmost = AtMost(xs.map(getCBLSVar(_)),SortedMap(vs.zip(ups).map(vl => (vl._1.min,CBLSIntConst(vl._2))): _*))
    List(atleast,atmost) ++ (if(closed) domains(xs,vs.map(_.min)) else List())
  }
  def get_global_cardinality(closed: Boolean, xs: Array[IntegerVariable],vs: Array[IntegerVariable],cnts: Array[IntegerVariable]) = {
     if(cnts.forall(c => c.min==c.max)){//fixed counts
       get_global_cardinality_low_up(closed,xs,vs,cnts.map(_.min),cnts.map(_.max))
     }else{
       //TODO: Might be more efficient...
       val dc = DenseCount.makeDenseCount(xs.map(getCBLSVar(_)));
       val counts = dc.counts
       val eqs = vs.toList.zip(cnts).map(_ match {case (v,c) => EQ(c,counts(v.min+dc.offset))})//TODO: +offset or -offset? seems to be +
       if(closed) domains(xs,vs.map(_.min)) ++ eqs else eqs
     }
  }

  def get_bin_packing_load(load: Array[IntegerVariable], bin: Array[IntegerVariable], w: Array[IntegerVariable], ann: List[Annotation]): CBLSConstraint = {
    MultiKnapsackLoad(bin.map(v => Sum2(getCBLSVar(v),-1)),w.map(getCBLSVar(_)),load.map(getCBLSVar(_)))
  }

  def get_table_int(xs: Array[IntegerVariable], ts:Array[IntegerVariable], ann: List[Annotation]): CBLSConstraint = {
    val foldedTs:Array[Array[Int]] = Array.tabulate(ts.size/xs.size)(row =>
      Array.tabulate(xs.size)(i => ts(row*xs.size + i).value)
    )
    Table(xs.map(getCBLSVar(_)),foldedTs)
  }

  def get_table_bool(xs: Array[BooleanVariable], ts:Array[BooleanVariable], ann: List[Annotation]): CBLSConstraint = {
    val foldedTs:Array[Array[Int]] = Array.tabulate(ts.size/xs.size)(row =>
      Array.tabulate(xs.size)(i => ts(row*xs.size + i).violValue)
    )
    Table(xs.map(getCBLSVar(_)),foldedTs)
  }



  implicit def cstrListToCstr(cstrs: List[CBLSConstraint]): CBLSConstraint = {
    val cs = new ConstraintSystem(m)
    for(cstr <- cstrs){
      cs.add(cstr)
    }
    cs.close()
    cs
  }


  def constructCBLSConstraint(constraint: Constraint):CBLSConstraint = {
    constraint match {
      case reif(cstr,r) =>
        BoolEQ(r,constructCBLSConstraint(cstr).violation)
      
      case array_bool_and(as, r, ann)                 => BoolEQ(r,get_array_bool_and_inv(as, r,r.id, ann))
      case array_bool_element(b, as, r, ann)          => BoolEQ(r,get_array_bool_element_inv(b, as, r, r.id, ann))
      case array_bool_or(as, r, ann)                  => get_array_bool_or(as,r, ann)
      case array_bool_xor(as, ann)                    => get_array_bool_xor(as, ann)
      case array_int_element(b, as, r, ann)           => EQ(r,get_array_int_element_inv(b, as, r,r.id, ann))
      case array_var_bool_element(b, as, r, ann)      => BoolEQ(r,get_array_bool_element_inv(b, as, r,r.id, ann))
      case array_var_int_element(b, as, r, ann)       => EQ(r,get_array_int_element_inv(b, as, r,r.id, ann))

      case bool2int(x, y, ann)                        => EQ(Bool2Int(x),y)
      case bool_and(a, b, r, ann)                     => BoolEQ(r,get_bool_and_inv(a, b, ann))
      case bool_clause(a, b, ann)                     => get_bool_clause(a, b, ann)
      case bool_eq(a, b, ann)                         => BoolEQ(a,b)
      case bool_le(a, b, ann)                         => BoolLE(a, b)
      case bool_lin_eq(params, vars, sum, ann)        => get_bool_lin_eq(params, vars, sum, ann)
      case bool_lin_le(params, vars, sum, ann)        => get_bool_lin_le(params, vars, sum, ann)
      case bool_lt(a, b, ann)                         => BoolLT(a,b)
      case bool_not(a, b, ann)                        => BoolEQ(Not(a),b)
      case bool_or(a, b, r, ann)                      => EQ(r,Or(Array(a,b)))
      case bool_xor(a, b, r, ann)                     => BoolEQ(r,XOR(a,b))

      case int_abs(x, y, ann)                         => EQ(y,get_int_abs_inv(x, y, y.id,ann))
      case int_div(x, y, z, ann)                      => EQ(x,get_int_div_inv(x,y,z,x.id,ann))//EQ(z,get_int_div_inv(x, y, z,z.id, ann))
      case int_eq(x, y, ann)                          => EQ(x,y)
      case int_le(x, y, ann)                          => LE(x, y)
      case int_lin_eq(params, vars, sum, ann)         => get_int_lin_eq(params, vars, sum, ann)
      case int_lin_le(params, vars, sum, ann)         => get_int_lin_le(params, vars, sum, ann)
      case int_lin_ne(params, vars, sum, ann)         => get_int_lin_ne(params, vars, sum, ann)
      case int_lt(x, y, ann)                          => L(x,y)
      case int_max(x, y, z, ann)                      => get_int_max(x, y, z, ann)
      case int_min(x, y, z, ann)                      => get_int_min(x, y, z, ann)
      case int_mod(x, y, z, ann)                      => EQ(z,get_int_mod_inv(x, y, z,z.id, ann))
      case int_ne(x, y, ann)                          => get_int_ne(x, y, ann)
      case int_plus(x, y, z, ann)                     => get_int_plus(x, y, z, ann)
      case int_times(x, y, z, ann)                    => EQ(z,get_int_times_inv(x, y, z,z.id, ann))
      case set_in(x, s, ann)                          => get_set_in(x, s, ann)
      case int_pow(a,b,c,ann)                         => get_int_pow(a,b,c,ann)
      
      case all_different_int(xs, ann)                 => get_alldifferent(xs, ann)
      case at_least_int(n,xs,v,ann)                   => get_at_least_int(n,xs,v,ann)
      case at_most_int(n,xs,v,ann)                    => get_at_most_int(n,xs,v,ann)
      case cumulative(s,d,r,b,ann)                    => get_cumulative(s,d,r,b,ann)
      case count_eq(xs,y,cnt,ann)                     => EQ(cnt,get_count_eq_inv(xs,y,cnt,cnt.id,ann))
      case exactly_int(n,xs,v,ann)                    => get_exactly_int(n,xs,v,ann)
      case inverse(xs,ys,ann)                         => get_inverse(xs,ys)
      case global_cardinality_closed(xs,vs,cs,ann)    => get_global_cardinality(true,xs,vs,cs)
      case global_cardinality(xs,vs,cs,ann)           => get_global_cardinality(false,xs,vs,cs)
      case global_cardinality_low_up_closed(xs,vs,ls,us,ann) => get_global_cardinality_low_up(true,xs,vs,ls.map(_.min),us.map(_.min))
      case global_cardinality_low_up(xs,vs,ls,us,ann) => get_global_cardinality_low_up(false,xs,vs,ls.map(_.min),us.map(_.min))
      case maximum_int(y,xs,ann)                      => EQ(y,get_maximum_inv(xs,ann))

     // case member_int(xs,y,ann)                       => get_member(xs,y) use the decomposition
      case minimum_int(y,xs,ann)                      => EQ(y,get_minimum_inv(xs,ann))
      case nvalue_int(y,xs,ann)                       => EQ(y,get_nvalue_inv(xs,ann))
      case bin_packing_load(load,bin,w,ann)           => get_bin_packing_load(load,bin,w,ann)
      case table_int(xs,ts, ann)                      => get_table_int(xs,ts,ann)
      case table_bool(xs,ts, ann)                     => get_table_bool(xs,ts,ann)
      case notimplemented                             => throw new NoSuchConstraintException(notimplemented.toString(),"CBLS Solver");
    }
  }


  def constructCBLSIntInvariant(constraint: Constraint, id:String): IntValue = {
    constraint match {
      case reif(cstr,r) => constructCBLSConstraint(cstr).violation
      
      case array_bool_and(as, r, ann)                 => get_array_bool_and_inv(as, r, id, ann)
      case array_bool_element(b, as, r, ann)          => get_array_bool_element_inv(b, as, r, id, ann)
      case array_bool_or(as, r, ann)                  => get_array_bool_or_inv(as, r, id, ann)
      case array_bool_xor(as, ann)                    => get_array_bool_xor_inv(as, id, ann)
      case array_int_element(b, as, r, ann)           => get_array_int_element_inv(b, as, r, id, ann)
      case array_var_bool_element(b, as, r, ann)      => get_array_bool_element_inv(b, as, r, id, ann)
      case array_var_int_element(b, as, r, ann)       => get_array_int_element_inv(b, as, r, id, ann)

      case bool2int(x, y, ann)                        => get_bool_2_int_inv(x, y, id, ann)
      case bool_and(a, b, r, ann)                     => get_bool_and_inv(a, b, ann)
      case bool_eq(a, b, ann)                         => get_bool_eq_inv(a, b, id, ann)
      case bool_lin_eq(params, vars, sum, ann)        => get_bool_lin_eq_inv(params, vars, sum, id, ann)
      case bool_not(a, b, ann)                        => get_bool_not_inv(a, b, id, ann)
      case bool_or(a, b, r, ann)                      => get_bool_or_inv(a, b, r, id, ann)
      case bool_xor(a, b, r, ann)                     => XOR(a,b) //This assumes that only r can be defined!

      case int_abs(x, y, ann)                         => get_int_abs_inv(x, y, id, ann)
      case int_div(x, y, z, ann)                      => get_int_div_inv(x, y, z, id, ann)
      case int_eq(x, y, ann)                          => get_int_eq_inv(x, y, id, ann)
      case int_lin_eq(params, vars, sum, ann)         => get_int_lin_eq_inv(params, vars, sum, id, ann)
      case int_max(x, y, z, ann)                      => get_int_max_inv(x, y, z, id, ann)
      case int_min(x, y, z, ann)                      => get_int_min_inv(x, y, z, id, ann)
      case int_mod(x, y, z, ann)                      => get_int_mod_inv(x, y, z, id, ann)
      case int_plus(x, y, z, ann)                     => get_int_plus_inv(x, y, z, id, ann)
      case int_times(x, y, z, ann)                    => get_int_times_inv(x, y, z, id, ann)
      case int_pow(x, y, z, ann)                      => get_int_pow_inv(x, y, z, id, ann)
     
      case count_eq(xs,y,cnt,ann)                     => get_count_eq_inv(xs,y,cnt,id,ann)
      case maximum_int(y,xs,ann)                      => get_maximum_inv(xs,ann)//assumes that the id is y.
      case minimum_int(y,xs,ann)                      => get_minimum_inv(xs,ann)
      case nvalue_int(y,xs,ann)                       => get_nvalue_inv(xs,ann)
      //case bin_packing_load(load,bin,w,ann)           => get_bin_packing_load_inv(load,bin,w,ann) //defining multiple variables is not supported by the fzn backend yet.

      case notimplemented                             => throw new NoSuchConstraintException(notimplemented.toString(),"CBLS Solver");
    }
  }
  

  def add_constraint(constraint: CBLSConstraint) = {
    c.add(constraint)// , CBLSIntVar(c.model,1,1 to 1000000, "Weight for "+ constraint.toString))
  }

  def construct_and_add_constraint(constraint: Constraint) = {
    add_constraint(constructCBLSConstraint(constraint))
  }

  def construct_and_add_invariant(constraint: Constraint):IntValue = {
    constraint.definedVar match {
      case None =>
        throw new Exception("Constraint "+constraint+" is not supposed to be an invariant.")
      case Some(v) =>
        val inv = constructCBLSIntInvariant(constraint,v.id)
        val dom = v match{
          case IntegerVariable(i,d,ann) => EnsureDomain(inv,d,c)
          case bv: BooleanVariable => FzDomainRange(if(bv.isTrue) 1 else 0, if(bv.isFalse) 0 else 1)
        }
        //WARNING: removing ensuredom from booleanvariables...
        //EnsureDomain(inv,dom,c)
        inv
    }
  }
}