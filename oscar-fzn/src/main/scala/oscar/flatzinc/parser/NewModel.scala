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

package oscar.flatzinc.parser

import java.lang.reflect.Constructor

import scala.collection.JavaConverters._
import scala.collection.mutable.WrappedArray
import oscar.flatzinc.{Log, NoSuchConstraintException, NoSuchMoveException, ParsingException}
import oscar.flatzinc.model._
import oscar.flatzinc.parser.intermediatemodel.ASTDecls.{ASTDecl, ASTFuncDecl, ASTParamDecl, ASTVarDecl}
import oscar.flatzinc.parser.intermediatemodel.ASTLiterals._
import oscar.flatzinc.parser.intermediatemodel.ASTTypes.{ASTArrayType, ASTConstants, ASTType, ASTVarType}
import oscar.flatzinc.parser.intermediatemodel._

import scala.collection.mutable
import scala.collection.mutable.Map

/**
  * @author Gustav Bjordal
  */
class Model(val log: Log, val acceptAnyCstr: Boolean) {


  val problem: FZProblem = new FZProblem()
  val funcDict: Map[String, ASTFuncDecl] = Map.empty[String, ASTFuncDecl]
  //val varDeclDict: Map[String,ASTVarDecl] = Map.empty[String,ASTVarDecl]
  //val paramDeclDict: Map[String,ASTParamDecl] = Map.empty[String,ASTParamDecl]

  val declDict: Map[String, ASTDecl] = Map.empty[String, ASTDecl]

  val varDict: Map[String, Variable] = Map.empty[String, Variable]


  def buildModel(m: ASTModel) = {
    for (p <- m.getParamDecls.asScala) {
      addParam(p)
    }
    for (v <- m.getVarDecls.asScala) {
      addVar(v)
    }
    for (c <- m.getConstraints.asScala) {
      addConstraint(c)
    }

    for (f <- m.getFuncDecls.asScala) {
      addFunc(f)
    }

    m.getSolve.getType match {
      case ASTSolve.SAT =>
        setSATObjective(getAnnotations(m.getSolve.getAnns.asScala.toList))
      case ASTSolve.MAX =>
        setMAXObjective(m.getSolve.getExpr, getAnnotations(m.getSolve.getAnns.asScala.toList))
      case ASTSolve.MIN =>
        setMINObjective(m.getSolve.getExpr, getAnnotations(m.getSolve.getAnns.asScala.toList))
    }

    m.getSolve.getAnns.asScala.foreach((lit: ASTLit) =>
                                 lit match {
                                   case ann: ASTAnnotation if ann.getId.getValue == "neighbourhood_and" =>
                                     val argArray = ann.getArgs.get(0).asInstanceOf[ASTArray]
                                     for (a <- argArray.getElems.asScala) {
                                       addNeighbourhood(a)
                                     }
                                   case notused => log(1, "Ignoring annotation: " + notused)
                                 }
    )
  }

  private def addDecl(decl: ASTDecl): String = {
    decl match {
      case d: ASTParamDecl => addParam(d)
      case d: ASTVarDecl => addVar(d)
    }
  }

  private def addParam(decl: ASTParamDecl): String = {
    //TODO: Handle annotations on params (output vals)
    declDict(decl.getId.getValue) = decl
    if (decl.getAnns != null) {
      handleOutputAnnotations(decl.getName,decl, getAnnotations(decl.getAnns.asScala.toList))
    }
    decl.getName
  }

  private def addVar(decl: ASTVarDecl): String = {
    if(decl.hasExpr && decl.getExpr.isInstanceOf[ASTId]){
      val alias:ASTId = decl.getExpr.asInstanceOf[ASTId]
      val aliasDecl = declDict(alias.getValue)
      declDict(decl.getId.getValue) = aliasDecl
      if (decl.getAnns != null) {
        handleOutputAnnotations(decl.getName, aliasDecl, getAnnotations(decl.getAnns.asScala.toList))
      }
      alias.getValue
    }else {
      declDict(decl.getId.getValue) = decl
      if (decl.getAnns != null) {
        handleOutputAnnotations(decl.getName, decl, getAnnotations(decl.getAnns.asScala.toList))
      }

      if (!decl.getType.isInstanceOf[ASTArrayType]) {
        decl.getType.getDataType match {
          case ASTConstants.INT =>
            val varToAdd = getIntVar(decl.getId)
            problem.addVariable(varToAdd)
          case ASTConstants.BOOL =>
            problem.addVariable(getBoolVar(decl.getId))
        }
      }
      decl.getName
    }
  }

  private def addFunc(decl: ASTFuncDecl) = {
    funcDict(decl.getId.getValue) = decl
  }

  private def addConstraint(c: ASTConstraint) = {
    //Constraints are added to the FZProblem as soon as they are created.
    val name = c.getId.getValue
    val args = c.getArgs
    val anns = getAnnotations(c.getAnns.asScala.toList)

    val cstr = constructConstraint(name, args.asScala.toList, anns)

    setDefinedVar(cstr)

    problem.addConstraint(cstr)
  }

  private def setDefinedVar(cstr: Constraint) = {
    val (ann_def, _ann_other) = cstr.annotations.partition(a => a.name == "defines_var")
    ann_def.foreach(a => a.args(0) match {
      case ann: Annotation if ann.args.equals(List.empty[Any]) =>
        cstr.setDefinedVar(getVar(declDict(ann.name).getId));
      case _ => throw new ParsingException("defines_var annotation without argument: " + a)
    })
  }

  private def setSATObjective(anns: List[Annotation]) = {
    problem.satisfy(anns)
  }

  private def setMINObjective(lit: ASTLit, anns: List[Annotation]) = {
    problem.minimize(getIntVar(lit), anns)
  }

  private def setMAXObjective(lit: ASTLit, anns: List[Annotation]) = {
    problem.maximize(getIntVar(lit), anns)
  }

  private def handleOutputAnnotations(name:String, decl: ASTDecl, anns: Iterable[oscar.flatzinc.model.Annotation]) = {
    decl.getType match {
      case arr: ASTArrayType =>
        if (anns.exists((a: Annotation) => a.name == "output_array")) {
          val idArray = decl.getExpr.asInstanceOf[ASTArray].getElems.asScala.map((lit: ASTLit) => lit match {
            case id: ASTId => id.getValue
            case constInt: ASTInt => constInt.getValue.toString
            case constBool: ASTBool => constBool.getValue.toString
          }).toArray

          val t = if (arr.getType.isInstanceOf[ASTVarType]) {
            arr.getType.asInstanceOf[ASTVarType].getType
          } else {
            arr.getType
          }
          t match {
            case ASTConstants.INT =>
              val dims = anns.find((p: Annotation) => p.name == "output_array").get.args(0)

              problem.solution.addOutputArrayVarInt(decl.getId.getValue,
                                                    idArray,
                                                    dims.asInstanceOf[List[Range]])
            case ASTConstants.BOOL =>
              val dims = anns.find((p: Annotation) => p.name == "output_array").get.args(0)

              problem.solution
              .addOutputArrayVarBool(decl.getId.getValue,
                                     idArray,
                                     dims.asInstanceOf[List[Range]])
          }
        }
      case v: ASTVarType =>
        if (anns.exists((a: Annotation) => a.name == "output_var")) {
          v.getType match {
            case ASTConstants.INT => problem.solution.addOutputVarInt(name, decl.getName) //TODO: why two arguments?
            // name can map to different var?
            case ASTConstants.BOOL => problem.solution.addOutputVarBool(name, decl.getName)
          }
        }
    }
  }


  def getVariablesFor(lit: ASTLit): Array[Variable] = {
    lit match {
      case id: ASTId => getVariablesFor(id.getValue)
      case arr: ASTArray => arr.getElems.asScala.toList.map((a: ASTLit) => getVar(a)).toArray
    }
  }

  def getVariablesFor(name: String): Array[Variable] = {
    val decl = declDict(name)
    val id = decl.getId
    decl.getType match {
      case arr: ASTArrayType =>
        arr.getDataType match {
          case ASTConstants.BOOL => getBoolVarArray(id).asInstanceOf[Array[Variable]]
          case ASTConstants.INT => getIntVarArray(id).asInstanceOf[Array[Variable]]
        }
      case any =>
        any.getDataType match {
          case ASTConstants.BOOL => Array(getBoolVar(id))
          case ASTConstants.INT => Array(getIntVar(id))
        }
    }
  }

  def getVar(lit: ASTLit): Variable = {
    lit match {
      case id: ASTId =>
        if(declDict(id.getValue).getId != id){
          return getVar(declDict(id.getValue).getId)
        }
        if (varDict.contains(id.getValue)) {
          return varDict(id.getValue)
        }
        createVariable(declDict(id.getValue))
      case constant: ASTInt =>
        val int = constant.getValue
        new IntegerVariable(int.toString, Int.unbox(int))
      case constant: ASTBool =>
        val b = constant.getValue
        new BooleanVariable(b.toString, Some(Boolean.unbox(b)))
    }
  }

  def getIntVar(lit: ASTLit): IntegerVariable = {
    lit match {
      case constant: ASTInt =>
        val int = constant.getValue
        new IntegerVariable(int.toString, Int.unbox(int))
      case id: ASTId =>
        if(declDict(id.getValue).getId != id){
          return getIntVar(declDict(id.getValue).getId)
        }
        if (varDict.contains(id.getValue)) {
          return varDict(id.getValue).asInstanceOf[IntegerVariable]
        }
        createVariable(declDict(id.getValue)).asInstanceOf[IntegerVariable]
    }
  }

  def getBoolVar(lit: ASTLit): BooleanVariable = {
    lit match {
      case constant: ASTBool =>
        val b = constant.getValue
        new BooleanVariable(b.toString, Some(Boolean.unbox(b)))
      case id: ASTId =>
        if(declDict(id.getValue).getId != id){
          return getBoolVar(declDict(id.getValue).getId)
        }
        if (varDict.contains(id.getValue)) {
          return varDict(id.getValue).asInstanceOf[BooleanVariable]
        }
        createVariable(declDict(id.getValue)).asInstanceOf[BooleanVariable]
    }
  }

  def getBoolVarArray(lit: ASTLit): Array[BooleanVariable] = {
    lit match {
      case id: ASTId =>
        getBoolVarArray(declDict(id.getValue).getExpr)
      case arr: ASTArray =>
        val a = arr.asInstanceOf[ASTArray]
        a.getElems.asScala.map((v: ASTLit) => getBoolVar(v)).toArray
      case err => throw new ParsingException("Expected a array of var int but got: " + err)
    }
  }

  //TODO: Check if this actually reduces the memory footprint and does not increase parsing time too much...
  private var knownarrays = Map.empty[mutable.WrappedArray[IntegerVariable], Array[IntegerVariable]]

  def getIntVarArray(lit: ASTLit): Array[IntegerVariable] = {
    lit match {
      case id: ASTId =>
        getIntVarArray(declDict(id.getValue).getExpr)
      case arr: ASTArray =>
        val array = arr.getElems.asScala.map(v => getIntVar(v)).toArray
        val wrap = genericWrapArray(array)
        if (knownarrays.contains(wrap)) {
          //Console.err.println("% reuse "+knownarrays.size +" "+ knownarrays(wrap).mkString("[",", ","]"))
          knownarrays(wrap)
        } else {
          knownarrays(wrap) = array
          //Console.err.println("% added new "+knownarrays.size+" " + array.mkString("[",", ","]"))
          array
        }
      case err => throw new ParsingException("Expected a array of var int but got: " + err)
    }
  }

  def getIntSet(lit: ASTLit): FzDomain = {
    if (lit.isInstanceOf[ASTSet]) {
      val s = lit.asInstanceOf[ASTSet];
      new FzDomainSet(s.getSet.asScala.toSet.map((i: ASTInt) => Int.unbox(i.getValue)))
    } else if (lit.isInstanceOf[ASTRange]){
      val r = lit.asInstanceOf[ASTRange];
      new FzDomainRange(r.getLb.getValue, r.getUb.getValue)
    } else {
      throw new ParsingException("Expected a constant set but got: " + lit)
    }
  }


  val dictCons: Map[String, (List[ASTLit], List[Annotation]) => Constraint] = Map(
    "int_eq" -> ((varList, ann) => int_eq(getIntVar(varList(0)), getIntVar(varList(1)), ann)),
    "int_eq_reif" -> ((varList, ann) => reif(int_eq(getIntVar(varList(0)), getIntVar(varList(1)), ann),
                                             getBoolVar(varList(2)))),
    "int_ne" -> ((varList, ann) => int_ne(getIntVar(varList(0)), getIntVar(varList(1)), ann)),
    "int_ne_reif" -> ((varList, ann) => reif(int_ne(getIntVar(varList(0)), getIntVar(varList(1)), ann),
                                             getBoolVar(varList(2)))),
    "int_le" -> ((varList, ann) => int_le(getIntVar(varList(0)), getIntVar(varList(1)), ann)),
    "int_le_reif" -> ((varList, ann) => reif(int_le(getIntVar(varList(0)), getIntVar(varList(1)), ann),
                                             getBoolVar(varList(2)))),
    "int_lt" -> ((varList, ann) => int_lt(getIntVar(varList(0)), getIntVar(varList(1)), ann)),
    "int_lt_reif" -> ((varList, ann) => reif(int_lt(getIntVar(varList(0)), getIntVar(varList(1)), ann),
                                             getBoolVar(varList(2)))),
    "bool_eq" -> ((varList, ann) => bool_eq(getBoolVar(varList(0)), getBoolVar(varList(1)), ann)),
    "bool_lt" -> ((varList, ann) => bool_lt(getBoolVar(varList(0)), getBoolVar(varList(1)), ann)),
    "bool_le" -> ((varList, ann) => bool_le(getBoolVar(varList(0)), getBoolVar(varList(1)), ann)),
    "bool_not" -> ((varList, ann) => bool_not(getBoolVar(varList(0)), getBoolVar(varList(1)), ann)),
    "bool_xor" -> ((varList, ann) => bool_xor(getBoolVar(varList(0)), getBoolVar(varList(1)), getBoolVar(varList(2)),
                                              ann)),
    "array_bool_or" -> ((varList, ann) => array_bool_or(getBoolVarArray(varList(0)), getBoolVar(varList(1)), ann)),
    "array_bool_xor" -> ((varList, ann) => array_bool_xor(getBoolVarArray(varList(0)), ann)),
    "array_bool_and" -> ((varList, ann) => array_bool_and(getBoolVarArray(varList(0)), getBoolVar(varList(1)), ann)),
    "bool_clause" -> ((varList, ann) => bool_clause(getBoolVarArray(varList(0)), getBoolVarArray(varList(1)), ann)),
    "bool2int" -> ((varList, ann) => bool2int(getBoolVar(varList(0)), getIntVar(varList(1)), ann)),
    "array_int_element" -> ((varList, ann) => array_int_element(getIntVar(varList(0)), getIntVarArray(varList(1)),
                                                                getIntVar(varList(2)), ann)),
    "array_var_int_element" -> ((varList, ann) => array_var_int_element(getIntVar(varList(0)),
                                                                        getIntVarArray(varList(1)),
                                                                        getIntVar(varList(2)), ann)),
    "array_bool_element" -> ((varList, ann) => array_bool_element(getIntVar(varList(0)), getBoolVarArray(varList(1)),
                                                                  getBoolVar(varList(2)), ann)),
    "array_var_bool_element" -> ((varList, ann) => array_var_bool_element(getIntVar(varList(0)),
                                                                          getBoolVarArray(varList(1)),
                                                                          getBoolVar(varList(2)), ann)),
    "int_lin_eq" -> ((varList, ann) => int_lin_eq(getIntVarArray(varList(0)), getIntVarArray(varList(1)),
                                                  getIntVar(varList(2)), ann)),
    "int_lin_le" -> ((varList, ann) => int_lin_le(getIntVarArray(varList(0)), getIntVarArray(varList(1)),
                                                  getIntVar(varList(2)), ann)),
    "int_lin_ne" -> ((varList, ann) => int_lin_ne(getIntVarArray(varList(0)), getIntVarArray(varList(1)),
                                                  getIntVar(varList(2)), ann)),
    "int_lin_eq_reif" -> ((varList, ann) => reif(
      int_lin_eq(getIntVarArray(varList(0)), getIntVarArray(varList(1)), getIntVar(varList(2)), ann),
      getBoolVar(varList(3)))),
    "int_lin_le_reif" -> ((varList, ann) => reif(
      int_lin_le(getIntVarArray(varList(0)), getIntVarArray(varList(1)), getIntVar(varList(2)), ann),
      getBoolVar(varList(3)))),
    "int_lin_ne_reif" -> ((varList, ann) => reif(
      int_lin_ne(getIntVarArray(varList(0)), getIntVarArray(varList(1)), getIntVar(varList(2)), ann),
      getBoolVar(varList(3)))),
    "count" -> ((varList, ann) => count(getIntVarArray(varList(0)), getIntVar(varList(1)), getIntVar(varList(2)), ann)),
    "exactly_int" -> ((varList, ann) => exactly_int(getIntVar(varList(0)), getIntVarArray(varList(1)),
                                                    getIntVar(varList(2)), ann)),
    "at_least_int" -> ((varList, ann) => at_least_int(getIntVar(varList(0)), getIntVarArray(varList(1)),
                                                      getIntVar(varList(2)), ann)),
    "at_most_int" -> ((varList, ann) => at_most_int(getIntVar(varList(0)), getIntVarArray(varList(1)),
                                                    getIntVar(varList(2)), ann)),
    "count_eq" -> ((varList, ann) => count_eq(getIntVarArray(varList(0)), getIntVar(varList(1)), getIntVar(varList(2)),
                                              ann)),
    "int_max" -> ((varList, ann) => int_max(getIntVar(varList(0)), getIntVar(varList(1)), getIntVar(varList(2)), ann)),
    "int_min" -> ((varList, ann) => int_min(getIntVar(varList(0)), getIntVar(varList(1)), getIntVar(varList(2)), ann)),
    "int_pow" -> ((varList, ann) => int_pow(getIntVar(varList(0)), getIntVar(varList(1)), getIntVar(varList(2)), ann)),
    "int_times" -> ((varList, ann) => int_times(getIntVar(varList(0)), getIntVar(varList(1)), getIntVar(varList(2)),
                                                ann)),
    "int_plus" -> ((varList, ann) => int_plus(getIntVar(varList(0)), getIntVar(varList(1)), getIntVar(varList(2)),
                                              ann)),
    "int_div" -> ((varList, ann) => int_div(getIntVar(varList(0)), getIntVar(varList(1)), getIntVar(varList(2)), ann)),
    "int_mod" -> ((varList, ann) => int_mod(getIntVar(varList(0)), getIntVar(varList(1)), getIntVar(varList(2)), ann)),
    "int_abs" -> ((varList, ann) => int_abs(getIntVar(varList(0)), getIntVar(varList(1)), ann)),
    "all_different_int" -> ((varList, ann) => all_different_int(getIntVarArray(varList(0)), ann)),
    "set_in" -> ((varList, ann) => set_in(getIntVar(varList(0)), getIntSet(varList(1)), ann)),
    "member_int" -> ((varList, ann) => member_int(getIntVarArray(varList(0)), getIntVar(varList(1)), ann)),
    "maximum_int" -> ((varList, ann) => maximum_int(getIntVar(varList(0)), getIntVarArray(varList(1)), ann)),
    "minimum_int" -> ((varList, ann) => minimum_int(getIntVar(varList(0)), getIntVarArray(varList(1)), ann)),
    //TODO: If the name is not the same, it should not be same in the output either.
    "inverse_no_offset" -> ((varList, ann) => inverse(getIntVarArray(varList(0)), getIntVarArray(varList(1)), ann)),
    "subcircuit_no_offset" -> ((varList, ann) => subcircuit(getIntVarArray(varList(0)), ann)),
    "circuit_no_offset" -> ((varList, ann) => circuit(getIntVarArray(varList(0)), ann)),
    "global_cardinality" -> ((varList, ann) => global_cardinality(getIntVarArray(varList(0)),
                                                                  getIntVarArray(varList(1)),
                                                                  getIntVarArray(varList(2)), ann)),
    "global_cardinality_closed" -> ((varList, ann) => global_cardinality_closed(getIntVarArray(varList(0)),
                                                                                getIntVarArray(varList(1)),
                                                                                getIntVarArray(varList(2)), ann)),
    "global_cardinality_low_up" -> ((varList, ann) => global_cardinality_low_up(getIntVarArray(varList(0)),
                                                                                getIntVarArray(varList(1)),
                                                                                getIntVarArray(varList(2)),
                                                                                getIntVarArray(varList(3)), ann)),
    "global_cardinality_low_up_closed" -> ((varList, ann) => global_cardinality_low_up_closed(
      getIntVarArray(varList(0)), getIntVarArray(varList(1)), getIntVarArray(varList(2)), getIntVarArray(varList(3)),
      ann)),
    "cumulative" -> ((varList, ann) => cumulative(getIntVarArray(varList(0)), getIntVarArray(varList(1)),
                                                  getIntVarArray(varList(2)), getIntVar(varList(3)), ann)),
    "nvalue_int" -> ((varList, ann) => nvalue_int(getIntVar(varList(0)), getIntVarArray(varList(1)), ann)),
    "bin_packing_load" -> ((varList, ann) => bin_packing_load(getIntVarArray(varList(0)), getIntVarArray(varList(1)),
                                                              getIntVarArray(varList(2)), ann)),
    "table_int" -> ((varList, ann) => table_int(getIntVarArray(varList(0)), getIntVarArray(varList(1)), ann)),
    "table_bool" -> ((varList, ann) => table_bool(getBoolVarArray(varList(0)), getBoolVarArray(varList(1)), ann))
  )

  private def addNeighbourhood(lit: ASTLit) = {
    println("Constructing neighoburhood for: " + lit)
    val neighbourhoodFunction = funcDict(lit.asInstanceOf[ASTAnnotation].getId.getValue)
    val decl = neighbourhoodFunction.getBody.getReturnValue.asInstanceOf[ASTAnnotation]

    //TODO:Support initalization!!!
    val (initVars, initCons) = if ( decl.getArgs.size() == 2) {
      val initCall = decl.getArgs.get(1).asInstanceOf[ASTAnnotation].getArgs.get(0).asInstanceOf[ASTAnnotation]
      getVarsAndConstraintsInFunction(initCall)
    } else {
      (Array.empty[Variable], List.empty[Constraint])
    }

    val fromNeighourhoods = decl.getArgs.get(0).asInstanceOf[ASTArray].getElems.asScala.toList.asInstanceOf[List[ASTAnnotation]]

    problem.addNeighbourhood(
      new FZNeighbourhood(lit.toString,
                          fromNeighourhoods.map((ann: ASTAnnotation) => constructFromNeighbourhood(ann.getId)),
                          initVars,
                          initCons))
  }

  private def constructFromNeighbourhood(id: ASTId): FZSubNeighbourhood = {
    println("Found from-neighbourhood: " + id)
    val from = funcDict(id.getValue).getBody

    //It variables
    val (iteratorVariables, whereExpr) = from.getBody.asScala.partition((p: ASTNode) =>
                                                                  p.isInstanceOf[ASTVarDecl] &&
                                                                    p.asInstanceOf[ASTVarDecl].hasAnnotation(
                                                                      "ls_defines_generator"))

    val itVars = iteratorVariables.toList.asInstanceOf[List[ASTDecl]].map((i: ASTDecl) => addDecl(i))
    val (whereConstr, whereVars) = whereExpr.partition((p: ASTNode) => p.isInstanceOf[ASTConstraint])

    val wVars = whereVars.toList.asInstanceOf[List[ASTDecl]].map((d: ASTDecl) => addDecl(d))
    val wCons = whereConstr.toList.asInstanceOf[List[ASTConstraint]].map(
      (c: ASTConstraint) => constructConstraint(c.getId.getValue, c.getArgs.asScala.toList, getAnnotations(c.getAnns.asScala.toList)))
    wCons.foreach(c => setDefinedVar(c))

    val in = from.getReturnValue.asInstanceOf[ASTAnnotation].getArgs.get(0).asInstanceOf[ASTArray].getElems.asScala.toList
    val (ensureAnn, moveAnnotations) = in.partition(
      p => p.isInstanceOf[ASTAnnotation] && p.asInstanceOf[ASTAnnotation].getId.getValue.equals("ensure"))

    val moves = moveAnnotations.map(
      m => getMove(m.asInstanceOf[ASTAnnotation])) //getAnnotations(moveAnnotations.asInstanceOf[List[ASTAnnotation]])

    val ensureCall = ensureAnn.head.asInstanceOf[ASTAnnotation].getArgs.get(0).asInstanceOf[ASTAnnotation]
    val (eVars, eCons) = getVarsAndConstraintsInFunction(ensureCall)
    eCons.foreach(c => setDefinedVar(c))

    val n = new FZSubNeighbourhood(id.getValue,
                                   itVars.foldLeft(Array.empty[Variable])((acc, id) => getVariablesFor(id) ++ acc),
                                   moves,
                                   wVars.foldLeft(Array.empty[Variable])((acc, id) => getVariablesFor(id) ++ acc),
                                   wCons,
                                   eVars,
                                   eCons)
    n
  }

  private def getMove(m: ASTAnnotation): FZMove = {
    m.getId.getValue match {
      case "assign" => FZAssignMove(getVar(m.getArgs.get(0)), getVar(m.getArgs.get(1)))
      case "assign_array" => FZAssignArrayMove(getVariablesFor(m.getArgs.get(0)),
                                             getVar(m.getArgs.get(1)),
                                             getVar(m.getArgs.get(2)))
      case "swap" => FZSwapMove(getVar(m.getArgs.get(0)), getVar(m.getArgs.get(1)))
      case "swap_array" => FZSwapArrayMove(getVariablesFor(m.getArgs.get(0)),
                                         getVar(m.getArgs.get(1)),
                                         getVariablesFor(m.getArgs.get(2)),
                                         getVar(m.getArgs.get(3)))
      case _ => throw new NoSuchMoveException(m.getId.getValue, "Intermediate Representation")
    }
  }

  private def getVarsAndConstraintsInFunction(call: ASTAnnotation): (Array[Variable], List[Constraint]) = {
    val funBody = funcDict(call.getId.getValue).getBody.getBody
    val (constraints, variables) = funBody.asScala.partition((p: ASTNode) => p.isInstanceOf[ASTConstraint])

    val vars = variables.toList.asInstanceOf[List[ASTDecl]].map((d: ASTDecl) => addDecl(d))
    val cons = constraints.toList.asInstanceOf[List[ASTConstraint]].map(
      (c: ASTConstraint) => constructConstraint(c.getId.getValue, c.getArgs.asScala.toList, getAnnotations(c.getAnns.asScala.toList)))
    (vars.toArray.foldLeft(Array.empty[Variable])((acc, id) => getVariablesFor(id) ++ acc), cons)
  }

  def constructConstraint(cstr: String, varList: List[ASTLit], ann: List[Annotation]): Constraint = {
    if (cstr.endsWith("_reif")) {
      reif(makeConstraint(cstr.substring(0, cstr.length - 5), varList.dropRight(1), ann), getBoolVar(varList.last))
    } else {
      makeConstraint(cstr, varList, ann)
    }
  }


  private def makeConstraint(c: String, args: List[ASTLit], ann: List[Annotation]): Constraint = {
    dictCons.getOrElse(c, (varList: List[ASTLit], ann: List[Annotation]) =>
      throw new NoSuchConstraintException(c, "Intermediate Representation")
    )(args, ann)
  }

  private def createVariable(v: ASTDecl): Variable = {
    val newVar = v.getType.asInstanceOf[ASTVarType].getType match {
      case ASTConstants.INT =>
        val dom = v.getType.asInstanceOf[ASTVarType].getDom match {
          case d: ASTRange =>
            getRangeDomain(d)
          case d: ASTSet =>
            getSetDomain(d)
          case noDomain => new FzDomainRange(Helper.FznMinInt, Helper.FznMaxInt) //TODO: This is dangerous!
        }
        val tmp = new IntegerVariable(v.getName, dom, getAnnotations(v.getAnns.asScala.toList))
        varDict(v.getName) = tmp;
        tmp

      case ASTConstants.BOOL =>
        val tmp = new BooleanVariable(v.getName, FzDomainRange(0, 1), getAnnotations(v.getAnns.asScala.toList))
        varDict(v.getName) = tmp;
        tmp
    }
    //problem.addVariable(newVar) //Every variable that is created is added to the FZProblem
    newVar
  }

  private def getRange(d: ASTRange): Range = {
    Range(d.getLb.getValue, d.getUb.getValue+1)
  }

  private def getRangeDomain(d: ASTRange): FzDomainRange = {
    FzDomainRange(d.getLb.getValue, d.getUb.getValue)
  }

  private def getSetDomain(d: ASTSet): FzDomainSet = {
    val s = d.getSet.asScala.toSet.map((i: ASTInt) => Int.unbox(i.getValue))
    FzDomainSet(s)
  }

  private def getAnnotations(anns: List[ASTLit]): List[oscar.flatzinc.model.Annotation] = {
    if (anns.isEmpty) {
      return List.empty[Annotation]
    }
    anns.map(transformAnnotation(_).asInstanceOf[Annotation])
  }

  private def transformAnnotation(ann: ASTLit): Any = {
    ann match {
      case id: ASTId => new Annotation(id.getValue)
      case i: ASTInt => i.getValue
      case b: ASTBool => b.getValue
      case s: ASTString => s.getValue
      case r: ASTRange => getRange(r)
      case arr: ASTArray => arr.getElems.asScala.map(transformAnnotation(_)).toList
      case a: ASTAnnotation =>
        val args = a.getArgs.asScala.map(a => transformAnnotation(a))
        new Annotation(a.getId.getValue, args.toList)
    }
  }

  // Strange stuff from old implementation that should probably be moved from here
  // TODO: get rid of all this stuff
  def isIntroducedVar(id: String): Boolean = {
    varDict.contains(id) &&
      varDict(id).anns.exists(_.name == "var_is_introduced");
  }

  def isDefinedVar(id: String): Boolean = {
    varDict(id).anns.exists(_.name == "is_defined_var");
  }

  def isOutputVar(id: String): Boolean = {
    varDict(id).anns.exists(_.name == "output_var")
  }

  def isOutputArray(id: String): Boolean = {
    varDict(id).anns.exists(_.name == "output_array")
  }
}
