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

package oscar.modeling.models.cp

import oscar.algo.DisjointSets
import oscar.algo.reversible.ReversibleInt
import oscar.cp
import oscar.cp.constraints.{CPObjective, CPObjectiveUnit, CPObjectiveUnitMaximize, CPObjectiveUnitMinimize}
import oscar.cp.core.CPPropagStrength
import oscar.cp.{CPBoolVarOps, CPIntVarOps}
import oscar.modeling.algebra.bool._
import oscar.modeling.algebra.integer._
import oscar.modeling.constraints._
import oscar.modeling.models._
import oscar.modeling.models.cp.CPModel.{InstantiateAndReuse, InstantiateAndStoreInCache}
import oscar.modeling.vars.cp.{CPBoolVar => ModelCPBoolVar, CPIntVar => ModelCPIntVar}
import oscar.modeling.vars.domainstorage.IntDomainStorage
import oscar.modeling.vars.nostorage.NoFloatDomainStorage
import oscar.modeling.vars.{BoolVar, IntVar}

import scala.collection.{immutable, mutable}

private case class CPCstEq(expr: IntExpression, cst: Int) extends Constraint

object CPModel {
  case class InstantiateAndStoreInCache(expr: IntExpression) extends Constraint
  case class InstantiateAndReuse(reuseFrom: IntExpression, toInstantiate: IntExpression) extends Constraint

  /**
   * Preprocess some things in order to improve performance of the solver
   * Currently preprocessed:
   * - Eq constraints
   * - Tables with some variables appearing more than one time
   *
   * TODO merge IntVars together
   */
  private def preprocessCP(p: UninstantiatedModel): UninstantiatedModel = {
    val (p2, exprCache) = preprocessEq(p)
    preprocessTables(p2, exprCache)
  }

  private def preprocessTables(p: UninstantiatedModel, exprCache: mutable.HashMap[IntExpression, IntExpression]): UninstantiatedModel = {
    // Find all the Table csts
    val tblcsts = p.constraints.filter{case Table(x, y, z) => true; case _ => false}
    if(tblcsts.isEmpty) //nothing to do here
      return p

    val otherConstraints = p.constraints.filter{case Table(x, y, z) => false; case _ => true}

    val newConstraints = tblcsts.map{case tab@Table(vars, tuples, star) => {
      val same = Array.fill(vars.length)(-1)
      val tocheck = Array.fill(vars.length)(-1)
      var nToCheck = 0
      var i = 0
      var j = 1
      while(i != vars.length) {
        j = i+1
        while (j != vars.length) {
          if(exprCache.getOrElse(vars(i), vars(i)) == exprCache.getOrElse(vars(j), vars(j))) {
            same(i) = j
            tocheck(nToCheck) = i
            nToCheck += 1
            j = vars.length
          }
          else
            j += 1
        }
        i += 1
      }

      if(nToCheck != 0) {
        val keep = same.zipWithIndex.filter((x) => x._1 == -1).map(x => x._2)
        val newTuples = tuples.filter(t => {
          var ok = true
          var k = 0
          while (k != nToCheck && ok) {
            val a = t(tocheck(k))
            val b = t(same(tocheck(k)))
            ok &= a == b || (star.isDefined && (a == star.get || b == star.get))
            k += 1
          }
          ok
        }).map(t => {
          keep.map(x => t(x))
        })
        val newVars = keep.map(x => vars(x))
        Table(newVars, newTuples)
      }
      else
        tab
    }}

    UninstantiatedModel(p.declaration, otherConstraints++newConstraints, p.intRepresentatives, p.floatRepresentatives, p.optimisationMethod)
  }


  private def preprocessEq(p: UninstantiatedModel): (UninstantiatedModel, mutable.HashMap[IntExpression, IntExpression]) = {
    var representatives = p.intRepresentatives

    // Find all the Eq
    val eqs = p.constraints.filter{case ExpressionConstraint(eq: Eq) => true; case _ => false}.map{case ExpressionConstraint(eq: Eq) => eq}.toArray
    if(eqs.isEmpty) //nothing to do here
      return (p, mutable.HashMap())

    val otherConstraints = p.constraints.filter{case ExpressionConstraint(eq: Eq) => false; case _ => true}

    // Find, for each expression, in which eq they are. The goal is to merge eq as much as possible
    val exprToSet = mutable.HashMap[IntExpression, mutable.Set[Int]]()
    for((eq, idx) <- eqs.zipWithIndex)
      for(expr <- eq.v)
        exprToSet.getOrElseUpdate(expr, mutable.Set()).add(idx)

    // Merge all the Eq containing same expressions
    val unionFind = new DisjointSets[mutable.Set[IntExpression]](0, eqs.length-1)
    for((expr, set) <- exprToSet) {
      set.sliding(2).map(_.toArray).foreach{
        case Array(a,b) => unionFind.union(a,b)
        case Array(a) => //ignore
      }
    }

    // Merge all the Eq containing "same" IntVars (pointing to the same effective values)
    val intvarsToSet = exprToSet.filter(_._1.isInstanceOf[IntVar]).toArray.asInstanceOf[Array[(IntVar, mutable.Set[Int])]]
    for(i <- 0 until intvarsToSet.size) {
      val repr1 = p.getRepresentative(intvarsToSet(i)._1)
      for(j <- i+1 until intvarsToSet.size) {
        val repr2 = p.getRepresentative(intvarsToSet(j)._1)
        if(repr1 == repr2)
          unionFind.union(intvarsToSet(i)._2.head, intvarsToSet(i)._1.head)
      }
    }

    // Each set in allSets is a new Eq
    val allSets = eqs.indices.map(unionFind.find).toSet
    allSets.foreach(_.data = Some(mutable.Set()))
    for((expr, set) <- exprToSet) {
      set.foreach(unionFind.find(_).data.get.add(expr))
    }

    // Merge IntVars in the same Eq
    for(setObj <- allSets) {
      val set = setObj.data.get
      val intvars = set.filter(_.isInstanceOf[IntVar]).map(_.asInstanceOf[IntVar]).toArray
      val notIntvars = set.filterNot(_.isInstanceOf[IntVar])
      if(intvars.length > 1) {
        // Compute new domain
        val newDomain = immutable.SortedSet[Int]() ++ intvars.map(p.getRepresentative(_).toVector.toSet).reduceLeft((a,b)=> b.intersect(a))
        val newDomainStorage = new IntDomainStorage(newDomain, intvars.head.name)
        representatives = (1 until intvars.length).foldLeft(p.intRepresentatives){case (iR, idx) => iR.union(intvars(0), intvars(idx), newDomainStorage)}
      }
    }

    // Map each expression to a constant representing the equality which it is in
    val exprToEq : Map[IntExpression, Int] = allSets.zipWithIndex.flatMap{case (eq, idx) => eq.data.get.map(_ -> idx)}.toMap

    // Generate instantiation order (constants first, then toposort of the expressions)
    val topoSortOrder = expressionTopoSort(allSets.flatMap(_.data.get))

    // Select the variable to instantiate for each Eq (the first appearing in the toposort)
    // Create InstantiateAndStoreInCache for each of these
    val newEqConstraints = mutable.ArrayBuffer[Constraint]()
    val eqFirstExpr = Array.fill[IntExpression](allSets.size)(null)
    val exprUsed = Array.fill(exprToEq.size)(false)
    for((expr,idx) <- topoSortOrder.zipWithIndex) {
      val eq = exprToEq(expr)
      if(null == eqFirstExpr(eq)) {
        eqFirstExpr(eq) = expr
        exprUsed(idx) = true
        newEqConstraints += InstantiateAndStoreInCache(expr)
      }
    }

    //Create a cache to help further processing
    val equivalentExpr = new mutable.HashMap[IntExpression, IntExpression]()

    // Create InstantiateAndReuse for all the other values (linked to the Eq, in the order of the toposort)
    for((expr,idx) <- topoSortOrder.zipWithIndex; if !exprUsed(idx)) {
      newEqConstraints += InstantiateAndReuse(eqFirstExpr(exprToEq(expr)), expr)
      equivalentExpr.put(expr, eqFirstExpr(exprToEq(expr)))
    }

    val newConstraints: List[Constraint]= newEqConstraints.toList ++ otherConstraints
    (UninstantiatedModel(p.declaration, newConstraints, representatives, p.floatRepresentatives, p.optimisationMethod), equivalentExpr)
  }

  private def expressionTopoSort(expressions: Set[IntExpression]): Array[IntExpression] = {
    /**
      * Fix some problems with on-the-fly-generated subexprs.
      * For example, as Div generates a Minus(y-Modulo(x,y)), we should ensures we checks its dependencies correctly
      */
    def customSubExpr(expr: IntExpression): Iterable[IntExpression] = expr match {
      case Div(x, y) => Array(x - Modulo(x, y))
      case Xor(a,b) => Array(And(Or(a,b), Not(And(a,b))))
      case default => default.subexpressions().asInstanceOf[Seq[IntExpression]]
    }

    // Create the dependency graph
    val links = expressions.map(_ -> mutable.Set[IntExpression]()).toMap

    for(expr <- expressions) {
      def recurFind(subExprs: Iterable[IntExpression]): Unit = {
        for(subExpr <- subExprs) {
          if(expressions.contains(subExpr))
            links(subExpr).add(expr) //subExpr must be instantiated before expr
          else
            recurFind(customSubExpr(subExpr))
        }
      }
      recurFind(customSubExpr(expr))
    }

    // We now have the graph, let's toposort it
    val visited = mutable.HashMap[IntExpression, Boolean]()
    visited ++= expressions.map(_ -> false)

    val topo = mutable.Stack[IntExpression]()

    def DFSVisit(expr: IntExpression): Unit = {
      visited.put(expr, true)
      links(expr).foreach(node => if(!visited(node)) DFSVisit(node))
      topo.push(expr)
    }
    for(expr <- expressions; if !visited(expr)) DFSVisit(expr)

    // Get the effective order.
    topo.toArray.sortBy{ //sortBy is stable
      case _: Constant => 0 //Put constants before everything else, as by def they do not have subexprs
      case _: BoolVar => 1 //Then put BoolVars as they should not have a big domain
      case _: IntVar => 2 //Then IntVar, as they are already instantiated
      case default => 3 //Then evrything that is not one of the above
    }
  }

  def apply(implicit modelDeclaration: ModelDeclaration): CPModel = {
    new CPModel(modelDeclaration.getCurrentModel.asInstanceOf[UninstantiatedModel])
  }
}

/**
  * Model associated with a CP Solver
  * @param p
  */
class CPModel(p: UninstantiatedModel) extends InstantiatedModel(CPModel.preprocessCP(p)) with ForkableInstantiatedModel {
  implicit lazy val cpSolver = new cp.CPSolver()
  override type IntVarImplementation = ModelCPIntVar
  override type FloatVarImplementation = NoFloatDomainStorage

  // TODO for now we only support one objective at a time
  def cpObjective: CPObjectiveUnit = {
    val objs = cpSolver.objective.objs
    if(objs.isEmpty)
      null
    else
      objs.head
  }

  private def getPropaStrength(mC: Constraint): CPPropagStrength = mC match {
    case _: StrongPropagation => oscar.cp.Strong
    case _: MediumPropagation => oscar.cp.Medium
    case _: WeakPropagation => oscar.cp.Weak
    case default => cpSolver.propagStrength
  }

  private def p(mC: Constraint, oC: oscar.cp.core.Constraint): Unit = cpSolver.add(oC, getPropaStrength(mC))
  private def p(mC: Constraint, oC: Array[oscar.cp.core.Constraint]): Unit = cpSolver.add(oC, getPropaStrength(mC))

  protected def postObjective(optimisationMethod: OptimisationMethod) = {
    val obj = optimisationMethod match {
      case m: Minimisation =>
        new CPObjectiveUnitMinimize(postIntExpressionAndGetVar(m.objective))
      case m: Maximisation =>
        new CPObjectiveUnitMaximize(postIntExpressionAndGetVar(m.objective))
      case _ => null
    }

    if(obj != null)
      cpSolver.optimize(new CPObjective(cpSolver, obj))
  }

  def getReversibleInt(init: Int) = new ReversibleInt(cpSolver, init)

  def instantiateIntVar(content: Iterable[Int], name: String): ModelCPIntVar = {
    if(content.min >= 0 && content.max <= 1)
      ModelCPBoolVar(content, name, cpSolver)
    else
      ModelCPIntVar(content, name, cpSolver)
  }

  override def instantiateFloatVar(min: Double, max: Double, name: String): NoFloatDomainStorage = throw new RuntimeException("CP has no support for float variables yet")

  override def post(constraint: Constraint): Unit = {
      constraint match {
        case instantiable: CPInstantiableConstraint => instantiable.cpPost(cpSolver)
        case InstantiateAndStoreInCache(expr) => postIntExpressionAndGetVar(expr)
        case InstantiateAndReuse(reuse, expr) => postIntExpressionWithVar(expr, postIntExpressionAndGetVar(reuse))
        case ExpressionConstraint(expr: BoolExpression) => postBooleanExpression(expr)
        case Among(n, x, s) => p(constraint,cp.modeling.constraint.among(postIntExpressionAndGetVar(n), x.map(postIntExpressionAndGetVar), s))
        case MinCumulativeResource(starts, durations, ends, demands, resources, capacity, id) =>
          val cpStart = starts map postIntExpressionAndGetVar
          val cpDuration = durations map postIntExpressionAndGetVar
          val cpEnds = ends map postIntExpressionAndGetVar
          val cpDemands = demands map postIntExpressionAndGetVar
          val cpResources = resources map postIntExpressionAndGetVar
          val cpCapacity = postIntExpressionAndGetVar(capacity)
          p(constraint,cp.modeling.constraint.minCumulativeResource(cpStart, cpDuration, cpEnds, cpDemands, cpResources, cpCapacity, id))
        case MaxCumulativeResource(starts, durations, ends, demands, resources, capacity, id) =>
          val cpStart = starts map postIntExpressionAndGetVar
          val cpDuration = durations map postIntExpressionAndGetVar
          val cpEnds = ends map postIntExpressionAndGetVar
          val cpDemands = demands map postIntExpressionAndGetVar
          val cpResources = resources map postIntExpressionAndGetVar
          val cpCapacity = postIntExpressionAndGetVar(capacity)
          p(constraint,cp.modeling.constraint.maxCumulativeResource(cpStart, cpDuration, cpEnds, cpDemands, cpResources, cpCapacity, id))
        case AllDifferent(array, exclude) =>
          if(exclude.isEmpty)
            p(constraint,cp.modeling.constraint.allDifferent(array.map(postIntExpressionAndGetVar)))
          else
            p(constraint,new oscar.cp.constraints.AllDifferentExcept(array.map(postIntExpressionAndGetVar), exclude))
        case LexLeq(a, b) => p(constraint,cp.modeling.constraint.lexLeq(a.map(postIntExpressionAndGetVar), b.map(postIntExpressionAndGetVar)))
        case Table(array, values, None) =>
          p(constraint,cp.modeling.constraint.table(array.map(postIntExpressionAndGetVar), values))
        case NegativeTable(array, values, None) => p(constraint,cp.modeling.constraint.negativeTable(array.map(postIntExpressionAndGetVar), values))
        case Table(array, values, Some(starred)) => p(constraint,new oscar.cp.constraints.tables.TableCTStar(array.map(postIntExpressionAndGetVar), values, starred))
        case NegativeTable(array, values, Some(starred)) => p(constraint,new oscar.cp.constraints.tables.TableCTNegStar(array.map(postIntExpressionAndGetVar), values, starred))
        case MinCircuit(succ, distMatrixSucc, cost) => p(constraint,cp.modeling.constraint.minCircuit(succ.map(postIntExpressionAndGetVar), distMatrixSucc, postIntExpressionAndGetVar(cost)))
        case MinCircuitWeak(succ, distMatrixSucc, cost) => p(constraint,cp.modeling.constraint.minCircuit(succ.map(postIntExpressionAndGetVar), distMatrixSucc, postIntExpressionAndGetVar(cost)))
        case GCC(x, minVal, low, up) =>
          p(constraint,new cp.constraints.GCC(x.map(postIntExpressionAndGetVar), minVal, low, up))
        case GCCVar(x, y) =>
          p(constraint,cp.modeling.constraint.gcc(x.map(postIntExpressionAndGetVar), y.map(a => (a._1, postIntExpressionAndGetVar(a._2)))))
        case BinPacking(x, w, l) => p(constraint,new cp.constraints.BinPacking(x.map(postIntExpressionAndGetVar), w, l.map(postIntExpressionAndGetVar)))
        case Circuit(succ, symmetric) => p(constraint,new cp.constraints.Circuit(succ.map(postIntExpressionAndGetVar), symmetric))
        case SubCircuit(succ, offset) => p(constraint,cp.constraints.SubCircuit(succ.map(postIntExpressionAndGetVar), offset))
        case Inverse(a, b) => p(constraint,new cp.constraints.Inverse(a.map(postIntExpressionAndGetVar), b.map(postIntExpressionAndGetVar)))
        case PartialInverse(a, b) => p(constraint,new cp.constraints.PartialInverse(a.map(postIntExpressionAndGetVar), b.map(postIntExpressionAndGetVar)))
        case MinAssignment(xarg, weightsarg, cost) => p(constraint,new cp.constraints.MinAssignment(xarg.map(postIntExpressionAndGetVar), weightsarg, postIntExpressionAndGetVar(cost)))
        case StrongEq(a, b) => p(constraint,postIntExpressionAndGetVar(a) === postIntExpressionAndGetVar(b))
        case Spread(a, s1, s2) => p(constraint,new cp.constraints.Spread(a.toArray.map(postIntExpressionAndGetVar), s1, postIntExpressionAndGetVar(s2), true))
        case UnaryResourceSimple(starts, durations, ends, resources, id) =>
          val cpStarts = starts.map(postIntExpressionAndGetVar)
          val cpDurations = durations.map(postIntExpressionAndGetVar)
          val cpEnds = ends.map(postIntExpressionAndGetVar)
          val cpResources = resources.map(postIntExpressionAndGetVar)
          p(constraint,cp.modeling.constraint.unaryResource(cpStarts, cpDurations, cpEnds, cpResources, id))
        case UnaryResourceTransitionType(starts, durations, ends, types, transitionTimes) =>
          val cpStarts = starts.map(postIntExpressionAndGetVar)
          val cpDurations = durations.map(postIntExpressionAndGetVar)
          val cpEnds = ends.map(postIntExpressionAndGetVar)
          p(constraint,cp.modeling.constraint.unaryResource(cpStarts, cpDurations, cpEnds, types, transitionTimes))
        case UnaryResourceTransition(starts, durations, ends, transitionTimes) =>
          val cpStarts = starts.map(postIntExpressionAndGetVar)
          val cpDurations = durations.map(postIntExpressionAndGetVar)
          val cpEnds = ends.map(postIntExpressionAndGetVar)
          p(constraint,cp.modeling.constraint.unaryResource(cpStarts, cpDurations, cpEnds, transitionTimes))
        case UnaryResourceTransitionFamilies(starts, durations, ends, familyMatrix, families) =>
          val cpStarts = starts.map(postIntExpressionAndGetVar)
          val cpDurations = durations.map(postIntExpressionAndGetVar)
          val cpEnds = ends.map(postIntExpressionAndGetVar)
          p(constraint,cp.modeling.constraint.unaryResource(cpStarts, cpDurations, cpEnds, familyMatrix, families))
        case DiffN(x, dx, y, dy) =>
          p(constraint,cp.modeling.constraint.diffn(x.map(postIntExpressionAndGetVar), dx.map(postIntExpressionAndGetVar), y.map(postIntExpressionAndGetVar), dy.map(postIntExpressionAndGetVar)).toArray)
        case Regular(on, automaton) =>
          p(constraint,cp.modeling.constraint.regular(on.map(postIntExpressionAndGetVar), automaton))
        case NotAllEqual(x) =>
          p(constraint,cp.modeling.constraint.notAllEqual(x.map(postIntExpressionAndGetVar)))
        case Channel(x, pos) =>
          p(constraint,new cp.constraints.Channel(x.map(postIntExpressionAndGetVar), postIntExpressionAndGetVar(pos)))
        case default => throw new Exception("Unknown constraint " + constraint.getClass.toString) //TODO: put a real exception here
      }
  }

  def postEquality(left: IntExpression, right: IntExpression, second: Boolean = false): Unit = (left, right) match {
    //TODO replace partially with preprocessing
    case (Minus(a, b), v: IntExpression) =>
      cpSolver.add(new cp.constraints.BinarySum(postIntExpressionAndGetVar(v),postIntExpressionAndGetVar(b),postIntExpressionAndGetVar(a)))
    case (Sum(Array(a, b)), v: IntExpression) =>
      cpSolver.add(new cp.constraints.BinarySum(postIntExpressionAndGetVar(a),postIntExpressionAndGetVar(b),postIntExpressionAndGetVar(v)))
    case (Prod(Array(a, b)), v: IntExpression) =>
      cpSolver.add(new cp.constraints.MulVar(postIntExpressionAndGetVar(a),postIntExpressionAndGetVar(b),postIntExpressionAndGetVar(v)))
    case _ =>
      if(!second) //retry with the reversed order
        postEquality(right, left, second = true)
      else
        postConstraintForPossibleConstant(left, right,
          (x,y)=>(y === x),
          (x,y)=>(x === y),
          (x,y)=>(x === y)
        )
  }

  def postBooleanExpression(expr: BoolExpression): Unit = {
    expr match {
      case instantiable: CPInstantiableBoolExpression => instantiable.cpPostAsConstraint(cpSolver)
      case And(array) => array.foreach(i => postBooleanExpression(i))
      case Eq(Array(a, b)) => //binary Eq
        postEquality(a, b)
      case Eq(x) => //n-ary Eq
        //TODO lots of ways to improve, must preprocess
        x.sliding(2).map(a => postEquality(a(0), a(1)))
      case Gr(a, b) =>
        cpSolver.add(new cp.constraints.Gr(postIntExpressionAndGetVar(a),postIntExpressionAndGetVar(b)))
      case GrEq(a, b) =>
        cpSolver.add(new cp.constraints.GrEq(postIntExpressionAndGetVar(a),postIntExpressionAndGetVar(b)))
      case Lr(a, b) =>
        cpSolver.add(new cp.constraints.Le(postIntExpressionAndGetVar(a),postIntExpressionAndGetVar(b)))
      case LrEq(a, b) =>
        cpSolver.add(new cp.constraints.LeEq(postIntExpressionAndGetVar(a),postIntExpressionAndGetVar(b)))
      case Or(Array(a,b)) => //binary Or
        cpSolver.add(cp.or(Array(postBoolExpressionAndGetVar(a), postBoolExpressionAndGetVar(b))))
      case Or(a) => //n-ary Or
        cpSolver.add(cp.or(a.map(postBoolExpressionAndGetVar)))
      case Not(a) =>
        cpSolver.add(postBoolExpressionAndGetVar(a).not)
      case NotEq(a, b) =>
        postConstraintForPossibleConstant(a, b,
          (x,y)=>(y !== x),
          (x,y)=>(x !== y),
          (x,y)=>(x !== y)
        )
      case InSet(a, b) =>
        cpSolver.add(new cp.constraints.InSet(postIntExpressionAndGetVar(a), b))
      case Implication(a, b) =>
        val v = cp.CPBoolVar(b = true)
        cpSolver.add(new cp.constraints.Implication(postBoolExpressionAndGetVar(a), postBoolExpressionAndGetVar(b), v))
      case Xor(a, b) => postBooleanExpression(And(Or(a,b), Not(And(a,b))))
      case v: BoolVar =>
        cpSolver.add(getRepresentative(v).asInstanceOf[cp.CPBoolVar])
      case default => throw new Exception("Unknown BoolExpression "+default.getClass.toString) //TODO: put a real exception here
    }
  }

  // Cache that stores equivalent cp.CPIntVar for each IntExpression, in order to avoid duplicates
  protected lazy val expr_cache: mutable.HashMap[IntExpression, cp.CPIntVar] = mutable.HashMap[IntExpression, cp.CPIntVar]()

  def postBoolExpressionAndGetVar(expr: BoolExpression): cp.CPBoolVar = {
    expr_cache.getOrElseUpdate(expr, expr match {
      case instantiable: CPInstantiableBoolExpression => instantiable.cpPostAndGetVar(cpSolver)
      case And(x) => //binary And
        val b = cp.CPBoolVar()
        cpSolver.add(new oscar.cp.constraints.And(x.map(postBoolExpressionAndGetVar), b))
        b
      case Eq(Array(a, b)) => //binary eq reif
        CPIntVarOps(postIntExpressionAndGetVar(a)) ?=== postIntExpressionAndGetVar(b)
      case Eq(x) => //n-ary eq reif
        // TODO Is it better to post n*(n-1)/2 reif constraints or only n-1?
        // map to binary Eq
        postBoolExpressionAndGetVar(And(for(a <- x; b <- x; if a != b) yield Eq(a,b).asInstanceOf[BoolExpression]))
      case Gr(a, b) =>
        CPIntVarOps(postIntExpressionAndGetVar(a)) ?> postIntExpressionAndGetVar(b)
      case GrEq(a, b) =>
        CPIntVarOps(postIntExpressionAndGetVar(a)) ?>= postIntExpressionAndGetVar(b)
      case Lr(a, b) =>
        CPIntVarOps(postIntExpressionAndGetVar(a)) ?< postIntExpressionAndGetVar(b)
      case LrEq(a, b) =>
        CPIntVarOps(postIntExpressionAndGetVar(a)) ?<= postIntExpressionAndGetVar(b)
      case Or(array) => //n-ary Or
        val b = cp.CPBoolVar()
        cpSolver.add(cp.modeling.constraint.or(array.map(postBoolExpressionAndGetVar), b))
        b
      case Not(a) =>
        postBoolExpressionAndGetVar(a).not
      case NotEq(a, b) =>
        CPIntVarOps(postIntExpressionAndGetVar(a)) ?!== postIntExpressionAndGetVar(b)
      case Implication(a, b) =>
        CPBoolVarOps(postBoolExpressionAndGetVar(a)) ==> postBoolExpressionAndGetVar(b)
      case Xor(a, b) => postBoolExpressionAndGetVar(And(Or(a,b), Not(And(a,b))))
      case InSet(a, b) =>
        val c = cp.CPBoolVar()
        cpSolver.add(new cp.constraints.InSetReif(postIntExpressionAndGetVar(a), b, c))
        c
      case v: BoolVar => getRepresentative(v).asInstanceOf[cp.CPBoolVar]
      case default => throw new Exception("Unknown BoolExpression "+default.getClass.toString) //TODO: put a real exception here
    }).asInstanceOf[cp.CPBoolVar]
  }

  def postBoolExpressionWithVar(expr: BoolExpression, result: cp.CPBoolVar): Unit = {
    if(expr_cache.contains(expr)) {
      // This should not happen too often, but is still feasible as we create new expr on the fly
      println("An expression given to postBoolExpressionWithVar has already been instantiated!")
      cpSolver.add(expr_cache(expr) === result)
    }
    expr match {
      case instantiable: CPInstantiableBoolExpression => instantiable.cpPostWithVar(cpSolver, result)
      case And(x) =>
        cpSolver.add(new oscar.cp.constraints.And(x.map(postBoolExpressionAndGetVar), result))
      case Eq(Array(a, b)) => //binary Eq
        cpSolver.add(new cp.constraints.EqReifVar(postIntExpressionAndGetVar(a), postIntExpressionAndGetVar(b), result))
      case Eq(x) => //n-ary Eq
        // TODO Is it better to post n*(n-1)/2 reif constraints or only n-1?
        // map to binary Eq
        postBoolExpressionWithVar(And(for(a <- x; b <- x; if a != b) yield Eq(a,b).asInstanceOf[BoolExpression]), result)
      case Gr(a, b) =>
        cpSolver.add(new cp.constraints.GrEqVarReif(postIntExpressionAndGetVar(a), postIntExpressionAndGetVar(b)+1, result))
      case GrEq(a, b) =>
        cpSolver.add(new cp.constraints.GrEqVarReif(postIntExpressionAndGetVar(a), postIntExpressionAndGetVar(b), result))
      case Lr(a, b) =>
        cpSolver.add(new cp.constraints.GrEqVarReif(postIntExpressionAndGetVar(b), postIntExpressionAndGetVar(a)+1, result))
      case LrEq(a, b) =>
        cpSolver.add(new cp.constraints.GrEqVarReif(postIntExpressionAndGetVar(b), postIntExpressionAndGetVar(a), result))
      case Or(array) =>
        cpSolver.add(new cp.constraints.OrReif2(array.map(postBoolExpressionAndGetVar), result))
      case Not(a) =>
        cpSolver.add(new cp.constraints.Eq(postBoolExpressionAndGetVar(a).not, result))
      case NotEq(a, b) =>
        cpSolver.add(new cp.constraints.DiffReifVar(postIntExpressionAndGetVar(a), postIntExpressionAndGetVar(b), result))
      case Implication(a, b) =>
        cpSolver.add(new cp.constraints.Implication(postBoolExpressionAndGetVar(a), postBoolExpressionAndGetVar(b), result))
      case Xor(a, b) => postBoolExpressionWithVar(And(Or(a,b), Not(And(a,b))), result)
      case InSet(a, b) => cpSolver.add(new cp.constraints.InSetReif(postIntExpressionAndGetVar(a), b, result))
      case v: BoolVar => cpSolver.add(postBoolExpressionAndGetVar(v) === result)
      case default => throw new Exception("Unknown BoolExpression "+default.getClass.toString) //TODO: put a real exception here
    }
    // Must ABSOLUTELY be put AFTER all the calls to postIntExpressionAndGetVar
    expr_cache.put(expr, result)
  }

  def postIntExpressionAndGetVar(expr: IntExpression): cp.CPIntVar = {
    expr match {
      case boolexpr: BoolExpression => postBoolExpressionAndGetVar(boolexpr) //should be done outside expr_cache
                                                                             //as it is updated by postBoolExpressionAndGetVar
      case default => expr_cache.getOrElseUpdate(expr, expr match {
          case instantiable: CPInstantiableIntExpression => instantiable.cpPostAndGetVar(cpSolver)
          case Abs(a) => postIntExpressionAndGetVar(a).abs
          case Constant(a) => cp.CPIntVar(a)
          case Count(x, y) =>
            val v = cp.CPIntVar(0, x.length)
            cpSolver.add(new cp.constraints.Count(v, x.map(postIntExpressionAndGetVar), postIntExpressionAndGetVar(y)))
            v
          case Element(x, y) =>
            val vx: Array[cp.CPIntVar] = x.map(postIntExpressionAndGetVar)
            val vy: cp.CPIntVar = postIntExpressionAndGetVar(y)
            vx(vy)
          case ElementCst(x, y) =>
            val vy: cp.CPIntVar = postIntExpressionAndGetVar(y)
            x(vy)
          case ElementCst2D(x, y, z) =>
            val vy: cp.CPIntVar = postIntExpressionAndGetVar(y)
            val vz: cp.CPIntVar = postIntExpressionAndGetVar(z)
            x(vy)(vz)
          case Max(x) =>
            val vx = x.map(postIntExpressionAndGetVar)
            val m = cp.CPIntVar(vx.map(_.min).max, vx.map(_.max).max)
            cpSolver.add(cp.maximum(vx, m))
            m
          case Min(x) =>
            val vx = x.map(postIntExpressionAndGetVar)
            val m = cp.CPIntVar(vx.map(_.min).min, vx.map(_.max).min)
            cpSolver.add(cp.minimum(vx, m))
            m
          case Minus(x, y) =>
            getCPIntVarForPossibleConstant(x, y,
              (a, b) => -b + a,
              (a, b) => a - b,
              (a, b) => a - b)
          case Modulo(x, y) =>
            val v = cp.CPIntVar(expr.min, expr.max)
            cpSolver.add(new CPIntVarOps(postIntExpressionAndGetVar(x)) % y == v)
            v
          case Prod(Array(x, y)) => //binary prod
            getCPIntVarForPossibleConstant(x, y,
              (a, b) => b * a,
              (a, b) => a * b,
              (a, b) => a * b)
          case Prod(x) => //n-ary prod
            //OscaR only has binary product; transform into a balanced binary tree to minimise number of constraints
            def recurmul(exprs: Array[cp.CPIntVar]): Array[cp.CPIntVar] = {
              if (exprs.length == 1)
                exprs
              else
                recurmul(exprs.grouped(2).map({
                  case Array(a, b) => CPIntVarOps(a) * b
                  case Array(a) => a
                }).toArray)
            }

            recurmul(x.map(postIntExpressionAndGetVar))(0)
          case Sum(Array(x, y)) => //binary sum
            getCPIntVarForPossibleConstant(x, y,
              (a, b) => b + a,
              (a, b) => a + b,
              (a, b) => a + b)
          case Sum(x) => //n-ary sum
            cp.sum(x.map(postIntExpressionAndGetVar))
          case UnaryMinus(a) =>
            -postIntExpressionAndGetVar(a)
          case WeightedSum(x, y) =>
            cp.weightedSum(y, x.map(postIntExpressionAndGetVar))
          case Div(x, y) =>
            val v = cp.CPIntVar(expr.min, expr.max)
            cpSolver.add(new oscar.cp.constraints.MulCte(v, y, postIntExpressionAndGetVar(x - Modulo(x, y))))
            v
          case NValues(x) =>
            val v = cp.CPIntVar(expr.min, expr.max)
            cpSolver.add(new oscar.cp.constraints.AtLeastNValue(x.map(postIntExpressionAndGetVar), v))
            v
          case Exponent(x, y) => throw new Exception() //TODO: real exception
          case v: IntVar =>
            getRepresentative(v).realCPVar
          case default => throw new Exception("Unknown IntExpression "+default.getClass.toString) //TODO: put a real exception here
        }
      )
    }
  }

  def postIntExpressionWithVar(expr: IntExpression, result: cp.CPIntVar): Unit = {
    if(expr_cache.contains(expr)) {
      // This should not happen too often, but is still feasible as we create new expr on the fly
      println("An expression given to postBoolExpressionWithVar has already been instantiated!")
      cpSolver.add(expr_cache(expr) === result)
    }
    expr match {
      case boolexpr: BoolExpression =>
        postBoolExpressionWithVar(boolexpr, result.asInstanceOf[cp.CPBoolVar])
      case instantiable: CPInstantiableIntExpression => instantiable.cpPostWithVar(cpSolver, result)
      case Abs(a) =>
        cpSolver.add(new cp.constraints.Abs(postIntExpressionAndGetVar(expr), result))
      case Constant(a) =>
        cpSolver.add(new cp.constraints.EqCons(result, a))
      case Count(x, y) =>
        cpSolver.add(new cp.constraints.Count(result, x.map(postIntExpressionAndGetVar), postIntExpressionAndGetVar(y)))
      case Element(x, y) =>
        val vx: Array[cp.CPIntVar] = x.map(postIntExpressionAndGetVar)
        val vy: cp.CPIntVar = postIntExpressionAndGetVar(y)
        cpSolver.add(new cp.constraints.ElementVar(vx, vy, result))
      case ElementCst(x, y) =>
        val vy: cp.CPIntVar = postIntExpressionAndGetVar(y)
        cpSolver.add(new cp.constraints.ElementCst(x, vy, result))
      case ElementCst2D(x, y, z) =>
        val vy: cp.CPIntVar = postIntExpressionAndGetVar(y)
        val vz: cp.CPIntVar = postIntExpressionAndGetVar(z)
        cpSolver.add(new cp.constraints.ElementCst2D(x, vy, vz, result))
      case Max(x) =>
        val vx = x.map(postIntExpressionAndGetVar)
        cpSolver.add(cp.maximum(vx, result))
      case Min(x) =>
        val vx = x.map(postIntExpressionAndGetVar)
        cpSolver.add(cp.minimum(vx, result))
      case Minus(x, y) =>
        cpSolver.add(new oscar.cp.constraints.BinarySum(result,postIntExpressionAndGetVar(y),postIntExpressionAndGetVar(x)))
      case Modulo(x, y) =>
        cpSolver.add(new CPIntVarOps(postIntExpressionAndGetVar(x)) % y == result)
      case Prod(Array(x, y)) => //binary prod
        cpSolver.add(new oscar.cp.constraints.MulVar(postIntExpressionAndGetVar(x), postIntExpressionAndGetVar(y), result))
      case Prod(x) => //n-ary prod
        //OscaR only has binary product; transform into a balanced binary tree to minimise number of constraints
        def recurmul(exprs: Array[cp.CPIntVar]): Array[cp.CPIntVar] = {
          if (exprs.length == 2)
            exprs
          else
            recurmul(exprs.grouped(2).map({
              case Array(a, b) => CPIntVarOps(a) * b
              case Array(a) => a
            }).toArray)
        }
        val bprod = recurmul(x.map(postIntExpressionAndGetVar))
        cpSolver.add(new oscar.cp.constraints.MulVar(bprod(0), bprod(1), result))
      case Sum(Array(x, y)) => //binary sum
        cpSolver.add(new oscar.cp.constraints.BinarySum(postIntExpressionAndGetVar(x), postIntExpressionAndGetVar(y), result))
      case Sum(x) => //n-ary sum
        cpSolver.add(cp.modeling.constraint.sum(x.map(postIntExpressionAndGetVar), result))
      case UnaryMinus(a) =>
        cpSolver.add(result === -postIntExpressionAndGetVar(a))
      case WeightedSum(x, y) =>
        cpSolver.add(cp.modeling.constraint.weightedSum(y, x.map(postIntExpressionAndGetVar), result))
      case Div(x, y) =>
        cpSolver.add(new oscar.cp.constraints.MulCte(result, y, postIntExpressionAndGetVar(x - Modulo(x, y))))
      case NValues(x) =>
        cpSolver.add(new oscar.cp.constraints.AtLeastNValue(x.map(postIntExpressionAndGetVar), result))
      case Exponent(x, y) => throw new Exception() //TODO: real exception
      case v: IntVar =>
        cpSolver.add(postIntExpressionAndGetVar(v) === result)
      case default => throw new Exception("Unknown IntExpression "+default.getClass.toString) //TODO: put a real exception here
    }
    // Must ABSOLUTELY be put AFTER all the calls to postIntExpressionAndGetVar
    expr_cache.put(expr, result)
  }

  /**
    * Post the right constraint depending on the type of a and b.
    *
    * @param a
    * @param b
    * @param leftCst this function will be called if a is constant
    * @param rightCst this function will be called if b is constant
    * @param allVar this function will be called if a and b are not constant
    */
  def postConstraintForPossibleConstant(a: IntExpression, b: IntExpression,
                                        leftCst: (Int, cp.CPIntVar) => cp.Constraint,
                                        rightCst: (cp.CPIntVar, Int) => cp.Constraint,
                                        allVar: (cp.CPIntVar, cp.CPIntVar) => cp.Constraint): Unit = {
    (a,b) match {
      case (Constant(value), variable:IntExpression) => cpSolver.add(leftCst(value, postIntExpressionAndGetVar(variable)))
      case (variable: IntExpression, Constant(value)) => cpSolver.add(rightCst(postIntExpressionAndGetVar(variable), value))
      case (v1: IntExpression, v2: IntExpression) => cpSolver.add(allVar(postIntExpressionAndGetVar(v1), postIntExpressionAndGetVar(v2)))
    }
  }

  /**
    * Post the right constraint depending on the type of a and b.
    *
    * @param a
    * @param b
    * @param leftCst this function will be called if a is constant
    * @param rightCst this function will be called if b is constant
    * @param allVar this function will be called if a and b are not constant
    */
  def getCPIntVarForPossibleConstant(a: IntExpression, b: IntExpression,
                                     leftCst: (Int, cp.CPIntVar) => cp.CPIntVar,
                                     rightCst: (cp.CPIntVar, Int) => cp.CPIntVar,
                                     allVar: (cp.CPIntVar, cp.CPIntVar) => cp.CPIntVar): cp.CPIntVar = {
    (a,b) match {
      case (Constant(value), variable:IntExpression) => leftCst(value, postIntExpressionAndGetVar(variable))
      case (variable: IntExpression, Constant(value)) => rightCst(postIntExpressionAndGetVar(variable), value)
      case (v1: IntExpression, v2: IntExpression) => allVar(postIntExpressionAndGetVar(v1), postIntExpressionAndGetVar(v2))
    }
  }

  /**
   * Fork (in the model tree). All the actions made on the model in a fork{} call will be reverted after the call.
   * It is not thread-safe.
   */
  override def fork[T](func: => T): T = {
    cpSolver.pushState()
    val ret = func
    cpSolver.pop()
    ret
  }
}
