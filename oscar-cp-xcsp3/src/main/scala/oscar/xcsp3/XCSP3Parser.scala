package oscar.xcsp3

import java.util

import org.xcsp.common.Condition
import org.xcsp.common.Condition.{ConditionIntvl, ConditionRel, ConditionVal, ConditionVar}
import org.xcsp.common.Types._
import org.xcsp.common.predicates.XNodeParent
import org.xcsp.parser.callbacks.XCallbacks
import org.xcsp.parser.callbacks.XCallbacks.{Implem, XCallbacksParameters}
import org.xcsp.parser.entries.ParsingEntry.{CEntry, OEntry, VEntry}
import org.xcsp.parser.entries.XConstraints.{XBlock, XGroup, XSlide}
import org.xcsp.parser.entries.XVariables.{XArray, XVarInteger}
import oscar.cp._
import oscar.cp.constraints._
import oscar.cp.core.{CPSolver, Constraint}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class XCSP3Parser(filename: String) extends XCallbacksDecomp {

  val impl = new Implem(this)

  impl.currParameters.clear()

  /*
  RECOGNIZE_UNARY_PRIMITIVES,
  RECOGNIZE_BINARY_PRIMITIVES,
  RECOGNIZE_TERNARY_PRIMITIVES,
  RECOGNIZE_LOGIC_CASES,
  RECOGNIZE_EXTREMUM_CASES,
  RECOGNIZE_COUNT_CASES,
  RECOGNIZE_NVALUES_CASES,
  INTENSION_TO_EXTENSION_ARITY_LIMIT,
  INTENSION_TO_EXTENSION_SPACE_LIMIT,
  INTENSION_TO_EXTENSION_PRIORITY;
*/
  impl.currParameters.put(XCallbacksParameters.RECOGNIZE_UNARY_PRIMITIVES, new Object)
  impl.currParameters.put(XCallbacksParameters.RECOGNIZE_BINARY_PRIMITIVES, new Object)
  impl.currParameters.put(XCallbacksParameters.RECOGNIZE_TERNARY_PRIMITIVES, new Object)
  impl.currParameters.put(XCallbacksParameters.RECOGNIZE_NVALUES_CASES, new Object)
  impl.currParameters.put(XCallbacksParameters.CONVERT_INTENSION_TO_EXTENSION_ARITY_LIMIT, 1000: java.lang.Integer) // included
  impl.currParameters.put(XCallbacksParameters.CONVERT_INTENSION_TO_EXTENSION_SPACE_LIMIT, 1000000: java.lang.Integer)
  //impl.currParameters.put(XCallbacksParameters.INTENSION_TO_EXTENSION_PRIORITY, java.lang.Boolean.FALSE)


  override def implem(): XCallbacks.Implem = {
    impl
  }

  implicit val cp = CPSolver()

  val varHashMap = collection.mutable.LinkedHashMap[String, CPIntVar]()
  //automagically maintains the order of insertion
  val cstHashMap = collection.mutable.HashMap[String, Constraint]()


  def toCPIntVar(list: Array[XVarInteger]): Array[CPIntVar] = list.map(i => varHashMap(i.id()))

  loadInstance(filename)

  // For now, structure don't really matters...
  override def beginInstance(`type`: TypeFramework): Unit = {}

  override def endInstance(): Unit = {}

  override def beginVariables(vEntries: util.List[VEntry]): Unit = {}

  override def endVariables(): Unit = {}

  override def beginArray(a: XArray): Unit = {}

  override def endArray(a: XArray): Unit = {}

  override def beginConstraints(cEntries: util.List[CEntry]): Unit = {}

  override def endConstraints(): Unit = {}

  override def beginGroup(g: XGroup): Unit = {}

  override def endGroup(g: XGroup): Unit = {}

  override def beginObjectives(oEntries: util.List[OEntry], `type`: TypeCombination): Unit = {}

  override def endObjectives(): Unit = {}

  override def beginBlock(b: XBlock): Unit = {}

  override def endBlock(b: XBlock): Unit = {}

  override def beginSlide(s: XSlide): Unit = {}

  override def endSlide(s: XSlide): Unit = {}

  // Variables
  override def buildVarInteger(x: XVarInteger, minValue: Int, maxValue: Int): Unit = {
    //println("Adding var "+x.id())
    varHashMap += (x.id() -> CPIntVar(minValue, maxValue, x.id())(cp)) // Some(x.id())
  }

  override def buildVarInteger(x: XVarInteger, values: Array[Int]): Unit = {
    varHashMap += (x.id() -> CPIntVar(values, x.id())(cp))
  }

  // Constraints
  override def buildCtrAllDifferent(id: String, list: Array[XVarInteger]): Unit = {
    //println("Adding AllDifferent "+id)
    val cons = allDifferent(list.map(elem => varHashMap(elem.id())))
    cp.add(cons, Weak)
    cstHashMap += (id -> cons)
  }

  override def buildCtrAllDifferentExcept(id: String, list: Array[XVarInteger], except: Array[Int]): Unit = {
    val cons = new AllDifferentExcept(list.map(elem => varHashMap(elem.id())), except.toSet)
    cp.add(cons)
    cstHashMap += (id -> cons)
  }

  override def buildCtrPrimitive(id: String, xvi: XVarInteger, op: TypeConditionOperatorSet, t: Array[Int]): Unit = {
    val x = varHashMap(xvi.id())
    val set = t.toSet
    op match {
      case TypeConditionOperatorSet.IN => {
        val dom = x.toArray
        for (v <- dom; if !set.contains(v)) {
          cp.remove(x, v)
        }
      }
      case TypeConditionOperatorSet.NOTIN => {
        for (v <- set) {
          cp.remove(x, v)
        }
      }
    }

  }

  override def buildCtrPrimitive(id: String, xvi: XVarInteger, opa: TypeArithmeticOperator, p: Int, op: TypeConditionOperatorRel, yvi: XVarInteger): Unit = {
    val x = varHashMap(xvi.id())
    val y = varHashMap(yvi.id())
    val r: CPIntVar = opa match {
      case TypeArithmeticOperator.ADD => x + p
      case TypeArithmeticOperator.DIST => (x - p).abs
      case TypeArithmeticOperator.DIV => throw new Exception("Division between vars is not implemented")
      case TypeArithmeticOperator.MUL => x * p
      case TypeArithmeticOperator.SUB => x - p
      case TypeArithmeticOperator.MOD => throw new Exception("Modulo between vars is not implemented")
      case TypeArithmeticOperator.POW => throw new Exception("Pow between vars is not implemented")
    }
    val r2: Constraint = op match {
      case TypeConditionOperatorRel.EQ => r === y
      case TypeConditionOperatorRel.GE => r >= y
      case TypeConditionOperatorRel.GT => r > y
      case TypeConditionOperatorRel.LE => r <= y
      case TypeConditionOperatorRel.LT => r < y
      case TypeConditionOperatorRel.NE => r !== y
    }
    cp.add(r2)
    cstHashMap += ((id, r2))

  }

  override def buildCtrPrimitive(id: String, xvi: XVarInteger, opa: TypeArithmeticOperator, yvi: XVarInteger, op: TypeConditionOperatorRel, k: Int): Unit = {
    //println(id)
    val x = varHashMap(xvi.id())
    val y = varHashMap(yvi.id())
    val r: CPIntVar = opa match {
      case TypeArithmeticOperator.ADD => x + y
      case TypeArithmeticOperator.DIST => (x - y).abs
      case TypeArithmeticOperator.DIV => throw new Exception("Division between vars is not implemented")
      case TypeArithmeticOperator.MUL => x * y
      case TypeArithmeticOperator.SUB => x - y
      case TypeArithmeticOperator.MOD => throw new Exception("Modulo between vars is not implemented")
      case TypeArithmeticOperator.POW => throw new Exception("Pow between vars is not implemented")
    }
    val r2: Constraint = op match {
      case TypeConditionOperatorRel.EQ => r === k
      case TypeConditionOperatorRel.GE => r >= k
      case TypeConditionOperatorRel.GT => r > k
      case TypeConditionOperatorRel.LE => r <= k
      case TypeConditionOperatorRel.LT => r < k
      case TypeConditionOperatorRel.NE => r !== k
    }
    cp.add(r2)
    cstHashMap += ((id, r2))
  }

  override def buildCtrPrimitive(id: String, xvi: XVarInteger, op: TypeConditionOperatorRel, k: Int): Unit = {
    val r = varHashMap(xvi.id())
    val r2: Constraint = op match {
      case TypeConditionOperatorRel.EQ => r === k
      case TypeConditionOperatorRel.GE => r >= k
      case TypeConditionOperatorRel.GT => r > k
      case TypeConditionOperatorRel.LE => r <= k
      case TypeConditionOperatorRel.LT => r < k
      case TypeConditionOperatorRel.NE => r !== k
    }
    cp.add(r2)
    cstHashMap += ((id, r2))
  }

  override def buildCtrPrimitive(s: String, xVarInteger: XVarInteger, typeArithmeticOperator: TypeArithmeticOperator, i: Int, typeConditionOperatorRel: TypeConditionOperatorRel, i1: Int): Unit = {
    println("here")
    super.buildCtrPrimitive(s, xVarInteger, typeArithmeticOperator, i, typeConditionOperatorRel, i1)
  }

  override def buildCtrPrimitive(id: String, xvi: XVarInteger, opa: TypeArithmeticOperator, yvi: XVarInteger, op: TypeConditionOperatorRel, zvi: XVarInteger): Unit = {
    val x = varHashMap(xvi.id())
    val y = varHashMap(yvi.id())
    val z = varHashMap(zvi.id())
    val r: CPIntVar = opa match {
      case TypeArithmeticOperator.ADD => x + y
      case TypeArithmeticOperator.DIST => (x - y).abs
      case TypeArithmeticOperator.DIV => throw new Exception("Division between vars is not implemented")
      case TypeArithmeticOperator.MUL => x * y
      case TypeArithmeticOperator.SUB => x - y
      case TypeArithmeticOperator.MOD => throw new Exception("Modulo between vars is not implemented")
      case TypeArithmeticOperator.POW => throw new Exception("Pow between vars is not implemented")
    }
    val r2: Constraint = op match {
      case TypeConditionOperatorRel.EQ => r === z
      case TypeConditionOperatorRel.GE => r >= z
      case TypeConditionOperatorRel.GT => r > z
      case TypeConditionOperatorRel.LE => r <= z
      case TypeConditionOperatorRel.LT => r < z
      case TypeConditionOperatorRel.NE => r !== z
    }
    cp.add(r2)
    cstHashMap += (id -> r2)
  }

  def _buildCrtWithCondition(id: String, expr: CPIntVar, operator: Condition): Unit = {
    val cons: Array[Constraint] = operator match {
      case c: ConditionVal =>
        c.operator match {
          case TypeConditionOperatorRel.EQ => Array(expr === c.k.toInt)
          case TypeConditionOperatorRel.GE => Array(expr >= c.k.toInt)
          case TypeConditionOperatorRel.GT => Array(expr > c.k.toInt)
          case TypeConditionOperatorRel.LE => Array(expr <= c.k.toInt)
          case TypeConditionOperatorRel.LT => Array(expr < c.k.toInt)
          case TypeConditionOperatorRel.NE => Array(expr !== c.k.toInt)
        }
      case c: ConditionVar =>
        c.operator match {
          case TypeConditionOperatorRel.EQ => Array(expr === varHashMap(c.x.id()))
          case TypeConditionOperatorRel.GE => Array(expr >= varHashMap(c.x.id()))
          case TypeConditionOperatorRel.GT => Array(expr > varHashMap(c.x.id()))
          case TypeConditionOperatorRel.LE => Array(expr <= varHashMap(c.x.id()))
          case TypeConditionOperatorRel.LT => Array(expr < varHashMap(c.x.id()))
          case TypeConditionOperatorRel.NE => Array(expr !== varHashMap(c.x.id()))
        }
      case c: ConditionIntvl =>
        c.operator match {
          case TypeConditionOperatorSet.IN => Array(expr <= c.max.toInt, expr >= c.min.toInt)
          case TypeConditionOperatorSet.NOTIN => Array(new Or(Array(expr ?> c.max.toInt, expr ?< c.min.toInt)))
        }
    }
    for (cst <- cons) {
      cp.add(cst)
      //cstHashMap += (id -> cons)
    }
  }

  override def buildCtrSum(id: String, list: Array[XVarInteger], condition: Condition): Unit = {
    _buildCrtWithCondition(id, sum(list.map(i => varHashMap(i.id()))), condition)
  }

  override def buildCtrSum(id: String, list: Array[XVarInteger], coeffs: Array[Int], condition: Condition): Unit = {
    _buildCrtWithCondition(id, weightedSum(coeffs, list.map(i => varHashMap(i.id()))), condition)
  }

  override def buildCtrSum(id: String, list: Array[XVarInteger], coeffs: Array[XVarInteger], condition: Condition): Unit = {
    _buildCrtWithCondition(id, sum(list.zip(coeffs).map(i => varHashMap(i._1.id()) * varHashMap(i._2.id()))), condition)
  }

  override def buildCtrCardinality(id: String, list: Array[XVarInteger], closed: Boolean, values: Array[Int], occurs: Array[Int]): Unit = {
    buildCtrCardinality(id, list, closed, values, occurs, occurs)
  }

  override def buildCtrCardinality(id: String, list: Array[XVarInteger], closed: Boolean, values: Array[Int], occursMin: Array[Int], occursMax: Array[Int]): Unit = {
    val minValue = values.min
    val maxValue = values.max
    val cardMin = Array.fill(maxValue - minValue + 1)(0)
    val cardMax = Array.fill(maxValue - minValue + 1)(0)
    for (i <- 0 until values.size) {
      cardMin(values(i) - minValue) = occursMin(i)
      cardMax(values(i) - minValue) = occursMax(i)
    }
    val cst: Constraint = gcc(toCPIntVar(list), minValue, cardMin, cardMax)
    cp.add(cst)
    cstHashMap += (id -> cst)
  }

  override def buildCtrCardinality(id: String, list: Array[XVarInteger], closed: Boolean, values: Array[Int], occurs: Array[XVarInteger]): Unit = {
    val x = toCPIntVar(list)
    val o = toCPIntVar(occurs)
    cp.add(gcc(x, values.zip(o)))

  }


  override def buildCtrOrdered(id: String, list: Array[XVarInteger], operator: TypeOperatorRel): Unit = {
    val rel: (CPIntVar, CPIntVar) => Constraint = operator match {
      case TypeOperatorRel.GE => (a, b) => a >= b
      case TypeOperatorRel.GT => (a, b) => a > b
      case TypeOperatorRel.LE => (a, b) => a <= b
      case TypeOperatorRel.LT => (a, b) => a < b
    }
    val csts = list.map(i => varHashMap(i.id())).sliding(2).map { x => rel(x(0), x(1)) }

    cp.add(csts.toArray)
  }

  override def buildCtrExtension(id: String, xvi: XVarInteger, values: Array[Int], positive: Boolean, flags: util.Set[TypeFlag]): Unit = {
    val x = varHashMap(xvi.id())
    val set = values.toSet
    if (positive) {
      val dom = x.toArray
      for (v <- dom; if !set.contains(v)) {
        cp.remove(x, v)
      }
    } else {
      for (v <- set) {
        cp.remove(x, v)
      }
    }
  }

  override def buildCtrExtension(id: String, list: Array[XVarInteger], tuples: Array[Array[Int]], positive: Boolean, flags: util.Set[TypeFlag]): Unit = {
    //println(list.map(x => x.id()).mkString(" "))
    val cst: Constraint = if (positive) {
      table(list.map(x => varHashMap(x.id())), tuples)
    }
    else {
      negativeTable(list.map(x => varHashMap(x.id())), tuples)
    }
    cp.add(cst)
    cstHashMap += ((id, cst))
  }

  override def buildCtrInstantiation(id: String, list: Array[XVarInteger], values: Array[Int]): Unit = {
    val csts = list.zip(values).map { case (x, v) => varHashMap(x.id()) === v }
    cp.add(csts)
  }

  // Objectives
  def _getExprForTypeObjective(objtype: TypeObjective, list: Array[XVarInteger]): CPIntVar = {
    objtype match {
      case TypeObjective.MAXIMUM => maximum(list.map(i => varHashMap(i.id())))
      case TypeObjective.MINIMUM => minimum(list.map(i => varHashMap(i.id())))
      case TypeObjective.SUM => sum(list.map(i => varHashMap(i.id())))
      case TypeObjective.EXPRESSION => ???
      case TypeObjective.LEX => ???
      case TypeObjective.NVALUES => ???
      case TypeObjective.PRODUCT => ???
    }
  }

  def _getExprForTypeObjective(objtype: TypeObjective, list: Array[XVarInteger], coefs: Array[Int]): CPIntVar = {
    objtype match {
      case TypeObjective.MAXIMUM => maximum(list.zip(coefs).map(i => varHashMap(i._1.id()) * i._2))
      case TypeObjective.MINIMUM => minimum(list.zip(coefs).map(i => varHashMap(i._1.id()) * i._2))
      case TypeObjective.SUM => weightedSum(coefs, list.map(i => varHashMap(i.id())))
      case TypeObjective.EXPRESSION => ???
      case TypeObjective.LEX => ???
      case TypeObjective.NVALUES => ???
      case TypeObjective.PRODUCT => ???
    }
  }


  override def buildObjToMinimize(id: String, objtype: TypeObjective, list: Array[XVarInteger]): Unit = {
    cp.minimize(_getExprForTypeObjective(objtype, list))
  }

  override def buildObjToMaximize(id: String, objtype: TypeObjective, list: Array[XVarInteger]): Unit = {
    cp.maximize(_getExprForTypeObjective(objtype, list))
  }

  override def buildObjToMinimize(id: String, x: XVarInteger): Unit = {
    cp.minimize(varHashMap(x.id()))
  }

  override def buildObjToMaximize(id: String, x: XVarInteger): Unit = {
    cp.maximize(varHashMap(x.id()))
  }

  override def buildObjToMinimize(id: String, objtype: TypeObjective, list: Array[XVarInteger], coeffs: Array[Int]): Unit = {
    cp.minimize(_getExprForTypeObjective(objtype, list, coeffs))
  }

  override def buildObjToMaximize(id: String, objtype: TypeObjective, list: Array[XVarInteger], coeffs: Array[Int]): Unit = {
    cp.maximize(_getExprForTypeObjective(objtype, list, coeffs))
  }


  override def buildObjToMinimize(id: String, syntaxTreeRoot: XNodeParent[XVarInteger]): Unit = {
    ???
    //modelDeclaration.minimize(_recursiveIntentionBuilder(syntaxTreeRoot).reify())
  }

  override def buildObjToMaximize(id: String, syntaxTreeRoot: XNodeParent[XVarInteger]): Unit = {
    ???
    //modelDeclaration.maximize(_recursiveIntentionBuilder(syntaxTreeRoot).reify())
  }

  override def buildCtrNValuesExcept(id: String, list: Array[XVarInteger], except: Array[Int], condition: Condition): Unit = ???

  override def buildCtrNValues(id: String, list: Array[XVarInteger], condition: Condition): Unit = ???

  def _getConditionVar(condition: Condition): CPIntVar = condition match {
    case c: ConditionVal => CPIntVar(c.k.toInt)
    case c: ConditionVar => varHashMap(c.x.id())
  }

  def _buildCumulativeConditionCst(id: String, starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], condition: ConditionRel) = {
    //Ensure start+durations==end
    for (i <- starts.indices)
      cp.add(starts(i) + durations(i) === ends(i))
    condition.operator match {
      case TypeConditionOperatorRel.EQ =>
        buildCtrCumulative(id, starts, durations, ends, demands, true, _getConditionVar(condition))
        buildCtrCumulative(id, starts, durations, ends, demands, false, _getConditionVar(condition))
      case TypeConditionOperatorRel.GE =>
        buildCtrCumulative(id, starts, durations, ends, demands, false, _getConditionVar(condition))
      case TypeConditionOperatorRel.GT =>
        buildCtrCumulative(id, starts, durations, ends, demands, false, _getConditionVar(condition) + 1)
      case TypeConditionOperatorRel.LE =>
        buildCtrCumulative(id, starts, durations, ends, demands, true, _getConditionVar(condition))
      case TypeConditionOperatorRel.LT =>
        buildCtrCumulative(id, starts, durations, ends, demands, true, _getConditionVar(condition) - 1)
      case TypeConditionOperatorRel.NE =>
        val tVar = CPIntVar(0, demands.map(_.max).sum)
        cp.add(tVar !== _getConditionVar(condition))
        buildCtrCumulative(id, starts, durations, ends, demands, true, tVar)
        buildCtrCumulative(id, starts, durations, ends, demands, false, tVar)
    }
  }

  def buildCtrCumulative(id: String, starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], maxCum: Boolean, limit: CPIntVar): Unit = {
    if (maxCum)
      cp.add(maxCumulativeResource(starts, durations, ends, demands, limit))
    else
      cp.add(minCumulativeResource(starts, durations, ends, demands, limit))
  }

  def buildCtrCumulative(id: String, starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], condition: Condition): Unit = {
    condition match {
      case c: ConditionVal => _buildCumulativeConditionCst(id, starts, durations, ends, demands, c)
      case c: ConditionVar => _buildCumulativeConditionCst(id, starts, durations, ends, demands, c)
      case c: ConditionIntvl => c.operator match {
        case TypeConditionOperatorSet.IN =>
          val tVar = CPIntVar(condition.asInstanceOf[ConditionIntvl].min.toInt, c.max.toInt)
          buildCtrCumulative(id, starts, durations, ends, demands, true, tVar)
          buildCtrCumulative(id, starts, durations, ends, demands, false, tVar)
        case TypeConditionOperatorSet.NOTIN =>
          val tVar = CPIntVar(0, demands.map(_.max).sum)
          cp.add(new Or(Array(tVar ?< condition.asInstanceOf[ConditionIntvl].min.toInt, tVar ?> c.max.toInt)))
          buildCtrCumulative(id, starts, durations, ends, demands, true, tVar)
          buildCtrCumulative(id, starts, durations, ends, demands, false, tVar)
      }
    }
  }

  override def buildCtrCumulative(id: String, origins: Array[XVarInteger], lengths: Array[Int], ends: Array[XVarInteger], heights: Array[Int], condition: Condition): Unit = {
    val mOrigins = origins map (x => varHashMap(x.id()))
    val mLengths = lengths map (x => CPIntVar(x))
    val mEnds = ends map (x => varHashMap(x.id()))
    val mHeights = heights map (x => CPIntVar(x))
    buildCtrCumulative(id, mOrigins, mLengths, mEnds, mHeights, condition)
  }

  override def buildCtrCumulative(id: String, origins: Array[XVarInteger], lengths: Array[Int], ends: Array[XVarInteger], heights: Array[XVarInteger], condition: Condition): Unit = {
    val mOrigins = origins map (x => varHashMap(x.id()))
    val mLengths = lengths map (x => CPIntVar(x))
    val mEnds = ends map (x => varHashMap(x.id()))
    val mHeights = heights map (x => varHashMap(x.id()))
    buildCtrCumulative(id, mOrigins, mLengths, mEnds, mHeights, condition)
  }

  override def buildCtrCumulative(id: String, origins: Array[XVarInteger], lengths: Array[XVarInteger], ends: Array[XVarInteger], heights: Array[Int], condition: Condition): Unit = {
    val mOrigins = origins map (x => varHashMap(x.id()))
    val mLengths = lengths map (x => varHashMap(x.id()))
    val mEnds = ends map (x => varHashMap(x.id()))
    val mHeights = heights map (x => CPIntVar(x))
    buildCtrCumulative(id, mOrigins, mLengths, mEnds, mHeights, condition)
  }

  override def buildCtrCumulative(id: String, origins: Array[XVarInteger], lengths: Array[XVarInteger], ends: Array[XVarInteger], heights: Array[XVarInteger], condition: Condition): Unit = {
    val mOrigins = origins map (x => varHashMap(x.id()))
    val mLengths = lengths map (x => varHashMap(x.id()))
    val mEnds = ends map (x => varHashMap(x.id()))
    val mHeights = heights map (x => varHashMap(x.id()))
    buildCtrCumulative(id, mOrigins, mLengths, mEnds, mHeights, condition)
  }

  override def buildCtrNoOverlap(id: String, origins: Array[XVarInteger], lengths: Array[Int], zeroIgnored: Boolean): Unit = {
    // TODO we ignore the value of zeroIgnored for now
    val starts = origins.map(x => varHashMap(x.id()))
    val durations = lengths.map(CPIntVar(_)(cp))
    val ends = starts.zip(lengths).map { case (s, d) => s + d }
    cp.add(unaryResource(starts, durations, ends))
  }

  override def buildCtrNoOverlap(id: String, origins: Array[XVarInteger], lengths: Array[XVarInteger], zeroIgnored: Boolean): Unit = {
    // TODO we ignore the value of zeroIgnored for now
    val starts = origins.map(x => varHashMap(x.id()))
    val durations = lengths.map(x => varHashMap(x.id()))
    val ends = starts.zip(durations).map { case (s, d) => s + d }
    cp.add(unaryResource(starts, durations, ends))
  }

  override def buildCtrCount(id: String, list: Array[XVarInteger], values: Array[Int], conditionL: Condition): Unit = {
    val setOfVal = values.toSet
    val counterVar: CPIntVar = sum(list.map(x => varHashMap(x.id())).map(_.isIn(setOfVal).asInstanceOf[CPIntVar]))
    conditionL match {
      case condition: ConditionIntvl =>
        val min = condition.asInstanceOf[ConditionIntvl].min.toInt
        val max = condition.asInstanceOf[ConditionIntvl].max.toInt
        condition.operator match {
          case TypeConditionOperatorSet.IN =>
            cp.add(counterVar <= max)
            cp.add(counterVar >= min)
          case TypeConditionOperatorSet.NOTIN =>
            for (v <- min to max) {
              cp.add(counterVar !== v)
            }
        }
      case condition: ConditionVal =>
        val c = condition.asInstanceOf[ConditionVal].k.toInt
        condition.operator match {
          case TypeConditionOperatorRel.LE => cp.add(counterVar <= c)
          case TypeConditionOperatorRel.GE => cp.add(counterVar >= c)
          case TypeConditionOperatorRel.GT => cp.add(counterVar > c)
          case TypeConditionOperatorRel.LT => cp.add(counterVar < c)
          case TypeConditionOperatorRel.EQ => cp.add(counterVar === c)
          case TypeConditionOperatorRel.NE => cp.add(counterVar !== c)
          case _ => throw new RuntimeException("not supported operator")
        }
      case condition: ConditionVar =>
        val c = varHashMap(condition.asInstanceOf[ConditionVar].x.id())
        condition.operator match {
          case TypeConditionOperatorRel.LE => cp.add(counterVar <= c)
          case TypeConditionOperatorRel.GE => cp.add(counterVar >= c)
          case TypeConditionOperatorRel.GT => cp.add(counterVar > c)
          case TypeConditionOperatorRel.LT => cp.add(counterVar < c)
          case TypeConditionOperatorRel.EQ => cp.add(counterVar === c)
          case TypeConditionOperatorRel.NE => cp.add(counterVar !== c)
          case _ => throw new RuntimeException("not supported operator")
        }
    }
  }

  override def buildCtrElement(id: String, list: Array[XVarInteger], startIndex: Int, index: XVarInteger, rank: TypeRank, value: XVarInteger): Unit = {
    if (rank != TypeRank.ANY)
      throw new Exception("Element constraint only supports ANY as position for the index")
    val idx: CPIntVar = varHashMap(index.id()) - startIndex
    val x: Array[CPIntVar] = toCPIntVar(list)
    val z: CPIntVar = varHashMap(value.id())
    cp.add(elementVar(x, idx, z))
  }

  override def buildCtrElement(id: String, list: Array[XVarInteger], startIndex: Int, index: XVarInteger, rank: TypeRank, value: Int): Unit = {
    if (rank != TypeRank.ANY)
      throw new Exception("Element constraint only supports ANY as position for the index")
    val idx: CPIntVar = varHashMap(index.id()) - startIndex
    val x: Array[CPIntVar] = toCPIntVar(list)
    val z: CPIntVar = CPIntVar(value)(cp)
    cp.add(elementVar(x, idx, z))
  }


  override def buildCtrAllEqual(id: String, list: Array[XVarInteger]): Unit = {
    val x = toCPIntVar(list)
    x.sliding(2).foreach { a => cp.add(a(0) === a(1)) }
  }

  override def buildCtrChannel(id: String, list1: Array[XVarInteger], startIndex1: Int, list2: Array[XVarInteger], startIndex2: Int): Unit = {
    cp.add(new Inverse(list1.map(e => varHashMap(e.id())).map(e => if (startIndex1 == 0) e else e - startIndex1),
      list2.map(e => varHashMap(e.id())).map(e => if (startIndex2 == 0) e else e - startIndex2)))
  }

  override def buildCtrChannel(id: String, list: Array[XVarInteger], startIndex: Int): Unit = {
    val corrected = list.map(e => varHashMap(e.id())).map(e => if (startIndex == 0) e else e - startIndex)
    cp.add(new Inverse(corrected, corrected))
  }

  override def buildCtrChannel(id: String, list: Array[XVarInteger], startIndex: Int, pos: XVarInteger): Unit = {
    // In this form, each variable has only domain 0/1. 'pos' should point to the index of the first variable having 1 as value
    val vars = list.map(e => varHashMap(e.id()))
    val posVar = varHashMap(pos.id())

    if(startIndex == 0)
      cp.add(new Channel(vars, posVar))
    else
      cp.add(new Channel(vars, posVar - startIndex))
  }

  override def buildCtrRegular(id: String, list: Array[XVarInteger], transitionsBase: Array[Array[AnyRef]], startStateBase: String, finalStatesBase: Array[String]): Unit = {
    val transitionsString = transitionsBase.map(x => (x(0).asInstanceOf[String], x(1).asInstanceOf[Long].toInt, x(2).asInstanceOf[String]))

    // First step is to convert all String "ids" of state to an equivalent Integer id
    val stringToId = mutable.HashMap[String, Integer]()
    val startState = stringToId.getOrElseUpdate(startStateBase, stringToId.size)
    val finalStates = finalStatesBase.map(x => stringToId.getOrElseUpdate(x, stringToId.size))
    var transitions = transitionsString.map({ case (from, emit, to) => (stringToId.getOrElseUpdate(from, stringToId.size), emit, stringToId.getOrElseUpdate(to, stringToId.size)) })

    // Compute min/max letter to be emitted
    var minLetter = transitions.map { case (from, emit, to) => emit }.min
    var maxLetter = transitions.map { case (from, emit, to) => emit }.max

    // The automaton in OscaR work with letters between 0 and maxLetter. If we have minLetter that is below 0, that might pose a problem
    val variables = if (minLetter < 0) {
      val newVars = list.map(e => varHashMap(e.id()) + minLetter)
      transitions = transitions.map { case (from, emit, to) => (from, emit + minLetter, to) }
      maxLetter -= minLetter
      minLetter = 0
      newVars
    }
    else {
      list.map(e => varHashMap(e.id()))
    }

    // Now let's create the automaton
    val automaton = new Automaton(stringToId.size, maxLetter + 1, startState, finalStates.toSet.asJava)
    transitions.foreach { case (from, emit, to) => automaton.addTransition(from, to, emit) }

    // And finally post the constraint
    val cst = new Regular(variables, automaton)
    cp.add(cst)
    cstHashMap += ((id, cst))
  }


  override def buildCtrLex(id: String, lists: Array[Array[XVarInteger]], operator: TypeOperatorRel): Unit = {
    operator match {
      case TypeOperatorRel.GE => {
        for (i <- 0 until lists.size - 1) {
          cp.add(lexLeq(lists(i + 1).map(x => varHashMap(x.id())), lists(i).map(x => varHashMap(x.id()))))
        }
      }
      case TypeOperatorRel.LE => {
        for (i <- 0 until lists.size - 1) {
          cp.add(lexLeq(lists(i).map(x => varHashMap(x.id())), lists(i + 1).map(x => varHashMap(x.id()))))
        }
      }
      case TypeOperatorRel.LT => {
        for (i <- 0 until lists.size - 1) {
          val a = lists(i).map(x => varHashMap(x.id()))
          a(lists(i).length - 1) += 1
          cp.add(lexLeq(a, lists(i + 1).map(x => varHashMap(x.id()))))
        }
      }
      case TypeOperatorRel.GT => {
        for (i <- 0 until lists.size - 1) {
          val a = lists(i + 1).map(x => varHashMap(x.id()))
          a(lists(i + 1).length - 1) += 1
          cp.add(lexLeq(a, lists(i).map(x => varHashMap(x.id()))))
        }
      }
    }

  }


  override def buildCtrMDD(id: String, list: Array[XVarInteger], transitions: Array[Array[AnyRef]]): Unit = {

    case class Transition(orig: String, value: Int, dest: String) {
      override def toString = s"($orig,$value,$dest)"
    }
    val allTransitions = transitions.map(t => Transition(t(0).toString, t(1).toString.toInt, t(2).toString))

    def outTransitions(nodeId: String): Array[Transition] = allTransitions.filter(t => t.orig.equals(nodeId))
    def inTransitions(nodeId: String): Array[Transition] = allTransitions.filter(t => t.dest.equals(nodeId))

    val nodes: Set[String] = allTransitions.map(_.orig).toSet union allTransitions.map(_.dest).toSet

    val root = nodes.filter(n => inTransitions(n).size == 0).head
    // val sink = nodes.filter(n => outTransitions(n).size == 0).head

    var tuples: Array[Array[Transition]] = outTransitions(root).map(Array(_))

    for (i <- 0 until list.size - 1) {
      tuples =
        for (t: Array[Transition] <- tuples;
             last = t(t.size - 1).dest;
             j <- outTransitions(last)) yield {
          t :+ j
        }
    }
    val tableTuples = for (t <- tuples) yield t.map(_.value)
    buildCtrExtension(id, list, tableTuples, true, new util.HashSet[TypeFlag]())
  }


  override def buildCtrAllDifferentList(id: String, lists: Array[Array[XVarInteger]]): Unit = {
    for (i <- 0 until lists.size; j <- i + 1 until lists.size) {
      val x1 = toCPIntVar(lists(i))
      val x2 = toCPIntVar(lists(j))
      cp.add(or(x1.zip(x2).map { case (a, b) => a ?!== b }))
    }
  }

  override def buildCtrMinimum(id: String, list: Array[XVarInteger], condition: Condition): Unit = {
    _buildCrtWithCondition(id, minimum(toCPIntVar(list)), condition)
  }

  override def buildCtrMaximum(id: String, list: Array[XVarInteger], condition: Condition): Unit =
    _buildCrtWithCondition(id, maximum(list.map(x => varHashMap(x.id()))), condition)

}

object XCSP3Parser {
  def apply(filename: String): XCSP3Parser = new XCSP3Parser(filename)
}

object RunXCSP3 extends App {

  val instance = "data/xcsp3/instancesTest/Blackhole-04-3-00.xml"
  val parser = XCSP3Parser(instance)

  val vars = parser.varHashMap.values.toArray

  println(vars.mkString(","))
  println(vars.filter(_.isBound).mkString(","))

  val valueHeuris: (Int => Int) = learnValueHeuristic(vars,i => vars(i).min)

  parser.cp.search {
    conflictOrderingSearch(vars,i => vars(i).size, valueHeuris)
    //binaryFirstFail(vars)
  }

  var solutions = ArrayBuffer[String]()
  parser.cp.onSolution {
    println("solution")
    val str1 = "<instantiation>\n\t<list>\n\t\t"
    val elems = vars.map(x => x.name).mkString(" ")
    var str2 = "\n\t</list>\n\t<values>\n\t\t"
    val vals = vars.map(x => x.value.toString).mkString(" ")
    val str3 = "\n\t</values>\n</instantiation>"
    solutions += str1 + elems + str2 + vals + str3
  }

  val stat = parser.cp.start(1)
  println(stat)


  println(solutions.mkString("\n\n"))


  def testSolution(instancePath: String, solution: String): Unit = {
    if (!new CheckerLib(instancePath, solution).valid)
      throw new XCSP3ValidationException()
  }

  solutions.foreach(sol => testSolution(instance, sol))
  println("<!-- solutions verified -->")


}
