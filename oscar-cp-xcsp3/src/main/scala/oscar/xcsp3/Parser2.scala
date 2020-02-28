package oscar.xcsp3

import java.io.File
import java.util

import org.xcsp.common.{Condition, IVar}
import org.xcsp.common.Condition.{ConditionIntvl, ConditionRel, ConditionVal, ConditionVar}
import org.xcsp.common.Types._
import org.xcsp.common.predicates.{XNode, XNodeLeaf, XNodeParent}
import org.xcsp.parser.callbacks.XCallbacks
import org.xcsp.parser.callbacks.XCallbacks.{Implem, XCallbacksParameters}
import org.xcsp.parser.entries.XVariables.XVarInteger
import oscar.cp.constraints.Automaton
import oscar.modeling.algebra.bool._
import oscar.modeling.algebra.integer._
import oscar.modeling.constraints._
import oscar.modeling.misc.SearchStatistics
import oscar.modeling.models.{ModelDeclaration, NoSolException}
import oscar.modeling.solvers.cp.decompositions.CartProdRefinement
import oscar.modeling.solvers.cp.{Branchings, CPApp, CPAppConfig}
import oscar.modeling.vars.IntVar
import oscar.modeling.algebra.integer.IntExpression._

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.language.reflectiveCalls

/**
  * An XCSP3 parser that converts an XCSP3 instance to an OscaR-Modeling model
  * @param modelDeclaration OscaR-Modeling base model declaration
  * @param filename path to the instance
  */
private class XCSP3Parser2(modelDeclaration: ModelDeclaration, filename: String) extends XCallbacksDecomp {
  implicit val implModelDeclaration = modelDeclaration
  val varHashMap = collection.mutable.LinkedHashMap[String, IntExpression]() //automagically maintains the order of insertion

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
  impl.currParameters.put(XCallbacksParameters.RECOGNIZE_BINARY_PRIMITIVES,  new Object)
  impl.currParameters.put(XCallbacksParameters.RECOGNIZE_TERNARY_PRIMITIVES,  new Object)
  impl.currParameters.put(XCallbacksParameters.RECOGNIZE_NVALUES_CASES,  new Object)
  impl.currParameters.put(XCallbacksParameters.CONVERT_INTENSION_TO_EXTENSION_ARITY_LIMIT, 1000:java.lang.Integer) // included
  impl.currParameters.put(XCallbacksParameters.CONVERT_INTENSION_TO_EXTENSION_SPACE_LIMIT, 1000000:java.lang.Integer)
  //impl.currParameters.put(XCallbacksParameters.INTENSION_TO_EXTENSION_PRIORITY, java.lang.Boolean.FALSE)


  loadInstance(filename)

  override def implem(): XCallbacks.Implem = {
    impl
  }

  // Variables
  override def buildVarInteger(x: XVarInteger, minValue: Int, maxValue: Int): Unit = {
    //println("Adding var "+x.id)
    varHashMap += x.id() -> IntVar(minValue, maxValue, x.id())
  }

  override def buildVarInteger(x: XVarInteger, values: Array[Int]): Unit = {
    varHashMap += x.id() -> IntVar(values.toSet, x.id())
  }

  // Constraints
  override def buildCtrAllDifferent(id: String, list: Array[XVarInteger]): Unit = {
    val cst = AllDifferent(list.map(elem => varHashMap(elem.id())))
    modelDeclaration.add(cst)
  }

  override def buildCtrAllDifferentExcept(id: String, list: Array[XVarInteger], except: Array[Int]): Unit = {
    val cst = AllDifferent(list.map(elem => varHashMap(elem.id())), except.toSet)
    modelDeclaration.add(cst)
  }

  override def buildCtrPrimitive(id: String, xvi: XVarInteger, op: TypeConditionOperatorSet, t: Array[Int]): Unit = {
    val x = varHashMap(xvi.id())
    // todo: ask Guillaume if it is the best way
    val set = t.toSet
    op match {
      case TypeConditionOperatorSet.IN => {
        val dom = x.values().toSet
        for (v <- dom; if !set.contains(v)) {
          modelDeclaration.add((x !== v).toConstraint)
        }
      }
      case TypeConditionOperatorSet.NOTIN => {
        for (v <- set) {
          modelDeclaration.add((x !== v).toConstraint)
        }
      }
    }

  }

  override def buildCtrPrimitive(id: String, xvi: XVarInteger, opa: TypeArithmeticOperator, p: Int, op: TypeConditionOperatorRel, yvi: XVarInteger): Unit = {
    val x = varHashMap(xvi.id())
    val y = varHashMap(yvi.id())
    val r: IntExpression = opa match {
      case TypeArithmeticOperator.ADD => x + p
      case TypeArithmeticOperator.DIST => Dist(x,p)
      case TypeArithmeticOperator.DIV => x / p
      case TypeArithmeticOperator.MUL => x * p
      case TypeArithmeticOperator.SUB => x - p
      case TypeArithmeticOperator.MOD => throw new Exception("Modulo between vars is not implemented")
      case TypeArithmeticOperator.POW => throw new Exception("Pow between vars is not implemented")
    }
    val r2 = (op match {
      case TypeConditionOperatorRel.EQ => {
        r === y
      }
      case TypeConditionOperatorRel.GE => r >= y
      case TypeConditionOperatorRel.GT => r > y
      case TypeConditionOperatorRel.LE => r <= y
      case TypeConditionOperatorRel.LT => r < y
      case TypeConditionOperatorRel.NE => r !== y
    }).toConstraint
    modelDeclaration.add(r2)
  }


  override def buildCtrPrimitive(id: String, xvi: XVarInteger, opa: TypeArithmeticOperator, yvi: XVarInteger, op: TypeConditionOperatorRel, k: Int): Unit = {
    //println(id)
    val x = varHashMap(xvi.id())
    val y = varHashMap(yvi.id())
    val r: IntExpression = opa match {
      case TypeArithmeticOperator.ADD => x + y
      case TypeArithmeticOperator.DIST => Dist(x, y)
      case TypeArithmeticOperator.DIV => x / y.evaluate()
      case TypeArithmeticOperator.MUL => x * y
      case TypeArithmeticOperator.SUB => x - y
      case TypeArithmeticOperator.MOD => throw new Exception("Modulo between vars is not implemented")
      case TypeArithmeticOperator.POW => throw new Exception("Pow between vars is not implemented")
    }
    val r2 = (op match {
      case TypeConditionOperatorRel.EQ => r === k
      case TypeConditionOperatorRel.GE => r >= k
      case TypeConditionOperatorRel.GT => r > k
      case TypeConditionOperatorRel.LE => r <= k
      case TypeConditionOperatorRel.LT => r < k
      case TypeConditionOperatorRel.NE => r !== k
    }).toConstraint
    modelDeclaration.add(r2)
  }

  override def buildCtrPrimitive(id: String, xvi: XVarInteger, op: TypeConditionOperatorRel, k: Int): Unit = {
    val r = varHashMap(xvi.id())
    val r2 = (op match {
      case TypeConditionOperatorRel.EQ => r === k
      case TypeConditionOperatorRel.GE => r >= k
      case TypeConditionOperatorRel.GT => r > k
      case TypeConditionOperatorRel.LE => r <= k
      case TypeConditionOperatorRel.LT => r < k
      case TypeConditionOperatorRel.NE => r !== k
    }).toConstraint
    modelDeclaration.add(r2)
  }

  override def buildCtrPrimitive(id: String, xvi: XVarInteger, opa: TypeArithmeticOperator, yvi: XVarInteger, op: TypeConditionOperatorRel, zvi: XVarInteger): Unit = {
    val x = varHashMap(xvi.id())
    val y = varHashMap(yvi.id())
    val z = varHashMap(zvi.id())
    val r: IntExpression = opa match {
      case TypeArithmeticOperator.ADD => x + y
      case TypeArithmeticOperator.DIST => Dist(x, y)
      case TypeArithmeticOperator.DIV => x / y.evaluate()
      case TypeArithmeticOperator.MUL => x * y
      case TypeArithmeticOperator.SUB => x - y
      case TypeArithmeticOperator.MOD => throw new Exception("Modulo between vars is not implemented")
      case TypeArithmeticOperator.POW => throw new Exception("Pow between vars is not implemented")
    }
    val r2 = (op match {
      case TypeConditionOperatorRel.EQ => r === z
      case TypeConditionOperatorRel.GE => r >= z
      case TypeConditionOperatorRel.GT => r > z
      case TypeConditionOperatorRel.LE => r <= z
      case TypeConditionOperatorRel.LT => r < z
      case TypeConditionOperatorRel.NE => r !== z
    }).toConstraint
    modelDeclaration.add(r2)
  }

  override def buildCtrPrimitive(id: String, xvi: XVarInteger, opa: TypeArithmeticOperator, y: Int, op: TypeConditionOperatorRel, z: Int): Unit = {
    val x = varHashMap(xvi.id())
    val r: IntExpression = opa match {
      case TypeArithmeticOperator.ADD => x + y
      case TypeArithmeticOperator.DIST => Dist(x, y)
      case TypeArithmeticOperator.DIV => x / y.evaluate()
      case TypeArithmeticOperator.MUL => x * y
      case TypeArithmeticOperator.SUB => x - y
      case TypeArithmeticOperator.MOD => throw new Exception("Modulo between vars is not implemented")
      case TypeArithmeticOperator.POW => throw new Exception("Pow between vars is not implemented")
    }
    val r2 = (op match {
      case TypeConditionOperatorRel.EQ => r === z
      case TypeConditionOperatorRel.GE => r >= z
      case TypeConditionOperatorRel.GT => r > z
      case TypeConditionOperatorRel.LE => r <= z
      case TypeConditionOperatorRel.LT => r < z
      case TypeConditionOperatorRel.NE => r !== z
    }).toConstraint
    modelDeclaration.add(r2)
  }

  def _buildCrtWithCondition(id: String, expr: IntExpression, operator: Condition): Unit = {
    val expr2: Array[BoolExpression] = operator match {
      case c: ConditionVal =>
        val cst = c.k.toInt
        c.operator match {
          case TypeConditionOperatorRel.EQ => Array(expr === cst)
          case TypeConditionOperatorRel.GE => Array(expr >= cst)
          case TypeConditionOperatorRel.GT => Array(expr > cst)
          case TypeConditionOperatorRel.LE => Array(expr <= cst)
          case TypeConditionOperatorRel.LT => Array(expr < cst)
          case TypeConditionOperatorRel.NE => Array(expr !== cst)
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
          case TypeConditionOperatorSet.NOTIN => Array(Or(Array(expr > c.max.toInt, expr < c.min.toInt)))
        }
    }
    expr2.map(_.toConstraint).foreach(modelDeclaration.add)
  }

  override def buildCtrSum(id: String, list: Array[XVarInteger], condition: Condition): Unit = {
    _buildCrtWithCondition(id, Sum(list.map(i => varHashMap(i.id()))), condition)
  }

  override def buildCtrSum(id: String, list: Array[XVarInteger], coeffs: Array[Int], condition: Condition): Unit = {
    _buildCrtWithCondition(id, WeightedSum(list.map(i => varHashMap(i.id())), coeffs), condition)
  }

  override def buildCtrSum(id: String, list: Array[XVarInteger], coeffs: Array[XVarInteger], condition: Condition): Unit = {
    _buildCrtWithCondition(id, Sum(list.zip(coeffs).map(i => varHashMap(i._1.id())*varHashMap(i._2.id()))), condition)
  }

  protected lazy val expr_cache: mutable.HashMap[String, IntExpression] = mutable.HashMap()

  def _recursiveIntentionBuilder[V <: IVar](node: XNode[V]): IntExpression = {
    expr_cache.getOrElseUpdate(node.toString, {
      node match {
        case x: XNodeLeaf[V] => _recursiveIntentionBuilderLeafNode(x)
        case x: XNodeParent[V] => _recursiveIntentionBuilderParentNode(x)
      }
    })
  }

  def _recursiveIntentionBuilderLeafNode[V <: IVar](node: XNodeLeaf[V]): IntExpression = {
    node.getType match {
      case TypeExpr.VAR => varHashMap(node.value.toString)
      case TypeExpr.LONG => node.value.asInstanceOf[Long].toInt
      case TypeExpr.RATIONAL => ???
      case TypeExpr.SYMBOL => ???
      case TypeExpr.DECIMAL => ???
      case _ => ???
    }
  }

  def _recursiveIntentionBuilderParentNode[V <: IVar](tree: XNodeParent[V]): IntExpression = {
    tree.getType match {
      case TypeExpr.IN =>
        assert(tree.sons(1).getType == TypeExpr.SET)
        try{
          val set = tree.sons(1).asInstanceOf[XNodeParent[V]].sons.map(i => i.asInstanceOf[XNodeLeaf[V]].value.asInstanceOf[Long].toInt)
          InSet(_recursiveIntentionBuilder(tree.sons(0)), set.toSet)
        }
        catch {
          case a: ClassCastException =>
            //Cannot cast to XNodeLeaf => not only integers
            val main = _recursiveIntentionBuilder(tree.sons(0))
            Or(tree.sons(1).asInstanceOf[XNodeParent[V]].sons.map(x => _recursiveIntentionBuilder(x) === main))
        }
      case TypeExpr.OR => Or(tree.sons.map(_recursiveIntentionBuilder(_).asInstanceOf[BoolExpression]))
      case TypeExpr.AND => And(tree.sons.map(_recursiveIntentionBuilder(_).asInstanceOf[BoolExpression]))
      case TypeExpr.EQ => Eq(tree.sons.map(_recursiveIntentionBuilder))
      case TypeExpr.LT => _recursiveIntentionBuilder(tree.sons(0)) < _recursiveIntentionBuilder(tree.sons(1))
      case TypeExpr.LE => _recursiveIntentionBuilder(tree.sons(0)) <= _recursiveIntentionBuilder(tree.sons(1))
      case TypeExpr.GE => _recursiveIntentionBuilder(tree.sons(0)) >= _recursiveIntentionBuilder(tree.sons(1))
      case TypeExpr.GT => _recursiveIntentionBuilder(tree.sons(0)) > _recursiveIntentionBuilder(tree.sons(1))
      case TypeExpr.NE => _recursiveIntentionBuilder(tree.sons(0)) !== _recursiveIntentionBuilder(tree.sons(1))
      case TypeExpr.ADD => Sum(tree.sons.map(_recursiveIntentionBuilder))
      case TypeExpr.SUB => _recursiveIntentionBuilder(tree.sons(0)) - _recursiveIntentionBuilder(tree.sons(1))
      case TypeExpr.DIV =>
        assert(tree.sons(1).getType == TypeExpr.LONG)
        _recursiveIntentionBuilder(tree.sons(0)) / tree.sons(1).asInstanceOf[XNodeLeaf[V]].value.asInstanceOf[Long].toInt
      case TypeExpr.ABS => Abs(_recursiveIntentionBuilder(tree.sons(0)))
      case TypeExpr.NEG => -_recursiveIntentionBuilder(tree.sons(0))
      case TypeExpr.MIN => Min(tree.sons.map(_recursiveIntentionBuilder))
      case TypeExpr.MAX => Max(tree.sons.map(_recursiveIntentionBuilder))
      case TypeExpr.DIST => Dist(_recursiveIntentionBuilder(tree.sons(0)), _recursiveIntentionBuilder(tree.sons(1)))
      case TypeExpr.NOT => !_recursiveIntentionBuilder(tree.sons(0))
      case TypeExpr.MOD =>
        assert(tree.sons(1).getType == TypeExpr.LONG)
        _recursiveIntentionBuilder(tree.sons(0)) % tree.sons(1).asInstanceOf[XNodeLeaf[V]].value.asInstanceOf[Long].toInt
      case TypeExpr.MUL =>
        Prod(tree.sons.map(_recursiveIntentionBuilder))
      case TypeExpr.IFF =>
        val csts = tree.sons.map(_recursiveIntentionBuilder).foldLeft((List[BoolExpression](), null: IntExpression))((cur, next) => {
          if(cur._2 == null) (List[BoolExpression](), next)
          else ((cur._2 === next) :: cur._1, next)
        })._1
        if(csts.length == 1)
          csts.last
        else
          And(csts.toArray) //TODO improve this, need for an n-ary eq
      case TypeExpr.IMP =>
        _recursiveIntentionBuilder(tree.sons(0)).asInstanceOf[BoolExpression] ==> _recursiveIntentionBuilder(tree.sons(1)).asInstanceOf[BoolExpression]
      case TypeExpr.XOR =>
        _recursiveIntentionBuilder(tree.sons(0)).asInstanceOf[BoolExpression] ^ _recursiveIntentionBuilder(tree.sons(1)).asInstanceOf[BoolExpression]
      case TypeExpr.SQR => ??? //1
      case TypeExpr.POW => ??? //2
      case TypeExpr.SET => ??? //0, Integer.MAX_VALUE
      case TypeExpr.IF => ??? //3
      case TypeExpr.CARD => ??? //1
      case TypeExpr.UNION => ??? //2, Integer.MAX_VALUE
      case TypeExpr.INTER => ??? //2, Integer.MAX_VALUE
      case TypeExpr.DIFF => ??? //2
      case TypeExpr.SDIFF => ??? //2, Integer.MAX_VALUE
      case TypeExpr.HULL => ??? //1
      case TypeExpr.DJOINT => ??? //2
      case TypeExpr.SUBSET => ??? //2
      case TypeExpr.SUBSEQ => ??? //2
      case TypeExpr.SUPSET => ??? //2
      case TypeExpr.SUPSEQ => ??? //2
      case TypeExpr.CONVEX => ??? //1
      case TypeExpr.FDIV => ??? //2
      case TypeExpr.FMOD => ??? //2
      case TypeExpr.SQRT => ??? //1
      case TypeExpr.NROOT => ??? //2
      case TypeExpr.EXP => ??? //1
      case TypeExpr.LN => ??? //1
      case TypeExpr.LOG => ??? //2
      case TypeExpr.SIN => ??? //1
      case TypeExpr.COS => ??? //1
      case TypeExpr.TAN => ??? //1
      case TypeExpr.ASIN => ??? //1
      case TypeExpr.ACOS => ??? //1
      case TypeExpr.ATAN => ??? //1
      case TypeExpr.SINH => ??? //1
      case TypeExpr.COSH => ??? //1
      case TypeExpr.TANH => ??? //1
      case TypeExpr.NOTIN => ??? //TODO
      case TypeExpr.PAR => ???
      case TypeExpr.VAR => throw new Exception("Variable in non-leaf node")
      case TypeExpr.LONG => throw new Exception("Variable in non-leaf node")
      case TypeExpr.RATIONAL => throw new Exception("Variable in non-leaf node")
      case TypeExpr.SYMBOL => throw new Exception("Variable in non-leaf node")
      case TypeExpr.DECIMAL => throw new Exception("Variable in non-leaf node")
    }
  }

  override def buildCtrIntension(id: String, scope: Array[XVarInteger], syntaxTreeRoot: XNodeParent[XVarInteger]): Unit = {
    val cst = _recursiveIntentionBuilder(syntaxTreeRoot).asInstanceOf[BoolExpression].toConstraint
    modelDeclaration.add(cst)
  }

  override def buildCtrCardinality(id: String, list: Array[XVarInteger], closed: Boolean, values: Array[Int], occurs: Array[Int]): Unit = {
    val cst = GCC(list.map(i => varHashMap(i.id())), values, occurs, occurs)
    modelDeclaration.add(cst)
  }

  override def buildCtrCardinality(id: String, list: Array[XVarInteger], closed: Boolean, values: Array[Int], occursMin: Array[Int], occursMax: Array[Int]): Unit = {
    val cst = GCC(list.map(i => varHashMap(i.id())), values, occursMin, occursMax)
    modelDeclaration.add(cst)
  }

  override def buildCtrCardinality(id: String, list: Array[XVarInteger], closed: Boolean, values: Array[Int], occurs: Array[XVarInteger]): Unit = {
    val cst = GCC(list.map(i => varHashMap(i.id())), values, occurs.map(i => varHashMap(i.id())))
    modelDeclaration.add(cst)
  }



  override def buildCtrOrdered(id: String, list: Array[XVarInteger], operator: TypeOperatorRel): Unit = {
    val rel: (IntExpression, IntExpression) => Constraint = operator match {
      case TypeOperatorRel.GE => (a, b) => a >= b
      case TypeOperatorRel.GT => (a, b) => a > b
      case TypeOperatorRel.LE => (a, b) => a <= b
      case TypeOperatorRel.LT => (a, b) => a < b
    }
    list.map(i => varHashMap(i.id())).sliding(2).foreach({
      case Array(x, y) => modelDeclaration.post(rel(x, y))
      case _ => // do nothing
    })
  }


  override def buildCtrExtension(id: String, x: XVarInteger, values: Array[Int], positive: Boolean, flags: util.Set[TypeFlag]): Unit = {
    assert(!flags.contains(TypeFlag.STARRED_TUPLES)) // no sense!
    if(positive) {
      //InSet constraint
      modelDeclaration.add(InSet(varHashMap(x.id()), values.toSet))
    }
    else {
      val v = varHashMap(x.id())
      values.foreach(y => modelDeclaration.add(v !== y))
    }
  }

  override def buildCtrExtension(id: String, list: Array[XVarInteger], tuples: Array[Array[Int]], positive: Boolean, flags: util.Set[TypeFlag]): Unit = {
    //println(list.map(x => x.id()).mkString(" "))
    val cst: Constraint = if(positive) {
      Table(list.map(x => varHashMap(x.id())), tuples, if(flags.contains(TypeFlag.STARRED_TUPLES)) Some(Integer.MAX_VALUE-1) else None)
    }
    else {
      NegativeTable(list.map(x => varHashMap(x.id())), tuples, if(flags.contains(TypeFlag.STARRED_TUPLES)) Some(Integer.MAX_VALUE-1) else None)
    }
    modelDeclaration.add(cst)
  }

  override def buildCtrInstantiation(id: String, list: Array[XVarInteger], values: Array[Int]): Unit = {
    val csts = list.zip(values).map(x => (varHashMap(x._1.id()) === x._2).toConstraint)
    csts.foreach(modelDeclaration.add)
  }

  override def buildCtrElement(id: String, list: Array[XVarInteger], startIndex: Int, index: XVarInteger, rank: TypeRank, value: XVarInteger): Unit = {
    if(rank != TypeRank.ANY)
      throw new Exception("Element constraint only supports ANY as position for the index")
    val array = list.map(x => varHashMap(x.id()))
    val indexExpr = if(startIndex == 0) varHashMap(index.id()) else varHashMap(index.id()) - startIndex
    val valueExpr = varHashMap(value.id())
    modelDeclaration.add(array(indexExpr) === valueExpr)
  }

  override def buildCtrElement(id: String, list: Array[XVarInteger], startIndex: Int, index: XVarInteger, rank: TypeRank, value: Int): Unit = {
    if(rank != TypeRank.ANY)
      throw new Exception("Element constraint only supports ANY as position for the index")
    val array = list.map(x => varHashMap(x.id()))
    val indexExpr = if(startIndex == 0) varHashMap(index.id()) else varHashMap(index.id()) - startIndex
    modelDeclaration.add(array(indexExpr) === value)
  }

  override def buildCtrElement(id: String, list: Array[Int], startIndex: Int, index: XVarInteger, rank: TypeRank, value: XVarInteger): Unit = {
    if(rank != TypeRank.ANY)
      throw new Exception("Element constraint only supports ANY as position for the index")
    val indexExpr = if(startIndex == 0) varHashMap(index.id()) else varHashMap(index.id()) - startIndex
    val valueExpr = varHashMap(value.id())
    modelDeclaration.add(list(indexExpr) === valueExpr)
  }

  override def buildCtrAmong(id: String, list: Array[XVarInteger], values: Array[Int], k: Int): Unit = {
    modelDeclaration.add(Among(k, list.map(x => varHashMap(x.id())), values.toSet))
  }

  override def buildCtrAmong(id: String, list: Array[XVarInteger], values: Array[Int], k: XVarInteger): Unit = {
    modelDeclaration.add(Among(varHashMap(k.id()), list.map(x => varHashMap(x.id())), values.toSet))
  }

  override def buildCtrExactly(id: String, list: Array[XVarInteger], value: Int, k: Int): Unit = {
    modelDeclaration.add(Among(k, list.map(x => varHashMap(x.id())), value))
  }

  override def buildCtrExactly(id: String, list: Array[XVarInteger], value: Int, k: XVarInteger): Unit = {
    modelDeclaration.add(Among(varHashMap(k.id()), list.map(x => varHashMap(x.id())), value))
  }

  override def buildCtrAtLeast(id: String, list: Array[XVarInteger], value: Int, k: Int): Unit = {
    modelDeclaration.add(AtLeast(k, list.map(x => varHashMap(x.id())), value))
  }

  override def buildCtrAtMost(id: String, list: Array[XVarInteger], value: Int, k: Int): Unit = {
    modelDeclaration.add(AtMost(k, list.map(x => varHashMap(x.id())), value))
  }

  override def buildCtrMinimum(id: String, list: Array[XVarInteger], condition: Condition): Unit = {
    _buildCrtWithCondition(id, Min(list.map(x => varHashMap(x.id()))), condition)
  }

  override def buildCtrMaximum(id: String, list: Array[XVarInteger], condition: Condition): Unit = {
    _buildCrtWithCondition(id, Max(list.map(x => varHashMap(x.id()))), condition)
  }


  override def buildCtrLex(id: String, lists: Array[Array[XVarInteger]], operator: TypeOperatorRel): Unit = {
    val constraintType: (Array[IntExpression], Array[IntExpression]) => Constraint = operator match {
      case TypeOperatorRel.GE => LexGeq.apply
      case TypeOperatorRel.GT => LexGr.apply
      case TypeOperatorRel.LE => LexLeq.apply
      case TypeOperatorRel.LT => LexLr.apply
    }
    lists.map(tuple => tuple.map(x => varHashMap(x.id()))).sliding(2).foreach(tuples => modelDeclaration.add(constraintType(tuples(0), tuples(1))))
  }

  def _getConditionVar(condition: Condition): IntExpression = condition match {
    case c: ConditionVal => c.k.toInt
    case c: ConditionVar => varHashMap(c.x.id())
  }

  def _buildCumulativeConditionCst(id: String, starts: Array[IntExpression], durations: Array[IntExpression], ends: Array[IntExpression], demands: Array[IntExpression], condition: ConditionRel) = {
    //Ensure start+durations==end
    for(i <- starts.indices)
      modelDeclaration.add(starts(i) + durations(i) === ends(i))
    condition.operator match {
      case TypeConditionOperatorRel.EQ =>
        buildCtrCumulative(id, starts, durations, ends, demands, true, _getConditionVar(condition))
        buildCtrCumulative(id, starts, durations, ends, demands, false, _getConditionVar(condition))
      case TypeConditionOperatorRel.GE =>
        buildCtrCumulative(id, starts, durations, ends, demands, false, _getConditionVar(condition))
      case TypeConditionOperatorRel.GT =>
        buildCtrCumulative(id, starts, durations, ends, demands, false, _getConditionVar(condition)+1)
      case TypeConditionOperatorRel.LE =>
        buildCtrCumulative(id, starts, durations, ends, demands, true, _getConditionVar(condition))
      case TypeConditionOperatorRel.LT =>
        buildCtrCumulative(id, starts, durations, ends, demands, true, _getConditionVar(condition)-1)
      case TypeConditionOperatorRel.NE =>
        val tVar = IntVar(0, demands.map(_.max).sum)
        modelDeclaration.add(tVar !== _getConditionVar(condition))
        buildCtrCumulative(id, starts, durations, ends, demands, true, tVar)
        buildCtrCumulative(id, starts, durations, ends, demands, false, tVar)
    }
  }
  def buildCtrCumulative(id: String, starts: Array[IntExpression], durations: Array[IntExpression], ends: Array[IntExpression], demands: Array[IntExpression], maxCum: Boolean, limit: IntExpression): Unit = {
    if(maxCum)
      modelDeclaration.add(MaxCumulativeResource(starts, durations, ends, demands, limit))
    else
      modelDeclaration.add(MinCumulativeResource(starts, durations, ends, demands, limit))
  }

  def buildCtrCumulative(id: String, starts: Array[IntExpression], durations: Array[IntExpression], ends: Array[IntExpression], demands: Array[IntExpression], condition: Condition): Unit = {
    condition match {
      case c: ConditionVal => _buildCumulativeConditionCst(id, starts, durations, ends, demands, c)
      case c: ConditionVar => _buildCumulativeConditionCst(id, starts, durations, ends, demands, c)
      case c: ConditionIntvl => c.operator match {
        case TypeConditionOperatorSet.IN =>
          val tVar = IntVar(condition.asInstanceOf[ConditionIntvl].min.toInt, c.max.toInt)
          buildCtrCumulative(id, starts, durations, ends, demands, true, tVar)
          buildCtrCumulative(id, starts, durations, ends, demands, false, tVar)
        case TypeConditionOperatorSet.NOTIN =>
          val tVar = IntVar(0, demands.map(_.max).sum)
          modelDeclaration.add(Or(Array(tVar < condition.asInstanceOf[ConditionIntvl].min.toInt, tVar > c.max.toInt)))
          buildCtrCumulative(id, starts, durations, ends, demands, true, tVar)
          buildCtrCumulative(id, starts, durations, ends, demands, false, tVar)
      }
    }
  }

  override def buildCtrCumulative(id: String, origins: Array[XVarInteger], lengths: Array[XVarInteger], ends: Array[XVarInteger], heights: Array[XVarInteger], condition: Condition): Unit = {
    val mOrigins = origins map(x => varHashMap(x.id()))
    val mLengths = lengths map(x => varHashMap(x.id()))
    val mEnds = ends map(x => varHashMap(x.id()))
    val mHeights = heights map(x => varHashMap(x.id()))
    buildCtrCumulative(id, mOrigins, mLengths, mEnds, mHeights, condition)
  }

  override def buildCtrCumulative(id: String, origins: Array[XVarInteger], lengths: Array[Int], ends: Array[XVarInteger], heights: Array[Int], condition: Condition): Unit = {
    val mOrigins = origins map(x => varHashMap(x.id()))
    val mLengths = lengths map(x => IntVar(x))
    val mEnds = ends map(x => varHashMap(x.id()))
    val mHeights = heights map(x => IntVar(x))
    buildCtrCumulative(id, mOrigins, mLengths, mEnds, mHeights, condition)
  }

  override def buildCtrCumulative(id: String, origins: Array[XVarInteger], lengths: Array[Int], ends: Array[XVarInteger], heights: Array[XVarInteger], condition: Condition): Unit = {
    val mOrigins = origins map(x => varHashMap(x.id()))
    val mLengths = lengths map(x => IntVar(x))
    val mEnds = ends map(x => varHashMap(x.id()))
    val mHeights = heights map(x => varHashMap(x.id()))
    buildCtrCumulative(id, mOrigins, mLengths, mEnds, mHeights, condition)
  }

  override def buildCtrCumulative(id: String, origins: Array[XVarInteger], lengths: Array[XVarInteger], ends: Array[XVarInteger], heights: Array[Int], condition: Condition): Unit = {
    val mOrigins = origins map(x => varHashMap(x.id()))
    val mLengths = lengths map(x => varHashMap(x.id()))
    val mEnds = ends map(x => varHashMap(x.id()))
    val mHeights = heights map(x => IntVar(x))
    buildCtrCumulative(id, mOrigins, mLengths, mEnds, mHeights, condition)
  }

  override def buildCtrCircuit(id: String, list: Array[XVarInteger], startIndex: Int): Unit = {
    modelDeclaration.add(SubCircuit(list.map(x => varHashMap(x.id())), startIndex))
  }

  override def buildCtrCircuit(id: String, list: Array[XVarInteger], startIndex: Int, size: Int): Unit = {
    if(size == list.length)
      modelDeclaration.add(Circuit(list.map(x => varHashMap(x.id()))))
    else {
      val vars = list.map(x => varHashMap(x.id()))
      // count non-self-looping variables
      modelDeclaration.add(Sum(vars.zipWithIndex.map({case (v, idx) => v !== (idx+startIndex)})) === size)
      modelDeclaration.add(SubCircuit(list.map(x => varHashMap(x.id())), startIndex))
    }
  }

  override def buildCtrCircuit(id: String, list: Array[XVarInteger], startIndex: Int, size: XVarInteger): Unit = {
    val vars = list.map(x => varHashMap(x.id()))
    // count non-self-looping variables
    modelDeclaration.add(Sum(vars.zipWithIndex.map({case (v, idx) => v !== (idx+startIndex)})) === varHashMap(size.id()))
    modelDeclaration.add(SubCircuit(list.map(x => varHashMap(x.id())), startIndex))
  }

  //unaryResource(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar])
  override def buildCtrNoOverlap(id: String, origins: Array[XVarInteger], lengths: Array[Int], zeroIgnored: Boolean): Unit = {
    val (starts, durations) = origins.zip(lengths).filter({case (v, l) => l != 0 || !zeroIgnored}).unzip
    val ends = buildEndsFromStartAndLength(starts, durations)
    val mStarts = starts.map(x => varHashMap(x.id()))
    val mDurations = durations.map(x => Constant(x):IntExpression)
    val mEnds = ends.map(x => varHashMap(x.id()))
    modelDeclaration.post(UnaryResource(mStarts, mDurations, mEnds))
    // Bind start, duration and ends
    (mStarts, mDurations, mEnds).zipped.foreach({case (s, d, e) => modelDeclaration.post((s+d)===e)})
  }

  override def buildCtrNoOverlap(id: String, origins: Array[XVarInteger], lengths: Array[XVarInteger], zeroIgnored: Boolean): Unit = {
    // TODO we ignore the value of zeroIgnored for now
    val ends = buildEndsFromStartAndLength(origins, lengths)
    val mStarts = origins.map(x => varHashMap(x.id()))
    val mDurations = lengths.map(x => varHashMap(x.id()))
    val mEnds = ends.map(x => varHashMap(x.id()))
    if(zeroIgnored) {
      val required: Array[BoolExpression] =  mDurations.map(_ !== 0)
      modelDeclaration.post(UnaryResource(mStarts, mDurations, mEnds, required))
    }
    else {
      modelDeclaration.post(UnaryResource(mStarts, mDurations, mEnds))
    }
    // Bind start, duration and ends
    (mStarts, mDurations, mEnds).zipped.foreach({case (s, d, e) => modelDeclaration.post((s+d)===e)})
  }

  override def buildCtrNoOverlap2D(id: String, x: Array[XVarInteger], dx: Array[Int], y: Array[XVarInteger], dy: Array[Int]): Unit = {
    modelDeclaration.post(DiffN(x.map(e => varHashMap(e.id())), dx.map(Constant), y.map(e => varHashMap(e.id())), dy.map(Constant)))
  }

  override def buildCtrNoOverlap2D(id: String, x: Array[XVarInteger], dx: Array[XVarInteger], y: Array[XVarInteger], dy: Array[XVarInteger]): Unit = {
    modelDeclaration.post(DiffN(x.map(e => varHashMap(e.id())), dx.map(e => varHashMap(e.id())), y.map(e => varHashMap(e.id())), dy.map(e => varHashMap(e.id()))))
  }

  override def buildCtrAllEqual(id: String, list: Array[XVarInteger]): Unit = {
    modelDeclaration.post(Eq(list.map(e => varHashMap(e.id()))))
  }

  override def buildCtrChannel(id: String, list1: Array[XVarInteger], startIndex1: Int, list2: Array[XVarInteger], startIndex2: Int): Unit = {
    modelDeclaration.post(Inverse(list1.map(e => varHashMap(e.id())).map(e => if(startIndex1 == 0) e else e-startIndex1),
      list2.map(e => varHashMap(e.id())).map(e => if(startIndex2 == 0) e else e-startIndex2)))
  }

  override def buildCtrChannel(id: String, list: Array[XVarInteger], startIndex: Int): Unit = {
    val corrected = list.map(e => varHashMap(e.id())).map(e => if(startIndex == 0) e else e-startIndex)
    modelDeclaration.post(Inverse(corrected, corrected))
  }

  override def buildCtrChannel(id: String, list: Array[XVarInteger], startIndex: Int, pos: XVarInteger): Unit = {
    // In this form, each variable has only domain 0/1. 'pos' should point to the index of the first variable having 1 as value
    val vars = list.map(e => varHashMap(e.id()))
    val posVar = varHashMap(pos.id())

    if(startIndex == 0)
      modelDeclaration.post(Channel(vars, posVar))
    else
      modelDeclaration.post(Channel(vars, posVar - startIndex))
  }

  override def buildCtrClause(id: String, pos: Array[XVarInteger], neg: Array[XVarInteger]): Unit = {
    modelDeclaration.post(Or(pos.map(e => varHashMap(e.id())).map(_ === 1) ++ neg.map(e => varHashMap(e.id())).map(_ === 0)))
  }

  override def buildCtrMDD(id: String, list: Array[XVarInteger], transitions: Array[Array[AnyRef]]): Unit = {
    // TODO move to XCallbacksDecomp
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

  override def buildCtrCount(id: String, list: Array[XVarInteger], values: Array[Int], bcondition: Condition): Unit = {
    val setOfVal = values.toSet
    val counterVar: IntExpression = Sum(list.map(x => varHashMap(x.id())).map( x => {
      if(setOfVal.size != 1) InSet(x, setOfVal) else x === setOfVal.head
    }))

    bcondition match {
      case condition: ConditionIntvl =>
        val min = condition.asInstanceOf[ConditionIntvl].min
        val max = condition.asInstanceOf[ConditionIntvl].max
        condition.operator match {
          case TypeConditionOperatorSet.IN =>
            modelDeclaration.add(counterVar <= max.toInt)
            modelDeclaration.add(counterVar >= min.toInt)
          case TypeConditionOperatorSet.NOTIN =>
            for (v <- min to max) {
              modelDeclaration.add(counterVar !== v.toInt)
            }
        }
      case condition: ConditionVal =>
        val c = condition.asInstanceOf[ConditionVal].k
        condition.operator match {
          case TypeConditionOperatorRel.LE => modelDeclaration.add(counterVar <= c.toInt)
          case TypeConditionOperatorRel.GE => modelDeclaration.add(counterVar >= c.toInt)
          case TypeConditionOperatorRel.GT => modelDeclaration.add(counterVar > c.toInt)
          case TypeConditionOperatorRel.LT => modelDeclaration.add(counterVar < c.toInt)
          case TypeConditionOperatorRel.EQ => modelDeclaration.add(counterVar === c.toInt)
          case TypeConditionOperatorRel.NE => modelDeclaration.add(counterVar !== c.toInt)
          case _ => throw new RuntimeException("not supported operator")
        }

      case condition: ConditionVar =>
        val c = varHashMap(condition.asInstanceOf[ConditionVar].x.id())
        condition.operator match {
          case TypeConditionOperatorRel.LE => modelDeclaration.add(counterVar <= c)
          case TypeConditionOperatorRel.GE => modelDeclaration.add(counterVar >= c)
          case TypeConditionOperatorRel.GT => modelDeclaration.add(counterVar > c)
          case TypeConditionOperatorRel.LT => modelDeclaration.add(counterVar < c)
          case TypeConditionOperatorRel.EQ => modelDeclaration.add(counterVar === c)
          case TypeConditionOperatorRel.NE => modelDeclaration.add(counterVar !== c)
          case _ => throw new RuntimeException("not supported operator")
        }
    }
  }

  override def buildCtrRegular(id: String, list: Array[XVarInteger], transitionsBase: Array[Array[AnyRef]], startStateBase: String, finalStatesBase: Array[String]): Unit = {
    val transitionsString = transitionsBase.map(x => (x(0).asInstanceOf[String], x(1).asInstanceOf[Long].toInt, x(2).asInstanceOf[String]))

    // First step is to convert all String "ids" of state to an equivalent Integer id
    val stringToId = mutable.HashMap[String, Integer]()
    val startState = stringToId.getOrElseUpdate(startStateBase, stringToId.size)
    val finalStates = finalStatesBase.map(x => stringToId.getOrElseUpdate(x, stringToId.size))
    var transitions = transitionsString.map({case (from, emit, to) => (stringToId.getOrElseUpdate(from, stringToId.size), emit, stringToId.getOrElseUpdate(to, stringToId.size))})

    // Compute min/max letter to be emitted
    var minLetter = transitions.map{case (from, emit, to) => emit}.min
    var maxLetter = transitions.map{case (from, emit, to) => emit}.max

    // The automaton in OscaR work with letters between 0 and maxLetter. If we have minLetter that is below 0, that might pose a problem
    val variables = if(minLetter < 0) {
      val newVars = list.map(e => varHashMap(e.id())+minLetter)
      transitions = transitions.map{case (from, emit, to) => (from, emit+minLetter, to)}
      maxLetter -= minLetter
      minLetter = 0
      newVars
    }
    else {
      list.map(e => varHashMap(e.id()))
    }

    // Now let's create the automaton
    val automaton = new Automaton(stringToId.size, maxLetter+1, startState, finalStates.toSet.asJava)
    transitions.foreach{case (from, emit, to) => automaton.addTransition(from, to, emit)}

    // And finally post the constraint
    modelDeclaration.post(Regular(variables, automaton))
  }

  override def buildCtrNValues(id: String, list: Array[XVarInteger], condition: Condition): Unit = {
    _buildCrtWithCondition(id, NValues(list.map(e => varHashMap(e.id()))), condition)
  }

  override def buildCtrNotAllEqual(id: String, list: Array[XVarInteger]): Unit = {
    modelDeclaration.post(NotAllEqual(list.map(e => varHashMap(e.id()))))
  }

  override def buildCtrCount(id: String, list: Array[XVarInteger], values: Array[XVarInteger], condition: Condition): Unit = ???
  override def buildCtrMinimum(id: String, list: Array[XVarInteger], startIndex: Int, index: XVarInteger, rank: TypeRank, condition: Condition): Unit = throw new Exception("Minimum/MinArg is not implemented")
  override def buildCtrMaximum(id: String, list: Array[XVarInteger], startIndex: Int, index: XVarInteger, rank: TypeRank, condition: Condition): Unit = throw new Exception("Maximum/MaxArg is not implemented")
  override def buildCtrCardinality(id: String, list: Array[XVarInteger], closed: Boolean, values: Array[XVarInteger], occurs: Array[XVarInteger]): Unit = throw new Exception("GCC with var cardinalities is not implemented")
  override def buildCtrCardinality(id: String, list: Array[XVarInteger], closed: Boolean, values: Array[XVarInteger], occurs: Array[Int]): Unit = throw new Exception("GCC with var cardinalities is not implemented")
  override def buildCtrCardinality(id: String, list: Array[XVarInteger], closed: Boolean, values: Array[XVarInteger], occursMin: Array[Int], occursMax: Array[Int]): Unit = throw new Exception("GCC with var cardinalities is not implemented")

  // Objectives
  def _getExprForTypeObjective(objtype: TypeObjective, list: Array[XVarInteger]): IntExpression = {
    objtype match {
      case TypeObjective.MAXIMUM => Max(list.map(i => varHashMap(i.id())))
      case TypeObjective.MINIMUM => Min(list.map(i => varHashMap(i.id())))
      case TypeObjective.SUM => Sum(list.map(i => varHashMap(i.id())))
      case TypeObjective.PRODUCT => Prod(list.map(i => varHashMap(i.id())))
      case TypeObjective.EXPRESSION => throw new XCSP3ParseException("TypeObjective.EXPRESSION should not be called without a tree")
      case TypeObjective.LEX => ???
      case TypeObjective.NVALUES => ???
    }
  }

  def _getExprForTypeObjective(objtype: TypeObjective, list: Array[XVarInteger], coefs: Array[Int]): IntExpression = {
    objtype match {
      case TypeObjective.MAXIMUM => Max(list.zip(coefs).map(i => varHashMap(i._1.id())*i._2))
      case TypeObjective.MINIMUM => Min(list.zip(coefs).map(i => varHashMap(i._1.id())*i._2))
      case TypeObjective.SUM => WeightedSum(list.map(i => varHashMap(i.id())), coefs)
      case TypeObjective.EXPRESSION => throw new XCSP3ParseException("TypeObjective.EXPRESSION should not be called without a tree")
      case TypeObjective.PRODUCT => Prod(list.map(i => varHashMap(i.id()))) //ignore coefs...
      case TypeObjective.LEX => ???
      case TypeObjective.NVALUES => ???
    }
  }

  override def buildObjToMinimize(id: String, objtype: TypeObjective, list: Array[XVarInteger]): Unit = {
    modelDeclaration.minimize(_getExprForTypeObjective(objtype, list).reify())
  }

  override def buildObjToMaximize(id: String, objtype: TypeObjective, list: Array[XVarInteger]): Unit = {
    modelDeclaration.maximize(_getExprForTypeObjective(objtype, list).reify())
  }

  override def buildObjToMinimize(id: String, x: XVarInteger): Unit = {
    modelDeclaration.minimize(varHashMap(x.id()).reify())
  }

  override def buildObjToMaximize(id: String, x: XVarInteger): Unit = {
    modelDeclaration.maximize(varHashMap(x.id()).reify())
  }

  override def buildObjToMinimize(id: String, objtype: TypeObjective, list: Array[XVarInteger], coeffs: Array[Int]): Unit = {
    modelDeclaration.minimize(_getExprForTypeObjective(objtype, list, coeffs).reify())
  }

  override def buildObjToMaximize(id: String, objtype: TypeObjective, list: Array[XVarInteger], coeffs: Array[Int]): Unit = {
    modelDeclaration.maximize(_getExprForTypeObjective(objtype, list, coeffs).reify())
  }

  override def buildObjToMinimize(id: String, syntaxTreeRoot: XNodeParent[XVarInteger]): Unit = {
    modelDeclaration.minimize(_recursiveIntentionBuilder(syntaxTreeRoot).reify())
  }

  override def buildObjToMaximize(id: String, syntaxTreeRoot: XNodeParent[XVarInteger]): Unit = {
    modelDeclaration.maximize(_recursiveIntentionBuilder(syntaxTreeRoot).reify())
  }

  ////////////

  override def buildCtrNValuesExcept(id: String, list: Array[XVarInteger], except: Array[Int], condition: Condition): Unit = ???

  // There is no public instance of XCSP3 with a stretch constraint yet :D
  override def buildCtrStretch(id: String, list: Array[XVarInteger], values: Array[Int], widthsMin: Array[Int], widthsMax: Array[Int]): Unit = ???
  override def buildCtrStretch(id: String, list: Array[XVarInteger], values: Array[Int], widthsMin: Array[Int], widthsMax: Array[Int], patterns: Array[Array[Int]]): Unit = ???
}

object XCSP3Parser2 {
  /**
    * Parse a XCSP3 instance
    * @param modelDeclarator the OscaR-Modeling ModelDeclator to be used
    * @param filename the path to the file to parse
    * @return a list of variables, and a solution generator.
    *         The solution generator, to be called when all variables are bound, will return a XCSP3 instantiation
    *         constraint equivalent to the solution found.
    */
  def parse(modelDeclarator: ModelDeclaration, filename: String): (Array[IntVar], () => String) = {
    val parser = new XCSP3Parser2(modelDeclarator, filename)
    val vars = parser.varHashMap.values.map(i => i.asInstanceOf[IntVar]).toArray
    (vars, () => parser.generateInstantiationWithSymbolic(vars.map(x => x.name), vars.map(x => x.evaluate())))
  }
}

object Parser2 extends CPApp[String] with App {


  class Parser2Config extends CPAppConfig {
    val instance = trailArg[String](descr = "Path to the file to parse")
    val check = opt[Boolean]("c", descr = "Check results with the XCSP3 checker")
  }
  override lazy val config: Parser2Config = new Parser2Config

  println("<--")
  val (vars, solutionGenerator) = XCSP3Parser2.parse(this.md, config.instance.get.get)

  //setSearch(Branchings.conflictOrderingSearch(vars,i => vars(i).size, i => vars(i).min))
  setSearch(Branchings.binaryMinDomOnWeightedDegree(vars))
  onSolution {
    solutionGenerator()
  }

  setDecompositionStrategy(new CartProdRefinement(vars, Branchings.conflictOrderingSearch(vars,i => vars(i).size, i => vars(i).min)))
  val (stats, solutions) = try {
    solve()
  }
  catch {
    case e: NoSolException =>
      println("This instance is already proven unsatisfeasible before solving")
      (SearchStatistics(nNodes=0, nFails=0, time=0, completed=true, timeInTrail=0, maxTrailSize=0, nSols=0, timeToLastSolution=0), List[String]())
  }

  println(stats)
  println("-->")
  println(solutions.mkString("\n\n"))

  def testSolution(instancePath: String, solution: String): Unit = {
    val sc = new CheckerLib(instancePath, solution)
    if(sc.getInvalidObjs.nonEmpty)
      throw new XCSP3ValidationException()
    if(sc.getViolatedCtrs.nonEmpty) {
      println(sc.getViolatedCtrs)
      throw new XCSP3ValidationException()
    }
  }
  if(config.check.get.contains(true)) {
    println("<!--")
    solutions.foreach(x => testSolution(config.instance.get.get, x))
    println("-->")
    println("<!-- solutions verified -->")
  }
}

object RunEverything extends CPApp[String] with App {
  //Basic/Basic-m1-s1/Allergy.xml.lzma -> symbolic
  //Basic/Basic-m1-s1/Purdey.xml.lzma -> symbolic
  //Bibd -> lex
  //PROBLEM WITH KNIGHTS

  def getFolderContent(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.toList.sortBy(_.getName)
    } else {
      List[File]()
    }
  }

  val inputs = getFolderContent("/Users/dervalguillaume/Downloads/xcsp3medium/")
    .filter(_.isDirectory)
    .map(f => (f.getName, f))
    .flatMap(x => getFolderContent(x._2.getAbsolutePath).filter(_.isDirectory).map(f => (x._1+"/"+f.getName, f)))
      .flatMap(x => {
        getFolderContent(x._2.getAbsolutePath).filter(_.getName.endsWith(".lzma")).map(f => (x._1, f.getName, f.getAbsolutePath))
      })

  val working = scala.collection.mutable.MutableList[String]()
  val notworking = scala.collection.mutable.MutableList[String]()

  val results = scala.util.Random.shuffle(inputs).map(tuple => {
    val (instanceName, fileName, instancePath) = tuple
    md.apply(md.getCurrentModel) {
      try {
        println("<!--" + instancePath)
        val (vars, solutionGenerator) = XCSP3Parser2.parse(md, instancePath)

        setSearch(Branchings.binaryFirstFail(vars.toSeq))
        onSolution {
          solutionGenerator()
        }

        setDecompositionStrategy(new CartProdRefinement(vars, Branchings.binaryFirstFail(vars.toSeq)))
        val (stats, solutions) = solve()
        //println(stats)
        println("-->")
        //println(solutions.mkString("\n\n"))
        solutions.foreach(x => testSolution(instancePath, x))
        //println("<!-- solutions verified -->")

        def testSolution(instancePath: String, solution: String): Unit = {
          if(!new CheckerLib(instancePath, solution).valid)
            throw new XCSP3ValidationException()
        }

        (instanceName, instancePath, "ok")
      }
      catch {
        case a: XCSP3TimeoutException => (instanceName, instancePath, "timeout")
        case a: XCSP3ValidationException => (instanceName, instancePath, "validation error")
        case a: Exception => (instanceName, instancePath, "exception")
        case a: NotImplementedError => (instanceName, instancePath, "not implemented")
      }
    }
  }).seq.sortBy(x => x._3)
  println(results.mkString("\n"))
}