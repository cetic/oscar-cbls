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

package oscar.modeling.visualisation

import oscar.modeling.algebra.bool._
import oscar.modeling.algebra.integer._
import oscar.modeling.constraints.{Constraint, ExpressionConstraint}
import org.graphstream.graph.implementations.SingleGraph
import org.graphstream.graph.{Edge, Graph, Node}
import org.graphstream.ui.view.{Viewer, ViewerListener}
import oscar.modeling.algebra.integer.{Constant, IntExpression}
import oscar.modeling.vars.{BoolVar, IntVar}
import oscar.modeling.visualisation.Color.Color

import scala.jdk.CollectionConverters._

object Color extends Enumeration {
  type Color = Value
  val DEFAULT, SPECIAL = Value
}

class ConstraintsVisualisation(constraints: Array[Constraint], name: String) {
  System.setProperty("org.graphstream.ui.renderer", "org.graphstream.ui.j2dviewer.J2DGraphRenderer")
  val graph = new SingleGraph("Constraint view: " + name)
  var nextidx: Int = 0

  graph.setAttribute("ui.stylesheet",
    """
      graph {
      }
      node {
        size-mode: fit;
        shape: box;
        fill-color: transparent;
        stroke-mode: plain;
        stroke-color: yellow;
        text-background-mode: rounded-box;
        text-visibility-mode: normal;
        text-background-color: #ddd;
        text-alignment: center;
        text-padding: 5px;
      }
      node.special {
        text-background-color: #c77;
      }
      edge {
        text-alignment: along;
        fill-color: #ccc;
      }
      edge.special {
        fill-color: #c77;
      }
    """)
  val roots = constraints.map(parseConstraint(_))


  def display(): Unit = {
    val viewer = graph.display(false)
    val layout = new ForestLayout
    viewer.enableAutoLayout(layout)
    layout.setRoots(roots.toList.asJava)
    new Thread(new Runnable {
      def run(): Unit = {
        new TerminalNodeDisplay(viewer, graph).pump()
      }
    }).start()
  }

  private def parseConstraint(const: Constraint): String = {
    const match {
      case ExpressionConstraint(expr) => createRoot(const, expr)
    }
  }

  private def createRoot(const: Constraint, expr: IntExpression): String = {
    //TODO: create a specific node for the constraint
    //TODO: each constraint may have more than one sub-expr
    parseExpression(expr)
  }

  private def parseExpression(expr: IntExpression): String = {
    expr match {
      //std expressions
      case And(array) => basicCreate(expr, "And", array, "&&")
      case Div(x, y) => basicCreate(expr, "Divide", Seq(x,Constant(y)), "/")
      case Eq(array) => basicCreate(expr, "Equality", array, "=")
      case Exponent(x, y) => basicCreate(expr, "Exponent", Seq(x,y), "**")
      case Gr(a, b) => basicCreate(expr, "Greater", Seq(a,b), ">")
      case GrEq(a, b) => basicCreate(expr, "Greater or equal", Seq(a,b), ">=")
      case Implication(a,b) => basicCreate(expr, "Implication", Seq(a,b), "=>")
      case Lr(a, b) => basicCreate(expr, "Lesser", Seq(a,b), "<")
      case LrEq(a, b) => basicCreate(expr, "Lesser or equal", Seq(a,b), "<=")
      case Max(a) => basicCreate(expr, "Max", a, "")
      case Min(a) => basicCreate(expr, "Min", a, "")
      case Minus(a, b) => basicCreate(expr, "Minus", Seq(a,b), "-")
      case Modulo(a, b) => basicCreate(expr, "Modulo", Seq(a,Constant(b)), "%")
      case Not(a) => basicCreate(expr, "Not", Seq(a), "")
      case NotEq(a, b) => basicCreate(expr, "Not equal", Seq(a, b), "!=")
      case Or(a) => basicCreate(expr, "Or", a, "||")
      case Prod(a) => basicCreate(expr, "Product", a, "*")
      case Sum(a) => basicCreate(expr, "Sum", a, "+")
      case UnaryMinus(a) => basicCreate(expr, "Unary minus", Seq(a), "")
      case Xor(a, b) => basicCreate(expr, "Xor", Seq(a,b), "^")

      //terminal expressions
      case Constant(a) => terminalNodeCreate(expr, "Constant ("+a.toString+")")
      case i: BoolVar => terminalNodeCreate(expr, if(!i.name.isEmpty) i.name+" (BoolVar)" else "BoolVar")
      case i: IntVar => terminalNodeCreate(expr, if(!i.name.isEmpty) i.name+" (IntVar)" else "IntVar")

      //More complicated expressions
      case Count(array, value) =>
        val t1 = array.zipWithIndex.map(entry => (parseExpression(entry._1), "array["+entry._2+"]", Color.DEFAULT))
        val t2 = Array((parseExpression(value), "value", Color.SPECIAL))
        val merge = t1 ++ t2
        nodeCreate(expr, "Count", merge, Array.tabulate(merge.size-1)(_ => ""), Color.DEFAULT)
      case Element(array, key) =>
        val t1 = array.zipWithIndex.map(entry => (parseExpression(entry._1), "array["+entry._2+"]", Color.DEFAULT))
        val t2 = Array((parseExpression(key), "key", Color.SPECIAL))
        val merge = t1 ++ t2
        nodeCreate(expr, "Element", merge, Array.tabulate(merge.size-1)(_ => ""), Color.DEFAULT)
      case InSet(value, set) =>
        val t1 = set.toArray.map(entry => (parseExpression(entry), "set element", Color.DEFAULT))
        val t2 = Array((parseExpression(value), "value", Color.SPECIAL))
        val merge = t1 ++ t2
        nodeCreate(expr, "In set", merge, Array.tabulate(merge.size-1)(_ => ""), Color.DEFAULT)
      case WeightedSum(array, weights) =>
        val t = array.zip(weights).map(entry => (parseExpression(entry._1), entry._2.toString, Color.DEFAULT))
        nodeCreate(expr, "Weighted Sum", t, Array.tabulate(t.size-1)(_ => ""), Color.DEFAULT)
    }
  }

  private def basicCreate(expr: IntExpression, name: String, children: Seq[IntExpression], between: Seq[String]): String = nodeCreate(expr, name, children.map(a => (parseExpression(a), "", Color.DEFAULT)), between, Color.DEFAULT)
  private def basicCreate(expr: IntExpression, name: String, children: Seq[IntExpression], between: String): String = basicCreate(expr, name, children, Array.tabulate(children.size-1)(_ => between))
  private def terminalNodeCreate(expr: IntExpression, name: String): String = nodeCreate(expr, name, Seq(), Seq(), Color.SPECIAL)

  private def nodeCreate(expr: IntExpression, name: String, children: Seq[(String, String, Color)], between: Seq[String], color: Color): String = {
    val idx = nextidx.toString
    nextidx += 1
    val node: Node = graph.addNode(idx)
    node.addAttribute("ui.label", name)
    node.addAttribute("expression", expr)

    color match {
      case Color.DEFAULT => {}//do nothing
      case Color.SPECIAL => node.setAttribute("ui.class", "special")
    }

    for(((child, edgeName, color), cidx) <- children.zipWithIndex) {
      val edge: Edge = graph.addEdge(idx+"-"+cidx+"-"+child, child, idx, true)
      if(edgeName != "")
        edge.setAttribute("ui.label", edgeName)
      color match {
        case Color.DEFAULT => {}//do nothing
        case Color.SPECIAL => edge.setAttribute("ui.class", "special")
      }
    }
    idx
  }
}

class TerminalNodeDisplay(viewer: Viewer, graph: Graph) extends ViewerListener {
  var stop = false
  val pipe = viewer.newViewerPipe()
  pipe.addViewerListener(this)
  pipe.addSink(graph)
  pipe.removeElementSink(graph);

  def pump(): Unit = { while(!stop) pipe.blockingPump() }
  override def buttonReleased(id: String): Unit = {
    val n:Node = graph.getNode(id)
    val expr: IntExpression= n.getAttribute("expression")

    expr match {
      case a: BoolVar =>
        println("Var id: "+a.varid)
        a.name.foreach(name => println("\tVar name: "+name))
        println("\tBoolean value: "+a.iterator.mkString(","))
      case a: IntVar =>
        println("Var id: "+a.varid)
        a.name.foreach(name => println("\tVar name: "+name))
        println("\tInteger value: "+a.iterator.mkString(","))
      case Constant(a) =>
        println("Constant value: "+a.toString)
      case default => {}
    }
  }

  override def buttonPushed(id: String): Unit = {}

  override def viewClosed(viewName: String): Unit = {
    stop = true
  }
}

