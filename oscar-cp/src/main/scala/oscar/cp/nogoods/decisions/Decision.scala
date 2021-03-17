package oscar.cp.nogoods.decisions

import oscar.cp._
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPBoolVar

/** @author Renaud Hartert ren.hartert@gmail.com */
abstract class Decision {
  def apply(): Unit
  def opposite: Decision
  def isTrue: Boolean
  def toLiteral: CPBoolVar
  def unary_!(): Decision = opposite
  def strengthen(): Unit = ()
  def variable(): CPIntVar
  def value(): Int
}

class Assign(val variable: CPIntVar, val value: Int) extends Decision {
  override def apply(): Unit = variable.store.post(variable.eq(value))
  override def opposite: Decision = new Remove(variable, value)
  override def isTrue: Boolean = variable.isBoundTo(value)
  override def toLiteral: CPBoolVar = variable ?=== value
  override def toString: String = s"[${variable.name} == $value]"
}

class Remove(val variable: CPIntVar, val value: Int) extends Decision {
  override def apply(): Unit = variable.store.post(variable.diff(value))
  override def opposite: Decision = new Assign(variable, value)
  override def isTrue: Boolean = !variable.hasValue(value)
  override def toLiteral: CPBoolVar = variable ?!== value
  override def toString: String = s"[${variable.name} != $value]"
}

class LowerEq(val variable: CPIntVar, var value: Int) extends Decision {
  override def apply(): Unit = {
    variable.store.post(variable <= value)
  }
  override def opposite: Decision = new Greater(variable, value)
  override def isTrue: Boolean = variable.max <= value
  override def toLiteral: CPBoolVar = variable ?<= value
  override def toString: String = s"[${variable.name} <= $value]"
  override def strengthen(): Unit = value = variable.max
}

class Greater(val variable: CPIntVar, var value: Int) extends Decision {
  override def apply(): Unit = {
    variable.store.post(variable > value)
  }
  override def opposite: Decision = new LowerEq(variable, value)
  override def isTrue: Boolean = variable.min > value
  override def toLiteral: CPBoolVar = variable ?> value
  override def toString: String = s"[${variable.name} > $value]"
  override def strengthen(): Unit = {
    val min = variable.min - 1
    value = min
  }
}