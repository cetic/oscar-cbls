// OscaR is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 2.1 of the License, or
// (at your option) any later version.
//
// OscaR is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License  for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with OscaR.
// If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html

package oscar.cbls.util.invBench

import oscar.cbls.core.computation.set.SetVariable
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import scala.annotation.tailrec
import oscar.cbls.core.computation.Variable

/** Defines the move that adds and removes values from a
  * [[oscar.cbls.core.computation.set.SetVariable]].
  *
  * @param addedValues
  *   The values that will be added
  * @param removedValues
  *   The values that will be removed
  * @param id
  *   The id of the variable
  */
case class SetMove(addedValues: Set[Int], removedValues: Set[Int], id: Int)
    extends VariableMove(id) {

  override def mkMove(testVar: Variable): Unit = {
    testVar match {
      case setTestVar: SetVariable => mkSetMove(setTestVar)
      case _ => throw new Error("Set Movement can only update variable of type Set")
    }
  }

  private def mkSetMove(testVar: SetVariable): Unit = {
    for (v <- addedValues)
      testVar :+= v
    for (v <- removedValues)
      testVar :-= v
  }

  override def updateState(state: VariableState): VariableState = {
    state match {
      case setState: SetVarState => updateSetState(setState)
      case _                     => throw new Error("Set Movement can only update Set State")
    }
  }

  override def toString = s"SetMovement(added: $addedValues,removed: $removedValues,id: $id)"

  private def updateSetState(state: SetVarState): SetVarState = {
    SetVarState(state.value ++ addedValues -- removedValues, state.id, state.domain)
  }
}

/** Defines the move that assigns a value to a [[oscar.cbls.core.computation.set.SetVariable]].
  *
  * @param newValues
  *   The new value that will be assigned.
  * @param id
  *   The id of the variable.
  */
case class SetAssignMove(newValues: Set[Int], id: Int) extends VariableMove(id) {
  override def mkMove(testVar: Variable): Unit = {
    testVar match {
      case setTestVar: SetVariable => setTestVar := newValues
      case _ => throw new Error("Set Movement can only update variable of type Set")
    }
  }

  override def updateState(state: VariableState): VariableState = {
    state match {
      case setState: SetVarState => SetVarState(newValues, setState.id, setState.domain)
      case _                     => throw new Error("Set Movement can only update Set State")
    }
  }

  override def toString: String = s"SetAssignMove($newValues id:$id)"

}

/** Defines the state of a [[oscar.cbls.core.computation.set.SetVariable]].
  *
  * @param value
  *   The current value of the variable
  * @param id
  *   The id of the variable
  * @param domain
  *   The domain of the variable if there is one
  */
case class SetVarState(value: Set[Int], id: Int, domain: Option[(Int, Int)])
    extends VariableState(id) {

  override def toString: String = {
    val domainString = domain.map(d => s", domain:[${d._1},${d._2}] id=$id").getOrElse("")
    s"SetVarState(value: $value$domainString)"
  }

  override def canMake(m: VariableMove): Boolean = {
    m match {
      case SetAssignMove(newValues, moveId) =>
        domain.forall(d => newValues.forall(v => d._1 <= v && v <= d._2)) &&
        id == moveId
      case SetMove(addedValues, removedValues, moveId) =>
        domain.forall(d => addedValues.forall(v => d._1 <= v && v <= d._2)) &&
        removedValues.forall(v => value.contains(v)) &&
        id == moveId
      case _ => throw new Error("SetVarState can only test moves of type SetAssignMove and SetMove")
    }
  }

  /** Checks if all possible values are in the set.
    *
    * Given the nature of a set, once it contains all its domain, there is no way to add more
    * values.
    */
  private def allValuesInSet(): Boolean = {
    domain match {
      case None => value.size == Int.MaxValue
      case Some((min, max)) =>
        val nbValues = max - min + 1
        value.size == nbValues
    }
  }

  @tailrec
  // Since this method is called only when allValuesInSet returns false, it'll never end up in an endless loop.
  private def getNextNotInValue(i: Int): Int = {
    if (value.contains(i)) {
      domain match {
        case None => getNextNotInValue(i + 1)
        case Some((min, max)) =>
          if (i == max) getNextNotInValue(min) else getNextNotInValue(i + 1)
      }
    } else {
      i
    }
  }

  private val generateDomainValue: Gen[Int] = domain match {
    case None             => arbitrary[Int]
    case Some((min, max)) => Gen.choose(min, max)
  }

  private def generateAddedValues(): Gen[Set[Int]] = {
    val generateNotInValue: Gen[Int] = for (v <- generateDomainValue) yield getNextNotInValue(v)

    for (l <- Gen.nonEmptyListOf(generateNotInValue)) yield l.toSet
  }

  private def generateRemovedValues(): Gen[Set[Int]] =
    for (l <- Gen.nonEmptyListOf(Gen.oneOf(value))) yield l.toSet

  private def generateAssignMove(): Gen[VariableMove] = for (l <- Gen.listOf(generateDomainValue))
    yield SetAssignMove(l.toSet, id)

  override def generateMove(): Gen[VariableMove] = {
    val onlyAdd = for { add <- generateAddedValues() } yield SetMove(add, Set.empty, id)
    val addRemoveGenMove: Gen[VariableMove] = if (value.isEmpty) {
      onlyAdd
    } else {
      val onlyRemove = for { rem <- generateRemovedValues() } yield SetMove(Set.empty, rem, id)
      if (allValuesInSet()) {
        onlyRemove
      } else {
        val both = for {
          add <- generateAddedValues()
          rem <- generateRemovedValues()
        } yield SetMove(add, rem, id)
        Gen.oneOf(onlyAdd, onlyRemove, both)
      }
    }
    Gen.oneOf(generateAssignMove(), addRemoveGenMove)
  }
}
