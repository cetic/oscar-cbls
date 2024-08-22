package oscar.cbls.test.invBench

import oscar.cbls.core.computation.set.SetVariable
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import scala.annotation.tailrec
import oscar.cbls.core.computation.Variable


case class SetMovement(addedValues: Set[Int], removedValues: Set[Int], id: Int)
    extends VariableMove(id) {

  override def mkMove(testVar : Variable) : Unit = {
    testVar match {
      case setTestVar : SetVariable => mkSetMove(setTestVar)
      case _ => throw new Error("Set Movement can only update variable of type Set")
    }
  }


  
  private def mkSetMove(testVar : SetVariable): Unit = {
    for (v <- addedValues)
      testVar :+= v
    for (v <- removedValues)
      testVar :-= v
    testVar.model.propagate()
  }

  override def updateState(state : VariableState) : VariableState = {
    state match {
      case setState : SetVarState => updateSetState(setState)
      case _ => throw new Error("Set Movement can only update Set State")
    }
  }


  private def updateSetState(state: SetVarState) : SetVarState= {
    SetVarState(state.value ++ addedValues -- removedValues,state.id,state.domain)
  }
}

case class SetAssignMovement(newValues: Set[Int], id: Int) extends VariableMove(id) {
  override def mkMove(testVar : Variable): Unit = {
    testVar match {
      case setTestVar : SetVariable => setTestVar := newValues
      case _ => throw new Error("Set Movement can only update variable of type Set")
    }
  }

  override def updateState(state: VariableState) : VariableState= {
    state match {
      case setState : SetVarState => SetVarState(newValues,setState.id,setState.domain)
      case _ => throw new Error("Set Movement can only update Set State")
    }
  }

  
}

case class SetVarState(value: Set[Int], id: Int, domain: Option[(Int, Int)])
    extends VariableState(id) {

  override def canMake(m : VariableMove) : Boolean = true

  /** Checks if all possible values are in the Set.
    *
    * Given the nature of a Set, once it contains all its domain, there is no way to add more
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
  // Since this method is called only when allValuesInSet returns false, it'll never end up in a endless loop.
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
    yield SetAssignMovement(l.toSet, id)

  override def generateMove(): Gen[VariableMove] = {
    val onlyAdd = for { add <- generateAddedValues() } yield SetMovement(add, Set.empty, id)
    val addRemoveGenMove: Gen[VariableMove] = if (value.isEmpty) {
      onlyAdd
    } else {
      val onlyRemove = for { rem <- generateRemovedValues() } yield SetMovement(Set.empty, rem, id)
      if (allValuesInSet()) {
        onlyRemove
      } else {
        val both = for {
          add <- generateAddedValues()
          rem <- generateRemovedValues()
        } yield SetMovement(add, rem, id)
        Gen.oneOf(onlyAdd, onlyRemove, both)
      }
    }
    Gen.oneOf(generateAssignMove(), addRemoveGenMove)
  }

}
