package oscar.cbls.test.invBench

import oscar.cbls.core.computation.set.SetVariable
import oscar.cbls.core.computation.Invariant
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import scala.annotation.tailrec

class SetTestVariable(override val variable: SetVariable) extends TestVariable(variable) {

  private def allValuesInSet(): Boolean = {
    variable.domain match {
      case None => false
      case Some((min, max)) =>
        val nbValues = max - min + 1
        variable.value().size == nbValues
    }
  }

  @tailrec
  private def getNextNotInValue(i: Int): Int = {
    if (variable.value().contains(i)) {
      variable.domain match {
        case None => getNextNotInValue(i + 1)
        case Some((min, max)) =>
          if (i == max) getNextNotInValue(min.toInt) else getNextNotInValue(i + 1)
      }
    } else {
      i
    }
  }

  private val generateDomainValue: Gen[Int] = variable.domain match {
    case None             => arbitrary[Int]
    case Some((min, max)) => Gen.choose(min.toInt, max.toInt)
  }

  private def generateAddedValues(): Gen[List[Int]] = {

    val generateNotInValue: Gen[Int] = for (v <- generateDomainValue) yield getNextNotInValue(v)

    Gen.nonEmptyListOf(generateNotInValue)
  }

  private def generateRemovedValues(): Gen[List[Int]] =
    Gen.nonEmptyListOf(Gen.oneOf(variable.value()))

  override def generateAssignMove(): Gen[VariableMove] = for (l <- Gen.listOf(generateDomainValue))
    yield SetAssignMovement(this, Set.from(l))

  override def generateMove(): Gen[VariableMove] = {
    val onlyAdd = for { add <- generateAddedValues() } yield SetMovement(this, add, List())
    val addRemoveGenMove: Gen[VariableMove] = if (variable.value().isEmpty) {
      onlyAdd
    } else {
      val onlyRemove = for { rem <- generateRemovedValues() } yield SetMovement(this, List(), rem)
      if (allValuesInSet()) {
        onlyRemove
      } else {
        val both = for {
          add <- generateAddedValues()
          rem <- generateRemovedValues()
        } yield SetMovement(this, add, rem)
        Gen.oneOf(onlyAdd, onlyRemove, both)
      }
    }
    Gen.oneOf(generateAssignMove(), addRemoveGenMove)
  }

  override def toString: String = {
    s"SetVariable($variable)"
  }
}

case class SetMovement(testVar: SetTestVariable, addedValues: List[Int], removedValues: List[Int])
    extends VariableMove(testVar) {
  override def mkMove(): Unit = {
    for (v <- addedValues)
      testVar.variable :+= v
    for (v <- removedValues)
      testVar.variable :-= v
    testVar.model.propagate()
  }
}

case class SetAssignMovement(testVar: SetTestVariable, newValues: Set[Int])
    extends VariableMove(testVar) {
  override def mkMove(): Unit = {
    testVar.variable := newValues
  }
}
