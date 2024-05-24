package oscar.cbls.test.core.computation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.core.computation.{Invariant, Store}
import oscar.cbls.core.computation.set.{
  SetConstant,
  SetIdentityInvariant,
  SetNotificationTarget,
  SetVariable
}

import scala.collection.mutable.{HashSet => MutSet}
import scala.util.Random

class SetVariableTestSuite extends AnyFunSuite {

  val random: Random     = Random
  private val seed: Long = random.nextLong()
  random.setSeed(seed)

  private def testSubjects(
    random: Random
  ): (Store, MutSet[Int], SetVariable, SetVariable, SetIdentityInvariant, TestSetInvariant) = {
    val store                     = new Store(debugLevel = 3)
    val randomSet: Set[Int]       = Set.fill(random.between(1, 1000))(random.between(-1000, 1000))
    val referenceSet: MutSet[Int] = MutSet.from(randomSet)

    val inputVar  = SetVariable(store, randomSet, name = Some("Input"))
    val outputVar = SetVariable(store, randomSet, name = Some("Output"))
    val idInv     = new SetIdentityInvariant(store, inputVar, outputVar)
    val testInv   = new TestSetInvariant(store, inputVar)
    store.close()

    (store, referenceSet, inputVar, outputVar, idInv, testInv)
  }

  private class TestSetInvariant(model: Store, fromValue: SetVariable)
      extends Invariant(model)
      with SetNotificationTarget {

    fromValue.registerStaticallyAndDynamicallyListeningElement(this)

    def notifySetChanges(
      setVariable: SetVariable,
      index: Int,
      addedElems: Iterable[Int],
      removedElems: Iterable[Int],
      oldValue: Set[Int],
      newValue: Set[Int]
    ): Unit = {
      Set.from(addedElems) intersect Set.from(removedElems) should be(Set.empty)
      newValue should be(oldValue ++ addedElems -- removedElems)
    }

    def checkInternals(): Unit = {}
  }

  test(s"Set identity invariant and SetVariable work when adding elements (seed: $seed).") {
    val (_, referenceSet, inputVar, outputVar, _, _) = testSubjects(random)
    val additions = List.fill(random.between(1, 100))(random.between(-1000, 1000))
    // TODO complete
  }

  test(s"Set identity invariant and SetVariable work when removing elements (seed: $seed).") {
    val (_, referenceSet, inputVar, outputVar, _, _) = testSubjects(random)
    // TODO complete
  }

  test(
    s"Set identity invariant and SetVariable work " +
      s"when adding and removing elements simultaneously (seed: $seed)."
  ) {
    val (_, referenceSet, inputVar, outputVar, _, _) = testSubjects(random)
    // TODO complete
  }

  test(s"Set identity invariant and SetVariable work when manually setting (seed: $seed).") {
    val (_, referenceSet, inputVar, outputVar, _, _) = testSubjects(random)
    // TODO complete
  }

  test(s"Save and restore methods work as expected. (seed: $seed)") {
    val (_, referenceSet, inputVar, outputVar, _, _) = testSubjects(random)
    val originalReferenceSet                         = Set.from(referenceSet)
    val savedValue                                   = inputVar.save()

    for (_ <- 0 until 10) {
      val value = random.nextInt(100)
      inputVar :+= value
      referenceSet += value
    }
    outputVar.value() should be(referenceSet)
    savedValue.restoreValue()
    outputVar.value() should be(originalReferenceSet)
  }

  test(s"A SetConstant cannot be modified. (seed: $seed)") {
    val store = new Store()
    val const = new SetConstant(store, Set.empty)
    store.close()

    def hasGoodMessage(e: Exception): Boolean =
      e.getMessage.contains("The value of a constant variable cannot be changed")

    val exAdd = intercept[IllegalArgumentException](const :+= 15)
    val exRem = intercept[IllegalArgumentException](const :-= 15)
    val exSet = intercept[IllegalArgumentException](const := Set(15))

    assert(hasGoodMessage(exAdd), "Exception issue on addition")
    assert(hasGoodMessage(exRem), "Exception issue on removal")
    assert(hasGoodMessage(exSet), "Exception issue on setting value")
  }
}
