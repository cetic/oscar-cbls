package oscar.cbls.test.core.computation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import oscar.cbls.core.computation.{Invariant, Store}
import oscar.cbls.core.computation.set._

import scala.collection.mutable.{HashSet => MutSet}
import scala.util.Random

class SetVariableTestSuite extends AnyFunSuite {

  private val random: Random = Random
  private val seed: Long     = Random.nextLong()
  random.setSeed(seed)

  // small utility method to generate a random list of integers
  private def randList(maxSize: Int = 1000, minVal: Int = -1000, maxVal: Int = 1000) =
    List.fill(random.between(1, maxSize))(random.between(minVal, maxVal))

  // or tails?
  private def heads() = random.nextInt() % 2 == 0

  // simple function to get a random subset of the input collection
  private def randSubset[A](xs: Iterable[A]): Iterable[A] = {
    val b = Iterable.newBuilder[A]
    for (x <- xs) {
      if (heads()) b += x
    }
    b.result()
  }

  // generate and exports the objects we need to test
  private def testSubjects()
    : (Store, MutSet[Int], SetVariable, SetVariable, SetIdentityInvariant, TestSetInvariant) = {
    val store                     = new Store(debugLevel = 3)
    val randomSet: Set[Int]       = Set.from(randList())
    val referenceSet: MutSet[Int] = MutSet.from(randomSet)

    val inputVar  = SetVariable(store, randomSet, name = Some("Input"))
    val outputVar = SetVariable(store, randomSet, name = Some("Output"))
    val idInv     = new SetIdentityInvariant(store, inputVar, outputVar)
    val testInv   = new TestSetInvariant(store, inputVar)
    store.close()

    (store, referenceSet, inputVar, outputVar, idInv, testInv)
  }

  // an auxiliary invariant. Upon notification should test the basic properties of the diff lists:
  // * addedElems has no elements in common with removedElems
  // * newValue = oldValue + addedElems - removedElems

  private class TestSetInvariant(model: Store, fromValue: SetVariable, testNotify: Boolean = false)
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
      if (testNotify) println("Notification testing occurred")
    }

    def checkInternals(): Unit = {}
  }

  test(s"Set identity invariant and SetVariable work when adding elements (seed: $seed).") {
    val (_, referenceSet, inputVar, outputVar, _, _) = testSubjects()
    val additions                                    = randList(maxSize = 10)
    for (a <- additions) {
      referenceSet += a
      inputVar :+= a
    }
    outputVar.value() should be(referenceSet)
  }

  test(s"Set identity invariant and SetVariable work when removing elements (seed: $seed).") {
    val (_, referenceSet, inputVar, outputVar, _, _) = testSubjects()
    val removals = randList(maxSize = 10) appendedAll randSubset(referenceSet)
    for (r <- removals) {
      referenceSet -= r
      inputVar :-= r
    }
    outputVar.value() should be(referenceSet)
  }

  test(
    s"Set identity invariant and SetVariable work " +
      s"when adding and removing elements simultaneously (seed: $seed)."
  ) {
    val (_, referenceSet, inputVar, outputVar, _, _) = testSubjects()
    val elements = randList(maxSize = 10) appendedAll randSubset(referenceSet)
    for (e <- elements) {
      if (heads()) {
        referenceSet += e
        inputVar :+= e
      } else {
        referenceSet -= e
        inputVar :-= e
      }
    }
    outputVar.value() should be(referenceSet)
  }

  test(
    s"Set identity invariant and SetVariable work " +
      s"when manually setting their value (seed: $seed)."
  ) {
    val (_, _, inputVar, outputVar, _, _) = testSubjects()
    val newSet                            = Set.from(randList())
    inputVar := newSet
    outputVar.value() should be(newSet)
  }

  test(s"Save and restore methods work as expected. (seed: $seed)") {
    val (_, referenceSet, inputVar, outputVar, _, _) = testSubjects()
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
