package oscar.cbls.test.algo

import org.scalatest.funsuite.AnyFunSuite
import oscar.cbls.algo.conflict.ConflictSearch

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import scala.util.Random

class ConflictSearchSuite extends AnyFunSuite {

  val rand = new Random(1000)

  /* The test data contains two conflicting sets. A set is said to have conflict if either
   * conflicting set one or conflicting set two is included into the set.
   */
  private case class TestData(
    conflictingSetOne: SortedSet[Int],
    conflictingSetTwo: SortedSet[Int]
  ) {
    // Returns true if s1 includes s2 (i.e. all elements of s1 are in s2)
    private def includes(s1: SortedSet[Int], s2: SortedSet[Int]): Boolean = {
      s2.foldRight(true)((elem, inRest) => s1.contains(elem) && inRest)
    }

    def conflict(s: SortedSet[Int]): Boolean = {
      includes(s, conflictingSetOne) || includes(s, conflictingSetTwo)
    }

  }

  private def inject(s: SortedSet[Int], elem: Int): SortedSet[Int] =
    s + elem

  private def remove(s: SortedSet[Int], elem: Int): SortedSet[Int] =
    s - elem

  private def randConflictingSet: SortedSet[Int] = {
    @tailrec
    def mkSet(size: Int, res: SortedSet[Int] = SortedSet.empty[Int]): SortedSet[Int] = {
      size match {
        case 0 => res
        case n =>
          val toInsert = rand.nextInt(15)
          if (res.contains(toInsert))
            mkSet(n, res)
          else
            mkSet(n - 1, res + toInsert)
      }
    }
    mkSet(1 + rand.nextInt(5))
  }

  private def randomTestData: TestData =
    TestData(randConflictingSet, randConflictingSet)

  @tailrec
  private def randSet(
    nb: Int,
    s: SortedSet[Int] = SortedSet.empty[Int],
    except: SortedSet[Int] = SortedSet.empty[Int]
  ): SortedSet[Int] = {
    nb match {
      case 0 => s
      case n =>
        val toInsert = rand.nextInt(15)
        val nextS    = if (except.contains(toInsert)) s else s + toInsert
        randSet(n - 1, nextS, except)
    }
  }

  @tailrec
  private def randList(
    nb: Int,
    starting: List[Int] = List(),
    except: List[Int] = List()
  ): List[Int] = {
    nb match {
      case 0 => starting
      case n =>
        val toInsert     = rand.nextInt(15)
        val nextStarting = if (except.contains(toInsert)) starting else toInsert :: starting
        randList(n - 1, nextStarting, except)
    }
  }

  private val testData = randomTestData

  /* The test cases shall be tested for the two algorithms: The algorithms without remove (algo 1, quickXPlain)
   * and the one with remove (algo 2, xPlainWithRemove)
   *
   * The test cases are the following
   * - When the starting set has conflict, the minimal conflict set is empty (test 1)
   * - Check that the resulting set creates conflict (test 2)
   *                  this test is divided in two tests
   *       * In test 2.1, we start with an init set with no conflicting elements in it
   *       * In test 2.2, some conflicting elements are already in the conflict set
   * - Check that the resulting set is minimal (check that one can not remove elements
   *                  from the result and still having conflicts) (test 3)
   * - Check that if the list of elements to add do not create conflicts, the algorithms returns an exception (test 4)
   */

  // test 1
  test("When the starting set has a conflict, the minimal conflict set is empty") {
    val injectElement = randList(10)

    val initSetWithConflict1 = randSet(10, testData.conflictingSetOne)
    val initSetWithConflict2 = randSet(10, testData.conflictingSetTwo)

    // Algo1
    val conflictingList1Algo1 =
      ConflictSearch.quickXPlain(initSetWithConflict1, injectElement, inject, testData.conflict)
    val conflictingList2Algo1 =
      ConflictSearch.quickXPlain(initSetWithConflict2, injectElement, inject, testData.conflict)

    assert(
      conflictingList1Algo1.isEmpty,
      s"inserting $injectElement in $initSetWithConflict1 should return " +
        s"an empty list but returned $conflictingList1Algo1"
    )
    assert(
      conflictingList2Algo1.isEmpty,
      s"inserting $injectElement in $initSetWithConflict2 should return " +
        s"an empty list but returned $conflictingList2Algo1"
    )

    // Algo2
    val conflictingList1Algo2 =
      ConflictSearch.xPlainWithRemove(
        initSetWithConflict1,
        injectElement,
        inject,
        remove,
        testData.conflict
      )
    val conflictingList2Algo2 =
      ConflictSearch.xPlainWithRemove(
        initSetWithConflict2,
        injectElement,
        inject,
        remove,
        testData.conflict
      )

    assert(
      conflictingList1Algo2.isEmpty,
      s"inserting $injectElement in $initSetWithConflict1 should return " +
        s"an empty list but returned $conflictingList1Algo2"
    )

    assert(
      conflictingList2Algo2.isEmpty,
      s"inserting $injectElement in $initSetWithConflict2 should return " +
        s"an empty list but returned $conflictingList2Algo2"
    )
  }

  // Test 2.1
  test(
    "Injecting the element of the conflict list in an empty list creates a conflict (No Elements in Init)"
  ) {
    val testData = randomTestData
    val initSet  = randSet(10, except = testData.conflictingSetOne union testData.conflictingSetTwo)

    val injectElement1 =
      randList(10, testData.conflictingSetOne.toList, testData.conflictingSetTwo.toList)
    val injectElement2 =
      randList(10, testData.conflictingSetTwo.toList, testData.conflictingSetOne.toList)

    // Algo 1
    val conflictingList1Algo1 =
      ConflictSearch.quickXPlain(initSet, injectElement1, inject, testData.conflict)
    val conflictingList2Algo1 =
      ConflictSearch.quickXPlain(initSet, injectElement2, inject, testData.conflict)

    assert(
      testData.conflict(initSet union SortedSet.from(conflictingList1Algo1)),
      s"result of injecting $injectElement1 in $initSet ($conflictingList1Algo1) shall create a conflict"
    )
    assert(
      testData.conflict(initSet union SortedSet.from(conflictingList2Algo1)),
      s"result of injecting $injectElement1 in $initSet ($conflictingList2Algo1) shall create a conflict"
    )

    // Algo 2
    val conflictingList1Algo2 =
      ConflictSearch.xPlainWithRemove(initSet, injectElement1, inject, remove, testData.conflict)
    val conflictingList2Algo2 =
      ConflictSearch.xPlainWithRemove(initSet, injectElement2, inject, remove, testData.conflict)

    assert(
      testData.conflict(initSet union SortedSet.from(conflictingList1Algo2)),
      s"result of injecting $injectElement1 in $initSet ($conflictingList1Algo2) shall create a conflict"
    )
    assert(
      testData.conflict(initSet union SortedSet.from(conflictingList2Algo2)),
      s"result of injecting $injectElement1 in $initSet ($conflictingList2Algo2) shall create a conflict"
    )

  }

  // Test 2.2
  test(
    "Injecting the element of the conflict list in an empty list creates a conflict (Elements in Init)"
  ) {
    val testData = randomTestData
    val (half1Conflict1, half2Conflict1) =
      testData.conflictingSetOne.splitAt(testData.conflictingSetOne.size / 2)

    val (half1Conflict2, half2Conflict2) =
      testData.conflictingSetTwo.splitAt(testData.conflictingSetTwo.size / 2)

    val initSet1 = randSet(10, half1Conflict1, half2Conflict1 union testData.conflictingSetTwo)
    val initSet2 = randSet(10, half1Conflict2, half2Conflict2 union testData.conflictingSetOne)

    val injectElement1 =
      randList(10, half2Conflict1.toList, testData.conflictingSetTwo.toList)
    val injectElement2 =
      randList(10, half2Conflict2.toList, testData.conflictingSetOne.toList)

    // Algo 1
    val conflictingList1Algo1 =
      ConflictSearch.quickXPlain(initSet1, injectElement1, inject, testData.conflict)
    val conflictingList2Algo1 =
      ConflictSearch.quickXPlain(initSet2, injectElement2, inject, testData.conflict)

    assert(
      testData.conflict(initSet1 union SortedSet.from(conflictingList1Algo1)),
      s"result of injecting $injectElement1 in $initSet1 ($conflictingList1Algo1) shall create a conflict. " +
        s"Conflicting Sets are $testData"
    )
    assert(
      testData.conflict(initSet2 union SortedSet.from(conflictingList2Algo1)),
      s"result of injecting $injectElement1 in $initSet2 ($conflictingList2Algo1) shall create a conflict. " +
        s"Conflicting Sets are $testData"
    )

    // Algo 2
    val conflictingList1Algo2 =
      ConflictSearch.xPlainWithRemove(initSet1, injectElement1, inject, remove, testData.conflict)
    val conflictingList2Algo2 =
      ConflictSearch.xPlainWithRemove(initSet2, injectElement2, inject, remove, testData.conflict)

    assert(
      testData.conflict(initSet1 union SortedSet.from(conflictingList1Algo2)),
      s"result of injecting $injectElement1 in $initSet1 ($conflictingList1Algo2) shall create a conflict. " +
        s"Conflicting Sets are $testData"
    )
    assert(
      testData.conflict(initSet2 union SortedSet.from(conflictingList2Algo2)),
      s"result of injecting $injectElement1 in $initSet2 ($conflictingList2Algo2) shall create a conflict. " +
        s"Conflicting Sets are $testData"
    )
  }

  // test 3
  test("The resulting conflicting set are minimal") {
    val testData = randomTestData
    val initSet  = randSet(10, except = testData.conflictingSetOne union testData.conflictingSetTwo)

    val injectElement1 =
      randList(10, testData.conflictingSetOne.toList, testData.conflictingSetTwo.toList)
    val injectElement2 =
      randList(10, testData.conflictingSetTwo.toList, testData.conflictingSetOne.toList)

    def removeElem(e: Int, l: List[Int]): List[Int] = {
      l match {
        case Nil    => Nil
        case h :: t => if (h == e) removeElem(e, t) else h :: removeElem(e, t)
      }
    }

    def checkList(l: List[Int]): Unit = {
      for (e <- l) {
        assert(
          !testData.conflict(initSet union SortedSet.from(removeElem(e, l))),
          s"$l is not a minimal conflict set. Removing $e does not remove the conflict"
        )
        // assert(false,s"$l -- $testData")
      }
    }
    // Algo 1
    val conflictingList1Algo1 =
      ConflictSearch.quickXPlain(initSet, injectElement1, inject, testData.conflict)
    val conflictingList2Algo1 =
      ConflictSearch.quickXPlain(initSet, injectElement2, inject, testData.conflict)

    // Checking that the result is a conflict set
    assert(
      testData.conflict(initSet union SortedSet.from(conflictingList1Algo1)),
      s"result of injecting $injectElement1 in $initSet ($conflictingList1Algo1) shall create a conflict"
    )
    checkList(conflictingList1Algo1)
    assert(
      testData.conflict(initSet union SortedSet.from(conflictingList2Algo1)),
      s"result of injecting $injectElement1 in $initSet ($conflictingList1Algo1) shall create a conflict"
    )
    checkList(conflictingList2Algo1)

    // Algo 2
    val conflictingList1Algo2 =
      ConflictSearch.xPlainWithRemove(initSet, injectElement1, inject, remove, testData.conflict)
    val conflictingList2Algo2 =
      ConflictSearch.xPlainWithRemove(initSet, injectElement2, inject, remove, testData.conflict)

    checkList(conflictingList1Algo2)
    checkList(conflictingList2Algo2)
  }

  // test 4
  test(
    "Check that if the list of elements to add do not create conflicts, the algorithms returns an exception"
  ) {
    val testData = randomTestData
    val initSet  = randSet(10, except = testData.conflictingSetOne union testData.conflictingSetTwo)

    val insertList =
      randList(15, except = (testData.conflictingSetOne union testData.conflictingSetTwo).toList)

    // Algo 1
    assertThrows[Exception] {
      ConflictSearch.quickXPlain(initSet, insertList, inject, testData.conflict)
    }

    // Algo 2
    assertThrows[Exception] {
      ConflictSearch.xPlainWithRemove(initSet, insertList, inject, remove, testData.conflict)
    }
  }
}
