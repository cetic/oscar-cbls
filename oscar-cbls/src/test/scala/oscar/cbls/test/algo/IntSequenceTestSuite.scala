package oscar.cbls.test.algo

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}
import oscar.cbls.algo.seq._
import SequenceTestUtils._

import scala.util.Random


class IntSequenceTestSuite extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

  test("ConcreteIntSequence : batch queries keep expected list"){
    forAll(testBenchGen){testBench => {
      whenever(testBench._1.size > 5){

        val actionsList = testBench._2
        val referenceList = testBench._1
        var seq = IntSequence(referenceList)
        var modifiedList = referenceList

        for (action <- actionsList) {
          action match {
            case MoveAfter() =>
              val (indexFrom, indexTo, destination) = getRandomParametersForMoveAfter(modifiedList)
              seq = seq.moveAfter(indexFrom, indexTo, destination, true, fast = false)
              modifiedList = flipListManually(modifiedList, indexFrom, indexTo, destination)

            case Insert() =>
              val (value, pos) = getRandomParametersForInsert(modifiedList)
              seq = seq.insertAtPosition(value,pos,fast = false)

              val (front, back) = modifiedList.splitAt(pos)
              modifiedList = front ++ List(value) ++ back

            case Delete() =>
              if(referenceList.nonEmpty){
                val index= Random.nextInt(seq.size)
                seq = seq.delete(index, fast = false)
                modifiedList = modifiedList.take(index) ++ modifiedList.drop(index+1)
              }

            case Flip() =>
              seq = seq.flip(fast = false)
              modifiedList = modifiedList.reverse

            case Regularize() =>
              seq = seq.regularize()

            case Commit() =>
              seq = seq.commitPendingMoves

            case default => {}
          }
        }
        compareAllAttributes(seq, modifiedList)
      }
    }}
  }

  test("MovedIntSequence : batch queries keep expected list"){
    forAll(testBenchGen){testBench => {
      whenever(testBench._1.size > 5){
        val actionsList = testBench._2
        val referenceList = testBench._1
        val (indexFrom, indexTo, destination) = getRandomParametersForMoveAfter(referenceList)
        var seq :IntSequence = new MovedIntSequence(IntSequence(referenceList),indexFrom,indexTo,destination,true)
        var modifiedList = flipListManually(referenceList,indexFrom,indexTo,destination)

        for (action <- actionsList) {
          action match {
            case MoveAfter() =>
              val (indexFrom, indexTo, destination) = getRandomParametersForMoveAfter(modifiedList)
              seq = seq.moveAfter(indexFrom, indexTo, destination, true, fast = false)
              modifiedList = flipListManually(modifiedList, indexFrom, indexTo, destination)

            case Flip() =>
              seq = seq.flip(fast = false)
              modifiedList = modifiedList.reverse

            case default => {}
          }
        }
        compareAllAttributes(seq, modifiedList)
      }
    }}
  }

  test("RemovedIntSequence : batch queries keep expected list"){
    forAll(testBenchGen){testBench => {
      whenever(testBench._1.size > 5){
        val actionsList = testBench._2
        val referenceList = testBench._1

        val i = Random.nextInt(referenceList.size)
        var seq :IntSequence = new RemovedIntSequence(IntSequence(referenceList),i)
        var modifiedList = referenceList.take(i) ++ referenceList.drop(i+1)

        for (action <- actionsList) {
          if(modifiedList.size > 1){
            val index= Random.nextInt(seq.size)
            seq = seq.delete(index, fast = false)
            modifiedList = modifiedList.take(index) ++ modifiedList.drop(index+1)
          }
        }
        compareAllAttributes(seq, modifiedList)
      }
    }}
  }

  test("InsertedIntSequence : batch queries keep expected list"){
    forAll(testBenchGen){testBench => {
      whenever(testBench._1.size > 5){
        val actionsList = testBench._2
        val referenceList = testBench._1

        val (value, pos) = getRandomParametersForInsert(referenceList)
        var seq :IntSequence = new InsertedIntSequence(IntSequence(referenceList),value,pos)
        val (front, back) = referenceList.splitAt(pos)
        var modifiedList = front ++ List(value) ++ back

        for (action <- actionsList) {
          val (value, pos) = getRandomParametersForInsert(modifiedList)
          seq = seq.insertAtPosition(value,pos,fast = false)

          val (front, back) = modifiedList.splitAt(pos)
          modifiedList = front ++ List(value) ++ back
        }
        compareAllAttributes(seq, modifiedList)
      }
    }}
  }

  test("Mixed IntSequence types : batch queries keep expected list"){
    forAll(testBenchGen){testBench => {
      whenever(testBench._1.size > 5){

        val actionsList = testBench._2
        val referenceList = testBench._1
        var seq = IntSequence(referenceList)
        var modifiedList = referenceList

        for (action <- actionsList) {
          action match {
            case MoveAfter() =>
              val (indexFrom, indexTo, destination) = getRandomParametersForMoveAfter(modifiedList)
              seq = seq.moveAfter(indexFrom, indexTo, destination, true, fast = true)
              modifiedList = flipListManually(modifiedList, indexFrom, indexTo, destination)

            case Insert() =>
              val (value, pos) = getRandomParametersForInsert(modifiedList)
              seq = seq.insertAtPosition(value,pos,fast = true)

              val (front, back) = modifiedList.splitAt(pos)
              modifiedList = front ++ List(value) ++ back

            case Delete() =>
              if(referenceList.nonEmpty){
                val index= Random.nextInt(seq.size)
                seq = seq.delete(index, fast = true)
                modifiedList = modifiedList.take(index) ++ modifiedList.drop(index+1)
              }

            case Flip() =>
              seq = seq.flip(fast = true)
              modifiedList = modifiedList.reverse

            case Regularize() =>
              seq.regularizeToMaxPivot(4)

            case Commit() =>
              seq = seq.commitPendingMoves

            case default =>
          }
        }
        compareAllAttributes(seq, modifiedList)
      }
    }}
  }


  val elem: Gen[Long] = for (n <- Gen.choose(0, 100)) yield n * 4L // Sparse elements
  val gen: Gen[Operation] = Gen.oneOf(List(MoveAfter(),Insert(),Delete(),Flip(),Regularize(),Commit()))

  val testBenchGen: Gen[(List[Long], List[Operation])] = for{
    numElems <- Gen.choose(20, 200)
    numActions <- Gen.choose(20, 100)
    elems <- Gen.listOfN(numElems, elem)
    actions <- Gen.listOfN(numActions, gen)
  } yield(elems,actions)

  abstract sealed class Operation()
  case class MoveAfter() extends Operation
  case class Insert() extends Operation
  case class Delete() extends Operation
  case class Flip() extends Operation
  case class Regularize() extends Operation
  case class Commit() extends Operation
}

object SequenceTestUtils{

  private val myTestUtils = new SequenceTestUtils()

  /**
    * Implements manually the moveAfter transformation (to compare with IntSequence)
    * @param list The original list to swap
    * @param indexFrom The starting index of the subsequence to flip (inclusive)
    * @param indexTo The ending index of the subsequence to flip (inclusive)
    * @param destination The index where the subsequence must be re-inserted (equivalent to moveAfter parameter)
    * @return A new sequence with the proper transformation
    */
  def flipListManually(list :List[Long], indexFrom :Int, indexTo :Int, destination :Int) : List[Long] = {

    var flippedList = List[Long]()
    val flip = list.slice(indexFrom,indexTo+1).reverse
    val start =  list.take(indexFrom)
    val end = if(indexTo < list.size - 1) list.takeRight(list.size - indexTo - 1) else List()

    if(destination == -1){
      flippedList = flip ::: start ::: end
    }
    else {
      if(destination < indexFrom){
        // Insert the flip at the left
        val part1 = start.take(destination + 1)
        val part2 = start.takeRight(indexFrom - destination - 1)
        flippedList = part1 ::: flip ::: part2 ::: end
      }
      else{
        // Insert the flip at the right
        val part1 = end.take(destination - indexTo)
        val part2 = end.takeRight(list.size - destination - 1)
        flippedList = start ::: part1 ::: flip ::: part2
      }
    }
    flippedList
  }

  def getRandomParametersForMoveAfter(list :List[Long]): (Int, Int, Int) = {
    val indexFrom = Random.nextInt(list.size - 1)
    val indexTo = indexFrom + Random.nextInt(list.size - indexFrom)
    var destination = -1
    if(Random.nextBoolean()){
      // Insert before indexFrom
      destination = if(indexFrom == 0) -1 else Random.nextInt(indexFrom)
    }
    else {
      // Insert after indexTo (if indexTo is not the very last index)
      destination =
        if(indexTo < list.size - 1)
          indexTo + Random.nextInt(list.size - indexTo - 1) + 1
        else
          -1
    }

    (indexFrom,indexTo,destination)
  }

  def getRandomParametersForInsert(list :List[Long]) : (Long,Int) = {
    val value = Random.nextInt(1000)
    val position = Random.nextInt(list.size)
    (value,position)
  }

  def compareAllAttributes(intSeq :IntSequence, list :List[Long]): Unit = myTestUtils.compare(intSeq,list)
}

class SequenceTestUtils extends FunSuite with Matchers {

  /**
    * Exhaustively compares the IntSequence with a reference list, supposed to be identical
    * @param intSeq
    * @param list
    */
  def compare(intSeq :IntSequence, list: List[Long]): Unit ={
    intSeq.size             should be (list.size)
    intSeq.isEmpty          should be (list.isEmpty)
    intSeq.nonEmpty         should be (list.nonEmpty)
    intSeq.iterator.toList  should be (list.iterator.toList)
    intSeq match {
      case sequence: ConcreteIntSequence =>
        sequence.largestValue.get should be(list.max)
        sequence.smallestValue.get should be(list.min)
      case _ =>
    }

    intSeq.unorderedContentNoDuplicate.sorted                 should be (list.sorted.distinct)
    intSeq.unorderedContentNoDuplicateWithNBOccurences.sorted should be (list.sorted.distinct.map(e => (e,list.count(_ == e))))

    for(i <- list.min until list.max){ // This will intentionally search for items that are not in the list
      intSeq.nbOccurrence(i)              should be (list.count(_ == i))
      intSeq.contains(i)                  should be (list.contains(i))
      if(intSeq.contains(i)){
        list(intSeq.positionOfAnyOccurrence(i).get) should be (i)
        intSeq.positionOfFirstOccurrence(i).get     should be (list.indexOf(i))
        intSeq.positionOfLastOccurrence(i).get      should be (list.lastIndexOf(i))
      }
      else{
        intSeq.positionOfAnyOccurrence(i)   should be (None)
        intSeq.positionOfFirstOccurrence(i) should be (None)
        intSeq.positionOfLastOccurrence(i)  should be (None)
      }
    }

    for(i <- list.indices){
      val positionsOfValue = list.zipWithIndex.filter(_._1 == list(i)).map(_._2).sorted
      intSeq.positionsOfValue(list(i)).toList.sorted      should be (positionsOfValue)
      intSeq.positionsOfValueQ(list(i)).toList.sorted     should be (positionsOfValue)
      intSeq.positionsOfValueSet(list(i)).toList.sorted   should be (positionsOfValue)
      intSeq.valueAtPosition(i).get                       should be (list(i))

      // Didn't find a matcher for that ...
      list containsSlice (intSeq.iterateFromAnyOccurrenceOfValue(list(i)).toList) should be (true)
    }

    // It is way too expensive to test all index values exhaustively
    // Instead, test with indexFrom fixed at 0 and exhaustive indexTo, then backwards

    for(j <- 1 until list.size - 1){
      val slice = list.zipWithIndex.slice(0,j+1).map{case (e,i) => (i,e)}.sorted
      intSeq.valuesBetweenPositionsQList(0,j).toList.sorted           should be (slice.map(_._2).sorted)
      intSeq.valuesBetweenPositionsSet(0,j).toList                    should be (slice.map(_._2).sorted.distinct)
      intSeq.positionsBetweenFromToAndTheirValues(0,j).toList.sorted  should be (slice)
    }

    for(j <- 1 until list.size){
      val slice = list.zipWithIndex.slice(j,list.size).map{case (e,i) => (i,e)}.sorted
      intSeq.valuesBetweenPositionsQList(j,list.size-1).toList.sorted           should be (slice.map(_._2).sorted)
      intSeq.valuesBetweenPositionsSet(j,list.size-1).toList                    should be (slice.map(_._2).sorted.distinct)
      intSeq.positionsBetweenFromToAndTheirValues(j, list.size-1).toList.sorted should be (slice)
    }
  }
}