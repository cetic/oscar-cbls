package oscar.cbls.test.algo

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}
import oscar.cbls.algo.dll.{DPFDLLStorageElement, DelayedPermaFilteredDoublyLinkedList}

import scala.util.Random

class PFDLLTestSuite extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

  case class CustomObject(int: Int, str: String)
  val customGen: Gen[CustomObject] = for {
    i <- Gen.choose(1, 100)
    s <- Gen.alphaStr
  } yield CustomObject(i, s)

  test("Filter prevents adding uncompliant elements"){
    forAll (Gen.listOf(customGen)) {objList => {
      val dll = new DelayedPermaFilteredDoublyLinkedList[CustomObject]()

      // Allow only even numbers
      val filteredList = dll.permaFilter(obj => obj.int % 2 == 0)

      // Add all the elements from the generated list
      objList.foreach(obj => dll.addElem(obj))

      // Check if original even numbers count equals dll filter
      objList.count(e => e.int % 2 == 0) should be (filteredList.size)
    }}
  }

  test("isEmpty returns true after deleting all elements"){
    forAll (Gen.listOf(customGen)) {objList => {
      val dll = new DelayedPermaFilteredDoublyLinkedList[CustomObject]()
      val filteredList = dll.permaFilter(obj => obj.int % 2 == 0)
      var storageElemList = List[DPFDLLStorageElement[CustomObject]]()

      objList.foreach(obj => {
        storageElemList = storageElemList ::: List(dll.addElem(obj))
      })

      storageElemList.foreach(elem => elem.delete())

      dll.isEmpty should be (true)
    }}
  }

  test("DelayedFilters keeps unfiltered until injector is called"){
    forAll (Gen.listOf(customGen)) {objList => {
      val dll = new DelayedPermaFilteredDoublyLinkedList[CustomObject]()

      var callbackList = List[() => Unit]()

      val filteredList = dll.delayedPermaFilter((obj,insertFunc,queryFunc) => {
        if(obj.int % 2 == 0)
          callbackList = callbackList ::: List(insertFunc)
      })

      objList.foreach(obj => dll.addElem(obj))

      // First assertion: filteredList should be empty
      filteredList.size should be (0)

      //Call all the injectors
      callbackList.foreach(c => c())

      // Seconds assertion: filteredList should be the size of all even numbers of list
      objList.count(e => e.int % 2 == 0) should be (filteredList.size)
    }}
  }

  test("StillValid reports false when injector has not been called yet"){
    forAll (Gen.listOf(customGen)) {objList => {

      val dll = new DelayedPermaFilteredDoublyLinkedList[CustomObject]()
      var callbackList = List[() => Unit]()
      var validList = List[() => Boolean]()

      val filteredList = dll.delayedPermaFilter((obj,insertFunc,queryFunc) => {
        if(obj.int % 2 == 0)
          callbackList = callbackList ::: List(insertFunc)
        validList = validList::: List(queryFunc)
      })

      validList.forall(x => !x()) should be (true)
    }}
  }

  test("StillValid reports true after injector has been called"){
    forAll (Gen.listOf(customGen)) {objList => {

      val dll = new DelayedPermaFilteredDoublyLinkedList[CustomObject]()
      var callbackList = List[() => Unit]()
      var validList = List[() => Boolean]()

      val filteredList = dll.delayedPermaFilter((obj,insertFunc,queryFunc) => {
        if(obj.int % 2 == 0)
          callbackList = callbackList ::: List(insertFunc)
        validList = validList::: List(queryFunc)
      })

      callbackList.foreach(c => c())
      validList.forall(x => x()) should be (true)
    }}
  }


  val action: Gen[Int] = for (n <- Gen.choose(0, 1)) yield n
  val actionList: Gen[List[Int]] =  for {
    numElems <- Gen.choose(20, 1000)
    elems <- Gen.listOfN(numElems, action)
  } yield elems


  test("addElem and delete keep the correct size (batch)") {
    forAll(actionList) { list => {

      val dll = new DelayedPermaFilteredDoublyLinkedList[CustomObject]()
      var size = 0
      var listPFDElem = List[DPFDLLStorageElement[CustomObject]]()

      for (i <- list) {
        if (i == 0) {
          listPFDElem = listPFDElem ::: List(dll.addElem(new CustomObject(i, "a")))
          size += 1
        }
        else {
          if (dll.size > 0) {
            val toDelete = listPFDElem(Random.nextInt(listPFDElem.size))
            toDelete.delete()
            listPFDElem = listPFDElem.filter(p => !p.equals(toDelete))
            size -= 1
          }
        }
      }

      size should be(dll.size)
    }}
  }

  test("foreach returns the expected list"){
    forAll (Gen.listOf(customGen)) {objList => {

      val dll = new DelayedPermaFilteredDoublyLinkedList[CustomObject]()
      var size = 0
      var listPFDElem = List[DPFDLLStorageElement[CustomObject]]()


      // Append the element at the head, since dll.addElem adds at the head
      for (i <- objList) {
        listPFDElem = List(dll.addElem(i)) ::: listPFDElem
      }

      var i = 0
      dll.foreach(obj =>{

        listPFDElem(i).elem should equal (obj)
        obj should equal(objList(objList.size - i - 1)) // Iterate on the list in reverse (because addElem)
        i += 1
      })
    }}
  }

  test("iterator returns the expected list"){
    forAll (Gen.listOf(customGen)) {objList => {

      val dll = new DelayedPermaFilteredDoublyLinkedList[CustomObject]()
      var size = 0
      var listPFDElem = List[DPFDLLStorageElement[CustomObject]]()

      for (i <- objList) {
        listPFDElem = List(dll.addElem(i)) ::: listPFDElem
      }

      dll.iterator.toList should be (listPFDElem.map(o => o.elem))
    }}
  }

  test("MapToList returns the expected list"){
    forAll (Gen.listOf(customGen)) {objList => {

      val dll = new DelayedPermaFilteredDoublyLinkedList[CustomObject]()
      var size = 0
      var listPFDElem = List[DPFDLLStorageElement[CustomObject]]()

      for (i <- objList) {
        listPFDElem =  listPFDElem ::: List(dll.addElem(i))
      }

      var i = 0
      dll.mapToList(e => e).foreach(e => {
        listPFDElem(i).elem should equal(e)
        i += 1
      })
    }}
  }
}