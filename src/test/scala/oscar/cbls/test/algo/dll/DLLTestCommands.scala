package oscar.cbls.test.algo.dll

import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Arbitrary.arbitrary

import oscar.cbls.algo.dll.DoublyLinkedList

import scala.util.{Failure, Success, Try}

/** Defines the System Under Test (SUT)
  *
  * In the DLL case, the SUT contains a DLL and a classical list that acts as witness. Both list are
  * modified the same way and methods allow to compare the lists to ensure that the two lists are
  * identical after the movements
  *
  * @param dll
  *   The dll to test
  * @param l
  *   The witness list to compare with
  */

case class DllTestStruct(dll: DoublyLinkedList[Int], var l: List[Int]) {

  var containerList: List[dll.DLLStorageElement] = List()

  def insertStart(elem: Int) = {
    val container = dll.insertStart(elem)
    containerList = container :: containerList
    l = elem :: l
  }

  def insertEnd(elem: Int) = {
    val container = dll.insertEnd(elem)
    containerList = containerList.appended(container)
    l = l.appended(elem)
  }

  def insertAfter(elem: Int, afterPos: Int) = {
    require(afterPos >= 0, s"InsertAfter($afterPos) impossible in $l")
    require(afterPos < l.size, s"InsertAfter($afterPos) impossible in $l")
    val (l1, l2) = l.splitAt(afterPos + 1)
    l = l1 ::: (elem :: l2)
    val (c1, c2)  = containerList.splitAt(afterPos + 1)
    val container = dll.insertAfter(elem, containerList(afterPos))
    containerList = c1 ::: (container :: c2)
  }

  def removeStart = {
    dll.popStart()
    l = l.tail
    containerList = containerList.tail
  }

  def removeEnd = {
    dll.popEnd()
    l = l.reverse.tail.reverse
    containerList = containerList.reverse.tail.reverse
  }

  def removePos(pos: Int) = {
    require(pos >= 0, s"RemovePos($pos) impossible in $l")
    require(pos < l.size, s"RemovePos($pos) impossible in $l")
    val cont = containerList(pos)
    cont.delete()
    val (c1, c2) = containerList.splitAt(pos)
    val (l1, l2) = l.splitAt(pos)
    containerList = c1 ::: c2.tail
    l = l1 ::: l2.tail
  }

  def dropAll() = {
    dll.dropAll()
    l = Nil
    containerList = Nil
  }

  /** compares the size of the dll and the size of its witness list */
  private def compareSize(dll: DoublyLinkedList[Int], l: List[Int]): Boolean = {
    dll.size == l.size
  }

  /** compares the dll and its witness list */
  private def compareLists(dll: DoublyLinkedList[Int], l: List[Int]): Boolean = {
    val dllIt  = dll.iterator
    val listIt = l.iterator
    var res    = true
    while (dllIt.hasNext) {
      if (listIt.hasNext) {
        val dllElem  = dllIt.next()
        val listElem = listIt.next()
        res = dllElem == listElem && res
      } else {
        return false
      }
    }
    res && listIt.isEmpty
  }

  def compare =
    compareSize(dll, l) && compareLists(dll, l)

}

/** On object that defines the commands to use on the lists
  */

object DLLTestCommands extends Commands {

  type State = Int

  // The type of the System Under Test (SUT)
  type Sut = DllTestStruct

  val rand = new scala.util.Random(1000)

  // Defining the generators
  private val genOpEmpty: Gen[Command] = for {
    v <- Gen.choose(1, 100)
    c <- Gen.oneOf(List(AddStart(v), AddEnd(v), DropAllOperation))
  } yield c

  private val genRemove: Gen[Command] = Gen.oneOf(List(RemoveStart))

  // Defines the precondition to define a new SUT
  override def canCreateNewSut(
    newState: State,
    initSuts: Iterable[State],
    runningSuts: Iterable[Sut]
  ): Boolean = true

  // How to destruy a SUT
  override def destroySut(sut: Sut): Unit = ()

  // How to generate a command (according to the state)
  override def genCommand(state: State): Gen[Command] = {
    val genAddStart: Gen[Command] = for {
      v <- arbitrary[Int]
    } yield AddStart(v)
    val genAddEnd: Gen[Command] = for {
      v <- arbitrary[Int]
    } yield AddEnd(v)
    val genRemoveAll = Gen.const(DropAllOperation)
    if (state == 0) {
      Gen.oneOf(genAddStart, genAddEnd, genRemoveAll)
    } else {
      val genAddPos: Gen[Command] = for {
        v   <- arbitrary[Int]
        pos <- Gen.choose(0, state - 1)
      } yield AddAfter(v, pos)
      val genRemoveStart = Gen.const(RemoveStart)
      val genRemoveEnd   = Gen.const(RemoveEnd)
      val genRemovePos = for {
        pos <- Gen.choose(0, state - 1)
      } yield RemovePos(pos)
      Gen.oneOf(
        genAddStart,
        genAddEnd,
        genRemoveAll,
        genAddPos,
        genRemoveStart,
        genRemoveEnd,
        genRemovePos
      )
    }
  }

  //  How to generate an initial state
  // (in this case, the state is the length of the list, so it is 0)
  override def genInitialState: Gen[State] =
    Gen.const(0)

  // An initial precondition
  override def initialPreCondition(state: State): Boolean = true

  // How to create an initial system under test
  override def newSut(state: State): Sut = DllTestStruct(new DoublyLinkedList[Int](), List())

  /** The abstract class of the command.
    *
    * Command gives an API that allows to describe how evolves the system under test and how evolves
    * the the state. Moreover, it allows to describe what are the pre- and post-conditions
    */

  abstract class DllOperation extends Command {

    type Result = DllTestStruct

    override def preCondition(state: State): Boolean = true

    var testedState: List[Int] = List()

    override def postCondition(state: State, result: Try[Result]): Prop = {
      result match {
        case Failure(_) => false
        case Success(res) =>
          res.compare
      }

    }

  }

  abstract class AddOperation extends DllOperation {
    override def nextState(state: State): State = state + 1
  }

  case class AddStart(value: Int) extends AddOperation {

    override def run(sut: Sut): Result = {
      sut.insertStart(value)
      sut
    }

  }

  case class AddEnd(value: Int) extends AddOperation {
    override def run(sut: Sut): Result = {
      sut.insertEnd(value)
      sut
    }

  }

  case class AddAfter(value: Int, pos: Int) extends AddOperation {
    override def run(sut: Sut): Result = {
      sut.insertAfter(value, pos)
      sut
    }
  }

  case object DropAllOperation extends DllOperation {

    override def nextState(state: State): State = 0

    override def run(sut: Sut): Result = {
      sut.dropAll()
      sut
    }

  }

  abstract class RemoveOperation extends DllOperation {
    override def nextState(state: State): State = state - 1
  }

  case object RemoveEnd extends RemoveOperation {
    override def run(sut: Sut): Result = {
      sut.removeEnd
      sut
    }
  }

  case object RemoveStart extends RemoveOperation {

    override def run(sut: Sut): Result = {
      sut.removeStart
      sut
    }

  }

  case class RemovePos(p: Int) extends RemoveOperation {
    override def run(sut: Sut): Result = {
      sut.removePos(p)
      sut
    }
  }

}
