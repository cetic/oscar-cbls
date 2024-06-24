package oscar.cbls.test.algo.dll

import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop}

import oscar.cbls.algo.dll.DoublyLinkedList

import scala.util.{Failure, Success, Try}

import scala.annotation.tailrec

case class DllTestStruct(dll : DoublyLinkedList[Int],
  var l : List[Int]) {

  def insertStart(elem : Int) = {
    dll.insertStart(elem)
    l = elem :: l
  }

  def insertEnd(elem : Int) = {
    dll.insertEnd(elem)
    l = (elem :: l.reverse).reverse
  }

  def removeStart = {
    dll.popStart()
    l = l.tail
  }

  def dropAll(v : Int) = {
    dll.dropAll(v)
    l = Nil
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

object DLLTestCommands extends Commands {

  type State = Int

  type Sut = DllTestStruct

  val rand = new scala.util.Random(1000)

  private val genElement = Gen.choose(1, 100)

  private val genOpEmpty : Gen[Command] = for {
    v <- Gen.choose(1,100)
    c <- Gen.oneOf(List(AddStart(v),AddEnd(v),DropAllOperation(v)))
  } yield c

  private val genRemove : Gen[Command] = Gen.oneOf(List(RemoveStart))

  override def canCreateNewSut(
    newState: State,
    initSuts: Traversable[State],
    runningSuts: Traversable[Sut]
  ): Boolean = true

  override def destroySut(sut: Sut): Unit = ()

  override def genCommand(state: State): Gen[Command] = if (state == 0)
    genOpEmpty
  else
    Gen.oneOf(genOpEmpty,genRemove)

  override def genInitialState: Gen[State] =
    Gen.const(0)

  override def initialPreCondition(state: State): Boolean = true

  override def newSut(state: State): Sut = DllTestStruct(new DoublyLinkedList[Int](),List())

  abstract class DllOperation extends Command {

    type Result = DllTestStruct

    override def preCondition(state: State): Boolean = true

    var testedState : List[Int] = List()

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

  case class AddStart(value : Int) extends AddOperation {

    override def run(sut: Sut): Result = {
      sut.insertStart(value)
      sut
    }

    override def toString = s"AddStart($value)"

  }

  case class AddEnd(value : Int) extends AddOperation {
    override def run(sut : Sut): Result = {
      sut.insertEnd(value)
      sut
    }
  }

  case class DropAllOperation(v : Int) extends DllOperation {

    override def nextState(state : State) : State = 0

    override def run(sut : Sut) : Result = {
      sut.dropAll(v)
      sut
    }

  }


  abstract class RemoveOperation extends DllOperation {
    override def nextState(state: State): State = state - 1
  }

  case object RemoveStart extends RemoveOperation {

    override def run(sut: Sut): Result = {
      sut.removeStart
      sut
    }

  }

}
