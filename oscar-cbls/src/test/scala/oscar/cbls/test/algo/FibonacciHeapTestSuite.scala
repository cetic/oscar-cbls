package oscar.cbls.test.algo

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}
import oscar.cbls.algo.heap.FibonacciHeap

class FibonacciHeapTestSuite extends FunSuite with Matchers with GeneratorDrivenPropertyChecks{

  val gen: Gen[Operation] = Gen.frequency(
    (10,Insert()),
    (4,RemoveMin()),
    (2,Delete()),
    (2, DecreaseKey()),
    (1, Clear())
  )
  val genOperations: Gen[List[Operation]] = for{
    size <- Gen.chooseNum(1,100)
    list <- Gen.listOfN(size,gen)
  } yield list

  test("Batch operations keep expected heap"){
    forAll(genOperations,minSuccessful(150)){ operations :List[Operation] =>
      whenever(operations.nonEmpty){

        val keys = (1L to operations.length).toList
        val heap = new FibonacciHeap[Long]()
        var listNodes = List[FibonacciHeap.Node[Long]]()
        var currentKey = 1

        for(operation <- operations){
          operation match{
            case Insert() =>
              val newnode = heap.insert(currentKey,currentKey)
              currentKey += 1

              listNodes = newnode :: listNodes

            case Delete() =>
              if (!heap.isEmpty) {
                val randomNode = Gen.oneOf(listNodes).sample.get
                heap.delete(randomNode)

                listNodes = listNodes.filter(_ != randomNode)
              }

            case DecreaseKey() =>
              if(heap.size > 1){
                val randomNode = Gen.oneOf(listNodes).sample.get
                if(randomNode.key > 0)
                  heap.decreaseKey(randomNode,randomNode.key - 1)

                // No need to update our list, it's done by reference
              }

            case Clear() =>
              heap.dropAll()
              listNodes = List[FibonacciHeap.Node[Long]]()

            case RemoveMin() =>
              val min = heap.popMin()

              if(min.isDefined){
                listNodes = listNodes.filter(_.value != min.get)
              }
          }
        }

        var list :List[Long] = List()
        while(!heap.isEmpty){
          list = heap.popMin().get :: list
        }

        list.sorted should be(listNodes.map(n => n.value).sorted)
      }
    }
  }

  // Generates two lists of at least 50 elements
  val genBigList :Gen[(List[Long],List[Long])] = for{
    size1 <- Gen.chooseNum(50,100)
    size2 <- Gen.chooseNum(50,100)
    list1 <- Gen.listOfN(size1,Gen.chooseNum(0L,1000))
    list2 <- Gen.listOfN(size2,Gen.chooseNum(0L,1000))
  } yield (list1,list2)

  test("Union yields the expected fibonacci heap"){
    forAll(genBigList,minSuccessful(150)){ bothLists =>
      whenever(bothLists._1.size > 2 && bothLists._2.size > 2){
        var list1 = bothLists._1
        var list2 = bothLists._2
        val heap1 = new FibonacciHeap[Long]()
        val heap2 = new FibonacciHeap[Long]()

        list1.foreach(e => {
          heap1.insert(e,e)
        })

        list2.foreach(e => {
          heap2.insert(e,e)
        })

        heap1.popMin() // Will consolidate the heaps
        heap2.popMin()
        list1 = removeMin(list1)
        list2 = removeMin(list2)

        val unionHeap = FibonacciHeap.union(heap1,heap2)
        val min = unionHeap.popMin() // Consolidate

        var list :List[Long] = List(min.get)
        while(!unionHeap.isEmpty){
          list = unionHeap.popMin().get :: list
        }

        list.sorted should be((list2 ::: list1).sorted)
      }
    }
  }

  def removeMin(list :List[Long]) :List[Long] = {
    val indexMin = list.indexOf(list.min)
    list.take(indexMin) ::: list.takeRight(list.size - indexMin - 1)
  }

  test("popMins returns expected list"){

    val heap = new FibonacciHeap[Long]()
    heap.insert(1,0)
    heap.insert(1,0)
    heap.insert(1,0)
    heap.insert(1,0)
    heap.insert(1,0)

    var list = heap.popMins
    list.size should be (5)
    list.forall(v => v == 1) should be(true)


    /////////////////

    val heap2 = new FibonacciHeap[Long]()
    heap2.insert(0,0)
    heap2.insert(1,1)
    heap2.insert(2,2)
    heap2.insert(3,4)
    heap2.insert(4,5)

    heap2.popMins.size should be (1)

    /////////////////

    val heap3 = new FibonacciHeap[Long]()
    heap3.popMins.size should be (0)
  }

  abstract sealed class Operation()
  case class Insert() extends Operation
  case class Clear() extends Operation
  case class RemoveMin() extends Operation
  case class DecreaseKey() extends Operation
  case class Delete() extends Operation
}
