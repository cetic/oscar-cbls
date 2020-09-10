package oscar.cbls.test

import oscar.cbls.algo.quick.QList

object DataStructSpeed extends App{

  val max = 100000

  def testArray(): Unit = {
    var i: Int = max
    val startTime = System.nanoTime()

    val storageArray = Array.fill(max + 1)(0)
    while (i > 0) {
      storageArray(i) = i
      i -= 1
    }
    var sum1: Long = 0
    i = max
    while (i > 0) {
      sum1 = sum1 + storageArray(i)
      i -= 1
    }

    val t1 = System.nanoTime()
    println(s"Array: ${t1 - startTime}")
  }

  def testList() {
    val t1 = System.nanoTime()
    var i: Int = max

    var storageList: List[Int] = Nil
    while (i > 0) {
      storageList = i :: storageList
      i -= 1
    }
    var sum2: Long = 0
    while (storageList.nonEmpty) {
      sum2 = sum2 + storageList.head
      storageList = storageList.tail
    }

    val t2 = System.nanoTime()
    println(s"List: ${t2 - t1}")
  }

  def testQList(): Unit = {
    val t2 = System.nanoTime()
    var i: Int = max
    var storageQList: QList[Int] = null

    while (i > 0) {
      storageQList = QList(i, storageQList)
      i -= 1
    }
    var sum3: Long = 0
    while (storageQList != null) {
      sum3 = sum3 + storageQList.head
      storageQList = storageQList.tail
    }

    val t3 = System.nanoTime()
    println(s"QList: ${t3 - t2}")
  }

  def test() {
    testArray()
    testList()
    testQList()
  }

  for(x <- 0 until 100) {
    println()
    println(x)
    test()
  }
}
