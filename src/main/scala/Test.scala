import scala.collection.{immutable, mutable}
import scala.util.Random

object Test extends App{

  var set: immutable.SortedSet[Int] = immutable.SortedSet.empty[Int]
  var list: List[Int] = Nil
  val size = 1000000

  var start = System.currentTimeMillis()

  for(x <- 0 until 10) {
    start = System.currentTimeMillis()
    set = immutable.SortedSet.empty[Int]
    val randSet = Random
    randSet.setSeed(0)
    for (i <- 0 until size) {
      set = set + randSet.nextInt(size)
    }
    println(System.currentTimeMillis() - start)
  }

  for (x <- 0 until 10) {
    start = System.currentTimeMillis()
    list = list.empty
    val randList = Random
    randList.setSeed(0)
    for (i <- 0 until size) {
      list = List(randList.nextInt()) ::: list
    }
    val s = immutable.SortedSet(list:_*)
    println(System.currentTimeMillis() - start)
  }

  for (x <- 0 until 10) {
    start = System.currentTimeMillis()
    list = List.empty
    val randList = Random
    randList.setSeed(0)
    for (i <- 0 until size) {
      list = List(i) ::: list
    }
    println(System.currentTimeMillis() - start)
  }
}
