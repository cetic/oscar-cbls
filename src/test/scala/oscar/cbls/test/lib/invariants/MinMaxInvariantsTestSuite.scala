package oscar.cbls.test.lib.invariants

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import oscar.cbls.core.computation.Store
import oscar.cbls.core.computation.integer.IntVariable
import oscar.cbls.lib.invariant.minmax._

import scala.util.Random
class MinMaxInvariantsTestSuite extends AnyFunSuite {
  test("Max2 initialization"){
    val store     = new Store()
    val a : Long  = Random.between(-1000, 1000)
    val b : Long  = Random.between(-1000, 1000)
    val input1    = new IntVariable(store, a)
    val input2    = new IntVariable(store, b)
    val output    = new IntVariable(store, a)
    val inv       = Max2(store, input1, input2, output, "Max2")
    store.close()

    output.value() should be >= a
    output.value() should be >= b
    inv.name() should be("Max2")
  }

  test("Max2 works as attended"){
    val store                       = new Store()
    var randomValues1: List[Long]   = List.fill(8)(Random.between(-1000, 1000))
    var randomValues2: List[Long]   = List.fill(8)(Random.between(-1000, 1000))
    var refInt1                     = randomValues1.head
    var refInt2                     = randomValues2.head
    val input1                      = new IntVariable(store, randomValues1.head)
    val input2                      = new IntVariable(store, randomValues2.head)
    val output                      = new IntVariable(store, randomValues1.head)
    randomValues1  = randomValues1.tail
    randomValues2 = randomValues2.tail
    Max2(store, input1, input2, output)
    store.close()

    input1 :+= randomValues1.head
    input2 :+= randomValues2.head
    refInt1 += randomValues1.head
    refInt2 += randomValues2.head
    randomValues1 = randomValues1.tail
    randomValues2 = randomValues2.tail

    input1 :-= randomValues1.head
    input2 :-= randomValues2.head
    refInt1 -= randomValues1.head
    refInt2 -= randomValues2.head
    randomValues1 = randomValues1.tail
    randomValues2 = randomValues2.tail

    input1 := randomValues1.head
    input2 := randomValues2.head
    refInt1 = randomValues1.head
    refInt2 = randomValues2.head
    randomValues1 = randomValues1.tail
    randomValues2 = randomValues2.tail

    input1 :*= randomValues1.head
    input2 :*= randomValues2.head
    refInt1 *= randomValues1.head
    refInt2 *= randomValues2.head
    randomValues1 = randomValues1.tail
    randomValues2 = randomValues2.tail

    input1 :/= randomValues1.head
    input2 :/= randomValues2.head
    refInt1 /= randomValues1.head
    refInt2 /= randomValues2.head
    randomValues1 = randomValues1.tail
    randomValues2 = randomValues2.tail

    input1 :*= randomValues1.head
    input2 :*= randomValues2.head
    refInt1 *= randomValues1.head
    refInt2 *= randomValues2.head
    randomValues1 = randomValues1.tail
    randomValues2 = randomValues2.tail

    input1 :+= randomValues1.head
    input2 :+= randomValues2.head
    refInt1 += randomValues1.head
    refInt2 += randomValues2.head
    randomValues1 = randomValues1.tail
    randomValues2 = randomValues2.tail

    input1.:--()
    input2.:--()
    refInt1 -= 1
    refInt2 -= 1

    input1.:++()
    input2.:++()
    refInt1 += 1
    refInt2 += 1

    output.value() should be >= refInt1
    output.value() should be >= refInt2
  }

  test("Exchange max"){
    val store = new Store()
    var a : Long = Random.between(-1000, 1000)
    var b : Long = Random.between(-1000, 1000)
    val input1 = new IntVariable(store, a)
    val input2 = new IntVariable(store, b)
    val output = new IntVariable(store, b)
    Max2(store, input1, input2, output)
    store.close()

    // The smaller value become the bigger
    val dist = math.abs(b - a)
    if (a < b) {
      input1 :+= (dist + 1)
      a += (dist + 1)
    } else {
      input2 :+= (dist + 1)
      b += (dist + 1)
    }
    output.value() should be >= a
    output.value() should be >= b
  }

  test("Min2 initialization"){
    val store     = new Store()
    val a : Long  = Random.between(-1000, 1000)
    val b : Long  = Random.between(-1000, 1000)
    val input1    = new IntVariable(store, a)
    val input2    = new IntVariable(store, b)
    val output    = new IntVariable(store, a)
    val inv       = Min2(store, input1, input2, output, "Min2")
    store.close()

    output.value() should be <= a
    output.value() should be <= b
    inv.name() should be("Min2")
  }

  test("Min2 works as attended"){
    val store                       = new Store()
    var randomValues1: List[Long]   = List.fill(8)(Random.between(-1000, 1000))
    var randomValues2: List[Long]   = List.fill(8)(Random.between(-1000, 1000))
    var refInt1                     = randomValues1.head
    var refInt2                     = randomValues2.head
    val input1                      = new IntVariable(store, randomValues1.head)
    val input2                      = new IntVariable(store, randomValues2.head)
    val output                      = new IntVariable(store, randomValues1.head)
    randomValues1  = randomValues1.tail
    randomValues2 = randomValues2.tail
    Min2(store, input1, input2, output)
    store.close()

    input1 :+= randomValues1.head
    input2 :+= randomValues2.head
    refInt1 += randomValues1.head
    refInt2 += randomValues2.head
    randomValues1 = randomValues1.tail
    randomValues2 = randomValues2.tail

    input1 :-= randomValues1.head
    input2 :-= randomValues2.head
    refInt1 -= randomValues1.head
    refInt2 -= randomValues2.head
    randomValues1 = randomValues1.tail
    randomValues2 = randomValues2.tail

    input1 := randomValues1.head
    input2 := randomValues2.head
    refInt1 = randomValues1.head
    refInt2 = randomValues2.head
    randomValues1 = randomValues1.tail
    randomValues2 = randomValues2.tail

    input1 :*= randomValues1.head
    input2 :*= randomValues2.head
    refInt1 *= randomValues1.head
    refInt2 *= randomValues2.head
    randomValues1 = randomValues1.tail
    randomValues2 = randomValues2.tail

    input1 :/= randomValues1.head
    input2 :/= randomValues2.head
    refInt1 /= randomValues1.head
    refInt2 /= randomValues2.head
    randomValues1 = randomValues1.tail
    randomValues2 = randomValues2.tail

    input1 :*= randomValues1.head
    input2 :*= randomValues2.head
    refInt1 *= randomValues1.head
    refInt2 *= randomValues2.head
    randomValues1 = randomValues1.tail
    randomValues2 = randomValues2.tail

    input1 :+= randomValues1.head
    input2 :+= randomValues2.head
    refInt1 += randomValues1.head
    refInt2 += randomValues2.head
    randomValues1 = randomValues1.tail
    randomValues2 = randomValues2.tail

    input1.:--()
    input2.:--()
    refInt1 -= 1
    refInt2 -= 1

    input1.:++()
    input2.:++()
    refInt1 += 1
    refInt2 += 1

    output.value() should be <= refInt1
    output.value() should be <= refInt2
  }

  test("Exchange min"){
    val store = new Store()
    var a : Long = Random.between(-1000, 1000)
    var b : Long = Random.between(-1000, 1000)
    val input1 = new IntVariable(store, a)
    val input2 = new IntVariable(store, b)
    val output = new IntVariable(store, b)
    Min2(store, input1, input2, output)
    store.close()

    // The bigger value become the smaller
    val dist = math.abs(b - a)
    if (a > b) {
      input1 :-= (dist + 1)
      a -= (dist + 1)
    } else {
      input2 :+= (dist + 1)
      b += (dist + 1)
    }
    output.value() should be <= a
    output.value() should be <= b
  }

}
