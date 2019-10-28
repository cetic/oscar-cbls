package oscar.cbls.test.invariants

import org.scalatest.{FunSuite, Matchers}

/**
  * This object's purpose is to test composition function for the TimeWindows Constraint.
  * It won't be on the official release.
  */
class ComposeFunctionTestSuite extends FunSuite with Matchers {

  test("Batch tests on TransferFunction (keep expected values)"){
    val earliestArrivalTimes = Array(6, 13)
    val taskDurations = Array(6,3)
    val latestArrivalTimes = Array(6, 15)
    val timeMatrix = Array(Array.fill(2)(2), Array.fill(2)(2))

    val n = earliestArrivalTimes.length
    val transferFunctions = Array.tabulate(n)(x =>
      DefinedTransferFunction(earliestArrivalTimes(x),latestArrivalTimes(x), earliestArrivalTimes(x) + taskDurations(x)))

    for (x <- 0 until n-1) {
      test(transferFunctions(x),
        transferFunctions(x+1),
        timeMatrix(x)(x+1),
        (0 until latestArrivalTimes.max*2/100).toArray.map(_*100))
    }
  }

  // The compose function that we are testing
  private def composeFunction (f1: TransferFunction, f2: TransferFunction, m: Int): TransferFunction ={
    if(f1.isEmpty)
      return f1
    else if(f2.isEmpty)
      return f2

    val earliestArrivalTimeAt2 = f1.el + m
    val latestArrivalTimeAt2 = f1.la + f1.el - f1.ea + m

    val earliestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2 = earliestArrivalTimeAt2 <= f2.ea
    val earliestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2 = earliestArrivalTimeAt2 <= f2.la
    val latestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2 = latestArrivalTimeAt2 <= f2.ea
    val latestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2 = latestArrivalTimeAt2 <= f2.la

    println((earliestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2,
      earliestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2,
      latestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2,
      latestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2))

    val (ea3, ll3, el3) =
      (earliestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2,
        earliestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2,
        latestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2,
        latestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2) match{
        case (true,true,true,true) =>
          (f1.la, f1.la, f2.el)                                    // e3 == d1 because latest arrival time at 2 is lower than earliest starting time at 2
        // so it doesn't matter when you arrive at 1 the resulting leaving time at 2 will be l2
        // => e3 == d1 (the formula says if (t <= e) => l
        case (true,true,false,true) =>
          (f2.ea - f1.el - m + f1.ea, f1.la, f2.el)
        case (true,true,false,false) =>
          (f2.ea - f1.el - m + f1.ea, f2.la - f1.el - m + f1.ea, f2.el)
        case (false,true,false,true) =>
          (f1.ea, f1.la,f1.el + f2.el - f2.ea + m)
        case (false,true,false,false) =>
          (f1.ea, f2.la - f1.el - m + f1.ea, f1.el + f2.el - f2.ea + m)
        case (false,false,false,false) =>
          (1, -1, -1)
        case _ =>
          throw new Error("Unhandled case : " + (earliestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2,
            earliestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2,
            latestArrivalTimeAt2_earlier_or_equal_than_earliestStartingTimeAt2,
            latestArrivalTimeAt2_earlier_or_equal_than_latestStartingTimeAt2))
      }

    if(ea3 > ll3)
      EmptyTransferFunction
    else
      DefinedTransferFunction(ea3, ll3, el3)
  }

  // This method tests the composition function with transfer functions
  // generated based on the permutation of a List of Integer
  private def permutationMethod(): Unit ={

    val permutationValues = List(0,1,2,4,8,16,32)
    var permutations: Set[Array[Int]] = Set()

    /**
      * This method create all the possible permutation of the given list
      * @param permValues The values to permute
      * @param currentPermutation The permutation we are currently building
      */
    def permute(permValues: List[Int], currentPermutation:  Array[Int] = Array.fill(permutationValues.size)(0)): Unit ={
      if(permValues.isEmpty)
        permutations = permutations + currentPermutation.clone()
      else
        for(curValue <- permValues){
          currentPermutation(currentPermutation.length - permValues.size) = curValue
          permute(permValues.diff(List(curValue)), currentPermutation)
        }
    }
    permute(permutationValues)

    // Generate the TransferFunction and testing
    for(permutation <- permutations if permutation.toSet.size == permutation.length){
      val it = permutation.toIterator
      val e1 = it.next()
      val d1 = it.next()
      val l1 = it.next()
      val m = it.next()
      val e2 = it.next()
      val d2 = it.next()
      val l2 = it.next()
      if(e1 <= d1 && e2 <= d2 && e1 <= l1 && e2 <= l2) {
        val f1 = DefinedTransferFunction(e1, d1, l1)
        val f2 = DefinedTransferFunction(e2, d2, l2)
        test(f1,f2,m, (0 until permutationValues.max * 2).toArray)
      }
    }
  }

  // This method tests the composition function with linearly generated transfer functions
  private def easyMethodWithEqualities() {
    val n = 10

    val timeMatrix = Array.tabulate(n)(x => Array.tabulate(n)(y => 10))
    val earliestArrivalTimes = Array.tabulate(n)(x => 100 + 20 * x)
    val latestArrivalTimes = Array.tabulate(n)(x => 200 + 20 * x)

    val transfertFunctions = Array.tabulate(n)(x =>
      DefinedTransferFunction(earliestArrivalTimes(x), latestArrivalTimes(x), earliestArrivalTimes(x)))

    for (x <- 0 until n) {
      for (y <- 0 until n) {
        // Compare f3(t) with f2(f1(t) + M12)
        // (f1, f2, M12, t varying from 0 to 500 by 5)
        test(transfertFunctions(y), transfertFunctions(x), timeMatrix(y)(x), (0 until 100).toArray.map(_*5))
      }
    }
  }

  private def test(f1: TransferFunction, f2: TransferFunction, travelDuration: Int, ts: Array[Int]): Unit ={
    val composedFunction = composeFunction(f1, f2, travelDuration)
    println(composedFunction)
    val t2s = Array.fill(ts.length)(0)
    for (i <- ts.indices) {
      val composedRes = composedFunction.apply(ts(i)).getOrElse(Int.MaxValue)
      val trueRes = f2.apply(
        f1.apply(ts(i)).getOrElse(Int.MaxValue - travelDuration) + travelDuration).
        getOrElse(Int.MaxValue)

      composedRes should be(trueRes)
      t2s(i) = trueRes
    }
  }
}

abstract class TransferFunction(val ea: Int, val la: Int, val el: Int){

  // This method is used to compute the leaving time
  def apply(t: Int): Option[Int]

  // If true it means that the TransferFunction isn't defined
  // and that apply() return always None
  def isEmpty: Boolean

  override def toString: String = {
    "earliest arrival time : " + ea + "\n latest arrival time : " + la + "\n earliest leaving time : " + el
  }
}

case class DefinedTransferFunction(override val ea: Int, override val la: Int, override val el: Int) extends TransferFunction(ea,la,el){
  require(la >= ea && el >= ea, "earliest arrival time : " + ea + ", latest arrival time : " + la + ", earliest leaving time : " + el)
  override def apply(t: Int): Option[Int] = {
    if(t <= ea)
      Some(el)
    else if(t <= la)
      Some(t + el - ea)
    else
      None
  }

  override def isEmpty: Boolean = false
}

case object EmptyTransferFunction extends TransferFunction(1,-1,-1){
  override def apply(t: Int): Option[Int] = None

  override def isEmpty: Boolean = true
}