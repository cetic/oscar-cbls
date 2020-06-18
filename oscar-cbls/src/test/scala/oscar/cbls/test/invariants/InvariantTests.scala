package oscar.cbls.test.invariants

/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import oscar.cbls._
import oscar.cbls.benchmarks.vrp.RoutingMatrixGenerator
import oscar.cbls.lib.constraint._
import oscar.cbls.lib.invariant.logic._
import oscar.cbls.lib.invariant.seq._
import oscar.cbls.lib.invariant.set._
import oscar.cbls.test.invariants.bench._

/**
  * @author yoann.guyot@cetic.be
  */
class InvariantTests extends AnyFunSuite with Checkers {

  {
    var assertActivated = false
    assert({assertActivated = true; true})
    if(!assertActivated)
      println("You are executing tests with asserts deactivated, you should activate them for a more thorough test")
  }

  val verbose = 0

  test("BelongsTo maintains the violation of a membership.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
      Random(), RandomDiff()))

    oscar.cbls.lib.invariant.set.BelongsTo(bench.genIntVar(0 to 10), bench.genIntSetVar(5, 0 to 10))
    bench.run()
  }



  test("MultiKnapsack") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
      Random(), RandomDiff()))
    MultiKnapsack(
      bench.genIntVarsArray(10, 0 to 5),
      bench.genIntVarsArray(10, 2 to 7),
      bench.genIntVarsArray(6, 1 to 10))
    bench.run()
  }

  test("MultiKnapsackLoad") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
      Random(), RandomDiff()))
    MultiKnapsackLoad(
      bench.genIntVarsArray(10, 0 to 5),
      bench.genIntVarsArray(10, 2 to 7),
      bench.genIntVarsArray(6, 1 to 10))
    bench.run()
  }

  test("Sequence") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
      Random(), RandomDiff()))
    Sequence(
      bench.genIntVarsArray(),
      Gen.choose(10, 20).sample.get,
      Gen.choose(2, 9).sample.get,
      Array.tabulate(101)(n => n%3 == 0))
    bench.run()
  }

  test("Access to ITE maintains output = if ifVar > 0 then thenVar else elseVar") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
      Random(), RandomDiff()))
    IntITE(
      bench.genIntVar(-2 to 3),
      bench.genIntVar(1 to 2),
      bench.genIntVar(10 to 11))
    bench.run()
  }

  test("Access to int element maintains output = array(index)") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(),
      Random(), RandomDiff()))
    IntElement(
      bench.genIntVar(0 to 19),
      bench.genIntVarsArray(20, 0 to 100))
    bench.run()
  }

  test("Access to int vars...") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    Elements(
      bench.genIntSetVar(3, 0 to 4),
      bench.genIntVarsArray(5, 0 to 10))
    bench.run()
  }

  test("Access to int set element maintains output = array(index)") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    SetElement(
      bench.genIntVar(0 to 19),
      bench.genIntSetVars(20, 10, 0 to 100))
    bench.run()
  }

  test("Sparse Cluster maintains a cluster of the indexes of an array.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    Cluster.makeSparse(bench.genIntVarsArray(50),
      Gen.containerOfN[List, Int](100, Gen.choose(0, 100)).sample.get)
    bench.run()
  }

  test("Dense Cluster maintains a cluster of all the indexes of an array.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    Cluster.makeDense(bench.genIntVarsArray(50))
    bench.run()
  }

  test("Dense Cluster maintains a cluster of all the indexes of an array"
    + " (assuming min and max).") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    Cluster.makeDenseAssumingMinMax(bench.genIntVarsArray(50), 0, 100)
    bench.run()
  }

  test("Dense Count maintains count(j) = #{i in index of values | values[i] == j}") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))

    makeDenseCount(bench.genIntVarsArray(10, 0 to 19))

    bench.run()
  }

  //ignore("Cross references...")(pending) it was tested extensively in the routing cases

  //ignore("Cumulative...")(pending) since it was tested in the scheduling examples.

  test("Filter...") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    Filter(
      bench.genIntVarsArray(4, 0 to 5),
      (i: Long) => (i % 2) == 0)
    bench.run()
  }

  test("SelectLEHeapHeap") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    SelectLEHeapHeap(
      bench.genIntVarsArray(4, 0 to 5),
      bench.genIntVar(3 to 10))
    bench.run()
  }

  //ignore("SelectLESetQueue") {
  // the forbidden value chenges are not forbidden in this test, so it fails
  //  //le pivot ne peut qu'augmenter
  //une valeur en dessous du pivot ne peut que prendre une valeur dÃ©passant toutes les autres valeurs
  //les valeurs au dessus du pivot ne peuvent pas changer
  //  val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
  //  new SelectLESetQueue(bench.genIntVarsArray(5, 0 to 5), bench.genIntVar(3 to 10, false)).toIntSetVar
  //  bench.run
  //}

  //ignore("Predecessor")(pending)

  //ignore("Routes")(pending)

  //this is not working so far.
  test("Sort") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    Sort.MakeSort(bench.genIntVarsArray(4, 0 to 30))
    bench.run()
  }



  /* test("RoundUpCustom") {
     val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
     new RoundUpCustom(
       bench.genIntVar(0 to 10),
       bench.genIntVar(1 to 10),
       InvGen.randomTuples(10, 0 to 10).map(ab => (ab._1,ab._1 + ab._2)))
     bench.run()
   }*/

  test("Union maintains the union of two sets.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    Union(bench.genIntSetVar(), bench.genIntSetVar())
    bench.run()
  }

  test("UnionAll maintains the union of a set of sets.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    UnionAll(bench.genIntSetVars())
    bench.run()
  }

  test("Inter maintains the intersection of two sets.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    Inter(bench.genIntSetVar(), bench.genIntSetVar())
    bench.run()
  }

  test("Diff maintains the difference between two sets.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    Diff(bench.genIntSetVar(), bench.genIntSetVar())
    bench.run()
  }

  test("Cardinality maintains the cardinality of a set.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    Cardinality(bench.genIntSetVar())
    bench.run()
  }

  test("IncludedSubsets counts the number of included subsets from a specified list") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    IncludedSubsets(bench.genIntSetVar(), List((List(0,1,2,3,4,5),2,2),(List(0,1,4,5),1,3),(List(0,1,24,5),2,2),(List(0,1,2,3,4,5),2,2)))
    bench.run()
  }

  test("ValuesInViolatedClauses includes all values in violated clauses") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    ValuesInViolatedClauses(bench.genIntSetVar(), List((List(0,1,2,3,4,5),2),(List(0,1,4,5),1),(List(0,1,24,5),2),(List(0,1,2,3,4,5),2)))
    bench.run()
  }

  test("MakeSet maintains an IntSetVar given a set of IntVar.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    MakeSet(bench.genSortedIntVars(10, 0 to 10))
    bench.run()
  }

  test("Interval maintains the set in the interval.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    Interval(
      bench.genIntVar(-100 to 100),
      bench.genIntVar(-100 to 100))
    bench.run()
  }

  test("TakeAny maintains a value taken from the set.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    TakeAny(bench.genIntSetVar(), 0)
    bench.run()
  }

  test("TakeAnyToSet maintains a value taken from the set.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    TakeAnyToSet(bench.genIntSetVar())
    bench.run()
  }

  test("Singleton maintains a singleton set that has an expected value.") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    Singleton(bench.genIntVar(-100 to 100))
    bench.run()
  }

  test("SetMap maintains the values mapped from the set") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    SetMap(bench.genIntSetVar(), e => e * 2)
    bench.run()
  }

  test("SetSum maintains the sum of variables (after optionnaly appliying a function).") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    SetSum(bench.genIntSetVar())
    bench.run()
  }



  /*  test("IdentityInt maintains the identity of an integer).") {
      val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
      new IdentityInt(bench.genIntVar(-100 to 100))
      bench.run()
    }
  */

  /*
  test("IdentityIntSet maintains the identity of a set of integers).") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    new IdentitySet(bench.genIntSetVar())
    bench.run()
  }
  */


  test ("ConcatenateVars concatenates two ChangingSeqValue") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    //val seqVars = bench.genIntSeqVars(2)
    val seqVar1 = bench.genIntSeqVar()
    val seqVar2 = bench.genIntSeqVar()
    Length(new Concatenate(seqVar1,seqVar2,4,20))
    bench.run()
  }


  test ("ConcatenateList Var concatenates List and ChangingSeqValue") {
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    //val seqVars = bench.genIntSeqVars(2)
    val seqVar2 = bench.genIntSeqVar()
    Length(new ConcatenateFirstConstant(List(1,6,8,30),seqVar2,4,20))
    bench.run()
  }

  test ("PositionsOfConst maintains the positions of a value"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    val seqVar = bench.genIntSeqVar()
    PositionsOf(seqVar,Gen.choose(0, 100).sample.get)
    bench.run()
  }

  test ("PositionsOf maintains the positions of a value"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    val seqVar = bench.genIntSeqVar(range = 0 to 100)
    val value = bench.genIntVar(0 to 100)
    PositionsOf(seqVar,value)
    bench.run()
  }

  test ("Map"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    val seqVar = bench.genIntSeqVar(range = 0 to 100)
    oscar.cbls.lib.invariant.seq.Map(seqVar, Array.tabulate(101)(n => 2*n))
    bench.run()
  }

  test ("Precedence"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    val seqVar = bench.genNotRandomIntSeqVar(100)
    val precedences = RoutingMatrixGenerator.generatePrecedence(seqVar.value.size,0,seqVar.value.size/2)
    Precedence(seqVar,precedences)
    bench.run()
  }

  test("SubSequence"){
    val bench = new InvBench(verbose, List(PlusOne(),Shuffle()))
    val maxsize = 25
    val seqVar = bench.genIntSeqVar(maxsize)
    val index = scala.util.Random.nextInt(seqVar.value.size)
    val length = scala.util.Random.nextInt(seqVar.value.size - index) + 1
    SubSequence(seqVar, index, length)
    bench.run()
  }

  test ("Successors"){
    val bench = new InvBench(verbose,List(PlusOne(),Shuffle(), Random(), RandomDiff()))
    val seqVar = bench.genIntSeqVar(25)
    Successors(seqVar)
    bench.run()
  }

  test ("Size maintains the size of the sequence"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    val seqVar = bench.genIntSeqVar(25)
    Length(seqVar)
    bench.run()
  }

  test ("OccurenceOf maintains the occurence of a certain value"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    val seqVar = bench.genIntSeqVar()
    OccurrencesOf(seqVar,Gen.choose(0, 100).sample.get)
    bench.run()
  }

  test("Content maintains the content of the intSeq"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff()))
    val seqVar = bench.genIntSeqVar()
    Content(seqVar)
    bench.run()
  }

  test("Flip maintains a flipped sequence"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff(),Shuffle()))
    val seqVar = bench.genIntSeqVar()
    Flip(seqVar)
    bench.run()
  }

  test("SeqSum maintains the sum of all values in a sequence"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff(),Shuffle()))
    val seqVar = bench.genIntSeqVar()
    SeqSum(seqVar)
    bench.run()
  }

  test("SortSequence maintains a sorted sequence out of a non-sorted one and a function (value => value for sort)"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff(),Shuffle()))
    val seqVar = bench.genIntSeqVar()
    SortSequence(seqVar, a => a)// -(a*3), "-a%3")
    bench.run()
  }

  test("SortSequence2 sorts a sequence based of vv => (-(v%3),v)"){
    val bench = new InvBench(verbose,List(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff(),Shuffle()))
    val seqVar = bench.genIntSeqVar(maxLength = 10)
    SortSequence(seqVar, a => -(a % 3), "-(a%3)")
    bench.run()
  }
}

