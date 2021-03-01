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

package oscar.ml.classificationtree.Constraints

import oscar.algo.reversible.{ReversibleBoolean, ReversibleInt}
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.CPVar
import oscar.cp.{CPIntVar, Constraint}
/**
 * Code of the paper "Learning Optimal Decision Tree Using CP", H. Verhaeghe, S. Nijssen, C-G Quimpert, G. Pesant, P. Schaus
 * @author helene.verhaeghe27@gmail.com
 */
class CoverSizeSR(val I: Array[CPIntVar], val I2: Array[CPIntVar], val Sup: CPIntVar, val nItems: Int, val nTrans: Int, TDB: Array[Array[Long]],TDBI: Array[Array[Long]]) extends Constraint(Sup.store, "CoverSizeVar") {


  //  println( "sup " + Sup)
  override def associatedVars(): Iterable[CPVar] = I ++ I2 ++ Array(Sup)

  //idempotent = true

  //init coverage
  private[this] val coverage = new ReversibleSparseBitSet2(s, nTrans, 0 until nTrans)

  def printcoverage = {
    coverage.toString()
  }



  ///Create matrix B (nItems x nTrans) (i = item, j = transaction)
  //Is such that columns(i) is the coverage of item i.

  private[this] val columns = Array.tabulate(nItems) { x => new coverage.BitSet(TDB(x)) }
  private[this] val columnsI = Array.tabulate(nItems) { x => new coverage.BitSet(TDBI(x)) }
  //  println(nItems + " " + TDB.length)

  ///contains all the unbound variables that are not in the closure of the current itemset.
  //closure => freq(I(D)U{i}) = freq(I(D))
  private[this] val unboundNotInClosure = Array.tabulate(I.length)(i => I(i))
  private[this] val nUnboundNotInClosure = new ReversibleInt(s, I.length)

  private[this] val unboundNotInClosure2 = Array.tabulate(I2.length)(i => I2(i))
  private[this] val nUnboundNotInClosure2 = new ReversibleInt(s, I2.length)

  private[this] val unusedItem = Array.tabulate(nItems)(i => i)
  private[this] val nUnusedItem = new ReversibleInt(s, nItems)

  private[this] val updateSupLB = new ReversibleBoolean(s, true)
  private[this] var supLB = 0

  def tryRm(v:Int) = {
    coverage.intersectCount(columnsI(v))
  }
  def tryAdd(v:Int) = {
    coverage.intersectCount(columns(v))
  }
  //  private[this] val alwaysFilter = Array.ofDim[Boolean](nItems)

  def countPartition(d: Int): (Int, Int) = {
    val take = coverage.intersectCount(columns(d))
    val leave = coverage.count() - take
    (take, leave)
  }

  /**
    *
    * @param l
    * @return CPOutcome state
    */
  override def setup(l: CPPropagStrength): Unit = {

    // Launch propagation on left decisions
    for (x <- I; if !x.isBound)
      x.callPropagateWhenBind(this)

    // Launch propagation of right decisions
    for (x <- I2; if !x.isBound)
      x.callPropagateWhenBind(this)

    if (!Sup.isBound) Sup.callPropagateWhenBoundsChange(this)

    propagate()
  }

  val countArray = Array.fill(nItems)(0)
  var counter1 = 0
  var counter2 = 0

  /**
    *
    * @return CPOutcome state
    */
  override def propagate(): Unit = {

    coverage.clearCollected()

    var nU = nUnboundNotInClosure.value
    var nU2 = nUnboundNotInClosure2.value
    var nbI = nUnusedItem.value
    val nUnbound = nU + nU2


    // update the coverage
    var i = nU2
    while (i > 0) {
      i -= 1
      val x = unboundNotInClosure2(i)
      if (x.isBound) {
        // rm var from reversible sparse set unboundNotInClosure
        nU2 -= 1
        unboundNotInClosure2(i) = unboundNotInClosure2(nU2)
        unboundNotInClosure2(nU2) = x

        // collect for intersection
        coverage.collect(columns(x.min))
        //        coverage.collectByIntersect(columnsI(x.min))
      }
    }
    //    coverage.reverseCollected()

    i = nU
    while (i > 0) {
      i -= 1
      val x = unboundNotInClosure(i)
      if (x.isBound) {

        // rm var from reversible sparse set unboundNotInClosure
        nU -= 1
        unboundNotInClosure(i) = unboundNotInClosure(nU)
        unboundNotInClosure(nU) = x

        // rm item from reversible sparse set unboundItem
        //        nI -= 1
        //        var j = 0
        //        while (j <= nI && unboundItems(j) != item)
        //          j+=1
        //        val tmp = unboundItems(nI)
        //        unboundItems(nI) = unboundItems(j)
        //        unboundItems(j) = tmp

        // collect for intersection
        //        coverage.collectByIntersect(columns(x.min))
        coverage.collect(columnsI(x.min))
      }
    }
    coverage.reverseCollected()
    coverage.intersectCollectedWithoutFeedback()

    val supUB = coverage.count()

    updateSupLB.value = nUnbound != (nU + nU2)
    //    if (!updateSupLB.getValue())
    //      println("--------------------------------------")
    //      println(updateSupLB.getValue())

    Sup.updateMax(supUB)

    if (nU == 0 && nU2 == 0) {
      //              println("case 1")
      Sup.updateMin(supUB)
      //      Sup.assign(supUB)
    } else {
      val SupMax = Sup.max
      val SupMin = Sup.min
      val SupMaxMMin = supUB - SupMin
      // remove items that if included induce a too small coverage

      i = nbI // nI //TODO improve this
      //    i = nItems // nI //TODO improve this
      //    var nUnboundItems_ = nUnboundItems.value
      //    i =  nUnboundItems_

      while (i > 0) {
        i -= 1
        val item = unusedItem(i)
        //      val item = i //unboundItems(i)

        val count = coverage.intersectCount(columns(item))
        //      countArray(item) = count
        if (count < SupMin) {
          //        rmItemForall(unboundNotInClosure,nU,item)
          var j = nU
          while (j > 0) {
            j -= 1
            unboundNotInClosure(j).removeValue(item)
          }

          if (SupMaxMMin < count) {
            //        rmItemForall(unboundNotInClosure2,nU2,item)
            j = nU2
            while (j > 0) {
              j -= 1
              unboundNotInClosure2(j).removeValue(item)
            }
            nbI -= 1
            unusedItem(i) = unusedItem(nbI)
            unusedItem(nbI) = item
          }

        } else {
          if (SupMaxMMin < count) {
            //        rmItemForall(unboundNotInClosure2,nU2,item)
            var j = nU2
            while (j > 0) {
              j -= 1
              unboundNotInClosure2(j).removeValue(item)
            }
          }
        }

      }
      nUnusedItem.value = nbI
      //      println(nU + " "+ nU2 )
      //    nUnboundItems.value = nUnboundItems_

      //      println("assign")

      //    if (nU == 0 && nU2 == 0) {
      ////              println("case 1")
      //      Sup.updateMin(supUB)
      ////      Sup.assign(supUB)
      //    }else {
      //        println("case 2")
      //      Sup.updateMax(supUB)
      //        println("case 2-2")
      //     update the Sup.min by intersect all the remaining unbound items
      if (updateSupLB.value /*&& I.length>1*/ ) {
        //          println("case 2-3")

        val array = Array.fill(nItems)(0)
        var continue = true

        if (nU > 0)
          for (value <- unboundNotInClosure(nU - 1)) {
            array(value) = 1
          }
        if (nU2 > 0)
          for (value <- unboundNotInClosure2(nU2 - 1); if continue) {
            continue = array(value) <= 0
            array(value) = -1
          }

        if (continue) {
          var i = nU -1
          while (i > 0) {
            i -= 1
            for (value <- unboundNotInClosure(i); if continue) {
              continue = array(value) >= 0
              array(value) = 1
            }
          }
          i = nU2 -1
          while (i > 0 && continue) {
            i -= 1
            for (value <- unboundNotInClosure2(i); if continue) {
              continue = array(value) <= 0
              array(value) = -1
            }
          }
          if (continue) {
            coverage.resetSparseCollect
            var i = nItems
            while (continue && i > 0) {
              i -= 1
              if (array(i) == -1)
                continue = coverage.sparseIntersectCollect(columnsI(i)) > 0
              else if (array(i) == 1)
                continue = coverage.sparseIntersectCollect(columns(i)) > 0
            }
            if (continue) {
              supLB = coverage.countCollected
              Sup.updateMin(supLB)

            }

          }
        }


        //        if (nU == 0 || nU2 == 0 || (!unboundNotInClosure(0).exists(v => unboundNotInClosure2(0).hasValue(v)))) {
        ////                    supLB = coverage.intersectCountAll2(columns, unboundNotInClosure, nU, unboundNotInClosure2, nU2) //TODO doit by value not for all unbound, rm redundant
        //
        //          coverage.resetSparseCollect
        //          var stop = 1
        //          if (nU > 0) {
        //            for (value <- unboundNotInClosure(nU - 1))
        //              stop = coverage.sparseIntersectCollect(columns(value))
        //          }
        //          if (stop > 0) {
        //            if (nU2 > 0) {
        //              for (value <- unboundNotInClosure2(nU2 - 1))
        //                stop = coverage.sparseIntersectCollectBis(columns(value))
        //            }
        //            if (stop > 0) {
        //              var j = nU -1
        //              while (j > 0 && stop > 0) {
        //                j -= 1
        //                for (value <- unboundNotInClosure(j))
        //                  stop = coverage.sparseIntersectCollect(columns(value))
        //              }
        //              j = nU2 - 1
        //              while (j > 0 && stop > 0) {
        //                j -= 1
        //                for (value <- unboundNotInClosure2(j))
        ////                  stop = coverage.sparseIntersectCollectBis(columns(value))
        //                  stop = coverage.sparseIntersectCollect(columnsI(value))
        //              }
        //              supLB = coverage.countCollected
        //              Sup.updateMin(supLB)
        //            }
        //          }
        ////          val sup2 = coverage.countCollected
        ////          assert(sup2==supLB, sup2 + " " + supLB)
        ////          Sup.updateMin(supLB)
        //
        ////          supLB =  coverage.countCollected
        ////          //
        ////          //        if (Sup.min < supLB)
        ////          //          println("----------->")
        ////          Sup.updateMin(supLB)
        //        }

        updateSupLB.value = false
      }


      //              if(Sup.max < supUB){ // TODO complete here remove item if one left unbound
      //                println("this is war")
      //              }
      if (supUB == Sup.min) {
        // remove all unbound items that are not super sets of coverage
        // because these would necessarily decrease the coverage
        i = nUnusedItem.value // TODO do it by item unbound, not for all
        //        i = nItems // TODO do it by item unbound, not for all
        while (i > 0) {
          i -= 1
          val item = unusedItem(i)
          //          val item = i //unboundItems(i)
          //        val idx = unboundNotInClosure(i)
          //            println(item )
          if (!coverage.isSubSetOf(columns(item))) {
            //          if(countArray(item) != supUB){
            //            rmItemForall(unboundNotInClosure,nU,item)
            var j = nU
            while (j > 0) {
              j -= 1
              unboundNotInClosure(j).removeValue(item)
            }

          }
          if (coverage.intersect(columns(item))) {
            //          if(countArray(item) != 0){
            //            rmItemForall(unboundNotInClosure2,nU2,item)
            var j = nU2
            while (j > 0) {
              j -= 1
              unboundNotInClosure2(j).removeValue(item)
            }
          }

        }

        //         println("end")
      }

    }
    //    println("sup min " + Sup.min)
    nUnboundNotInClosure.value = nU
    nUnboundNotInClosure2.value = nU2
    //    nUnboundItems.value = nI
    //    } catch{
    //      case e:Inconsistency =>
    //        println("cover gone wrong")
    //        throw Inconsistency
    //    }
  }


  @inline private def rmItemForall(rss: Array[CPIntVar], limit: Int, item: Int) = {
    var j = limit
    while (j > 0) {
      j -= 1
      rss(j).removeValue(item)
    }
  }

}

class CoverSizeSRInter(val I: Array[CPIntVar], val I2: Array[CPIntVar], val Sup: CPIntVar, val nItems: Int, val nTrans: Int, TDB: Array[Array[Long]]) extends Constraint(Sup.store, "CoverSizeVar") {


  //  println( "sup " + Sup)
  override def associatedVars(): Iterable[CPVar] = I ++ I2 ++ Array(Sup)

  //idempotent = true

  //init coverage
  private[this] val coverage = new ReversibleSparseBitSet2(s, nTrans, 0 until nTrans)

  def printcoverage = {
    coverage.toString()
  }

  ///Create matrix B (nItems x nTrans) (i = item, j = transaction)
  //Is such that columns(i) is the coverage of item i.

  private[this] val columns = Array.tabulate(nItems) { x => new coverage.BitSet(TDB(x)) }
  private[this] val columnsI = Array.tabulate(nItems) { x =>
    val cl = TDB(x).clone()
    for (i <- cl.indices)
      cl(i) = ~cl(i)
    new coverage.BitSet(cl)
  }
  //  println(nItems + " " + TDB.length)

  ///contains all the unbound variables that are not in the closure of the current itemset.
  //closure => freq(I(D)U{i}) = freq(I(D))
  private[this] val unboundNotInClosure = Array.tabulate(I.length)(i => I(i))
  private[this] val nUnboundNotInClosure = new ReversibleInt(s, I.length)

  private[this] val unboundNotInClosure2 = Array.tabulate(I2.length)(i => I2(i))
  private[this] val nUnboundNotInClosure2 = new ReversibleInt(s, I2.length)

  private[this] val updateSupLB = new ReversibleBoolean(s, true)
  private[this] var supLB = 0

  private[this] val unusedItem = Array.tabulate(nItems)(i => i)
  private[this] val nUnusedItem = new ReversibleInt(s, nItems)

  //  private[this] val alwaysFilter = Array.ofDim[Boolean](nItems)

  def countPartition(d: Int): (Int, Int) = {
    val take = coverage.intersectCount(columns(d))
    val leave = coverage.count() - take
    (take, leave)
  }

  /**
    *
    * @param l
    * @return CPOutcome state
    */
  override def setup(l: CPPropagStrength): Unit = {

    // Launch propagation on left decisions
    for (x <- I; if !x.isBound)
      x.callPropagateWhenBind(this)

    // Launch propagation of right decisions
    for (x <- I2; if !x.isBound)
      x.callPropagateWhenBind(this) // TODO when bind? or when domain change?

    if (!Sup.isBound) Sup.callPropagateWhenBoundsChange(this)

    propagate()
  }

  /**
    *
    * @return CPOutcome state
    */
  override def propagate(): Unit = {

    coverage.clearCollected()

    //    var nI = nUnboundItems.value
    var nU = nUnboundNotInClosure.value
    var nU2 = nUnboundNotInClosure2.value
    var nbI = nUnusedItem.value
    val nUnbound = nU + nU2
    //      println(nU + " "+ nU2)

    // update the coverage
    var i = nU2
    while (i > 0) {
      i -= 1
      val x = unboundNotInClosure2(i)
      if (x.isBound) {
        // rm var from reversible sparse set unboundNotInClosure
        nU2 -= 1
        unboundNotInClosure2(i) = unboundNotInClosure2(nU2)
        unboundNotInClosure2(nU2) = x

        // collect for intersection
        coverage.collect(columns(x.min))
        //        coverage.collectByIntersect(columnsI(x.min))
      }
    }
    //    coverage.reverseCollected()

    i = nU
    while (i > 0) {
      i -= 1
      val x = unboundNotInClosure(i)
      if (x.isBound) {

        // rm var from reversible sparse set unboundNotInClosure
        nU -= 1
        unboundNotInClosure(i) = unboundNotInClosure(nU)
        unboundNotInClosure(nU) = x

        // rm item from reversible sparse set unboundItem
        //        nI -= 1
        //        var j = 0
        //        while (j <= nI && unboundItems(j) != item)
        //          j+=1
        //        val tmp = unboundItems(nI)
        //        unboundItems(nI) = unboundItems(j)
        //        unboundItems(j) = tmp

        // collect for intersection
        //        coverage.collectByIntersect(columns(x.min))
        coverage.collect(columnsI(x.min))
      }
    }
    coverage.reverseCollected()
    coverage.intersectCollectedWithoutFeedback()



    val supUB = coverage.count() //if (coverChanged) coverage.count() else Sup.max
    Sup.updateMax(supUB)
    if (nU == 0 && nU2 == 0) {
      //        println("case 1")
      Sup.updateMin(supUB)
    } else {
      val SupMax = Sup.max
      val SupMin = Sup.min
      val SupMaxMMin = SupMax - SupMin
    //      println("supUB " + supUB)

    // remove items that if included induce a too small coverage

      i = nbI
    //    var nUnboundItems_ = nUnboundItems.value
    //    i =  nUnboundItems_
      while (i > 0) {
        i -= 1
        val item = unusedItem(i)
        //      val item = i //unboundItems(i)

        val count = coverage.intersectCount(columns(item))
        //      countArray(item) = count
        if (count < SupMin) {
          //        rmItemForall(unboundNotInClosure,nU,item)
          var j = nU
          while (j > 0) {
            j -= 1
            unboundNotInClosure(j).removeValue(item)
          }

          if (SupMaxMMin < count) {
            //        rmItemForall(unboundNotInClosure2,nU2,item)
            j = nU2
            while (j > 0) {
              j -= 1
              unboundNotInClosure2(j).removeValue(item)
            }
            nbI -= 1
            unusedItem(i) = unusedItem(nbI)
            unusedItem(nbI) = item
          }

        } else {
          if (supUB - count < Sup.min){//SupMaxMMin < count) {
            //        rmItemForall(unboundNotInClosure2,nU2,item)
            var j = nU2
            while (j > 0) {
              j -= 1
              unboundNotInClosure2(j).removeValue(item)
            }
          }
        }

      }
      nUnusedItem.value = nbI
    //      println(nU + " "+ nU2 )
    //    nUnboundItems.value = nUnboundItems_

    //      println("assign")


      //        println("case 2")
      Sup.updateMax(supUB)
      //        println("case 2-2")
      //     update the Sup.min by intersect all the remaining unbound items
      if (updateSupLB.value /*&& I.length>1*/ ) {
        //          println("case 2-3")
        supLB = coverage.intersectCountAll2(columns, unboundNotInClosure, nU, unboundNotInClosure2, nU2) //TODO doit by value not for all unbound, rm redundant
        //
        //        if (Sup.min < supLB)
        //          println("----------->")
        Sup.updateMin(supLB)

        updateSupLB.value = false
      }


      //              if(Sup.max < supUB){ // TODO complete here remove item if one left unbound
      //                println("this is war")
      //              }
      if (supUB == Sup.min) {
        //          println("case 2-4")
        // remove all unbound items that are not super sets of coverage
        // because these would necessarily decrease the coverage
        i = nItems // TODO do it by item unbound, not for all
        while (i > 0) {
          i -= 1
          val item = i //unboundItems(i)
          //        val idx = unboundNotInClosure(i)
          //            println(item )
          if (!coverage.isSubSetOf(columns(item))) {
            //              println("rm from closure " + item)
            var j = nU
            while (j > 0) {
              j -= 1
              //                println("t" + unboundNotInClosure(j))
              unboundNotInClosure(j).removeValue(item)
            }
          }
          if (coverage.intersect(columns(item))) {
            //              println("rm from closure2 " + item)
            var j = nU2
            while (j > 0) {
              j -= 1
              //                println("t" + unboundNotInClosure2(j))
              unboundNotInClosure2(j).removeValue(item)
            }
          }

        }
        //         println("end")
      }

    }
    //    println("sup min " + Sup.min)
    nUnboundNotInClosure.value = nU
    nUnboundNotInClosure2.value = nU2
    //    nUnboundItems.value = nI
    //    } catch{
    //      case e:Inconsistency =>
    //        println("cover gone wrong")
    //        throw Inconsistency
    //    }
  }

}

class CoverSizeSROld(val I: Array[CPIntVar], val I2: Array[CPIntVar], val Sup: CPIntVar, val nItems: Int, val nTrans: Int, TDB: Array[Array[Long]]) extends Constraint(Sup.store, "CoverSizeVar") {


  //  println( "sup " + Sup)
  override def associatedVars(): Iterable[CPVar] = I ++ I2 ++ Array(Sup)

  //idempotent = true

  //init coverage
  private[this] val coverage = new ReversibleSparseBitSet2(s, nTrans, 0 until nTrans)

  def printcoverage = {
    coverage.toString()
  }

  ///Create matrix B (nItems x nTrans) (i = item, j = transaction)
  //Is such that columns(i) is the coverage of item i.

  private[this] val columns = Array.tabulate(nItems) { x => new coverage.BitSet(TDB(x)) }
  //  println(nItems + " " + TDB.length)

  ///contains all the unbound variables that are not in the closure of the current itemset.
  //closure => freq(I(D)U{i}) = freq(I(D))
  private[this] val unboundNotInClosure = Array.tabulate(I.length)(i => I(i))
  private[this] val nUnboundNotInClosure = new ReversibleInt(s, I.length)

  private[this] val unboundNotInClosure2 = Array.tabulate(I2.length)(i => I2(i))
  private[this] val nUnboundNotInClosure2 = new ReversibleInt(s, I2.length)

  private[this] val updateSupLB = new ReversibleBoolean(s, true)
  private[this] var supLB = 0

  //  private[this] val alwaysFilter = Array.ofDim[Boolean](nItems)

  def countPartition(d: Int): (Int, Int) = {
    val take = coverage.intersectCount(columns(d))
    val leave = coverage.count() - take
    (take, leave)
  }

  /**
    *
    * @param l
    * @return CPOutcome state
    */
  override def setup(l: CPPropagStrength): Unit = {

    // Launch propagation on left decisions
    for (x <- I; if !x.isBound)
      x.callPropagateWhenBind(this)

    // Launch propagation of right decisions
    for (x <- I2; if !x.isBound)
      x.callPropagateWhenBind(this) // TODO when bind? or when domain change?

    if (!Sup.isBound) Sup.callPropagateWhenBoundsChange(this)

    propagate()
  }

  /**
    *
    * @return CPOutcome state
    */
  override def propagate(): Unit = {

    coverage.clearCollected()

    //    var nI = nUnboundItems.value
    var nU = nUnboundNotInClosure.value
    var nU2 = nUnboundNotInClosure2.value

    //      println(nU + " "+ nU2)

    // update the coverage
    var i = nU2
    while (i > 0) {
      i -= 1
      val x = unboundNotInClosure2(i)
      if (x.isBound) {
        //        val item = x.min

        // rm var from reversible sparse set unboundNotInClosure
        nU2 -= 1
        unboundNotInClosure2(i) = unboundNotInClosure2(nU2)
        unboundNotInClosure2(nU2) = x

        // rm item from reversible sparse set unboundItem
        //        nI -= 1
        //        var j = 0
        //        while (j <= nI && unboundItems(j) != item)
        //          j+=1
        //        val tmp = unboundItems(nI)
        //        unboundItems(nI) = unboundItems(j)
        //        unboundItems(j) = tmp

        // collect for intersection
        coverage.collect(columns(x.min))
        //        coverage.collect(columns(item))

        // update required
        updateSupLB.value = true
      }
    }
    //      println(nU + " "+ nU2)
    coverage.reverseCollected()

    i = nU
    while (i > 0) {
      i -= 1
      val x = unboundNotInClosure(i)
      if (x.isBound) {
        //        val item = x.min

        // rm var from reversible sparse set unboundNotInClosure
        nU -= 1
        unboundNotInClosure(i) = unboundNotInClosure(nU)
        unboundNotInClosure(nU) = x

        // rm item from reversible sparse set unboundItem
        //        nI -= 1
        //        var j = 0
        //        while (j <= nI && unboundItems(j) != item)
        //          j+=1
        //        val tmp = unboundItems(nI)
        //        unboundItems(nI) = unboundItems(j)
        //        unboundItems(j) = tmp

        // collect for intersection
        coverage.collectByIntersect(columns(x.min))
        //        coverage.collectByIntersect(columns(item))
      }
    }
    //      println(nU + " "+ nU2)
    coverage.intersectCollectedWithoutFeedback()

    val supUB = coverage.count() //if (coverChanged) coverage.count() else Sup.max

    //      println("supUB " + supUB)

    // remove items that if included induce a too small coverage
    i = nItems // nI //TODO improve this
    //    var nUnboundItems_ = nUnboundItems.value
    //    i =  nUnboundItems_
    while (i > 0) {
      i -= 1
      //      val item = unboundItems(i)
      val item = i //unboundItems(i)

      val count = coverage.intersectCount(columns(item))
      if (count < Sup.min) {
        //          println("to rm " + item)

        var j = nU
        while (j > 0) {
          j -= 1
          unboundNotInClosure(j).removeValue(item)
        }

        // rm var from reversible sparse set unboundItems
        //        nUnboundItems_ -= 1
        //        unboundItems(i) = unboundItems(nUnboundItems_)
        //        unboundItems(nUnboundItems_) = item

        // update required
        updateSupLB.value = true
      } else if (supUB - count < Sup.min) {
        //          println("to rm2 " + item)
        var j = nU2
        while (j > 0) {
          j -= 1
          unboundNotInClosure2(j).removeValue(item)
        }

        // rm var from reversible sparse set unboundItems
        //        nUnboundItems_ -= 1
        //        unboundItems(i) = unboundItems(nUnboundItems_)
        //        unboundItems(nUnboundItems_) = item

        // update required
        updateSupLB.value = true
      }
    }
    //      println(nU + " "+ nU2 )
    //    nUnboundItems.value = nUnboundItems_

    //      println("assign")

    if (nU == 0 && nU2 == 0) {
      //        println("case 1")
      Sup.assign(supUB)
    } else {
      //        println("case 2")
      Sup.updateMax(supUB)
      //        println("case 2-2")
      //     update the Sup.min by intersect all the remaining unbound items
      if (updateSupLB.value /*&& I.length>1*/ ) {
        //          println("case 2-3")
        supLB = coverage.intersectCountAll2(columns, unboundNotInClosure, nU, unboundNotInClosure2, nU2) //TODO doit by value not for all unbound, rm redundant
        //
        //        if (Sup.min < supLB)
        //          println("----------->")
        Sup.updateMin(supLB)

        updateSupLB.value = false
      }


      //              if(Sup.max < supUB){ // TODO complete here remove item if one left unbound
      //                println("this is war")
      //              }
      if (supUB == Sup.min) {
        //          println("case 2-4")
        // remove all unbound items that are not super sets of coverage
        // because these would necessarily decrease the coverage
        i = nItems // TODO do it by item unbound, not for all
        while (i > 0) {
          i -= 1
          val item = i //unboundItems(i)
          //        val idx = unboundNotInClosure(i)
          //            println(item )
          if (!coverage.isSubSetOf(columns(item))) {
            //              println("rm from closure " + item)
            var j = nU
            while (j > 0) {
              j -= 1
              //                println("t" + unboundNotInClosure(j))
              unboundNotInClosure(j).removeValue(item)
            }
          }
          if (coverage.intersect(columns(item))) {
            //              println("rm from closure2 " + item)
            var j = nU2
            while (j > 0) {
              j -= 1
              //                println("t" + unboundNotInClosure2(j))
              unboundNotInClosure2(j).removeValue(item)
            }
          }

        }
        //         println("end")
      }

    }
    //    println("sup min " + Sup.min)
    nUnboundNotInClosure.value = nU
    nUnboundNotInClosure2.value = nU2
    //    nUnboundItems.value = nI
    //    } catch{
    //      case e:Inconsistency =>
    //        println("cover gone wrong")
    //        throw Inconsistency
    //    }
  }

}