package oscar.cp.examples

import oscar.algo.Inconsistency
import oscar.cp._
import oscar.cp.core.delta.{DeltaIntVar, DeltaSetVar}
import oscar.cp.core.{CPPropagStrength, CPStore}


/**
 */
object ConnectedGraph extends CPModel with App {

  val codes = Array("E", "BG", "TR", "SLO", "RO", "F", "BIH", "A", "M", "MD", "KOS", "I", "LT", "PL", "CH", "LV", "CY", "L", "HR", "GB", "B", "P", "MC", "UA", "SRB", "H", "EW", "SF", "CZ", "GR", "BY", "MNE", "SK", "MK", "DK", "NL", "R", "AND", "AL", "IRL", "D", "S")
  println(codes.length)
  println(codes.toSet.size)
  val borders = List((29,33), (10,31), (10,24), (10,33), (5,11), (5,17), (5,22), (0,5), (0,21), (7,28), (7,40), (7,25), (7,11), (7,32), (7,14), (28,40), (28,32), (25,32), (14,40), (30,36), (15,36), (12,15), (12,13), (12,36), (13,32), (13,23), (13,36), (20,40), (20,35), (6,24), (6,10), (6,18), (18,24), (18,31), (1,29), (1,33), (1,24), (1,4), (1,2), (4,24), (4,23), (26,36), (9,23), (19,39), (29,38), (33,38), (31,38), (10,38), (2,29), (24,33), (24,31), (5,37), (0,37), (3,7), (13,28), (5,40), (13,40), (17,40), (35,40), (34,40), (24,25), (3,25), (23,25), (18,25), (4,25), (3,11), (23,32), (5,14), (11,14), (15,30), (12,30), (13,30), (23,30), (23,36), (27,36), (5,20), (17,20), (3,18), (15,26), (27,41), (4,9))
  /*
  println()
  for ((a,b) <- borders) {
    println(codes(a)+" - "+codes(b)+" ;")
  }
  println()
*/
  val cc = new oscar.algo.DisjointSets[Int](0,codes.size-1)
  for ((u,v) <- borders) {
    cc.union(u,v)
  }
  val clusters = (0 until codes.size).groupBy(cc.find(_))
  println("#cc:"+clusters.size)

  /*
  for (c <- clusters) {
    println("---")
    println(c._2)
    println(c._2.map(codes(_)))
    //println(c.map(codes(_)).mkString(","))
  }*/

  // cluster of countries
  val x: Array[CPIntVar] = Array.fill(codes.size)(CPIntVar(0 until 4))

  // the induced subgraph of nodes with same block id must be connected
  solver.post(new ConnectedGraphConstraint2(x,borders))

  search {
    //conflictOrderingSearch(x,i => x(i).size, i => x(i).min)
    x.find(!_.isBound) match {
      case None => noAlternative
      case Some(x) => {
        val v = x.min
        branch(solver.post(x === v))(solver.post(x !== v))
      }
    }
  }

  def check(): Boolean = {
    assert(false)
    val ds = new oscar.algo.DisjointSets[Int](0,x.size-1)
    for ((i,j) <- borders; if x(i).value == x(j).value) {
      ds.union(i,j)
    }
    for (i1 <- 0 until x.size; i2 <- i1+1 until x.size; if x(i1).value == x(i2).value) {
      if (!ds.inSameSet(i1,i2)) {
        return false
      }
    }
    return true;
  }

  onSolution {
    println(x.mkString("-"))
    if (!check()) {
      println("error")
    }

  }

  val stats = start()
  println(stats)



}



class ConnectedGraphConstraint2(val x: Array[CPIntVar], val edges: List[(Int,Int)]) extends Constraint(x(0).store) {


  val nBlocks: Int = x.map(_.max).max + 1
  val nNodes = x.size

  val cc = new oscar.algo.DisjointSets[Int](0,x.size-1)


  override def setup(l: CPPropagStrength): Unit = {

    for (i <- 0 until x.size) {
      x(i).callPropagateWhenDomainChanges(this);
    }

  }

  def associatedVars() = x

  override def propagate(): Unit = {

    for (b <- 0 until nBlocks) {
      cc.reset()
      for (e <- edges; if x(e._1).hasValue(b) && x(e._2).hasValue(b)) {
        cc.union(e._1,e._2)
      }
      var rep = -1
      for (i <- 0 until nNodes; if (x(i).isBoundTo(b)); rep = i;  j <- i+1 until nNodes; if (x(j).isBoundTo(b))) {
        if (!cc.inSameSet(i,j)) {
          throw Inconsistency
        }
      }
      if (rep != -1) {
        for (i <- 0 until nNodes; if !cc.inSameSet(i,rep)) {
          x(i).removeValue(b)
        }
      }
    }
  }

}
