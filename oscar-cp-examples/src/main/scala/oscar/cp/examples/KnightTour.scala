package oscar.cp.examples


import oscar.cp._

/**
  * Created by dervalguillaume on 15/10/16.
  */
object KnightTour extends CPModel with App {


  def dist(a: CPIntVar, b: CPIntVar): CPIntVar = (a-b).abs
  def mod(a: CPIntVar, b: Int): CPIntVar = {
    val m = CPIntVar(0, b-1)
    add(a%b == m)
    m
  }
  def div(a: CPIntVar, b: Int): CPIntVar = {
    //we know a's are positive...
    val v = CPIntVar(0, a.max/b)
    add(new oscar.cp.constraints.MulCte(v, b, a - mod(a,b)))
    v
  }
  def and(a: CPBoolVar, b: CPBoolVar): CPBoolVar = {
    val v = CPBoolVar()
    add(new oscar.cp.constraints.BinaryAnd(a,b,v))
    v
  }

  val x = Array.fill(36)(CPIntVar(0,36))
  add(allDifferent(x))
  add(x(0) === 0)
  add(x(1) === 8)

  //val divs = x.map(div(_,6))
  //val mods = x.map(mod(_,6))

  for(i <- 0 until 36) {
    val v1 = x(i)
    val v2 = x((i+1)%36)

    val and1 = and(dist(div(v1, 6), div(v2, 6)) ?=== 1, dist(mod(v1, 6), mod(v2, 6)) ?=== 2)
    val and2 = and(dist(div(v1, 6), div(v2, 6)) ?=== 2, dist(mod(v1, 6), mod(v2, 6)) ?=== 1)

    add(or(Array(and1, and2)))
  }

  search(binaryStatic(x))
  onSolution(println(x.zipWithIndex.map({case (v, idx) => "x["+idx+"]="+v.value}).mkString(" ")))

  println(start(nSols = 1))
}
