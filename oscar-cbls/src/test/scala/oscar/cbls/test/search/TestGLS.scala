package oscar.cbls.test.search

import oscar.cbls.{IntValue, Store}
import oscar.cbls.core.computation.{CBLSIntVar, Domain, FullRange}
import oscar.cbls.core.objective.Objective
import oscar.cbls.lib.invariant.logic.{Int2Int, IntInt2Int}
import oscar.cbls.lib.invariant.numeric.Sum2
import oscar.cbls.lib.search.neighborhoods.{GradientDescent, NarrowingStepSlide}

object TestGLS extends App{

  val m = new Store()

  val x = CBLSIntVar(m,10,Domain(0,1000),"x")
  val y = new CBLSIntVar(m,10,Domain(0,1000),"y")

  val f:IntValue = new IntInt2Int(x,y,{case (x,y) => (x - 300)^2 + (y - 100)^2},FullRange)

  val obj = Objective(f)

  //trigo are in radiant
  val g:IntValue = new IntInt2Int(x,y,{case (x,y) => math.abs(100*math.sin(x /10) + 50*math.sin(y /10)).floor.toInt},Domain(0,150))

  m.close()
  printModel()

  def printModel(): Unit ={
    println(x)
    println(y)
    println("f:" + f)
  }
  val search = GradientDescent(Array(x,y),
    selectVars = 0L to 1L,
    variableIndiceToDeltaForGradientDefinition = _ => 10L,
    linearSearch = new NarrowingStepSlide(3L, minStep = 1L),
    trySubgradient = true)

  search.verbose = 2

  search.doAllMoves(obj = obj)

  printModel()
}
