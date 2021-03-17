package oscar.cp.searches.lns.operators

import oscar.algo.Inconsistency
import oscar.cp.{CPIntVar, CPSolver}
import oscar.cp.searches.lns.CPIntSol

import scala.collection.mutable
import scala.util.Random

class CostImpact(solver: CPSolver, vars: Iterable[CPIntVar], n: Int, alpha: Double){
  val variables: Array[CPIntVar] = vars.toArray
  val costImpacts: Array[Double] = Array.fill[Double](variables.length)(0.0)
  var divesPerformed = 0

  solver.onSolution(if(divesPerformed > 0) clear())

  def clear(): Unit ={
    costImpacts.transform(_ => 0.0)
    divesPerformed = 0
  }

  def dive(sol: CPIntSol): Unit = {
    val objVar: CPIntVar = solver.objective.objs.head.objVar
    var objSize = objVar.size
    solver.pushState()
    val permutation = Random.shuffle[Int, mutable.IndexedSeq[Int]](variables.indices.toArray).sortBy(-costImpacts(_))
    var i = 0
    while(i < permutation.length && !objVar.isBound){
      val x = permutation(i)
      solver.add(variables(x) === sol.values(x))
      val newSize = objVar.size
      costImpacts(x) += (objSize - newSize)
      objSize = newSize
      i += 1
    }
    solver.pop()
  }

  def performDives(sol: CPIntSol, n: Int): Unit = {
    var i = n
    while(i > 0) {
      dive(sol)
      divesPerformed += 1
      i -= 1
    }
  }

  def costImpactRelax(currentSol: CPIntSol, k: Int): Unit = {
    //Computing selection probabilites:
//    println("starting dive")
    performDives(currentSol, n)
//    println("dive done")
    val costImpactAverages = costImpacts.map(_/divesPerformed)
//    println("avg impacts: " + costImpactAverages.mkString(","))
    val avgImpactsSum = costImpactAverages.sum
    val selectProbas = Array.tabulate[Double](variables.length)(i => {
      alpha * costImpactAverages(i) + (1 - alpha) * (avgImpactsSum/variables.length)
    })
//    println("select probas: " + selectProbas.mkString(","))

    //relaxing variables:
    var r = selectProbas.sum
    val varArray = variables.indices.toArray //map to real indice of variable
    var relaxStart = varArray.length //Elements of varArray from this index are to be relaxed

    while(varArray.length - relaxStart < k){
      var v = Random.nextDouble() * r
      var i = 0
      while(i < relaxStart && v > 0){
        v -= selectProbas(varArray(i))
        i += 1
      }
      if(i > 0) i -= 1

      val x = varArray(i)
      r -= selectProbas(x)

      //marking var as relaxed:
      relaxStart -= 1
      varArray(i) = varArray(relaxStart)
      varArray(relaxStart) = x
    }

    //binding remaining vars:
    var i = 0
    while(i < relaxStart){
      val x = varArray(i)
      solver.add(variables(x) === currentSol.values(x))
      if(!variables(x).isBound) throw Inconsistency
      i += 1
    }
  }
}
