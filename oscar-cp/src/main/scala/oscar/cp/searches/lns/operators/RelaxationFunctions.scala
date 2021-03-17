package oscar.cp.searches.lns.operators

import oscar.algo.Inconsistency
import oscar.cp.{CPIntVar, CPSolver}
import oscar.cp.searches.lns.CPIntSol

import scala.collection.mutable
import scala.util.Random


object RelaxationFunctions {

  /**
    * Relaxes randomly k variables.
    * @param k The number of variables to relax (must be >= 0 and < vars.size)
    */
  def randomRelax(solver: CPSolver, vars: Iterable[CPIntVar], currentSol: CPIntSol, k: Int): Unit = {
    val varSeq = vars.toSeq
    val varArray = varSeq.indices.toArray //map to real indice of variable
    var boundStart = varArray.length //Elements of varArray from this index are bound

    while(vars.count(!_.isBound) > k){
      val i = Random.nextInt(boundStart) //Selecting new var to freeze randomly
      val x = varArray(i)
      solver.add(varSeq(x) === currentSol.values(x))
      if(! varSeq(x).isBound) throw Inconsistency

      //marking var as bound:
      boundStart -= 1
      varArray(i) = varArray(boundStart)
      varArray(boundStart) = x
    }

//    println("relaxation done, " + (vars.size - k) + " vars frozen")
  }

  /**
    * Relaxes randomly k successive variables.
    * @param k The number of variables to relax (must be >= 0 and < vars.size)
    */
  def successiveRelax(solver: CPSolver, vars: Iterable[CPIntVar], currentSol: CPIntSol, k: Int): Unit = {
  //    println("relaxing " + k + " variables")
      val varSeq = vars.toSeq
      var i = Random.nextInt(vars.size)-1 //start of the relaxed sequence (inclusive)
      if(i < 0) i = varSeq.length-1

      while(i > 0 && vars.count(!_.isBound) > k) {
        solver.add(varSeq(i) === currentSol.values(i))
        if (!varSeq(i).isBound) throw Inconsistency

        i -= 1
        if(i < 0) i = varSeq.length-1
      }

  //    println("relaxation done, " + (varSeq.length - k) + " vars frozen")
  }

  /**
    * Relaxes randomly n sequences of k successive variables.
    * @param n The number of sequences of variables to relax
    * @param k The number of variables per sequence to relax (n * k must be >= 0 and < vars.size)
    */
  def seqRelax(solver: CPSolver, vars: Iterable[CPIntVar], currentSol: CPIntSol, n: Int, k: Int): Unit = {
    //TODO
    ???
  }

  /**
    * Relaxes variables using propagation to guide the relaxation until the estimated size s of the neighbourhood is attained.
    * @param s The estimated size of the neighbourhood to attain.
    */
  def propagationGuidedRelax(solver: CPSolver, vars: Iterable[CPIntVar], currentSol: CPIntSol, s: Double): Unit =
    PropagationGuidedRelax.propagationGuidedRelax(solver, vars, currentSol, s)

  /**
    * Relaxes variables using propagation to guide the relaxation until the estimated size s of the neighbourhood is attained.
    * @param s The estimated size of the neighbourhood to attain.
    */
  def reversedPropagationGuidedRelax(solver: CPSolver, vars: Iterable[CPIntVar], currentSol: CPIntSol, s: Double): Unit =
    PropagationGuidedRelax.reversePropagationGuidedRelax(solver, vars, currentSol, s)

  /**
    * Relaxes variables using their values to guide the relaxation.
    * @param k The number of variables to relax (must be >= 0 and < vars.size)
    * @param scheme The way to relax variables. Possible keys:
    *               Random: selects the groups randomly
    *               MaxGroup: relaxes first the biggest groups of variables having the same value
    *               MinGroup: relaxes first the smallest groups of variables having the same values
    *               MaxVal: relaxes first the groups of variables having the biggest values
    *               MinVal: relaxes first the groups of variables having the smallest values
    */
  def valueGuidedRelax(solver: CPSolver, vars: Iterable[CPIntVar], currentSol: CPIntSol, k: Int, scheme: String): Unit = {

    //Identifying groups of variables
    val varGroups = mutable.HashMap[Int, mutable.HashSet[Int]]()
    val varSeq: Seq[CPIntVar] = vars.toSeq
    var i = 0
    while(i < varSeq.length){
      val value = currentSol.values(i)
      if (varGroups.contains(value)) varGroups(value) += i
      else varGroups += value -> mutable.HashSet[Int](i)
      i += 1
    }

    val varList: Seq[Int] = scheme match{

      case "RandomGroups" => Random.shuffle(varGroups.toSeq).flatMap(_._2)

      case "MaxGroups" => varGroups.toSeq.sortWith(_._2.size < _._2.size).flatMap(_._2)

      case "MinGroups" => varGroups.toSeq.sortWith(_._2.size > _._2.size).flatMap(_._2)

      case "MaxVal" => varGroups.toSeq.sortWith(_._1 < _._1).flatMap(_._2)

      case "MinVal" => varGroups.toSeq.sortWith(_._1 > _._1).flatMap(_._2)

      case "MinMaxVal" =>
        val sortedVars = varGroups.toSeq.sortWith(_._1 > _._1).flatMap(_._2)
        val minMaxVars = mutable.ListBuffer[Int]()
        var i = 0
        var j = sortedVars.length - 1
        while(i < j){
          minMaxVars += sortedVars(i)
          minMaxVars += sortedVars(j)
          i += 1
          j -= 1
        }
        if(i == j) minMaxVars += sortedVars(i)
        sortedVars
    }

    var j = 0
    while(j < varSeq.length && vars.count(!_.isBound) > k){
      val x = varList(j)
      solver.add(varSeq(x) === currentSol.values(x))
      if(! varSeq(x).isBound) throw Inconsistency
      j += 1
    }
  }

  def randomGroupsRelax(solver: CPSolver, vars: Iterable[CPIntVar], currentSol: CPIntSol, k: Int): Unit =
    valueGuidedRelax(solver, vars, currentSol, k, "RandomGroups")

  def maxGroupsRelax(solver: CPSolver, vars: Iterable[CPIntVar], currentSol: CPIntSol, k: Int): Unit =
    valueGuidedRelax(solver, vars, currentSol, k, "MaxGroups")

  def minGroupsRelax(solver: CPSolver, vars: Iterable[CPIntVar], currentSol: CPIntSol, k: Int): Unit =
    valueGuidedRelax(solver, vars, currentSol, k, "MinGroups")

  def maxValRelax(solver: CPSolver, vars: Iterable[CPIntVar], currentSol: CPIntSol, k: Int): Unit =
    valueGuidedRelax(solver, vars, currentSol, k, "MaxVal")

  def minValRelax(solver: CPSolver, vars: Iterable[CPIntVar], currentSol: CPIntSol, k: Int): Unit =
    valueGuidedRelax(solver, vars, currentSol, k, "MinVal")

  def minMaxValRelax(solver: CPSolver, vars: Iterable[CPIntVar], currentSol: CPIntSol, k: Int): Unit =
    valueGuidedRelax(solver, vars, currentSol, k, "MinMaxVal")


  /**
    * Makes the hypothesis of a predecessor/successor model: relaxes a sequence of k variables by using their value to find the
    * next variable in the sequence. Reordering is only allowed within the relaxed sequence.
    * @param k The number of variables to relax (must be >= 0 and < vars.size)
    */
  def predRelaxSeqFixed(solver: CPSolver, vars: Iterable[CPIntVar], currentSol: CPIntSol, k: Int): Unit = {
    val varSeq = vars.toSeq
    val varArray = varSeq.indices.toArray //map to real indice of variable
    val mapToIdx = varArray.indices.toArray //map to indice of variable in varArray
    var relaxStart = varArray.length //Elements of varArray from this index are to be relaxed
    var lastVal = -1

    while(varArray.length - relaxStart < k){
      val x = if(lastVal >= 0 && lastVal < relaxStart) lastVal else varArray(Random.nextInt(relaxStart))
      lastVal = currentSol.values(x)

      //marking var as to relax:
      relaxStart -= 1
      val i = mapToIdx(x)
      varArray(i) = varArray(relaxStart)
      mapToIdx(varArray(relaxStart)) = i
      varArray(relaxStart) = x
      mapToIdx(x) = relaxStart
    }

    var i = 0
    while(vars.count(!_.isBound) > k) {
      val x = varArray(i)
      solver.add(varSeq(x) === currentSol.values(x))
      if (!varSeq(x).isBound) throw Inconsistency
      i += 1
    }
  }

  /**
    * Makes the hypothesis of a predecessor/successor model: relaxes k edges and allows change of direction between
    * fixed paths.
    * @param k The number of edges (variables) to relax (must be >= 0 and <= vars.size)
    */
  def predRelaxKopt(solver: CPSolver, vars: Iterable[CPIntVar], currentSol: CPIntSol, k: Int): Unit = {
    //Computing predecessors:
    val pred = Array.fill[Int](vars.size)(-1)
    currentSol.values.filter(x => x >= 0 && x < vars.size).zipWithIndex.foreach{ case(x,i) => pred(x) = i }

    val varSeq = vars.toSeq
    val varArray = varSeq.indices.toArray //map to real indice of variable
    var relaxStart = varArray.length //Elements of varArray from this index are to be relaxed

    val endPoints = mutable.Set[Int]()

    while(varArray.length - relaxStart < k){
      val i = Random.nextInt(relaxStart)
      val x = varArray(i)

      //marking var as to relax:
      relaxStart -= 1
      varArray(i) = varArray(relaxStart)
      varArray(relaxStart) = x

      //adding self and sucessor as endpoints (extremities of a seq of connected edges):
      endPoints += x
      val successor = currentSol.values(x)
      if(successor >= 0 && successor < varSeq.length) endPoints += successor
    }

    //Fixing other vars (allowing existing edges inversion)
    (0 until relaxStart).foreach(i => {
      val x = varArray(i)
      var valSet = Set(currentSol.values(x)) //Adding successor as potential val
      if(pred(x) != -1) valSet += pred(x) //Adding predecessor as potential val
      if(endPoints.contains(x)) valSet ++= endPoints //If endpoint, other endpoints are potential vals
      solver.add(varSeq(x) in valSet)
    })
  }

  def precedencyRelax(solver: CPSolver, vars: Iterable[CPIntVar], currentSol: CPIntSol, k: Int): Unit = {
    //selecting vars to relax
    val varSeq = vars.toSeq
    val varArray = varSeq.indices.toArray //map to real indice of variable
    var boundStart = varArray.length //Elements of varArray from this index will be relaxed

    while(boundStart > varArray.length - k){
      val i = Random.nextInt(boundStart) //Selecting new var to relax randomly
      val x = varArray(i)

      //marking var as bound:
      boundStart -= 1
      varArray(i) = varArray(boundStart)
      varArray(boundStart) = x
    }

    //Precedencies:
    val precedencies = varArray
      .dropRight(k) //Removing k last vars that have to be relaxed
      .map(i => (currentSol.values(i), i)) //Zipping with val
      .groupBy(_._1)//Grouping by values
      .toArray.sortBy(_._1) //Sorting by values
      .map(_._2.map(_._2).toSet) //Formatting into array of sets

    //Adding constraints:
    var i = 0
    var j = 1
    while(j < precedencies.length){
      for(prev <- precedencies(i); next <- precedencies(j)){
        solver.add(varSeq(prev) <= varSeq(next))
      }
      i += 1
      j += 1
    }
  }

//  /**
//    * TODO
//    */
//  def valWindowRelax(solver: CPSolver, vars: Iterable[CPIntVar], currentSol: CPIntSol, lr: Double, ur: Double): Unit = {
//    vars.zipWithIndex.foreach{case(x, i) =>{
//      val sTime = currentSol.values(i)
//      val lb = sTime - (lr * (sTime - x.min)).floor.toInt
//      val ub = sTime + (ur * (x.max - sTime)).ceil.toInt
//      solver.add(x >= lb)
//      solver.add(x <= ub)
//    }}
//  }
}
