package oscar.cp.searches.lns.operators

import oscar.algo.Inconsistency
import oscar.algo.reversible.ReversibleInt
import oscar.cp.{CPIntVar, CPSolver}
import oscar.cp.searches.lns.CPIntSol

import scala.collection.mutable
import scala.util.Random

/**
 * Performs a Propagation Guided Relaxation (see Propagation Guided Large Neighborhood Search - Perron 2004)
 *
 * @author Charles Thomas cftmthomas@gmail.com
 */
class PropagationGuidedRelax(variables: Seq[CPIntVar]){
  val nVars: Int = variables.length

  //Data structure to keep track of domain size:
  val startDomainSize: Array[Int] = Array.tabulate(nVars)(i => variables(i).size)
  val prevDomainSize: Array[ReversibleInt] = Array.tabulate(nVars)(i => ReversibleInt(variables(i).size)(variables(i).store))

  //Data structure to keep track of closeness:
  lazy val clStore: ClosenessStore = if(variables.length > 1000000) new MapClosenessStore else new ArrayClosenessStore(variables.length)

  /**
    * Updates the closeness store with the propagation given an instantiated variable.
    * @param instantiated the var that has been instantiated.
    */
  def updateCloseness(instantiated: Int): Unit ={
    for(i <- variables.indices) if(i != instantiated){
      val currentDomSize = variables(i).size

      //If domain size has changed:
      if(currentDomSize < prevDomainSize(i).value){
        val impact = (prevDomainSize(i) - currentDomSize).toDouble / startDomainSize(i)

        //Updating closeness:
        val oldCloseness = clStore.getCloseness(instantiated)(i)
        val oldImpact = clStore.nImpacted(instantiated)(i)
        clStore.updateEdge(instantiated, i, oldImpact + 1, (oldCloseness * oldImpact + impact) / (oldImpact + 1))

        prevDomainSize(i).setValue(currentDomSize)
      }
    }
//    println("closeness:\n" + clStore.toString)
  }

  /**
   * Updates the closeness store with the propagation given an instantiated variable.
   * @param instantiated the var that has been instantiated.
   * @return the index of the most impacted unbound variable (-1 if no variable has been impacted)
   */
  def updateClosenessAndGetMostImpacted(instantiated: Int): Int ={
    var maxImpact = 0.0
    var mostImpacted = -1

    for(i <- variables.indices) if(i != instantiated){
      val currentDomSize = variables(i).size

      //If domain size has changed:
      if(currentDomSize < prevDomainSize(i).value){
        val impact = (prevDomainSize(i) - currentDomSize).toDouble / startDomainSize(i)

        //Updating closeness:
        val oldCloseness = clStore.getCloseness(instantiated)(i)
        val oldImpact = clStore.nImpacted(instantiated)(i)
        clStore.updateEdge(instantiated, i, oldImpact + 1, (oldCloseness * oldImpact + impact) / (oldImpact + 1))

        //Checking impact:
        if(!variables(i).isBound && impact > maxImpact){
          mostImpacted = i
          maxImpact = impact
        }

        prevDomainSize(i).setValue(currentDomSize)
      }
    }
//    println("closeness:\n" + clStore.toString)

    mostImpacted
  }

  /**
   * Returns the most impacted unbound variable.
   * @param instantiated the var that has been instantiated.
   * @return the index of the most impacted unbound variable (-1 if no variable has been impacted)
   */
  def getMostImpacted(instantiated: Int): Int ={
    var maxImpact = 0.0
    var mostImpacted = -1

    for(i <- variables.indices) if(i != instantiated){
      val currentDomSize = variables(i).size

      //If domain size has changed:
      if(currentDomSize < prevDomainSize(i).value){
        val impact = (prevDomainSize(i) - currentDomSize).toDouble / startDomainSize(i)

        //Checking impact:
        if(!variables(i).isBound && impact > maxImpact){
          mostImpacted = i
          maxImpact = impact
        }

        prevDomainSize(i).setValue(currentDomSize)
      }
    }

    mostImpacted
  }

  /**
    * Get the close vars (closeness > 0) of i.
    * @return a list of the close vars of i (empty if no var is close or i is not a valid index).
    */
  def getClose(i: Int): Iterable[Int] = clStore.allCloseness(i).map{case (v, _) => v}

  /**
    * returns the close subset to which initalVar is a part of.
    * @return all the vars in the subset excluding initial var (might be empty).
    */
  def getCloseSubset(initialVar: Int, maxSubsetSize: Int): mutable.LinkedHashSet[Int]= {
    val subset = new mutable.LinkedHashSet[Int]  //Current subset
    if(initialVar >= nVars) return subset

    val closeToSubset = Array.fill[Double](nVars){0.0}
    val toAddNext = new mutable.PriorityQueue[Int]()(Ordering.by[Int, Double](i => closeToSubset(i)))
    for(v <- getClose(initialVar)){
      closeToSubset(v) = clStore.getCloseness(initialVar)(v)
      toAddNext += v
    }

    while(subset.size < maxSubsetSize && toAddNext.nonEmpty){
      val v = toAddNext.dequeue()
      if(v != initialVar && !subset.contains(v)){
        subset += v
        for(x <- getClose(v)) if(!subset.contains(v) && clStore.getCloseness(v)(x) > closeToSubset(x)){
          closeToSubset(x) = clStore.getCloseness(v)(x)
          toAddNext += x
        }
      }
    }

    subset
  }

  /**
   * Relaxes variables using propagation to guide the relaxation until the estimated size s of the neighbourhood is attained.
   * @param s The estimated size of the neighbourhood to attain.
   */
  def propagationGuidedRelax(solver: CPSolver, currentSol: CPIntSol, s: Double, updateCloseness: Boolean = false): Unit = {
    //    println("relaxing to size " + s)
    val varArray = Array.tabulate(variables.length)(i => i) //map to real index of variables
    var boundStart = varArray.length //Elements of varArray from this index are bound
    var size = variables.map(v => math.log(v.size)).sum //Current estimation of the search space obtained
    var toFreezeNext = -1 //next var to freeze (most impacted by previous propagation)

    while (size > s) {

      val next = if (toFreezeNext == -1) varArray(Random.nextInt(boundStart)) //If no var to freeze next, selecting random var
      else toFreezeNext
      solver.add(variables(next) === currentSol.values(next)) //Freezing var => propagation occurs
      // propagation should be called as var is frozen
      if (!variables(next).isBound) throw Inconsistency

      //Updating closeness and next var to freeze:
      toFreezeNext = if(updateCloseness) updateClosenessAndGetMostImpacted(next) else getMostImpacted(next)

      //Updating size and bounded vars:
      size = 0.0
      var i = 0
      while (i < boundStart) {
        val x = varArray(i)
        size += math.log(variables(x).size) //Updating size

        //Marking var as bound:
        if (variables(x).isBound) {
          boundStart -= 1
          varArray(i) = varArray(boundStart)
          varArray(boundStart) = x
        }
        else i += 1
      }
    }
//    println("relaxation done, " + (variables.length - boundStart) + " vars frozen")
  }

  /**
   * Relaxes variables using propagation to guide the relaxation until the estimated size s of the neighbourhood is attained.
   * @param s The estimated size of the neighbourhood to attain.
   */
  def reversePropagationGuidedRelax(solver: CPSolver, currentSol: CPIntSol, s: Double): Unit = {
    //    println("relaxing to size " + s)
    val varArray = Array.tabulate(variables.length)(i => i) //map to real index of variables

    var next = Random.nextInt(variables.length) //Selecting randomly first var to relax
    varArray(next) = variables.length-1
    varArray(variables.length-1) = next

    var relaxStart = varArray.length-1 //Elements of varArray from this index are part of the relaxed variables
    var avgSize = variables(next).size
    var size = math.log(avgSize) //Current estimation of the search space obtained
    var subset = getCloseSubset(next, (math.round(s - size) / avgSize).toInt) //Subset of next

    while (size < s && relaxStart > 0) {

      if(subset.isEmpty){ //No more element in subset:
        next = Random.nextInt(relaxStart) //Selecting new random var as next
        subset = getCloseSubset(next, 10) //Retrieving subset of this var
      }
      else{
        next = subset.head
        subset -= next
      }

      relaxStart -= 1
      varArray(next) = varArray(relaxStart)
      varArray(relaxStart) = next
      val relaxed = variables(next).size
      avgSize = (avgSize * (variables.length - relaxStart - 1) + relaxed) / (variables.length - relaxStart)
      size += math.log(relaxed)
    }

    for(i <- (0 until relaxStart).map(x => varArray(x))){
      solver.add(variables(i) === currentSol.values(i)) //Freezing var => propagation occurs
      if (!variables(i).isBound) throw Inconsistency
      updateCloseness(i)
    }
//    println("relaxation done, " + relaxStart + " vars frozen")
  }
}

object PropagationGuidedRelax{
  var varSeq: Seq[CPIntVar] = Seq()
  lazy val propGuidedEngine: PropagationGuidedRelax = new PropagationGuidedRelax(varSeq) // Propagation guided relax engine

  /**
   * Relaxes variables using propagation to guide the relaxation until the estimated size s of the neighbourhood is attained.
   * @param s The estimated size of the neighbourhood to attain.
   * @param updateCloseness Set to true to maintain and update an internal closeness store if this relaxation is to be used conjointly with reverse PGLNS.
   */
  def propagationGuidedRelax(solver: CPSolver, vars: Iterable[CPIntVar], currentSol: CPIntSol, s: Double, updateCloseness: Boolean = false): Unit = {
    if(varSeq.isEmpty) varSeq = vars.toSeq
    propGuidedEngine.propagationGuidedRelax(solver, currentSol, s, updateCloseness)
  }

  /**
   * Relaxes variables using propagation to guide the relaxation until the estimated size s of the neighbourhood is attained.
   * @param s The estimated size of the neighbourhood to attain.
   */
  def reversePropagationGuidedRelax(solver: CPSolver, vars: Iterable[CPIntVar], currentSol: CPIntSol, s: Double): Unit = {
    if(varSeq.isEmpty) varSeq = vars.toSeq
    propGuidedEngine.reversePropagationGuidedRelax(solver, currentSol, s)
  }
}

/**
 * This class keeps track of the closeness relationship between variables. The closeness is defined as the average
 * volume of propagation that is involved on one variable when another is instantiated.
 */
abstract class ClosenessStore{
  /**
   * Get the number of times that variable j has been impacted by an assignment of i.
   */
  def nImpacted(i: Int)(j: Int): Int

  def allImpacted(i: Int): Iterable[(Int, Int)]

  /**
   * Get the closeness between vars i and j.
   * @return the closeness between i and j or 0.0 if i or j are not valid indices.
   */
  def getCloseness(i: Int)(j: Int): Double

  def allCloseness(i: Int): Iterable[(Int, Double)]

  /**
   * Get the close vars (closeness > 0) of i.
   * @return a list of the close vars of i (empty if no var is close or i is not a valid index).
   */
  def getCloseVars(i: Int): Iterable[Int] = allCloseness(i).map{case (v, _) => v}

  def updateEdge(i: Int, j: Int, newImpact: Int, newCloseness: Double): Unit

  def updateEdge(i: Int, j: Int, newImpact: Int): Unit = updateEdge(i, j, newImpact, getCloseness(i)(j))
  def updateEdge(i: Int, j: Int, newCloseness: Double): Unit = updateEdge(i, j, nImpacted(i)(j), newCloseness)
}

class ArrayClosenessStore(size: Int) extends ClosenessStore{
  private val impacted: Array[Array[Int]] = Array.fill(size, size)(0)
  private val closeness: Array[Array[Double]] = Array.fill(size, size)(0.0)

  override def nImpacted(i: Int)(j: Int): Int = impacted(i)(j)

  override def allImpacted(i: Int): Iterable[(Int, Int)] = impacted(i).zipWithIndex.filter(_._1 > 0).map(_.swap)

  override def getCloseness(i: Int)(j: Int): Double = closeness(i)(j)

  override def allCloseness(i: Int): Iterable[(Int, Double)] = closeness(i).zipWithIndex.filter(_._1 > 0.0).map(_.swap)

  override def updateEdge(i: Int, j: Int, newImpact: Int, newCloseness: Double): Unit = {
    impacted(i)(j) = newImpact
    closeness(i)(j) = newCloseness
  }

  override def updateEdge(i: Int, j: Int, newImpact: Int): Unit = impacted(i)(j) = newImpact

  override def updateEdge(i: Int, j: Int, newCloseness: Double): Unit = closeness(i)(j) = newCloseness

  override def toString: String = {
    closeness.indices.map(i => {
      closeness(i).indices.map(j => "(" + closeness(i)(j) + ", " + impacted(i)(j) + ")").mkString("  ")
    }).mkString("\n")
  }
}

class MapClosenessStore extends ClosenessStore{
  private val edges: mutable.Map[Int, mutable.Map[Int, (Int, Double)]] = mutable.Map()

  override def nImpacted(i: Int)(j: Int): Int = {
    if(edges.contains(i)) edges(i).getOrElse(j, (0, 0.0))._1
    else 0
  }

  override def allImpacted(i: Int): Iterable[(Int, Int)] = {
    if(edges.contains(i)) edges(i).map{case (key, value) => (key, value._1)}
    else Seq()
  }

  override def getCloseness(i: Int)(j: Int): Double = {
    if(edges.contains(i)) edges(i).getOrElse(j, (0, 0.0))._2
    else 0.0
  }

  override def allCloseness(i: Int): Iterable[(Int, Double)] = {
    if(edges.contains(i)) edges(i).map{case (key, value) => (key, value._2)}
    else Seq()
  }

  override def updateEdge(i: Int, j: Int, newImpact: Int, newCloseness: Double): Unit = {
    val map = edges.getOrElseUpdate(i, mutable.Map[Int, (Int, Double)]())
    map += j -> (newImpact, newCloseness)
  }

  override def toString: String = {
    edges.map{case (i, subMap) => {
      i + ": " + subMap.map{case (j, (imp, cl)) => j + ":(" + cl + ", " + imp + ")"}.mkString("  ")
    }}.mkString("\n")
  }
}
