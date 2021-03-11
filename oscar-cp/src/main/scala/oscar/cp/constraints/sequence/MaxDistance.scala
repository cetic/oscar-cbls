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

package oscar.cp.constraints.sequence

import oscar.algo.graph._
import oscar.algo.reversible._
import oscar.cp.core.variables._
import oscar.cp.core._

/**
 * @author Charles Thomas (cftmthomas@gmail.com)
 *
 * Maximum Distance constraint:
 * distance = sum of transitions of sequence
 *
 * @param sequence A sequence variable
 * @param distance An Int variable
 * @param transitions A transition matrix between elements of sequence
 */
abstract class MaxDistance(sequence: CPSeqVar, distance: CPIntVar, transitions: Array[Array[Int]]) extends Constraint(sequence.store){
  override def associatedVars(): Iterable[CPVar] = Array(sequence, distance)

  protected val currentDist = new ReversibleInt(sequence.store, 0)
  protected def maxDist: Int = distance.max

  protected var computeExactBound = false

  override def setup(l: CPPropagStrength): Unit = {
    //Setting up propagation strength:
//    l match {
//      case CPPropagStrength.Weak => computeExactBound = false
//      case _ => computeExactBound = true
//    }

    propagate() //Initial propagation
    distance.callPropagateWhenBoundsChange(this)
    sequence.callPropagateWhenDomainChanges(this)
  }
}

class MaxDistanceHead(sequence: CPHeadSeqVar, distance: CPIntVar, transitions: Array[Array[Int]]) extends MaxDistance(sequence, distance, transitions){

  private def updateDist(visits: Iterator[Int] = sequence.allMembers.iterator): Unit = {
    if(visits.isEmpty) return
    var prevVisit = visits.next()
    var d = 0
    for(newVisit <- visits){
      d += transitions(prevVisit)(newVisit)
      prevVisit = newVisit
    }
    currentDist.setValue(d)
  }

  override def propagate(): Unit = {
    updateDist()

    if(sequence.isBound){
      distance.assign(currentDist.value)
      deactivate()
    } else {
      //Computing lower bound for distance between mandatory elements:
      val mandatoryActs = sequence.allRequiredNotMember.toSeq
      val (mandatoryEdges, mandatoryDist) = GraphUtils.kruskal(mandatoryActs, transitions)
      distance.updateMin(currentDist.value + mandatoryDist)

      //Excluding possibles activities if dist too long:
      for (actId <- sequence.allAppendableNext) {
        //Check with arbitrary edge that connection with mst is under upper bound:
        if (sequence.nonEmpty && currentDist.value + mandatoryDist + transitions(sequence.lastAppended)(actId) > distance.max) {
          //If not: computing mst with new activity:
          val edges = (
            mandatoryEdges ++ mandatoryActs.flatMap(a => Seq((a, actId), (actId, a)))
            ).sortBy { case (i: Int, j: Int) => transitions(i)(j) }
          val (_, actDist) = GraphUtils.kruskal(mandatoryActs ++ Seq(actId), edges, transitions)
          if (currentDist.value + actDist > distance.max) sequence.excludes(actId)
        }
      }
    }
  }
}

class MaxDistanceInsert(sequence: CPInsertSeqVar, distance: CPIntVar, transitions: Array[Array[Int]]) extends MaxDistance(sequence, distance, transitions){

  private def evaluateSeqPath(seq: Seq[Int]): (Seq[(Int, Int)], Int) = {
    if(seq.isEmpty) return (Seq(), 0)

    val edges = seq.zip(seq.drop(1))
    val dist = edges.map(edge => transitions(edge._1)(edge._2)).sum
    (edges, dist)
  }

  override def propagate(): Unit = {
    val seq = sequence.allMembers
    val (seqPath, seqDist) = evaluateSeqPath(seq)
    currentDist.setValue(seqDist)
    distance.updateMin(seqDist)

    if(sequence.isBound){
      distance.updateMax(currentDist.value)
      deactivate()
    } else {
      for((elem, preds) <- sequence.currentInsertionsPerElem){
        for(pred <- preds){
          val next = sequence.nextMember(pred)

          //Computing distance cost of inserting element at given position:
          val diff = if(sequence.isEmpty) 0
          else if(pred == -1) transitions(elem)(next)
          else if(next == -1) transitions(pred)(elem)
          else transitions(pred)(elem) + transitions(elem)(next) - transitions(pred)(next)

          //Removing insertion if violation:
          if(currentDist.value + diff > distance.max) sequence.removeInsertion(elem, pred)
        }
      }

      //Computing mandatory activities and possible edges:
      if(sequence.requiredSize > sequence.length){
        val mandatoryActs = sequence.allRequiredNotMember.toSeq
        val possibleEdges = (
          seqPath ++
            mandatoryActs.map(act => (act, sequence.allCurrentInsertionsFor(act))).flatMap{ case (act, prevs) =>
              prevs.flatMap(prev => Seq((prev, act), (act, sequence.nextMember(prev))))
            } ++
            mandatoryActs.flatMap(a => mandatoryActs.map((a, _)))
          ).filterNot { case (_, j: Int) => j == -1 }
          .sortBy { case (i: Int, j: Int) => transitions(i)(j) }

        //Computing lower bound for distance between mandatory elements:
        val (mandatoryEdges, mandatoryDist) = GraphUtils.kruskal(seq ++ mandatoryActs, possibleEdges, transitions)
        distance.updateMin(mandatoryDist)

        //TODO: activate this if strong filtering
//        //Excluding possibles activities if dist too long:
//        for ((actId, prevIds) <- sequence.insertionsPerElem) {
//          //Check with arbitrary edge that connection with mst is under upper bound:
//          if (currentDist.value + mandatoryDist + transitions(actId)(0) > distance.max) {
//            //If not: computing mst with new activity:
//            val edges = (
//              mandatoryEdges ++
//                prevIds.flatMap(prev => Seq((prev, actId), (actId, sequence.nextMember(prev)))) ++
//                mandatoryActs.flatMap(a => Seq((a, actId), (actId, a)))
//              ).sortBy { case (i: Int, j: Int) => transitions(i)(j) }
//            val (_, actDist) = GraphUtils.kruskal(seq ++ mandatoryActs :+ actId, edges, transitions)
//            if (actDist > distance.max) sequence.excludes(actId)
//          }
//        }
      }
    }
  }
}

object MaxDistance {
  def apply(sequence: CPHeadSeqVar, distance: CPIntVar, transitions: Array[Array[Int]]): MaxDistance =
    new MaxDistanceHead(sequence, distance, transitions)

  def apply(sequence: CPInsertSeqVar, distance: CPIntVar, transitions: Array[Array[Int]]): MaxDistance =
    new MaxDistanceInsert(sequence, distance, transitions)
}
