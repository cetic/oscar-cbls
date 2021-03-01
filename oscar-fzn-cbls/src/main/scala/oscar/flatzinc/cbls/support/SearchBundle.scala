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
/**
 * @author Gustav Björdal
 * @author Jean-Noël Monette
 */
package oscar.flatzinc.cbls.support

import oscar.cbls.core.computation.CBLSIntVar
import oscar.cbls.core.constraint.{ConstraintSystem}
import oscar.cbls.lib.invariant.logic._
import oscar.flatzinc.cbls.FZCBLSModel
import oscar.cbls.core.computation.IntValue
import oscar.cbls.lib.search.LinearSelectors

import scala.collection.mutable




abstract class SearchProcedure extends LinearSelectors{
  
  def run(): Unit
  def selectMinImb[R,S](r: Iterable[R] , s: R => Iterable[S],f: (R,S) => Int, st: ((R,S) => Boolean) = ((r:R, s:S) => true)): (R,S) = {
    //TODO: check that it is fine
    val flattened:Iterator[(R,S)] = for (rr <- r.toIterator; ss <- s(rr).toIterator) yield (rr,ss)
    selectMin[(R,S)](flattened.toIterable)((rands:(R,S)) => {/*Console.err.println(rands);*/ f(rands._1,rands._2)}, (rands:(R,S)) => st(rands._1,rands._2))
  }
  
  def showViolatedConstraints(c: ConstraintSystem): Unit ={
    for(cc <- c.violatedConstraints){
      println(cc + " "+cc.violation.value)
    }
  }
}

class Chain(val a: SearchProcedure*) extends SearchProcedure {
  def run() = {
    a.foreach(_.run())
  }
}

class ActionSearch(action:() => Unit) extends SearchProcedure {
  def run() = action()
}
class FakeSearch extends SearchProcedure {
  def run() = {}
}

class GreedySearch(val level: Int = 1, val m: FZCBLSModel,val sc: SearchControl) extends SearchProcedure {
  assert(level > 0 && level < 4)
  val log = m.log
  def run(): Unit ={
    var it = 0;
    log("Starting Greedy Search level "+level)
    log("Starting Violation: "+m.objective.violation.value)
    //val varval = for (x <- m.vars; v <- x.domain) yield (x,v)
    if(m.vars.length>0){
      sc.handlePossibleSolution();
      var cont = true;
      while(cont && !sc.stop()){
        val varval = for (x <- m.vars.filter(m.c.violation(_).value>0); v <- x.domain) yield (x,v)
        val cur = m.objective.objective.value
        val move = level match {
          case 1 => selectFirst(varval.map(List(_)),(vv:List[(CBLSIntVar,Int)]) => cur > m.objective.objective.assignVal(vv))
          case 2 => val x:Iterator[List[(CBLSIntVar,Int)]] = varval.iterator.flatMap(v => varval.iterator.map(v2 => List(v,v2)))
            selectFirst(x.toIterable,(vv:List[(CBLSIntVar,Int)]) => cur > m.objective.objective.assignVal(vv))
          case 3 => val x:Iterator[List[(CBLSIntVar,Int)]] = varval.iterator.flatMap(v1 => varval.iterator.flatMap(v => varval.iterator.map(v2 => List(v1,v,v2))))
            selectFirst(x.toIterable,(vv:List[(CBLSIntVar,Int)]) => cur > m.objective.objective.assignVal(vv))
        }
        if(move != null && m.objective.objective.assignVal(move)<cur){
          move.foreach{ case (x,v) => x := v}
          sc.handlePossibleSolution
          it +=1;
        }else cont = false
      }
    }
    log("Done Greedy Search at "+m.getWatch())
    log("Ending Violation: "+m.objective.violation.value)
    log("Nb Moves: "+it)
  }
}

class SimpleLocalSearch(val m:FZCBLSModel,val sc: SearchControl) extends SearchProcedure {
  val violation: Array[IntValue] = m.vars.map(m.c.violation(_)).toArray;
  val log = m.log
  def run(): Unit ={
    var it = 0;
    if(m.vars.length>0) {
      var improving = 3;
      var i = RandomGenerator.nextInt(violation.length);
      var lastImproved = i;
      var bestObj = Int.MaxValue

      log("Starting Simple Local Search")
      log("Starting Violation: " + m.objective.violation.value)
      if (m.vars.length > 0) {
        sc.handlePossibleSolution();
        while (improving > 0 && !sc.stopOnTime()) {
          val currentVar = m.vars(i);
          if (violation(i).value > 0) {
            //val k = selectMin(currentVar.domain)(k=> m.objective.objective.assignVal(currentVar,k))
            val currentObj = m.objective.objective.value
            val k = if (currentVar.domain.size > 50000) {
              selectMin(RandomGenerator.shuffle(currentVar.domain).take(50000))((k: Int) => m.objective.objective.assignVal(currentVar, k))
            } else {
              selectMin(currentVar.domain)(k => m.objective.objective.assignVal(currentVar, k))
            }
            if (k != currentVar.value) {
              val obj = m.objective.objective.value
              currentVar := k;
              sc.handlePossibleSolution();
              it += 1;
              if (m.objective.objective.value < bestObj) {
                lastImproved = i;
                improving = 3
                bestObj = m.objective.objective.value
              }
            }
          }
          i += 1;
          if (i == m.vars.length) {
            i = 0;
            //log(2,"turned around "+m.objective.objective.value)
          }
          if (i == lastImproved)
            improving -= 1;
        }
      }
    }
    log("Done Simple Local Search at "+m.getWatch())
    log("Ending Violation: "+m.objective.violation.value)
    log("Nb Moves: "+it)
  }
}

class SearchControl(val m: FZCBLSModel, val objLB:Int, val MaxTimeMilli: Int,val stopOnSat:Boolean){

  //TODO: Set the timeout to be runtime based rather than deadline based.
  val searchVariables = m.neighbourhoods.foldLeft(Set.empty[CBLSIntVar])((acc: Set[CBLSIntVar], x: Neighbourhood) => acc ++ x.getVariables().filterNot(_.isInstanceOf[StoredCBLSIntConst])).toArray

  var bestSolution = Array.empty[Int]

  var lastBestKnownObjective = Int.MaxValue
  var bestKnownObjective = Int.MaxValue
  //var bestKnownViolation = Int.MaxValue
  var bestPair = (Int.MaxValue,Int.MaxValue)


  def stopOnTime(): Boolean = {
    m.getWatch() >= MaxTimeMilli //ran out of time
  }

  def stop(): Boolean = {
    val newSolution = bestKnownObjective < lastBestKnownObjective;
    lastBestKnownObjective = bestKnownObjective
    val outOfTime = m.getWatch() >= MaxTimeMilli //ran out of time
    val satProblem = (newSolution && stopOnSat) //sat problem
    val ignoreObj = (newSolution && m.objective.objectiveWeight.value==0)
    val lowestValue = (m.objective.violation.value == 0 && m.objective.getObjectiveValue() == objLB) //reached the lower bound
    val alreadyFoundOpt = bestKnownObjective == objLB
    return outOfTime || satProblem || ignoreObj || lowestValue || alreadyFoundOpt
  }


  def handlePossibleSolution(): Unit ={
    if(m.objective().value < weightedBest){
      bestPair = (m.objective.violation.value,m.objective.getObjectiveValue())
    }
    if(m.objective.violation.value==0 && m.objective.getObjectiveValue() < bestKnownObjective){
      //bestKnownViolation = 0
      bestKnownObjective = m.objective.getObjectiveValue();
      saveBestSolution()
      m.handleSolution();
      if(bestKnownObjective==objLB && !stopOnSat)println("==========")//added !stopOnSat to not print it on Satisfaction problems.
      if(m.objective.getObjectiveValue() != bestKnownObjective){
        //If propagation changed the objective value, we may have a new solution
        handlePossibleSolution()
      }
    }else if(bestKnownObjective == Int.MaxValue){
      m.log("Best Violation: "+m.objective.violation.value+ "\tat "+m.getWatch()+ " ms")
    }
  }
  def weightedBest = bestPair._1 * m.objective.violationWeight.value + bestPair._2 * m.objective.objectiveWeight.value
  def cancelObjective() = {
    m.objective.objectiveWeight := 0;
  }
  def restoreObjective() = {
    m.objective.objectiveWeight := 1;
  }

  def saveBestSolution() = {
    bestSolution = searchVariables.map(_.value)
  }

  def restoreBestSolution() = {
    for( i <- bestSolution.indices){
      searchVariables(i) := bestSolution(i)
    }
  }

}


abstract class NeighbourhoodSearch(val m: FZCBLSModel,val sc: SearchControl) extends SearchProcedure {
  val log = m.log
  val neighbourhoods: List[Neighbourhood] = m.neighbourhoods 
  val searchVariables = neighbourhoods.foldLeft(Set.empty[CBLSIntVar])((acc: Set[CBLSIntVar], x: Neighbourhood) => acc ++ x.getVariables().filterNot(_.isInstanceOf[StoredCBLSIntConst])).toArray
}


abstract class NeighbourhoodTabuSearch(m: FZCBLSModel, sc: SearchControl) extends NeighbourhoodSearch(m,sc){
  
  val variableMap = (0 until searchVariables.length).foldLeft(Map.empty[CBLSIntVar, Int])((acc, x) => acc + (searchVariables(x) -> x));  
  
  val it = CBLSIntVar(m.m, 1, 0 to Int.MaxValue, "it");
  val tabu: Array[CBLSIntVar] = searchVariables.map(v => CBLSIntVar(sc.m.m, 0, 0 to Int.MaxValue,  "Tabu_" + v.name)).toArray;
  val nonTabuVariables = SelectLEHeapHeap(tabu.asInstanceOf[Array[IntValue]], it);
  val MaxTenure = (searchVariables.length * 0.6).toInt
  val MinTenure = 2
  val tenureIncrement = Math.max(1, (MaxTenure - MinTenure) / 10);
  var tenure = MinTenure
  var ecnt = 0;
  var bcnt = 0;
  val baseSearchSize = 100;
  val searchFactor = 20;

  var currentNeighbour = 0

  val visitedStates = mutable.TreeSet.empty[Int]

  var lastTime = System.currentTimeMillis()
  def acceptMove(best: Int,nonTabuSet: Set[CBLSIntVar])(m:Move): Boolean = {
    //changed forall to exists after a suggestion of Gustav.
    m.getModified.exists(nonTabuSet.contains(_)) || m.value < best 
  }

  def acceptVar(nonTabuSet: Set[CBLSIntVar])(v:CBLSIntVar): Boolean ={
    nonTabuSet.contains(v)
  }
  
  def makeMove(extendedSearch: Boolean): Unit ={
    val nonTabuSet = nonTabuVariables.value.map(searchVariables(_));
    val bestValue = sc.weightedBest
    if(extendedSearch) ecnt+=1 else bcnt+=1


    val foo  = m.c.getPostedConstraints.map(_._1).filter(_.violation.value>0)
    //m.printCurrentAssignment

    //log("IT time: " + (System.currentTimeMillis() - lastTime))
    lastTime = System.currentTimeMillis()
    val bestNeighbour = selectMin(neighbourhoods.map((n: Neighbourhood) =>
      if (extendedSearch) {
        n.getExtendedMinObjective(it.value, acceptMove(bestValue,nonTabuSet),acceptVar(nonTabuSet))
      } else {
        n.getMinObjective(it.value, acceptMove(bestValue,nonTabuSet),acceptVar(nonTabuSet))
      }))(_.value)

    if(bestNeighbour!=null){
      //TODO: Aspiration sometimes accepts moves that do not improve but seem to improve because of changing weights. 
      if(log.level > 0 && bestNeighbour.getModified.forall(!nonTabuSet.contains(_))){
        log(2,"Aspiration");
        log(3,bestNeighbour.value.toString +" < "+bestValue.toString +" ; "+m.objective().value)
      }
      log(3,bestNeighbour.toString)
      log(4,tabu.filter(t => t.value > it.value).toList.toString())

      bestNeighbour.commit()
      sc.handlePossibleSolution()

    }else
      log("No move exists!")
    val modifiedVars = if(bestNeighbour!=null) bestNeighbour.getModified else Set.empty[CBLSIntVar]
    for (v <- modifiedVars) {
      val index = variableMap(v);
      //This could be it.value + tenure + random(tenureIncrement) to introduce more randomness
      //tabu(index) := it.value + tenure;
      tabu(index) := it.value + Math.min(MaxTenure, tenure + RandomGenerator.nextInt(tenureIncrement));
    }

    /*val hc = searchVariables.toList.map(_.value).hashCode
    if(visitedStates.contains(hc)){
      log("hash code: " + hc + " has been visited before - "+"Storing " + visitedStates.size + " violation: " +m.c.violation.value)
      //log(searchVariables.map(_.value).mkString(", "))
    }
    //log("Storing " + visitedStates.size + " solutions at it "+it.value)
    visitedStates += hc
*/

    it++
  }
}

class NeighbourhoodSearchOPT(m:FZCBLSModel, sc: SearchControl) extends NeighbourhoodTabuSearch(m,sc) {
  
  override def run()= {
    log("Starting Optimization Search")
    
    var bestNow = Int.MaxValue;
    var best = bestNow;
    var itSinceBest = 0;
    var numOfMaxTenure = 0;

    var timeOfBest = m.getWatch();
    var itOfBalance = 0;
    var minViolationSinceBest = Int.MaxValue;
    var minObjectiveSinceBest = Int.MaxValue;
    var lastMinObjective = Int.MinValue;
    

    var wait = 0;
    val waitDec = 1;


    //m.objective.objectiveWeight := 0;//TODO: What is this??? Remove it?
    while (!sc.stop()) {
      makeMove(true)

      //log("Violation " + m.c.violation.value)

      if (wait > 0) {
        wait -= waitDec;
      } else {
        itSinceBest += 1;
      }

      // Minimize the problem
      // There are two special cases to look out for here.
      // 1) The violation is within such a small range (compared with the objective) that the violation is ignored by the search.
      //	- This shows when the violation is above 0 for a long time (possibly forever) and the objective is at a "good" low value
      // 2) The violation can grow so quickly that it overshadows the objective (ie the opposite of 1).
      //  - This shows when the violation is 0 for a long time (possibly forever) and the objective does not decrease
      //
      // There is of course also the problem of the dynamic tenure behaving badly but that is waaaaay harder to detect and do something about.
      minViolationSinceBest = Math.min(minViolationSinceBest, m.c.violation.value)
      minObjectiveSinceBest = Math.min(minObjectiveSinceBest, m.objective.getObjectiveValue())
      if (m.objective.getObjectiveValue() < bestNow || (m.c.violation.value == 0 && m.objective.getObjectiveValue() < best)) {
        bestNow = m.objective.getObjectiveValue()
        tenure = Math.max(MinTenure, tenure - 1)
        if (m.c.violation.value == 0 && bestNow < best) {
          best = bestNow;
          timeOfBest = m.getWatch();
          itOfBalance = it.value
          minViolationSinceBest = Int.MaxValue
          minObjectiveSinceBest = Int.MaxValue
          lastMinObjective = bestNow;
          tenure = Math.max(MinTenure, tenure / 2)
        }
        itSinceBest = 0;
      }

      if (it.value - itOfBalance > baseSearchSize * 2 && wait == 0) {
        if (minViolationSinceBest > 0) { // 1)
          m.objective.increaseViolationWeight(minObjectiveSinceBest)
        } else if (bestNow <= lastMinObjective) { // 2)
          m.objective.increaseObjectiveWeight(minObjectiveSinceBest)
        }
        lastMinObjective = bestNow
        minViolationSinceBest = Int.MaxValue
        minObjectiveSinceBest = Int.MaxValue

        itOfBalance = it.value;
      }
      if (itSinceBest > tenure + baseSearchSize + searchFactor * (tenure / tenureIncrement)) {
        itSinceBest = 0;
        tenure = Math.min(MaxTenure, tenure + tenureIncrement);
        if (tenure == MaxTenure) {
          //Wait will be long enough to clear the tabu list.
          if (m.getWatch() - timeOfBest > sc.MaxTimeMilli / 8) {
            //println("% Reset");
            timeOfBest = m.getWatch();
            log("Reset neighourhoods")
            for (n <- neighbourhoods)
              n.reset();
          }
          wait = tenure + baseSearchSize;
          tenure = MinTenure;
          bestNow = m.objective.getObjectiveValue()
        }
      }
      if(m.getWatch() > timeOfBest + 300000){
        timeOfBest = m.getWatch()
        log("Reset neighourhoods 5 minutes")
        for (n <- neighbourhoods)
          n.reset();
      }
    }
    log("Completed Optimization Search at "+m.getWatch())
    log("nb moves "+ecnt+"\t"+bcnt)
  }
}

class NeighbourhoodSearchSAT(m:FZCBLSModel, sc: SearchControl) extends NeighbourhoodTabuSearch(m,sc) {
  override def run()= {
    log("Starting Satisfaction Search")
    var extendedSearch = false
    var roundsWithoutSat = 0
    val maxRounds = 5

    var timeOfBest = m.getWatch()
    var itSinceBest = 0
    var bestViolation = Int.MaxValue


    var wait = 0
    val waitDec = 1


    while (!sc.stop()) {
      makeMove(extendedSearch)
      if (wait > 0) {
        wait -= waitDec;
      } else {
        itSinceBest += 1;
      }
      if (m.c.violation.value < bestViolation) {
        bestViolation = m.c.violation.value
        itSinceBest = 0
        timeOfBest = m.getWatch()
        tenure = Math.max(MinTenure, tenure - 1)
        if (tenure == MinTenure) {
          extendedSearch = false;
        }
      }
      if (m.c.violation.value > bestViolation * 2) {
        extendedSearch = true;
      }

      //log(itSinceBest + ">" + (tenure + baseSearchSize + searchFactor * (tenure / tenureIncrement)))

      if (itSinceBest > tenure + baseSearchSize + searchFactor * (tenure / tenureIncrement)) {
        extendedSearch = true;
        itSinceBest = 0;
        tenure = Math.min(MaxTenure, tenure + tenureIncrement);
        if (tenure == MaxTenure) {
          //Wait will be long enough to clear the tabu list.
          wait = tenure + baseSearchSize;
          bestViolation = Int.MaxValue
          tenure = MinTenure;
          roundsWithoutSat += 1;
          if (roundsWithoutSat >= maxRounds) {
            log("Reset neighourhoods")
            for (n <- neighbourhoods)
              n.reset();
            roundsWithoutSat = 0;
            bestViolation = m.c.violation.value
          }
        }
      }
      if(m.getWatch() > timeOfBest + 300000){
        timeOfBest = m.getWatch()
        log("Reset neighourhoods 5 minutes")
        for (n <- neighbourhoods)
          n.reset();
      }
    }

    log("Completed Satisfaction Search at "+m.getWatch())
    log("nb moves "+ecnt+"\t"+bcnt)
  }
}

class NeighbourhoodSearchOPTbySAT(m:FZCBLSModel, sc: SearchControl) extends NeighbourhoodTabuSearch(m,sc) {
  override def run()= {
    log("Starting Optimisation by Satisfaction Search")
    var extendedSearch = false;
    var roundsWithoutSat = 0;
    val maxRounds = 5;

    var timeOfBest = m.getWatch();
    var itSinceBest = 0;
    var bestViolation = Int.MaxValue


    var wait = 0;
    val waitDec = 1;

    sc.cancelObjective()

    while (!(m.getWatch() >= sc.MaxTimeMilli || (m.objective.violation.value==0 && m.objective.getObjectiveValue() == sc.objLB))) {
      makeMove(extendedSearch)

      if (wait > 0) {
        wait -= waitDec;
      } else {
        itSinceBest += 1;
      }
      if (m.c.violation.value < bestViolation) {
        bestViolation = m.c.violation.value
        itSinceBest = 0;
        timeOfBest = m.getWatch()
        tenure = Math.max(MinTenure, tenure - 1)
        if (tenure == MinTenure) {
          extendedSearch = false;
        }
        if(m.c.violation.value == 0){
          bestViolation = Int.MaxValue
        /*  m.objective.objectiveBound := (m.fzModel.search.obj match{
            case FZObjective.MINIMIZE => m.objective.objectiveVar.value-1
            case FZObjective.MAXIMIZE => m.objective.objectiveVar.value+1
          })
          println("Objective bound is now: " +m.objective.objectiveBound.value)*/
        }
      }
      if (m.c.violation.value > bestViolation * 10) {
        extendedSearch = true;
      }
      if (itSinceBest > tenure + baseSearchSize + searchFactor * (tenure / tenureIncrement)) {
        extendedSearch = true;
        itSinceBest = 0;
        tenure = Math.min(MaxTenure, tenure + tenureIncrement);
        if (tenure == MaxTenure) {
          //Wait will be long enough to clear the tabu list.
          wait = tenure + baseSearchSize;
          bestViolation = Int.MaxValue
          tenure = MinTenure;
          roundsWithoutSat += 1;
          if (roundsWithoutSat >= maxRounds) {
            log("Reset neighourhoods")
            for (n <- neighbourhoods)
              n.reset();
            roundsWithoutSat = 0;
            bestViolation = m.c.violation.value
          }
        }
      }
      if(m.getWatch() > timeOfBest + 300000){
        timeOfBest = m.getWatch()
        log("Reset neighourhoods 5 minutes")
        for (n <- neighbourhoods)
          n.reset();
      }
    }
  }

  override def makeMove(extendedSearch: Boolean): Unit ={
    val nonTabuSet = nonTabuVariables.value.map(searchVariables(_).asInstanceOf[CBLSIntVar]);
    val bestValue = sc.weightedBest
    if(extendedSearch) ecnt+=1 else bcnt+=1
    val bestNeighbour = selectMin(neighbourhoods.map((n: Neighbourhood) =>
      if (extendedSearch) {
        n.getExtendedMinObjective(it.value, acceptMove(bestValue,nonTabuSet))
      } else {
        n.getMinObjective(it.value, acceptMove(bestValue,nonTabuSet))
      }))(_.value)
    if(bestNeighbour!=null){
      //TODO: Aspiration sometimes accepts moves that do not improve but seem to improve because of changing weights.
      if(log.level > 0 && bestNeighbour.getModified.forall(!nonTabuSet.contains(_))){
        log(2,"Aspiration");
        log(3,bestNeighbour.value.toString +" < "+bestValue.toString +" ; "+m.objective().value)
      }
      log(3,bestNeighbour.toString)
      log(4,tabu.filter(t => t.value > it.value).toList.toString())
      bestNeighbour.commit()
      sc.handlePossibleSolution()
    }else
      log("No move exists!")
    val modifiedVars = if(bestNeighbour!=null) bestNeighbour.getModified else Set.empty[CBLSIntVar]
    for (v <- modifiedVars) {
      val index = variableMap(v);
      //This could be it.value + tenure + random(tenureIncrement) to introduce more randomness
      //tabu(index) := it.value + tenure;
      tabu(index) := it.value + Math.min(MaxTenure, tenure + RandomGenerator.nextInt(tenureIncrement));
    }
    it++
  }
}

class GLSSAT(m:FZCBLSModel,sc: SearchControl) extends NeighbourhoodSearch(m,sc) {
  val it = CBLSIntVar(m.m, 1, 0 to Int.MaxValue, "it");

  def run(): Unit ={

    var improved = true

    var best = Int.MaxValue


    log("Starting GLSSAT Search")
    log("Starting Violation: "+m.objective.violation.value)

    sc.handlePossibleSolution();
    while(!sc.stop()) {
      improved = true
      while (improved && !sc.stop()) {
        improved = false

        val bestNeighbour =
          selectMin(neighbourhoods.map((n: Neighbourhood) =>
                                         n.getExtendedMinObjective(it.value, _.value < best)))(_.value)

        if (!bestNeighbour.isInstanceOf[NoMove]) {
          if(m.objective.objective.value > bestNeighbour.value){
            improved = true
          }
          bestNeighbour.commit()

          if (m.c.violation.value == 0) {
            sc.handlePossibleSolution()
          }
          //log("improved violation: " + m.objective.violation.value)
          best = m.objective.objective.value
        }
      }
      sc.handlePossibleSolution()
      if (m.objective.violation.value > 0) {

        /*val  (c,w) = selectMax(m.c.getPostedConstraints,
                               (t:(Constraint,IntValue)) =>
                                 ((t._1.violation.value.toFloat/t._2.value)*100).toInt,
                               (t:(Constraint,IntValue)) =>
                                 t._1.violation.value>0)
        if (w.isInstanceOf[CBLSIntVar]) {
          w.asInstanceOf[CBLSIntVar] :+= c.violation.value
        }*/

        //log("violation before weights increased: " + m.objective.violation.value)
        for ((c, w) <- m.c.getPostedConstraints) {
          if (c.violation.value > 0) {
            if (w.isInstanceOf[CBLSIntVar]) {
              w.asInstanceOf[CBLSIntVar] :+= c.violation.value
            }
          }
        }

        //log("violation after weights increased: " + m.objective.violation.value)
        if (RandomGenerator.nextInt(1000) < 2) {
          neighbourhoods.foreach(_.reset())
          log("Rest neighbourhoods")
        }
        //log("Best " + best)
        best = m.objective.objective.value
        //log("New Best " + best)
      }
    }



    log("Done GLSSAT at "+m.getWatch())
    log("Ending Violation: "+m.objective.violation.value)
    log("Nb Moves: "+it)
  }
}

class RestrictedNeighbourhoodSearch(m:FZCBLSModel, sc: SearchControl) extends NeighbourhoodTabuSearch(m,sc) {
  override def run()= {
    log("Starting Restricted Neighbourhood Search")
    var extendedSearch = false;
    var roundsWithoutSat = 0;
    val maxRounds = 5;

    var timeOfBest = m.getWatch();
    var itSinceBest = 0;
    var bestViolation = Int.MaxValue


    var wait = 0;
    val waitDec = 1;

    sc.cancelObjective()

    while (!(m.getWatch() >= sc.MaxTimeMilli || (m.objective.violation.value==0 && m.objective.getObjectiveValue() == sc.objLB))) {
      makeMove(extendedSearch)

      if (wait > 0) {
        wait -= waitDec;
      } else {
        itSinceBest += 1;
      }
      if (m.c.violation.value < bestViolation) {
        bestViolation = m.c.violation.value
        itSinceBest = 0;
        timeOfBest = m.getWatch()
        tenure = Math.max(MinTenure, tenure - 1)
        if (tenure == MinTenure) {
          extendedSearch = false;
        }
        if(m.c.violation.value == 0){
          bestViolation = Int.MaxValue
          /*  m.objective.objectiveBound := (m.fzModel.search.obj match{
              case FZObjective.MINIMIZE => m.objective.objectiveVar.value-1
              case FZObjective.MAXIMIZE => m.objective.objectiveVar.value+1
            })
            println("Objective bound is now: " +m.objective.objectiveBound.value)*/
        }
      }
      if (m.c.violation.value > bestViolation * 10) {
        extendedSearch = true;
      }
      if (itSinceBest > tenure + baseSearchSize + searchFactor * (tenure / tenureIncrement)) {
        extendedSearch = true;
        itSinceBest = 0;
        tenure = Math.min(MaxTenure, tenure + tenureIncrement);
        if (tenure == MaxTenure) {
          //Wait will be long enough to clear the tabu list.
          wait = tenure + baseSearchSize;
          bestViolation = Int.MaxValue
          tenure = MinTenure;
          roundsWithoutSat += 1;
          if (roundsWithoutSat >= maxRounds) {
            log("Reset neighourhoods")
            for (n <- neighbourhoods)
              n.reset();
            roundsWithoutSat = 0;
            bestViolation = m.c.violation.value
          }
        }
      }
      if(m.getWatch() > timeOfBest + 300000){
        timeOfBest = m.getWatch()
        log("Reset neighourhoods 5 minutes")
        for (n <- neighbourhoods)
          n.reset();
      }
    }
  }

  override def makeMove(extendedSearch: Boolean): Unit ={
    val nonTabuSet = nonTabuVariables.value.map(searchVariables(_).asInstanceOf[CBLSIntVar]);
    val bestValue = sc.weightedBest
    if(extendedSearch) ecnt+=1 else bcnt+=1
    val bestNeighbour = selectMin(neighbourhoods.map((n: Neighbourhood) =>
                                                       if (extendedSearch) {
                                                         n.getExtendedMinObjective(it.value, acceptMove(bestValue,nonTabuSet))
                                                       } else {
                                                         n.getMinObjective(it.value, acceptMove(bestValue,nonTabuSet))
                                                       }))(_.value)
    if(bestNeighbour!=null){
      //TODO: Aspiration sometimes accepts moves that do not improve but seem to improve because of changing weights.
      if(log.level > 0 && bestNeighbour.getModified.forall(!nonTabuSet.contains(_))){
        log(2,"Aspiration");
        log(3,bestNeighbour.value.toString +" < "+bestValue.toString +" ; "+m.objective().value)
      }
      log(3,bestNeighbour.toString)
      log(4,tabu.filter(t => t.value > it.value).toList.toString())
      bestNeighbour.commit()
      sc.handlePossibleSolution()
    }else
      log("No move exists!")
    val modifiedVars = if(bestNeighbour!=null) bestNeighbour.getModified else Set.empty[CBLSIntVar]
    for (v <- modifiedVars) {
      val index = variableMap(v);
      //This could be it.value + tenure + random(tenureIncrement) to introduce more randomness
      //tabu(index) := it.value + tenure;
      tabu(index) := it.value + Math.min(MaxTenure, tenure + RandomGenerator.nextInt(tenureIncrement));
    }
    it++
  }
}