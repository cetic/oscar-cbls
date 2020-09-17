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
package oscar.cbls.lib.search.neighborhoods

import oscar.cbls.core.computation.{CBLSIntVar, Domain, IntValue, Store}
import oscar.cbls.core.objective.Objective
import oscar.cbls.lib.invariant.numeric.Sum2

/**
  * this is a class that explores a range of value between 0L and maxValue.
  * It only needs to poll the value, not to return anything
  */
abstract class LinearOptimizer{
  /**
    * This is the method that linear selectors must implement.
    * it performs a search and has not other interface than this one.
    *
    * @param startPos the starting position in the search
    * @param startObj the starting objective value
    * @param minValue the minimal value to explore
    * @param maxValue the maximal value to explore
    * @param obj a function to evaluate a value; it returns an objective value (BEWARE this is time consuming!)
    * @return a value in the range (minValue,maxValue) and the objective value associated
    */
  def search(startPos: Long,
             startObj: Long,
             minValue: Long,
             maxValue: Long,
             obj: Long => Long):(Long, Long)

  def carryOnTo(b:LinearOptimizer) = new CarryOnTo(this,b)

  def andThen(b:LinearOptimizer) = AndThen(this,b)

  def restrictBounds(newMinValue:Long, newMaxValue:Long) = RestrictBounds(this, newMinValue:Long, newMaxValue:Long)

  def restrictSlide(maxIncrease:Long, maxDecrease:Long) = RestrictSlide(this, maxIncrease:Long, maxDecrease:Long)

  def modulo(modulo: Long, overlapPercentage:Int = 99) = new ModuloOptimizer(modulo, overlapPercentage,  baseOptimizer = this)
}

class CarryOnTo(a:LinearOptimizer, b:LinearOptimizer) extends LinearOptimizer{
  override def search(startPos: Long,
                      startObj: Long,
                      minValue: Long,
                      maxValue: Long,
                      obj: Long => Long): (Long, Long) = {
    val newStartPoint = a.search(startPos, startObj, minValue, maxValue, obj)
    //println("CarryOnTo(newStartPoint:" + newStartPoint + ")")
    b.search(newStartPoint._1, newStartPoint._2, minValue, maxValue, obj)
  }

  override def toString: String = s"($a carryOnTo $b)"
}

case class AndThen(a:LinearOptimizer, b:LinearOptimizer) extends LinearOptimizer{
  override def search(startPos: Long,
                      startObj: Long,
                      minValue: Long,
                      maxValue: Long,
                      obj: Long => Long): (Long, Long) = {

    var bestX = startPos
    var bestObj = startObj

    def exploreB(x:Long):Long = {
      val (newX,newObj) = b.search(x, obj(x), minValue, maxValue, obj)
      if (newObj < bestObj){
        bestX = newX
        bestObj = newObj
      }
      newObj
    }

    val newStartPoint = a.search(startPos, startObj, minValue, maxValue, exploreB)

    (bestX,bestObj)
  }
  override def toString: String = s"($a andThen $b)"
}

case class RestrictBounds(a:LinearOptimizer, newMinValue:Long, newMaxValue:Long) extends LinearOptimizer{
  override def search(startPos: Long, startObj: Long, minValue: Long, maxValue: Long, obj: Long => Long): (Long, Long) =
    a.search(startPos, startObj, minValue max newMinValue, maxValue min newMaxValue, obj)
}

case class RestrictSlide(a:LinearOptimizer, maxIncrease:Long, maxDecrease:Long) extends LinearOptimizer{
  override def search(startPos: Long, startObj: Long, minValue: Long, maxValue: Long, obj: Long => Long): (Long, Long) =
    a.search(startPos, startObj, minValue max (startPos - maxDecrease), maxValue min (startPos + maxIncrease), obj)
}

class Exhaustive(step:Long = 1L,skipInitial:Boolean = false, maxIt: Long) extends LinearOptimizer{

  require(step != 0,"Exhaustive Linear Optimizer with a step equal to 0 will make an endless loop")

  override def search(startPos: Long, startObj: Long, minValue: Long, maxValue: Long, obj: Long => Long): (Long, Long) = {

    var it = maxIt
    var bestX = startPos
    var bestF = startObj

    //this is a bit dirty, but ranges do not work when there are more than MaxInt elements
    var value = minValue
    while(value <= maxValue){
      
      if(it > 0L && (!skipInitial || value != startPos)){
        val newF = obj(value)

        if(newF < bestF){
          bestF = newF
          bestX = value
        }
      }
      it = it - 1L
      value += step
    }

    (bestX,bestF)
  }

  override def toString: String = s"Exhaustive(step:$step)"
}


class NarrowingStepSlide(dividingRatio:Long, minStep: Long) extends LinearOptimizer {

  override def toString: String = s"NarrowingStepSlide(dividingRatio:$dividingRatio)"

  override def search(startPos: Long, startObj: Long, minValue: Long, maxValue: Long, obj: Long => Long): (Long, Long) = {
    new SlideVaryingSteps(generateSteps((maxValue - minValue)/dividingRatio).reverse, false,Long.MaxValue).
      search(startPos, startObj, minValue, maxValue, obj)
  }

  private def generateSteps(maxStepSize:Long):List[Long] = {
    if(maxStepSize <= minStep) List(minStep)
    else if(maxStepSize < dividingRatio) List(1L)
    else  maxStepSize :: generateSteps(maxStepSize/dividingRatio)
  }
}

class NarrowingExhaustive(dividingRatio:Long, minStep: Long = 1,isLazy : Boolean = false) extends LinearOptimizer {

  require(minStep != 0,"Narrowing Exhaustive Linear Optimizer with a step equal to 0 will make an endless loop")

  override def toString: String = s"NarrowingExhaustive(dividingRatio:$dividingRatio minStep:$minStep)"

  override def search(startPos: Long, startObj: Long, minValue: Long, maxValue: Long, obj: Long => Long): (Long, Long) = {
    val width = maxValue - minValue

    if(width < dividingRatio) {
      val localExhaustiveSearch = new Exhaustive(step = minStep, skipInitial = true,maxIt = Long.MaxValue)
      localExhaustiveSearch.search(startPos, startObj, minValue, maxValue, obj)
    }else{
      val step = width/dividingRatio
      if(step <= minStep){
        //we have to do one search, with minStep, and return
        val localExhaustiveSearch = new Exhaustive(step = minStep, skipInitial = true, maxIt = Long.MaxValue)
        localExhaustiveSearch.search(startPos, startObj, minValue, maxValue, obj)
      }else{
        val localExhaustiveSearch = new Exhaustive(step = step, skipInitial = true, maxIt = Long.MaxValue)
        val (newVal, newObj) = localExhaustiveSearch.search(startPos, startObj, minValue, maxValue, obj)
        if (isLazy) {
          if (newObj <= startObj)
            this.search(newVal, newObj, minValue max (newVal - step), maxValue min (newVal + step), obj)
          else
            (newVal,newObj)
        }
        else
          this.search(newVal, newObj, minValue max (newVal - step), maxValue min (newVal + step), obj)
      }
    }
  }
}

class TryExtremes() extends LinearOptimizer {
  override def search(startPos: Long, startObj: Long, minValue: Long, maxValue: Long, obj: Long => Long): (Long, Long) = {
    //println("TryExtremes.search(startPos:" + startPos + " startObj:" + startObj +  " minValue:" + minValue + " maxValue:" + maxValue + ")")
    val tries:List[(Long,Long)] = List((startPos,obj(startPos)),(minValue,obj(minValue)),(maxValue,obj(maxValue)))
    //println("found: " + tries)
    tries.minBy(_._2)
  }

  override def toString: String = "TryExtremes()"
}


class SlideVaryingSteps(stepSequence:List[Long] = List(1L), gradualIncrease:Boolean, maxIt:Long) extends LinearOptimizer{
  override def search(startPos: Long, startObj: Long, minValue: Long, maxValue: Long, obj: Long => Long): (Long, Long) = {

    var positionOfBestSoFar:Long = startPos
    var bestObjSoFar:Long = startObj

    def myObj(v:Long):Long = {
      val toReturn = obj(v)
      if(toReturn < bestObjSoFar){
        positionOfBestSoFar = v
        bestObjSoFar = toReturn
      }
      toReturn
    }

    def recurExploreNoGradualIncrease(toExploreSteps:List[Long]): Unit ={
      toExploreSteps match{
        case Nil => ;
        case head::tail =>
          recurExploreNoGradualIncrease(tail)
          //bigger step did improve, try again this one step
          new Slide(head,maxIt = maxIt).search(positionOfBestSoFar, bestObjSoFar: Long, minValue: Long, maxValue: Long, myObj: Long => Long)
      }
    }

    def recurExploreGradualIncrease(toExploreSteps:List[Long]):Boolean = {
      toExploreSteps match{
        case Nil => false
        case head::tail =>

          new Slide(head,maxIt = maxIt).search(positionOfBestSoFar: Long, bestObjSoFar: Long, minValue: Long, maxValue: Long, myObj: Long => Long)

          if(positionOfBestSoFar != startPos) {
            //found something, no need to use bigger step
            return true
          }

          if(!recurExploreGradualIncrease(tail)) {
            //bigger step did not improve, no more try
            return false
          }

          //bigger step did improve, try again this one step

          new Slide(head,maxIt = maxIt).search(positionOfBestSoFar: Long, bestObjSoFar: Long, minValue: Long, maxValue: Long, myObj: Long => Long)
          true
      }
    }
    if(gradualIncrease) {
      recurExploreGradualIncrease(stepSequence)
    }else{
      recurExploreNoGradualIncrease(stepSequence)
    }

    (positionOfBestSoFar,bestObjSoFar)
  }
}

class Slide(step:Long = 1L, maxIt: Long) extends LinearOptimizer {

  override def toString: String = s"Slide(step:$step)"

  override def search(startPos: Long, startObj: Long, minValue: Long, maxValue: Long, obj: Long => Long): (Long, Long) = {

    var currentPoint = startPos
    var currentValue = startObj

    val pointAbove = maxValue min (currentPoint + step)
    val valueAbove = obj(pointAbove)

    val pointBelow = minValue max (currentPoint - step)
    val valueBelow = obj(pointBelow)

    if((valueAbove min valueBelow) >= currentValue) {
      //println("not moving")
      return (currentPoint,currentValue)
    }

    val goingUp = valueAbove < valueBelow

    if(goingUp){
      // println("going up")
      currentPoint = pointAbove
      currentValue = valueAbove
    }else{
      //println("going down")
      currentPoint = pointBelow
      currentValue = valueBelow
    }
    var it = maxIt
    while(it > 0L){

      //println("nextPoint:" + currentPoint + " nextValue:" + currentValue)

      it -= 1L
      val nextPoint = if(goingUp){
        if(currentPoint == maxValue) return (currentPoint,currentValue)
        maxValue min (currentPoint + step)
      }else{
        if(currentPoint == minValue) return (currentPoint,currentValue)
        minValue max (currentPoint - step)
      }
      val nextValue = obj(nextPoint)

      if(nextValue >= currentValue){
        //println("stopped")
        return (currentPoint,currentValue)
      } else{
        currentPoint = nextPoint
        currentValue = nextValue
      }
    }
    (currentPoint,currentValue)
  }
}

//this one finds a root!!!
class NewtonRaphsonRoot(dXForDetivativeEvalution:Long,
                        maxIt: Long) extends LinearOptimizer{

  override def toString: String = "NewtonRaphsonRoot(dXForDetivativeEvalution:" + dXForDetivativeEvalution + ")"

  override def search(startPos: Long, startObj: Long, minValue: Long, maxValue: Long, obj: Long => Long): (Long, Long) = {

    var it = maxIt
    var x = startPos
    var fdx = startObj
    while(it > 0L) {
      it -= 1L
      val f = obj(x)
      //println("iterate x:" + x + " f:" + f)

      val fPdx = obj(x + dXForDetivativeEvalution)
      val fMdx = obj(x - dXForDetivativeEvalution)
      if (fPdx > f && fMdx > f) {
        //best is closer to x than dx, we stop here.
        return (x,f)
      }

      val slope:Double = (fPdx - fMdx).toDouble / (2L*dXForDetivativeEvalution)
      val newX = (x - (f / slope)).toLong
      if(x == newX) return (x,f)
      x = newX
      fdx = f
    }
    (x,fdx)
  }
}


//this one finds a min!!!
class NewtonRaphsonMinimize(dXForDerivativeEvalution:Long,
                            maxIt: Long) extends LinearOptimizer {

  override def toString: String = s"NewtonRaphson(dXForDerivativeEvalution:$dXForDerivativeEvalution)"

  override def search(startPos: Long, startObj: Long, minValue: Long, maxValue: Long, obj: Long => Long): (Long, Long) = {
    var positionOfBestSoFar:Long = startPos
    var bestObjSoFar:Long = startObj

    def myObj(v:Long):Long = {
      val toReturn = obj(v)
      if(toReturn < bestObjSoFar && minValue <= v && v <= maxValue ){
        positionOfBestSoFar = v
        bestObjSoFar = toReturn
      }
      toReturn
    }

    val evaluate2: Long => Long = x => {
      val f = myObj(x)
      val fpdx = myObj(x + dXForDerivativeEvalution)

      ((fpdx - f).toDouble / dXForDerivativeEvalution) toLong
    }

    new NewtonRaphsonRoot(dXForDerivativeEvalution: Long, maxIt: Long).search(startPos: Long, evaluate2(startPos), minValue: Long, maxValue: Long, evaluate2)
    (positionOfBestSoFar,bestObjSoFar)
  }
}

class ModuloOptimizer(modulo: Long, overlapPercentage:Int = 99, baseOptimizer:LinearOptimizer)
  extends LinearOptimizer {

  require(overlapPercentage <= 99, s"overlapPercentage should be < 99, got $overlapPercentage")
  require(overlapPercentage >= 0, s"overlapPercentage should be >=0 , got $overlapPercentage")

  /**
    * This is the method that linear selectors must implement.
    * it performs a search and has not other interface than this one.
    *
    * @param startPos the starting position in the search
    * @param startObj the starting objective value
    * @param minValue the minimal value to explore
    * @param maxValue the maximal value to explore
    * @param obj a function to evaluate a value; it returns an objective value (BEWARE this is time consuming!)
    * @return a value in the range (minValue,maxValue) and the objective value associated
    */
  override def search(startPos: Long, startObj: Long, minValue: Long, maxValue: Long, obj: Long => Long): (Long, Long) = {

    require(modulo <= (maxValue - minValue))

    // Halfwidth spans on both sides of startPos
    val halfWidth:Long = scala.math.ceil(modulo.toFloat * (0.5 + (overlapPercentage / 200))).toLong

    def valToModulo(v: Long): Long = {
      if (v > maxValue) v - modulo
      else if (v < minValue) v + modulo
      else v
    }

    val (newVal,newObj) = baseOptimizer.search(
      startPos,
      startObj,
      minValue = startPos - halfWidth,
      maxValue = startPos + halfWidth,
      obj = (v: Long) => obj({
        val x = valToModulo(v)
        require(x <= maxValue)
        if(minValue > x) {
          require(minValue <= x, s"minValue:$minValue should be <= x:$x")
        }
        x
      })
    )

    (valToModulo(newVal),newObj)
  }

  override def toString: String = s"Modulo($modulo, $overlapPercentage, $baseOptimizer)"
}

object TestRN extends App{

  def f1:Long => Long = x => {x*x - 150L*x + 5090L}
  def f2:Long => Long = x => {-150L*x + 5090L}
  def f3:Long => Long = x => {(math.cos(x)*500L).toLong - 150L*x + 5090L}

  val f = f1
  val maxIt = 100L

  def eval(l:LinearOptimizer): Unit ={
    val aa = l.search(0L, f(0L), -1000L, 15000L, f)
    println(l + " " + aa)
  }

  //slide should be avoided at all cost; cfr the stop criterion on numerical methods that stop earlier.
  //we should consider numbers as floats even if they are not because the range of value is very large.
  eval(new Exhaustive(step = 50L, maxIt = maxIt) carryOnTo new Slide(step = 1L, maxIt: Long))
  eval(new NewtonRaphsonMinimize(1L, maxIt: Long) carryOnTo new  TryExtremes())
  eval(new Slide(step = 10L, maxIt: Long))
  eval(new NarrowingStepSlide(10L, minStep = 1L))
  eval(new NewtonRaphsonMinimize(1L, maxIt: Long) carryOnTo new Slide(step = 1L, maxIt: Long))
  eval(new Exhaustive(step = 50L, maxIt = maxIt) carryOnTo new NewtonRaphsonMinimize(1L, maxIt: Long) carryOnTo new Slide(step = 1L, maxIt: Long))
  eval(new Exhaustive(step = 50L, maxIt = maxIt) andThen (new NewtonRaphsonMinimize(1L, maxIt: Long) carryOnTo new Slide(step = 1L, maxIt: Long)))
  eval(new TryExtremes() carryOnTo new NewtonRaphsonMinimize(1L, maxIt: Long) carryOnTo new Slide(step=1L, maxIt: Long))
  eval(new NarrowingExhaustive(100L, minStep = 1))
  eval(new ModuloOptimizer(360, baseOptimizer = new NewtonRaphsonMinimize(1, maxIt)))

}

object ParaboloideTest extends App{

  val m = Store()

  val x = CBLSIntVar(m,10,Domain(0,1000),"x")
  val y = new CBLSIntVar(m,10,Domain(0,1000),"y")

  val a:IntValue = (x - 300L) * (x - 300L)
  val b:IntValue = (y - 100L) * (y - 100L)
  val f:IntValue = Sum2(a,b)
  val obj = Objective(f)

  m.close()
  printModel()

  def printModel(): Unit ={
    println(x)
    println(y)
    println("f:" + f)
  }
  val search = DiscretizedDirectionGradient(Array(x,y),
    selectVars = 0 to 1,
    variableIndiceToDeltaForGradientDefinition = _ => 10L,
    linearSearchForGradientDescent = new NarrowingStepSlide(3L, minStep = 1L),
    trySubgradient = true)

  search.verbose = 2

  search.doAllMoves(obj = obj)

  printModel()
}
