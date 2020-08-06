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

import oscar.cbls.algo.search.HotRestart
import oscar.cbls.core.computation.{CBLSIntVar, Variable}
import oscar.cbls.core.search.{Best, EasyNeighborhoodMultiLevel, LoopBehavior, Move}

import scala.collection.immutable.SortedSet

case class GradientComponent(variable:CBLSIntVar,
                             initValue:Long,
                             indice:Int,
                             slope:Double){
  require(slope != 0, "zero slope!")
  require(!slope.isInfinity, "infinite slope") //handles both + and - infty
  require(!slope.isNaN,"NAN slope")

  override def toString: String =
    s"GradientComponent(variable:$variable,var.max:${variable.max},var.min:${variable.min},initValue:$initValue,indice:$indice,slope:$slope,maxStep:$maxStep,minStep:$minStep)"

  def takeStep(step:Long): Unit ={
    variable := valueAfterStep(step)
  }

  def valueAfterStep(step:Long):Long = initValue + (step / slope).toLong

  def rollBack(): Unit ={
    variable := initValue
  }

  val bound1 = ((variable.max - initValue) * slope).toLong
  val bound2 = ((variable.min - initValue) * slope).toLong

  val (minStep, maxStep) = if (bound1 < bound2) (bound1, bound2) else (bound2, bound1)
  require(maxStep >=0, s"maxStep should be >=0, got:$maxStep $this")
  require(minStep <=0, s"minStep hould be <=0, got:$minStep $this")
}

/**
 *this gradient descent finds a direction by explicitely probing variables together, and not imdpendently as done in classic gradient descent.
 * for instance, let be (x,y), the neighborhood will esplore
 * (x,y+dy), (x,y-dy), (x+dx,y),(x+dx,y+dy),(x+dx,y-dy),(x-dx,y),(x-dx,y+dy),(x-dx,y-dy)
 * this makes it possible to circumvent situations where x and y
 * have non-negligible mixed term derivative wrt. the objective function.
 * The gradient descent selects the direction based on the best neighbors among he explored points,
 * and preforms a descent in this direction by applying the provided linear optimizer.
 * The directions that can be explored by the gradient descent are therefore discrete.
 *
 * The huge downside is that it creates a combinatorial combination;
 * if there are n dimensions, 3^n neighbors wil be probed
 * to mitigate this; some mechanisms are provided
 * such as the min and max number of variables to change in the probed neigbors.
 *
 * @param vars
 * @param name
 * @param maxNbVars
 * @param selectVars
 * @param variableIndiceToDeltaForGradientDefinition
 * @param linearSearchForGradientDescent
 * @param gradientSearchBehavior
 * @param trySubgradient
 */
case class DiscretizedDirectionGradient(vars:Array[CBLSIntVar],
                                        name:String = "GradientDescent",
                                        maxNbVars:Int = Integer.MAX_VALUE,
                                        minNbVar:Int = 1,
                                        selectVars:Iterable[Int],
                                        variableIndiceToDeltaForGradientDefinition:Long => Long,
                                        linearSearchForGradientDescent:LinearOptimizer,
                                        gradientSearchBehavior:LoopBehavior = Best(),
                                        trySubgradient:Boolean = false,
                                        hotRestartOnVariableSelection:Boolean = true)
  extends AbstractGradientDescent(vars:Array[CBLSIntVar],
    name:String,
    linearSearchForGradientDescent,
    trySubgradient)  {

  var firstIndiceInPreviousCall:Int = 0

  //TODO: il faut pouvoir spécifier des co-variance entre les variables d'input.
  //typiquement on va dire, sachant un gradient déjà sélectionné,
  // doit-on examiner les co-variances +, - ou les deux ou aucune?

  //comment faire?
  //sur base de l'indice de la première variable sélectionnée, on doit shifter tout le range du tableau.
  override def findGradient(initialObj: Long): List[GradientComponent] = {

    val (iterator,notifyFound) = gradientSearchBehavior.toIterator(0 until Int.MaxValue)

    var bestObj = initialObj
    var bestGradient:List[GradientComponent] = Nil

    def testCurrentGradientTrueIfExplorationMustContinue(currentGradient:List[GradientComponent]):Boolean = {
      if(currentGradient.isEmpty) return true

      //TODO: set here the global vars to instantiate the move in case a crossproduct is used
      val newObj = obj.value

      if(newObj < bestObj){
        bestObj = newObj
        bestGradient = currentGradient
        notifyFound()
      }

      iterator.next
      iterator.hasNext
    }

    //on itère sur toutes les variables

    //on teste +delta et - delta en construisant un cube.
    //on arrête dès qu'on a trouvé une pente négative ou on prend la meilleure
    //il y a donc un first/best

    //pour itérer,on doit faire un truc récursif?
    //peut-on faire par nombre de variable incluse?
    //oui; probablement plus rapide d'ailleurs.

    ///returns true if shouldStop
    def exploreVars(varsToTest:List[Int],
                    maxNbVarsToTest:Int,
                    currentGradient:List[GradientComponent]): Boolean ={

      if(maxNbVarsToTest == 0) {
        if(!testCurrentGradientTrueIfExplorationMustContinue(currentGradient)) return true
        else return false
      }

      if(varsToTest.isEmpty) return false

      val currentVarID::tail = varsToTest

      //first: no change on this var
      if(exploreVars(tail, maxNbVarsToTest, currentGradient)) return true

      val initVal = vars(currentVarID).newValue
      val delta = variableIndiceToDeltaForGradientDefinition(currentVarID)

      //2: +delta
      if(initVal + delta < vars(currentVarID).max){
        //We can explore + delta
        vars(currentVarID) :+= delta
        val component = GradientComponent(
          vars(currentVarID),
          initVal,
          currentVarID,
          1.0/delta)

        if(exploreVars(tail, maxNbVarsToTest-1, component :: currentGradient)){
          vars(currentVarID) := initVal
          return true
        }
        vars(currentVarID) := initVal
      }

      //3: -delta
      if(vars(currentVarID).min < initVal - delta){
        //We can explore - delta
        vars(currentVarID) :-= delta
        val component = GradientComponent(
          vars(currentVarID),
          initVal,
          currentVarID,
          -1.0/delta)

        if(exploreVars(tail, maxNbVarsToTest-1, component :: currentGradient)){
          vars(currentVarID) := initVal
          return true
        }
        vars(currentVarID) := initVal
      }

      false
    }

    for(nbVar <- minNbVar to (maxNbVars min (vars.length-1))){
      //il y a donc l'ensemble des sous-ensembles = 3^nbVars possibilités

      if(exploreVars(
        varsToTest =
          (if(hotRestartOnVariableSelection)
            HotRestart(selectVars.toList,firstIndiceInPreviousCall).toList
          else
            selectVars.toList.map(v => v)
            ),
        nbVar,
        currentGradient = Nil)) {

        if(bestGradient.nonEmpty) {
          firstIndiceInPreviousCall = bestGradient.last.indice
        }
        return bestGradient
      }
    }
    if(bestGradient.nonEmpty) {
      firstIndiceInPreviousCall = bestGradient.last.indice
    }
    bestGradient
  }
}

/**
 * this neighborhood performs a gradient descent.
 * it first sense each dimension independently, and then performs
 * a linear descent on the steepest identified gradient.
 * @param vars
 * @param name
 * @param maxNbVars
 * @param selectVars
 * @param variableIndiceToDeltaForGradientDefinition
 * @param hotRestart
 * @param linearSearch
 * @param trySubgradient
 */
case class GradientDescent(vars:Array[CBLSIntVar],
                           name:String = "GradientDescent",
                           maxNbVars:Int = Integer.MAX_VALUE,
                           selectVars:Iterable[Int],
                           variableIndiceToDeltaForGradientDefinition:Long => Long,
                           hotRestart:Boolean = true,
                           linearSearch:LinearOptimizer,
                           trySubgradient:Boolean = false)
  extends AbstractGradientDescent(vars:Array[CBLSIntVar],
    name:String,
    linearSearch,
    trySubgradient) {

  var startIndiceForHotRestart: Int = 0

  override def findGradient(initialObj: Long):List[GradientComponent] = {

    var toReturnGradient : List[GradientComponent] = Nil
    var selectedVarSet:SortedSet[Int] = SortedSet.empty

    val selectVarsWithHotRestart =
      if (hotRestart) HotRestart(selectVars, startIndiceForHotRestart)
      else selectVars

    val selectVarsIt = selectVarsWithHotRestart.iterator

    while (selectedVarSet.size < maxNbVars && selectVarsIt.hasNext) {
      val currentVarIndice = selectVarsIt.next
      startIndiceForHotRestart = currentVarIndice

      if (!(selectedVarSet contains currentVarIndice)) {
        val currentVar = vars(currentVarIndice)
        val deltaForVar = variableIndiceToDeltaForGradientDefinition(currentVarIndice)

        val oldVal = currentVar.newValue

        val slope: Double = {
          //valueAbove
          val valueAbove = oldVal + deltaForVar
          if (valueAbove >= currentVar.max) {
            //no point of increasing it; thy decreasing?
            val valueBelow = (oldVal - deltaForVar) max currentVar.min
            samplePoint = List(AssignMove(currentVar,valueBelow,-1,Long.MaxValue,"exploreGradient"))
            val objBelow = obj.assignVal(currentVar, valueBelow)
            samplePoint = Nil
            (objBelow - initialObj).toDouble / (valueBelow - oldVal).toDouble
          } else {
            samplePoint = List(AssignMove(currentVar,valueAbove,-1,Long.MaxValue,"exploreGradient"))
            val objAbove = obj.assignVal(currentVar, valueAbove)
            samplePoint = Nil
            (objAbove - initialObj).toDouble / (valueAbove - oldVal).toDouble
          }
        }

        if (slope != 0L &&
          (slope < 0L || ((oldVal - deltaForVar) > currentVar.min))
          && (slope > 0L || ((oldVal + deltaForVar) < currentVar.max))) {

          toReturnGradient =
            GradientComponent(
              currentVar,
              oldVal,
              currentVarIndice,
              slope) :: toReturnGradient
          selectedVarSet += currentVarIndice
        }
      }
    }

    if(selectVarsIt.hasNext){
      startIndiceForHotRestart = selectVarsIt.next
    }else{
      startIndiceForHotRestart = startIndiceForHotRestart + 1
    }

    toReturnGradient
  }
}

abstract class AbstractGradientDescent(vars:Array[CBLSIntVar],
                                       name:String,
                                       linearSearch:LinearOptimizer,
                                       trySubgradient:Boolean = false)
  extends EasyNeighborhoodMultiLevel[GradientMove](name) {

  private var gradientDefinition:List[GradientComponent] = List.empty
  private var currentStep:Long = 0
  protected var samplePoint:List[AssignMove] = List.empty

  def findGradient(initialObj: Long):List[GradientComponent]

  /**
   * This is the method you must implement and that performs the search of your neighborhood.
   * every time you explore a neighbor, you must perform the calls to notifyMoveExplored or moveRequested(newObj) && submitFoundMove(myMove)){
   * as explained in the documentation of this class
   */
  override def exploreNeighborhood(initialObj: Long): Unit = {
    //step1: interroger le gradient dans toutes les directions de selectedVars
    this.gradientDefinition = List.empty
    val gradientDefinition:List[GradientComponent] = findGradient(initialObj: Long)
    samplePoint = List.empty
    performDescent(initialObj: Long, gradientDefinition)
  }

  def performDescent(initialObj: Long,
                     initGradientDefinition:List[GradientComponent]): Unit ={
    this.gradientDefinition = initGradientDefinition
    currentStep = 0
    while (gradientDefinition.nonEmpty) {

      val minStep = gradientDefinition.map(_.minStep).max
      val maxStep = gradientDefinition.map(_.maxStep).min

      require(minStep < maxStep, s"minStep:$minStep should be < maxStep:$maxStep")

      def evaluateStep(step: Long): Long = {
        this.currentStep = step

        for (component <- gradientDefinition) {
          component.takeStep(step)
        }

        val newObj = obj.value

        for (component <- gradientDefinition) {
          component.rollBack()
        }

        evaluateCurrentMoveObjTrueIfSomethingFound(newObj)
        newObj
      }

      //step2: find proper step with numeric method considered
      val (bestStep, newObj) = linearSearch.search(0, initialObj, minStep, maxStep, evaluateStep)
      //we do not consider the value because it is saved through the evaluateStep method.

      if (moveHasBeenFound) {
        return
      } else if (trySubgradient) {

        //we remove the smallest slope because the absence of move is possibly due to numerical artifacts

        def abs(d: Double): Double = if (d < 0) -d else d

        val flattestSlope = gradientDefinition.map(s => abs(s.slope)).min

        gradientDefinition = gradientDefinition.filter(c => abs(c.slope) != flattestSlope)
      } else {
        return
      }
    }
  }

  override def instantiateCurrentMove(newObj: Long): GradientMove = {
    GradientMove(gradientDefinition, currentStep, samplePoint, newObj, name)
  }
}


case class GradientMove(gradientDefinition : List[GradientComponent], step:Long, simpleAffectMoves:List[AssignMove], override val objAfter:Long, override val neighborhoodName:String = null)
  extends Move(objAfter, neighborhoodName){

  override def commit(): Unit = {
    if(gradientDefinition.isEmpty){
      //still is the expore phase
      for(affect <- simpleAffectMoves){
        affect.commit()
      }
    }else{
      require(simpleAffectMoves.isEmpty)
      for(component <- gradientDefinition){
        component.takeStep(step)
      }
    }
  }

  def valueAfterOn(variable:CBLSIntVar):Option[Long] = {
    if(gradientDefinition.isEmpty){
      //still is the expore phase
      for(affect <- simpleAffectMoves){
        if (affect.i == variable){
          return Some(affect.value)
        }
      }
      None
    }else{
      require(simpleAffectMoves.isEmpty, s"moves and gradient?:$simpleAffectMoves")
      for(component <- gradientDefinition){
        if (component.variable == variable){
          return Some(component.valueAfterStep(step))
        }
      }
      None
    }
  }

  override def toString: String = {
    neighborhoodNameToString + "GradientMove(" + gradientDefinition.map(component => {
      val delta = (step / component.slope).toLong
      s"${component.variable.name}:+=$delta (slope:${component.slope})"
    } ).mkString("; ") + objToString + ")"
  }

  override def touchedVariables: List[Variable] = gradientDefinition.map(_.variable)
}
