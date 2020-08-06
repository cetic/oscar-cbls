package oscar.cbls.lib.search.neighborhoods

import oscar.cbls.core.computation.{CBLSIntVar, Variable}
import oscar.cbls.core.search.{EasyNeighborhoodMultiLevel, First, LoopBehavior, Move}

case class HyperCube(vars:Array[CBLSIntVar],
                     name:String = "HyperCubeNeighborhood",
                     maxNbVars:Int = Integer.MAX_VALUE,
                     selectVars:Option[() => Iterable[Long]] = None,
                     variableIndiceToDelta:Long => Long,
                     globalLoopBehavior:LoopBehavior = First())
  extends EasyNeighborhoodMultiLevel[CubeAssignMove](name){

  var currentDeltas:List[CubeAssign] = Nil

  /**
    * This is the method you must implement and that performs the search of your neighborhood.
    * every time you explore a neighbor, you must perform the calls to notifyMoveExplored or moveRequested(newObj) && submitFoundMove(myMove)){
    * as explained in the documentation of this class
    */
  override def exploreNeighborhood(initialObj: Long): Unit = {

    val (iterator,notifyFound) = globalLoopBehavior.toIterator(0 until Int.MaxValue)

    currentDeltas = Nil

    //on itère sur toutes les variables

    //on teste +delta et - delta en construisant un cube.
    //on arrête dès qu'on a trouvé une pente négative ou on prend la meilleure
    //il y a donc un first/best

    //pour itérer,on doit faire un truc récursif?
    //peut-on faire par nombre de variable incluse?
    //oui; probablement plus rapide d'ailleurs.

    ///returns true if shouldStop
    def exploreVarsTrueIfShouldStop(varsToTest:List[Int],
                                    maxNbVarsToTest:Int): Boolean ={

      if(currentDeltas.nonEmpty){
        //We can test the current point
        val newObj = obj.value
        iterator.next
        if (evaluateCurrentMoveObjTrueIfSomethingFound(newObj)) {
          notifyFound()
        }
        if(!iterator.hasNext) return true // should stop here
      }

      if(maxNbVarsToTest == 0) return false
      if(varsToTest.isEmpty) return false

      val currentVarID::tail = varsToTest

      val initVal = vars(currentVarID).newValue
      val delta = variableIndiceToDelta(currentVarID)

      //first: no change on this var
      if(exploreVarsTrueIfShouldStop(tail, maxNbVarsToTest)){
        return true
      }

      //2: +delta
      if(initVal + delta < vars(currentVarID).max){
        //We can explore + delta
        vars(currentVarID) :+= delta
        val component = CubeAssign(vars(currentVarID), currentVarID, initVal + delta)

        currentDeltas = component :: currentDeltas
        if(exploreVarsTrueIfShouldStop(tail, maxNbVarsToTest-1)){
          vars(currentVarID) := initVal
          return true
        }
        currentDeltas = currentDeltas.tail
        vars(currentVarID) := initVal
      }

      //3: -delta
      if(vars(currentVarID).min < initVal - delta){
        //We can explore - delta
        vars(currentVarID) :-= delta
        val component = CubeAssign(vars(currentVarID), currentVarID, initVal - delta)

        currentDeltas = component :: currentDeltas
        if(exploreVarsTrueIfShouldStop(tail, maxNbVarsToTest-1)){
          vars(currentVarID) := initVal
          return true
        }

        currentDeltas = currentDeltas.tail
        vars(currentVarID) := initVal
      }

      false
    }

    val allVarsToTest = selectVars match{
      case Some(s) => s().map(_.toInt).toList
      case None => vars.indices.toList
    }

    for(nbVar <- 1 to (maxNbVars min vars.length-1)){
      //il y a donc l'ensemble des sous-ensembles = 3^nbVars posibilités
      if(exploreVarsTrueIfShouldStop(
        varsToTest = allVarsToTest,
        nbVar))
        return
    }

    currentDeltas = Nil
  }

  override def instantiateCurrentMove(newObj: Long): CubeAssignMove =
    CubeAssignMove(currentDeltas, newObj, name)
}

case class CubeAssign(variable:CBLSIntVar, indice:Int, newValue:Long) {
  def commit(): Unit ={
    variable := newValue
  }

  override def toString: String = s"${variable.name}:=$newValue"
}

case class CubeAssignMove(l:List[CubeAssign], override val objAfter:Long, override val neighborhoodName:String = null)
  extends Move(objAfter, neighborhoodName){

  override def commit(): Unit = {
    l.foreach(_.commit())
  }

  override def toString: String = {
    s"${neighborhoodNameToString}CubeAssignMove(${l.map(_.toString).mkString(",")}$objToString)"
  }

  override def touchedVariables: List[Variable] = l.map(_.variable)
}
