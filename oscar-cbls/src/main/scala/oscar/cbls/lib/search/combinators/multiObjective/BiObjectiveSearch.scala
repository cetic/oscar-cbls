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

package oscar.cbls.lib.search.combinators.multiObjective

import oscar.cbls.Solution
import oscar.cbls.algo.dll.{DLLStorageElement, DoublyLinkedList}
import oscar.cbls.algo.heap.BinomialHeapWithMove
import oscar.cbls.visual.SingleFrameWindow

/**
 * This is an implementation of the rectangle splitting heuristics for heuristic optimization presented in
 *     Matl, Piotr & Hartl, Richard & Vidal, Thibaut. (2017).
 *     Heuristic Rectangle Splitting: Leveraging Single-Objective Heuristics to Efficiently Solve Multi-Objective Problems.
 *
 * @param globalMaxObj1 the max value of objective 1
 * @param globalMinObj2 the min value of objective 2
 * @param solutionAtMax1Min2 the solution (max1,min2)
 * @param optimize a function that performs the optimization
 *                 it input a max value for obj2, and an initial solution.
 *                 This initial solution has obj2 < max2, and can be used as initial solution to perform the search.
 *                 It is expected to return a triplet with obj1,obj2 and the solution corresponding to these values.
 *                 In case the search cannot produce a suitable solution it returns None.
 *                 Notice that the absence of solution is unlikely
 *                 because the initial solution is actually acceptable, but will be filtered out by the Pareto filtering.
 * @param stopSurface this is a stop criterion based on the heuristics.
 *                    At any time, the remaining surface if the sum of all surface that must be explored.
 *                    If < stopSurface, the search is interrupted. to deactivate, set to zero.
 * @param maxPoints the max number of points that is searched. the search stops as soon as
 * @param verbose if true, prints the outcome of eery search
 *                outcome of search: storing new Pareto point, new point is dominated, removed dominated point
 *                metrics: remaining surface and number of points
 *                notice that the optimize method can also print verbose message; up to you)
 * @param visu true to display the Pareto front in real time
 * @param visuTitle the title to use for the visu
 * @param obj1Name the name of obj1, used for verbosities on the console and for the visu
 * @param obj2Name the name of obj2, used for verbosities on the console and for the visu
 * @param filterSquare an additional method that you can specify to filter away some squares of the search,
 *                     typically when you want to trade time for granularity of the Pareto front.
 */
class BiObjectiveSearch(globalMaxObj1:Long,
                        globalMinObj2:Long,
                        solutionAtMax1Min2:Solution,
                        optimize:(Long/*maxObj2*/,Solution)  => Option[(Long,Long,Solution)],
                        stopSurface:Long = 0,
                        maxPoints:Int = Int.MaxValue,
                        verbose:Boolean = false,
                        visu:Boolean = true,
                        visuTitle: String = "Pareto",
                        obj1Name: String = "obj1",
                        obj2Name: String = "obj2",
                        filterSquare:(Long,Long,Long,Long) => Boolean = (_:Long,_:Long,_:Long,_:Long) =>true,
                        stayAlive:Boolean = false
                       ) {

  val (plot,window) = if(visu) {
    val p = new PlotPareto(null,obj1Name,obj2Name)
    val window = SingleFrameWindow.show(p,visuTitle, width = 2000, height = 2000)
    (p,window)
  }else (null,null)

  var oldParetoPoints:List[(Long,Long)] = Nil

  var nbFilteredSquares = 0

  var nextSquareUid:Int = 0
  //a square, anchored at a solution
  case class Square(obj1:Long,maxObj1:Long,
                    obj2:Long,minObj2:Long,
                    solution:Solution) {

    require(minObj2 <= obj2)
    require(maxObj1 >= obj1)

    val uid: Int = nextSquareUid
    nextSquareUid = nextSquareUid+1

    var elemInFront:DLLStorageElement[Square] = null
    def surface:Long = (maxObj1 - obj1) * (obj2 - minObj2)
    def getUpperSquareAboveObj2Cut(obj2Cut:Long):Square =
      Square(obj1, maxObj1, obj2, obj2Cut+1 min obj2, solution)
    def rectifyOnNewObj1(newOBj1:Long):Option[Square] = {
      if(newOBj1-1 < obj1) None
      else Some(this.copy(maxObj1 = newOBj1-1))
    }
    def enlargeBounds(otherSquare:Square):Square = this.copy(
      maxObj1 = maxObj1 max otherSquare.maxObj1,
      minObj2 = minObj2 min otherSquare.minObj2
    )

    override def toString: String = "Square(" + obj1 + "," + maxObj1 + "," + obj2 + "," + minObj2 + "surf:" + surface + ")"

    def objString:String = "(" + obj1Name + ":" + obj1 + ";" + obj2Name + ":" + obj2 + ")"
  }

  implicit val A: Ordering[Square] = new Ordering[Square]{
    override def compare(x: Square, y: Square): Int = x.uid compare y.uid
  }

  // //////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  //this one uses a sortedMAp to perform removes, squares cannot be changed.
  //the squares to develop, sorted in decreasing surface
  val squaresToDevelop = new BinomialHeapWithMove[Square](getKey = - _.surface,maxPoints*2)

  //Stores the front in increasing obj1 order
  val squareList = new DoublyLinkedList[Square]

  // //////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  var remainingSurface:Long = 0
  var nbSquare:Int = 0
  def storeSquare(squareToStore:Square, after: DLLStorageElement[Square],isNew:Boolean = false): Unit = {
    if (verbose && isNew) println("non dominated solution " + squareToStore.objString)

    squareToStore.elemInFront = squareList.insertAfter(squareToStore, after)

    if (squareToStore.surface != 0) {
      if (filterSquare(squareToStore.obj1, squareToStore.maxObj1, squareToStore.obj2, squareToStore.minObj2)) {
        squaresToDevelop.insert(squareToStore)
        remainingSurface += squareToStore.surface
      } else {
        nbFilteredSquares += 1
      }
    }

    nbSquare += 1

    if(plot != null){
      plot.reDrawPareto(squareList.map(square => (square.obj1,square.obj2)), Some(oldParetoPoints))
    }
  }

  def removeSquare(square:Square): Unit ={
    if(squaresToDevelop.deleteIfPresent(square)){
      remainingSurface -= square.surface
    }
    square.elemInFront.delete()
    nbSquare -= 1
  }

  def popFirstSquare():Square = {
    val square = squaresToDevelop.removeFirst()
    remainingSurface -= square.surface
    square
  }

  def notifyDeleted(square:Square): Unit ={
    oldParetoPoints = (square.obj1,square.obj2) :: oldParetoPoints
    if(verbose) println("removed dominated solution  " + square.objString)
  }

  // //////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def pruneLeftSquares(currentSquare:Square, obj1:Long): Unit = {
    val currentPos = currentSquare.elemInFront

    if (obj1 <= currentSquare.maxObj1) {
      //something to do; start by removing it

      val predecessor = currentPos.prev

      removeSquare(currentSquare)

      //TODO: maybe we should extend the first remaining square in case some were removed?
      currentSquare.rectifyOnNewObj1(obj1) match {
        case Some(trimmedSquare) =>
          storeSquare(trimmedSquare, predecessor)
        case None => ;
          //this one was completely deleted, so we carry on to the next
          notifyDeleted(currentSquare)
          if (squareList.phantom != predecessor) {
            pruneLeftSquares(predecessor.elem, obj1: Long)
          }
      }
    }
  }

  def insertAndPruneRightSquares(currentSquareForPruning:Square,
                                 squareToInsert:Square): Unit = {

    if(currentSquareForPruning == null){
      //insert at last position
      storeSquare(squareToInsert,squareList.phantom.prev,isNew = true)
    }else{

      if (currentSquareForPruning.obj1 <= squareToInsert.obj1
        && currentSquareForPruning.obj2 <= squareToInsert.obj2) {
        //on oublie squareToInsert, elle est au mieux équivalente, au pire, dominée
        if(verbose) println("found dominated solution")
      }else if (currentSquareForPruning.obj1 >= squareToInsert.obj1
        && currentSquareForPruning.obj2 >= squareToInsert.obj2) {
        //currentSquareForPruning est au mieux équivalent, au pire, dominé.
        // on supprime currentSquareForPruning et on hérite de ses valeurs dans currentSquare

        notifyDeleted(currentSquareForPruning)
        val nextSquare = currentSquareForPruning.elemInFront.next.elem
        val enlargedSquareToInsert = squareToInsert.enlargeBounds(currentSquareForPruning)
        removeSquare(currentSquareForPruning)
        insertAndPruneRightSquares(nextSquare, enlargedSquareToInsert)

      }else{
        //ils sont incomparables
        //forcément, c'est le new qui est à gauche et au dessus, on l'insère avant le currentSquareForPruning
        require(squareToInsert.obj1 <= currentSquareForPruning.obj1)
        require(squareToInsert.obj2 >= currentSquareForPruning.obj2)

        storeSquare(squareToInsert,currentSquareForPruning.elemInFront.prev,isNew = true)
      }
    }
  }


  def paretoOptimize():List[(Long,Long,Solution)] = {

    val startSearchNanotime = System.nanoTime()

    //Store initial given solution
    val square1 = Square(
      globalMaxObj1, globalMaxObj1,
      globalMinObj2, globalMinObj2,
      solutionAtMax1Min2)

    storeSquare(square1,squareList.phantom,isNew=true)

    //initialization, search for other extreme of he spectre
    val startSol = optimize(Long.MaxValue,solutionAtMax1Min2).get
    val square = Square(
      startSol._1, globalMaxObj1,
      startSol._2, globalMinObj2,
      startSol._3)
    storeSquare(square,squareList.phantom,isNew=true)

    if(verbose) println("BiObjectiveSearch: Start front exploration")

    var foundPoints = 2

    def printStopCriterion(): Unit ={
      if(verbose) {
        println(s"stopCriterion(surface:$remainingSurface/$stopSurface nonDominatedSolutions:$nbSquare/$maxPoints toDevelop:${squaresToDevelop.size})")
      }
    }
    while ((!squaresToDevelop.isEmpty) && (remainingSurface > stopSurface) && (nbSquare < maxPoints)) {
      printStopCriterion()

      assert(remainingSurface == squaresToDevelop.getElements.toList.map(square => square.surface).sum)
      assert(nbSquare == squareList.size, "nbSquare:" + nbSquare + " != squareList.size:" + squareList.size)
      val currentSquareToSplit = popFirstSquare()

      val c = ((currentSquareToSplit.obj2 + currentSquareToSplit.minObj2) / 2.0).ceil.toLong

      //we remove the sol from the front
      val oldPrev = currentSquareToSplit.elemInFront.prev
      currentSquareToSplit.elemInFront.delete()
      nbSquare -= 1

      //add the split square back into the front
      val remainingUpperSquare = currentSquareToSplit.getUpperSquareAboveObj2Cut(c)

      storeSquare(remainingUpperSquare, oldPrev)
      val prev = remainingUpperSquare.elemInFront

      optimize(c, Option(prev.next.elem).map(_.solution).getOrElse(solutionAtMax1Min2)) match {
        case None => ;
        //nothing to do, we already updated the structures accordingly
        case Some((obj1, obj2, sol)) =>
          //we have a solution.
          //correct the front
          foundPoints +=1

          val squareOnTheRight = prev.next
          pruneLeftSquares(remainingUpperSquare, obj1) //this will correct the maxObj1 of the split square

          //this square, without any pruning
          val firstSolutionSquare = Square(
            obj1, currentSquareToSplit.maxObj1 max obj1, //we are not lucky at all
            obj2, currentSquareToSplit.minObj2 min obj2, //we can get below the anticipated min
            sol)

          insertAndPruneRightSquares(squareOnTheRight.elem, firstSolutionSquare)
      }
    }

    if(verbose) {
      printStopCriterion()
      println("elapsed(ms):" + ((System.nanoTime() - startSearchNanotime)/1000000).toInt)
      println("nbFoundSolutions:" + foundPoints)
      println("nbNonDominatedSolutions:" + nbSquare)
      println("removedDominatedSolutions:" + oldParetoPoints.size)
      println("foundDominatedSolutions:" + (foundPoints - nbSquare - oldParetoPoints.size))
      println("totalDominatedSolutions:" + (foundPoints - nbSquare))
      println("remainingSurface:" + remainingSurface)
      println("nbSquaresToDevelop:" + squaresToDevelop.size)
      println("filteredSquares:" + nbFilteredSquares)
    }

    if(!stayAlive) window.close()

    squareList.toList.map(square => (square.obj1,square.obj2,square.solution))
  }
}


