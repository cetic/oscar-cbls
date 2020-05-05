package oscar.cbls.lib.search

import oscar.cbls.Solution
import oscar.cbls.algo.dll.{DLLStorageElement, DoublyLinkedList}
import oscar.cbls.algo.heap.BinomialHeapWithMove

class BiObjectiveSearch(globalMaxObj1:Long,
                        globalMinObj2:Long,
                        solutionAtMax1Mn2:Solution,
                        optimize:(Long,Solution) /*maxObj2*/ => Option[(Long,Long,Solution)],
                        stopSurface:Long = 1000,
                        maxPoints:Int = 1000,
                        verbose:Boolean = false) {

  var nextSquareUid:Int = 0
  //a square, anchored at a solution
  case class Square(obj1:Long,maxObj1:Long,
                    obj2:Long,minObj2:Long,
                    solution:Solution) {

    require(minObj2 <= obj2)
    require(maxObj1 >= obj1)

    val uid = nextSquareUid
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
  }

  implicit val A = new Ordering[Square]{
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
  def storeSquare(squareToStore:Square, after: DLLStorageElement[Square]): Unit ={
    squareToStore.elemInFront = squareList.insertAfter(squareToStore, after)
    if(squareToStore.surface!=0) squaresToDevelop.insert(squareToStore)
    remainingSurface += squareToStore.surface
    nbSquare += 1
  }

  def removeSquare(square:Square): Unit ={
    squaresToDevelop.deleteIfPresent(square)
    square.elemInFront.delete()
    remainingSurface -= square.surface
    nbSquare -= 1
  }

  def popFirstSquare():Square = {
    val square = squaresToDevelop.removeFirst()
    remainingSurface -= square.surface
    square
  }

  // //////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def pruneLeftSquares(currentSquare:Square, obj1:Long): Unit = {
    val currentPos = currentSquare.elemInFront

    if (obj1 <= currentSquare.maxObj1) {
      //something to do; start by removing it

      val predecessor = currentPos.prev

      removeSquare(currentSquare)

      currentSquare.rectifyOnNewObj1(obj1) match {
        case Some(trimmedSquare) =>
          storeSquare(trimmedSquare, predecessor)
        case None => ;
          //this one was completely deleted, so we carry on to the next
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
      if(verbose) println("inserting new square")
      storeSquare(squareToInsert,squareList.phantom.prev)
    }else{

      if (currentSquareForPruning.obj1 <= squareToInsert.obj1
        && currentSquareForPruning.obj2 <= squareToInsert.obj2) {
        //on oublie squareToInsert, elle est au mieux équivalente, au pire, dominée
        if(verbose) println("new square is dominated")
      }else if (currentSquareForPruning.obj1 >= squareToInsert.obj1
        && currentSquareForPruning.obj2 >= squareToInsert.obj2) {
        //currentSquareForPruning est au meux équivalent, au pire, dominé.
        // on supprime currentSquareForPruning et on hérite de ses valeurs dans currentSquare
        if(verbose) println("old square is dominated")

        val nextSquare = currentSquareForPruning.elemInFront.next.elem
        val enlargedSquareToInsert = squareToInsert.enlargeBounds(currentSquareForPruning)
        removeSquare(currentSquareForPruning)
        insertAndPruneRightSquares(nextSquare, enlargedSquareToInsert)

      }else{
        //ils sont incomparables
        //forcément, c'est le new qui est à gauche et au dessus, on l'insère avant le currentSquareForPruning
        require(squareToInsert.obj1 <= currentSquareForPruning.obj1)
        require(squareToInsert.obj2 >= currentSquareForPruning.obj2)

        if(verbose) println("inserting new square")
        storeSquare(squareToInsert,currentSquareForPruning.elemInFront.prev)
      }
    }
  }


  def paretoOptimize():List[(Long,Long,Solution)] = {

    //Store initial given solution
    val square1 = Square(
      globalMaxObj1, globalMaxObj1,
      globalMinObj2, globalMinObj2,
      solutionAtMax1Mn2)
    storeSquare(square1,squareList.phantom)

    //initialization, search for other extreme of he spectre
    val startSol = optimize(Long.MaxValue,solutionAtMax1Mn2).get
    val square = Square(
      startSol._1, globalMaxObj1,
      startSol._2, globalMinObj2,
      startSol._3)

    storeSquare(square,squareList.phantom)

    if(verbose) println("start")

    while ((!squaresToDevelop.isEmpty) && (remainingSurface > stopSurface) && (nbSquare < maxPoints)) {
      if(verbose) {
        println("loop")
        println("remainingSurface:" + remainingSurface)
        println("nbSquare:" + nbSquare)
      }

      require(remainingSurface == squareList.toList.map(square => square.surface).sum)
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

      optimize(c, Option(prev.next.elem).map(_.solution).getOrElse(solutionAtMax1Mn2)) match {
        case None => ;
        //nothing to do, we already updated the structures accordingly
        case Some((obj1, obj2, sol)) =>
          //we have a solution.
          //correct the front

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
      println("finished")
      println("remainingSurface:" + remainingSurface)
      println("nbSquare:" + nbSquare)
      println("squaresToDevelop.isEmpty:" + squaresToDevelop.isEmpty)
    }
    squareList.toList.map(square => (square.obj1,square.obj2,square.solution))
  }
}


