package oscar.cbls.routing.neighborhood

import oscar.cbls.routing.model
import oscar.cbls.modeling.Algebra._
import java.nio.file.OpenOption
import oscar.cbls.routing.model.{ PositionInRouteAndRouteNr, VRPObjective, MoveDescription, VRP }
import oscar.cbls.routing.model.UnroutedImpl
import oscar.cbls.routing.model.StrongConstraints
import oscar.cbls.routing.model.StrongConstraints

abstract class Move(val objAfter: Int, val vrp: VRP with MoveDescription) {
  def encodeMove()
  def doMove() {
    vrp.cleanRecordedMoves
    encodeMove
    vrp.commit(false)
  }
}

//c'est toujours le first improve, jamais le best improve.

abstract class Neighborhood() {

  /**
   * @param s the search zone, including the VRP that we are examining
   * @param moveAcceptor a function that given the old and new value of the objective function, tell whether the move is considered as an improvement or not.
   * @return
   */
  final def climbAll(s: SearchZone, moveAcceptor: (Int) => (Int) => Boolean = (oldVal) => (newVal) => newVal < oldVal): Int = {
    var toreturn = 0;

    while (doSearch(s, moveAcceptor, false).found) {
      toreturn += 1
    }
    return toreturn
  }

  /**
   * performs the first discovered move that yields an improvement
   * @param s the search zone, including the VRP that we are examining
   * @param moveAcceptor a function that given the old and new value of the objective function, tell whether the move is considered as an improvement or not.
   * @return true if a move vans discovered, false otherwise
   */
  final def climbFirst(s: SearchZone, moveAcceptor: (Int) => (Int) => Boolean = (oldVal) => (newVal) => newVal < oldVal): Boolean = {
    s.vrp.cleanRecordedMoves()
    doSearch(s, moveAcceptor, false).found
  }

  /**
   * search and returns the first improving move that yields an improvement, according to moveAcceptor
   * @param s
   * @return
   */
  final def firstImprovingMove(s: SearchZone, moveAcceptor: (Int) => (Int) => Boolean = (oldVal) => (newVal) => newVal < oldVal): Option[Move] = {
    s.vrp.cleanRecordedMoves()
    doSearch(s, moveAcceptor, true) match {
      case MoveFound(move) => Some(move)
      case _ => None
    }
  }

  final def bestImprovingMove(
    s: SearchZone,
    moveAcceptor: (Int) => (Int) => Boolean = (oldVal) => (newVal) => newVal < oldVal): Option[Move] = {
    var bestMove: Option[Move] = None
    var bestObj = Int.MaxValue
    while (true) {
      firstImprovingMove(s, moveAcceptor) match {
        case None => return bestMove
        case Some(move) if (move.objAfter < bestObj) =>
          bestMove = Some(move)
          bestObj = move.objAfter
        case _ => ()
      }
    }
    None
  }

  /**
   * effectue la recherche et s'arrête au premier mouvement trouvé qui améliore
   *
   * @param s the search zone, including the VRP that we are examining
   * @param returnMove true: returns first improving move false: perform first improving move
   * @return
   */
  protected def doSearch(s: SearchZone, moveAcceptor: (Int) => (Int) => Boolean, returnMove: Boolean): SearchResult

  abstract class SearchResult {
    def found: Boolean
  }
  case class MovePerformed() extends SearchResult {
    def found: Boolean = true
  }
  case class MoveFound(move: Move) extends SearchResult {
    def found: Boolean = true
  }
  case class NoMoveFound() extends SearchResult {
    def found = false
  }

  /**
   * this method evaluates the result of moveAcceptor(objectiveFunction) after having comited the encoded move
   * it return the result of this evaluation
   * it restores the state as it was before the move was comited (and records the move again)
   * except if StayIfImprove is set to true. In this case, it does not restore the state if  moveAcceptor(objectiveFunction) return true
   * Besides, it wipes out the move description
   * @param moveAcceptor says if the move is accepted or not
   * @param StayIfAccept
   * @param vrp
   * @return true if this improved, false otherwise, and the objective fucntion after the move
   */
  def checkEncodedMove(
    moveAcceptor: Int => Boolean,
    StayIfAccept: Boolean,
    vrp: VRPObjective with VRPObjective with StrongConstraints): (Boolean, Int) = {
//    println("dans checkEncodeMove AVANT commit: " + vrp.strongConstraints)
    vrp.commit(true)
//    println("dans checkEncodeMove APRES commit: " + vrp.strongConstraints)
    val obj = vrp.getObjectiveAfterRegisteredMove
    val accept = moveAcceptor(obj)
    if (accept & StayIfAccept) {
//      println("move accepted")
      (accept, obj)
    } else {
      vrp.undo(false)
      (accept, obj)
    }
  }
}

case class SearchZone(relevantNeighbors: (Int => Iterable[Int]),
  //This is a stateful iteration on nodes, it might be re-used, actually so only consume that you really examined
  primaryNodeIterator: Iterator[Int],
  vrp: VRP with VRPObjective with PositionInRouteAndRouteNr with MoveDescription)
