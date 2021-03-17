package oscar.cp.scheduling.precedencegraph.nogoods

class NoGoodWithWatchedLiterals(lits : Seq[PrecedenceLiteralWatcher]) extends NoGood(lits) {

  val watchedLiterals = lits.toArray

  var watchedLiteral1 : PrecedenceLiteralWatcher = lits(0)
  var watchedLiteral2 : PrecedenceLiteralWatcher = lits(lits.length - 1)

  override def toString: String = {
    s"Size: $size || " + watchedLiterals.mkString(" ^ ") + "\t | watchers are w1 : " + watchedLiteral1 + " & w2 : " + watchedLiteral2
  }

  override def size = watchedLiterals.length
}
