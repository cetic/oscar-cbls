package oscar.cbls.algo.sequence.affineFunction

/** Represents the shifting bijection of a subsequence.
  *
  * With each moved subsequence comes a [[Pivot]]. The subsequence starts at fromValue and end at
  * the next [[Pivot]]'s start (if one) We can get the new position of each element of the
  * subsequence by using the [[UnitaryAffineFunction]]
  *
  * @param fromValue
  *   The starting position of the subsequence
  * @param f
  *   The [[UnitaryAffineFunction]] attached to this subsequence
  */
class Pivot(val fromValue: Int, val f: UnitaryAffineFunction) {
  override def toString: String =
    "Pivot(from:" + fromValue + " " + f + " f(from)=" + f(fromValue) + "))"

  /** Flip this pivot */
  def flip(endX: Int): Pivot = {
    if (f.flip) {
      new Pivot(fromValue, UnitaryAffineFunction(f(endX) - fromValue, flip = false))
    } else {
      new Pivot(fromValue, UnitaryAffineFunction(fromValue + f(endX), flip = true))
    }
  }
}
