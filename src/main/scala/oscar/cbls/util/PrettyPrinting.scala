package oscar.cbls.util

import java.util.concurrent.TimeUnit
import scala.annotation.tailrec
import scala.concurrent.duration._

object PrettyPrinting {
  @tailrec
  private def prettyRec(
    acc: List[FiniteDuration],
    remUnits: List[TimeUnit],
    rem: FiniteDuration,
    isPast: Boolean
  ): String = {
    remUnits match {
      case h :: t =>
        if (rem > Duration(1, h)) {
          val x = Duration(rem.toUnit(h).toLong, h)
          prettyRec(x :: acc, t, rem - x, isPast)
        } else {
          prettyRec(acc, t, rem, isPast)
        }
      case Nil =>
        acc.reverse.map(_.toString).mkString(" ") + (if (isPast) " ago" else "")
    }
  }

  def apply(duration: Duration): String = {
    val timeUnitList: List[TimeUnit] =
      List(DAYS, HOURS, MINUTES, SECONDS, MILLISECONDS, MICROSECONDS, NANOSECONDS)
    duration match {
      case Duration.Zero => duration.toString
      case f: FiniteDuration if f < Duration.Zero =>
        prettyRec(Nil, timeUnitList, f * -1, isPast = true)
      case f: FiniteDuration => prettyRec(Nil, timeUnitList, f, isPast = false)
      case Duration.Inf      => "infinite"
      case Duration.MinusInf => "minus infinite"
      case _                 => "undefined"
    }
  }
}
