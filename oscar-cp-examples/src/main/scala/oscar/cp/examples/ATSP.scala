package oscar.cp.examples

import scala.io.Source
import oscar.cp._
import oscar.util._

/**
 * Asymmetric Traveling Salesman Problem
 * @author Pierre Schaus  pschaus@gmail.com
 */
object ATSP extends CPModel with App {

	//var lines = Source.fromFile("data/ATSP/ftv64.atsp").getLines.toArray
    var lines = Source.fromFile("data/ATSP/ftv64.atsp").getLines.toArray//
    //var lines = Source.fromFile("data/ATSP/ftv33.atsp").getLines.toArray//


    lines = lines.take(lines.size-1) // drop EOF
    val n = lines(3).split(":")(1).trim().toInt
    val dist = lines.drop(7).reduceLeft(_ + " " + _).split("[ ,\t]").toList.filterNot(_ == "").map(_.toInt)

    val distMatrixSucc = dist.sliding(n,n).toArray.map(_.toArray)

    distMatrixSucc.foreach(i => println(i.mkString("\t")))
    val succ = Array.fill(n)(CPIntVar(0 until n))
    val obj = CPIntVar(0 until 1000000)
    add(minCircuit(succ, distMatrixSucc,obj),Strong)

    minimize(obj)

    search {
      // Select the not yet bound city with the smallest number of possible successors
      selectMin(0 until n)(!succ(_).isBound)(succ(_).size) match {
        case None => noAlternative
        case Some(x) => {
          // Select the closest successors of the city x
          val v = selectMin(0 until n)(succ(x).hasValue(_))(distMatrixSucc(x)(_)).get
          branch(add(succ(x) === v))(add(succ(x) !== v))
        }
      }
    }

    val stat = start()
    println(stat)

}
